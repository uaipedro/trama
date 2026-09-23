; Trama-Setup.iss — instalador Windows do trama, sem assinatura, sem
; privilégio de administrador. Requisitos críticos (ver
; docs/plans/2026-09-23-instalador-windows-design.md, "anti-Defender"):
;   - Este .exe não grava nenhum executável nosso em disco: ele baixa e
;     roda em modo silencioso o instalador OFICIAL do R (cloud.r-project.org)
;     e depois chama o Rscript.exe dele para instalar o trama.launcher e a
;     release. Nada de descompactar R portátil.
;   - Sem .vbs. Sem UPX. Sem compressão exótica (fica no padrão lzma2).
;   - VersionInfo completo, para reputação estável no SmartScreen — o exe
;     só muda quando a linha do R muda (ver RVersion abaixo).
;
; Compilado com `iscc Trama-Setup.iss` (Inno Setup 6.1+, por causa de
; CreateDownloadPage). Não foi compilado neste ambiente (Linux sem iscc);
; revisar a sintaxe Pascal Script numa máquina com Inno Setup antes de
; publicar (ver relatório da task 3.2).

#define AppName "Trama"
#define AppPublisher "Pedro Mambelli Fernandes"
#define AppURL "https://github.com/uaipedro/trama"
; Linha do R que este Setup instala. O AppVersion do instalador segue essa
; linha (major.minor), não a versão do trama — é o manifesto quem versiona
; o trama (release.json), e o Setup só precisa mudar quando a linha do R
; muda (troca_de_r em tl_status()).
; ATENÇÃO: bump RVersion/RVersionLinha aqui quando o campo "r" de
; release.json mudar de linha (ex.: 4.5.x -> 4.6.x). O manifesto é a fonte
; da verdade em runtime (bootstrap.R confere getRversion() contra ele e
; falha com mensagem clara se não bater); estas duas constantes só
; controlam qual R este .exe específico instala numa máquina limpa.
#define RVersion "4.5.1"
#define RVersionLinha "4.5"

[Setup]
AppId={{B6E3A6F4-6E9A-4C2B-9A9C-2E8B6C6C9C3E}
AppName={#AppName}
AppVersion={#RVersionLinha}
AppVerName={#AppName} (R {#RVersionLinha})
AppPublisher={#AppPublisher}
AppPublisherURL={#AppURL}
AppSupportURL={#AppURL}/issues
AppUpdatesURL={#AppURL}/releases
VersionInfoVersion={#RVersion}.0
VersionInfoCompany={#AppPublisher}
VersionInfoDescription=Instalador do Trama
VersionInfoProductName={#AppName}
VersionInfoCopyright=Copyright (C) 2026 {#AppPublisher}
DefaultDirName={localappdata}\Trama
DefaultGroupName={#AppName}
DisableProgramGroupPage=yes
DisableWelcomePage=no
PrivilegesRequired=lowest
PrivilegesRequiredOverridesAllowed=commandline
OutputBaseFilename=Trama-Setup
OutputDir=..\..\..\dist
SetupIconFile=trama.ico
UninstallDisplayIcon={app}\trama.ico
Compression=lzma2
SolidCompression=yes
WizardStyle=modern
ArchitecturesAllowed=x64compatible
ArchitecturesInstallIn64BitMode=x64compatible

[Languages]
Name: "portuguese"; MessagesFile: "compiler:Languages\Portuguese.isl"
Name: "english"; MessagesFile: "compiler:Default.isl"

[Files]
; Só scripts e o ícone — nenhum binário nosso. O instalador do R é baixado
; em tempo de instalação (ver [Code]), não embutido aqui.
Source: "..\bootstrap.R"; DestDir: "{app}"; Flags: ignoreversion
Source: "abrir.R"; DestDir: "{app}"; Flags: ignoreversion
Source: "trama.ico"; DestDir: "{app}"; Flags: ignoreversion

[Tasks]
Name: "desktopicon"; Description: "Criar um atalho na área de trabalho"; GroupDescription: "Atalhos adicionais:"; Flags: unchecked

[Icons]
Name: "{userprograms}\{#AppName}"; Filename: "{app}\R\bin\Rscript.exe"; Parameters: """{app}\abrir.R"""; IconFilename: "{app}\trama.ico"; WorkingDir: "{app}"
Name: "{userdesktop}\{#AppName}"; Filename: "{app}\R\bin\Rscript.exe"; Parameters: """{app}\abrir.R"""; IconFilename: "{app}\trama.ico"; WorkingDir: "{app}"; Tasks: desktopicon

[Run]
Filename: "{app}\R\bin\Rscript.exe"; Parameters: """{app}\abrir.R"""; Description: "Abrir o Trama agora"; Flags: postinstall nowait skipifsilent unchecked; WorkingDir: "{app}"

[UninstallDelete]
; Remove {app} inteiro — R, libs por release, estado, logs — mas NUNCA a
; pasta de projetos do usuário (Documentos\Trama), que fica fora de {app}.
Type: filesandordirs; Name: "{app}"

[Code]
var
  DownloadPage: TDownloadWizardPage;

// -- "x.y.z" -> "x.y" (mesma regra de tl_status()/troca_de_r: um patch
// novo do R, tipo 4.5.1 -> 4.5.2, não é uma linha diferente). -------------
function RMajorMinor(Versao: String): String;
var
  P1, P2: Integer;
begin
  Result := Versao;
  P1 := Pos('.', Versao);
  if P1 = 0 then Exit;
  P2 := Pos('.', Copy(Versao, P1 + 1, Length(Versao)));
  if P2 = 0 then Exit;
  Result := Copy(Versao, 1, P1 + P2 - 1);
end;

// -- R já instalado nesta {app}, e na linha certa? -------------------------
// `Exec()` sozinho não captura stdout do processo filho; para conferir a
// versão de verdade (não só "existe Rscript.exe") é preciso rodar via
// cmd.exe /C com redirecionamento para um arquivo, e ler esse arquivo com
// LoadStringFromFile — sem isso, um Rscript.exe de uma linha antiga (ex.:
// de uma instalação manual anterior) seria erradamente considerado "já
// instalado" e o Setup nunca atualizaria o R.
function PrecisaInstalarR(): Boolean;
var
  RscriptPath: String;
  ResultCode: Integer;
  VersaoOutput: AnsiString;
  TmpFile: String;
  CmdLine: String;
begin
  Result := True;
  RscriptPath := ExpandConstant('{app}\R\bin\Rscript.exe');
  if not FileExists(RscriptPath) then
    Exit;

  TmpFile := ExpandConstant('{tmp}\r-versao.txt');
  if FileExists(TmpFile) then
    DeleteFile(TmpFile);

  CmdLine := '/C ""' + RscriptPath + '" -e "cat(as.character(getRversion()))" > "' +
    TmpFile + '" 2>&1"';
  if Exec(ExpandConstant('{cmd}'), CmdLine, '', SW_HIDE, ewWaitUntilTerminated, ResultCode) then
  begin
    // LoadStringFromFile exige AnsiString no Inno Unicode (senão "Type
    // mismatch" na compilação) — VersaoOutput é AnsiString e convertido
    // para String (via String(...)) só na hora de comparar/usar.
    if (ResultCode = 0) and FileExists(TmpFile) and LoadStringFromFile(TmpFile, VersaoOutput) then
    begin
      if RMajorMinor(Trim(String(VersaoOutput))) = '{#RVersionLinha}' then
        Result := False;
    end;
  end;

  if FileExists(TmpFile) then
    DeleteFile(TmpFile);
end;

// -- Baixa e roda o instalador oficial do R, silencioso, por usuário -------
function InstalarR(): Boolean;
var
  ResultCode: Integer;
  InstaladorPath: String;
  UrlPrincipal, UrlFallback: String;
  Baixou: Boolean;
begin
  Result := False;
  InstaladorPath := ExpandConstant('{tmp}\R-{#RVersion}-win.exe');
  UrlPrincipal := 'https://cloud.r-project.org/bin/windows/base/old/{#RVersion}/R-{#RVersion}-win.exe';
  UrlFallback := 'https://cloud.r-project.org/bin/windows/base/R-{#RVersion}-win.exe';
  Baixou := False;

  DownloadPage.Clear;
  DownloadPage.Add(UrlPrincipal, 'R-{#RVersion}-win.exe', '');
  DownloadPage.Show;
  try
    try
      DownloadPage.Download;
      Baixou := True;
    except
      // 404 na URL "old" (acontece quando {#RVersion} é a versão atual do
      // R, que não fica no diretório /old/): tenta a URL genérica. O
      // fallback tem o próprio try/except — se ele também falhar, o erro
      // é tratado aqui mesmo (Baixou continua False) em vez de propagar
      // como uma janela de erro de runtime crua.
      try
        DownloadPage.Clear;
        DownloadPage.Add(UrlFallback, 'R-{#RVersion}-win.exe', '');
        DownloadPage.Download;
        Baixou := True;
      except
        Baixou := False;
      end;
    end;
  finally
    DownloadPage.Hide;
  end;

  if not Baixou then
  begin
    MsgBox('Não consegui baixar o instalador do R (tentei ' + UrlPrincipal +
      ' e ' + UrlFallback + '). Verifique sua conexão e rode o Setup de novo.',
      mbError, MB_OK);
    Exit;
  end;

  // Instalação oficial do R: silenciosa, por usuário (CURRENTUSER), sem
  // reiniciar, só o componente principal (sem 32-bit, sem HTML/manuais
  // extras) — dentro de {app}\R, nunca no Program Files do sistema.
  if not Exec(InstaladorPath,
    '/VERYSILENT /SUPPRESSMSGBOXES /CURRENTUSER /NORESTART ' +
    '/DIR="' + ExpandConstant('{app}') + '\R" /COMPONENTS="main,x64"',
    '', SW_HIDE, ewWaitUntilTerminated, ResultCode) then
  begin
    MsgBox('Não consegui iniciar o instalador do R. Baixe manualmente em ' +
      'cloud.r-project.org e tente de novo.', mbError, MB_OK);
    Exit;
  end;
  if ResultCode <> 0 then
  begin
    MsgBox('O instalador do R terminou com erro (código ' + IntToStr(ResultCode) +
      '). Tente rodar o Setup de novo.', mbError, MB_OK);
    Exit;
  end;

  DeleteFile(InstaladorPath);
  Result := True;
end;

// -- Roda bootstrap.R (instala trama.launcher + a release do manifesto) ----
// TRAMA_BOOTSTRAP_ARGS (variável de ambiente, opcional): argumentos extras
// repassados a bootstrap.R sem mudar este .iss — hoje só usado pelo CI
// (job `windows` de .github/workflows/installer.yml) para passar
// `--local <pasta>` e instalar os tarballs do job `pacotes` em vez de
// baixar do r-universe, que ainda não existe/está vazio em alguns
// releases. Fica vazia (comportamento normal) fora do CI.
function RodarBootstrap(): Boolean;
var
  ResultCode: Integer;
  RscriptPath, LogPath, ExtraArgs, Params: String;
begin
  Result := False;
  RscriptPath := ExpandConstant('{app}\R\bin\Rscript.exe');
  LogPath := ExpandConstant('{localappdata}\Trama\logs');

  Params := '"' + ExpandConstant('{app}\bootstrap.R') + '"';
  // GetEnv, não GetEnvironmentVariable (esse é o nome do Windows API/.NET;
  // a função de Pascal Script do Inno Setup é GetEnv — "Unknown identifier"
  // na compilação senão).
  ExtraArgs := GetEnv('TRAMA_BOOTSTRAP_ARGS');
  if ExtraArgs <> '' then
    Params := Params + ' ' + ExtraArgs;

  WizardForm.StatusLabel.Caption := 'Instalando o trama (pode levar alguns minutos)...';
  WizardForm.ProgressGauge.Style := npbstMarquee;
  try
    if not Exec(RscriptPath, Params,
        ExpandConstant('{app}'), SW_HIDE, ewWaitUntilTerminated, ResultCode) then
    begin
      MsgBox('Não consegui rodar o instalador do trama (bootstrap.R). ' +
        'Veja o log em ' + LogPath, mbError, MB_OK);
      Exit;
    end;
    if ResultCode <> 0 then
    begin
      MsgBox('A instalação do trama falhou (código ' + IntToStr(ResultCode) + '). ' +
        'Veja o log em ' + LogPath, mbError, MB_OK);
      Exit;
    end;
  finally
    WizardForm.ProgressGauge.Style := npbstNormal;
  end;

  Result := True;
end;

procedure InitializeWizard();
begin
  DownloadPage := CreateDownloadPage(SetupMessage(msgWizardPreparing),
    SetupMessage(msgPreparingDesc), nil);
end;

// -- Depois dos arquivos copiados: garante o R e roda o bootstrap. A etapa
// aparece como "instalando" na página de progresso padrão do Wizard.
procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssPostInstall then
  begin
    if PrecisaInstalarR() then
    begin
      if not InstalarR() then
      begin
        Abort;
      end;
    end;

    if not RodarBootstrap() then
    begin
      Abort;
    end;
  end;
end;
