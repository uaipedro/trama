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
; A instalação de verdade acontece em PrepareToInstall() (R + bootstrap.R),
; ANTES da cópia de arquivos — mudar {app} depois disso não faz sentido
; (o R já teria sido instalado no {app} escolhido no momento da leitura da
; página), então a página de escolha de pasta fica desligada (F).
DisableDirPage=yes
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
; Segunda cópia do MESMO arquivo, com "dontcopy": não é extraída para {app}
; durante a cópia normal (que só acontece DEPOIS de PrepareToInstall), mas
; fica disponível para ExtractTemporaryFile('bootstrap.R') dentro de
; PrepareToInstall, que roda o bootstrap ali (precisa dele antes da cópia
; normal existir).
Source: "..\bootstrap.R"; DestDir: "{tmp}"; Flags: dontcopy
Source: "abrir.R"; DestDir: "{app}"; Flags: ignoreversion
Source: "trama.ico"; DestDir: "{app}"; Flags: ignoreversion

[Tasks]
Name: "desktopicon"; Description: "Criar um atalho na área de trabalho"; GroupDescription: "Atalhos adicionais:"; Flags: unchecked

[Icons]
; runminimized: Rscript.exe é um executável de console (não GUI) — sem essa
; flag o atalho abre com uma janela de terminal preta visível por trás da
; janela do app até o Shiny estar de pé.
Name: "{userprograms}\{#AppName}"; Filename: "{app}\R\bin\Rscript.exe"; Parameters: "--vanilla ""{app}\abrir.R"""; IconFilename: "{app}\trama.ico"; WorkingDir: "{app}"; Flags: runminimized
Name: "{userdesktop}\{#AppName}"; Filename: "{app}\R\bin\Rscript.exe"; Parameters: "--vanilla ""{app}\abrir.R"""; IconFilename: "{app}\trama.ico"; WorkingDir: "{app}"; Tasks: desktopicon; Flags: runminimized

[Run]
Filename: "{app}\R\bin\Rscript.exe"; Parameters: "--vanilla ""{app}\abrir.R"""; Description: "Abrir o Trama agora"; Flags: postinstall nowait skipifsilent unchecked runminimized; WorkingDir: "{app}"

[UninstallDelete]
; Remove {app} inteiro — R, libs por release, estado, logs — mas NUNCA a
; pasta de projetos do usuário (Documentos\Trama), que fica fora de {app}.
Type: filesandordirs; Name: "{app}"

[Code]
var
  DownloadPage: TDownloadWizardPage;

// -- Mensagem de erro que NÃO trava o modo silencioso -----------------------
// MsgBox() comum ignora /SUPPRESSMSGBOXES (essa flag só suprime os MsgBox
// internos do próprio Setup) e fica esperando um clique que nunca chega no
// CI/instalação silenciosa — travando o processo até o timeout do runner.
// SuppressibleMsgBox() é a API do Inno Setup para uma caixa que RESPEITA
// /SUPPRESSMSGBOXES (some no silencioso, respondendo com o `Default`
// passado, aqui IDOK) e continua aparecendo normalmente na instalação
// interativa. Log() sempre grava a mensagem em Setup.log (--LOG=...),
// então a causa do erro fica registrada mesmo quando a caixa não aparece.
procedure ErroMsg(const Msg: String);
begin
  Log('ERRO: ' + Msg);
  SuppressibleMsgBox(Msg, mbError, MB_OK, IDOK);
end;

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
    ErroMsg('Não consegui baixar o instalador do R (tentei ' + UrlPrincipal +
      ' e ' + UrlFallback + '). Verifique sua conexão e rode o Setup de novo.');
    Exit;
  end;

  // Instalação oficial do R: silenciosa, por usuário (CURRENTUSER), sem
  // reiniciar, só o componente principal (sem 32-bit, sem HTML/manuais
  // extras) — dentro de {app}\R, nunca no Program Files do sistema.
  // /MERGETASKS="!desktopicon,!recordversion,!associate" desliga as tasks
  // padrão do instalador oficial do R (atalho de área de trabalho, gravar a
  // versão no registro, associar .RData ao R) — o Setup do trama tem o seu
  // próprio atalho e não quer nenhum rastro do R "solto" fora de {app}\R;
  // /NOICONS reforça isso para o grupo de atalhos do Menu Iniciar do R.
  // ATENÇÃO: nomes de task confirmados no fonte do instalador do R (NSIS,
  // `src/gnuwin32/installer/`) até a série 4.x — não foram checados contra
  // o instalador de {#RVersion} especificamente; se o Setup do R mudar de
  // task set numa versão futura, tasks desconhecidas em /MERGETASKS são
  // ignoradas silenciosamente pelo NSIS (não quebram a instalação), mas
  // deixam de fazer efeito — revisar se o R.exe instalado passar a criar
  // atalhos/ícones de novo.
  if not Exec(InstaladorPath,
    '/VERYSILENT /SUPPRESSMSGBOXES /CURRENTUSER /NORESTART /NOICONS ' +
    '/MERGETASKS="!desktopicon,!recordversion,!associate" ' +
    '/DIR="' + ExpandConstant('{app}') + '\R" /COMPONENTS="main,x64"',
    '', SW_HIDE, ewWaitUntilTerminated, ResultCode) then
  begin
    ErroMsg('Não consegui iniciar o instalador do R. Baixe manualmente em ' +
      'cloud.r-project.org e tente de novo.');
    Exit;
  end;
  if ResultCode <> 0 then
  begin
    ErroMsg('O instalador do R terminou com erro (código ' + IntToStr(ResultCode) +
      '). Tente rodar o Setup de novo.');
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
//
// `BootstrapPath`: caminho de bootstrap.R já extraído (ver PrepareToInstall,
// que usa ExtractTemporaryFile porque {app}\bootstrap.R ainda não existe
// nesse ponto — a cópia normal do [Files] só acontece depois).
//
// stdout/stderr do Rscript vão para {localappdata}\Trama\logs\
// bootstrap-console.log via `cmd /C ... > log 2>&1`, mesma técnica (e
// mesmo aninhamento de aspas) de PrecisaInstalarR: o /C precisa de um único
// par de aspas duplas envolvendo o comando inteiro quando ele já contém
// caminhos entre aspas, senão cmd.exe interpreta tudo depois do primeiro
// espaço fora de aspas como argumentos do próprio cmd, não do Rscript.
function RodarBootstrap(BootstrapPath: String): Boolean;
var
  ResultCode: Integer;
  RscriptPath, LogDir, ConsoleLog, ExtraArgs, CmdLine: String;
begin
  Result := False;
  RscriptPath := ExpandConstant('{app}\R\bin\Rscript.exe');
  LogDir := ExpandConstant('{localappdata}\Trama\logs');
  ForceDirectories(LogDir);
  ConsoleLog := LogDir + '\bootstrap-console.log';

  // GetEnv, não GetEnvironmentVariable (esse é o nome do Windows API/.NET;
  // a função de Pascal Script do Inno Setup é GetEnv — "Unknown identifier"
  // na compilação senão).
  ExtraArgs := GetEnv('TRAMA_BOOTSTRAP_ARGS');

  CmdLine := '/C ""' + RscriptPath + '" --vanilla "' + BootstrapPath + '"';
  if ExtraArgs <> '' then
    CmdLine := CmdLine + ' ' + ExtraArgs;
  CmdLine := CmdLine + ' > "' + ConsoleLog + '" 2>&1"';

  WizardForm.StatusLabel.Caption := 'Instalando o trama (pode levar alguns minutos)...';
  WizardForm.ProgressGauge.Style := npbstMarquee;
  try
    if not Exec(ExpandConstant('{cmd}'), CmdLine,
        ExpandConstant('{app}'), SW_HIDE, ewWaitUntilTerminated, ResultCode) then
    begin
      ErroMsg('Não consegui rodar o instalador do trama (bootstrap.R). ' +
        'Veja o log em ' + LogDir);
      Exit;
    end;
    if ResultCode <> 0 then
    begin
      ErroMsg('A instalação do trama falhou (código ' + IntToStr(ResultCode) + '). ' +
        'Veja o log em ' + LogDir);
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

// -- Instala o R (se preciso) e roda o bootstrap, ANTES da cópia normal dos
// arquivos do [Files] — não em CurStepChanged(ssPostInstall) como antes:
// no modo silencioso (/VERYSILENT /SUPPRESSMSGBOXES), qualquer MsgBox()
// comum trava o processo esperando um clique que nunca vem (o
// /SUPPRESSMSGBOXES só afeta os MsgBox INTERNOS do Setup, não os chamados
// pelo nosso [Code]); e devolver uma string de erro não vazia aqui aborta o
// Setup com código de saída != 0 no modo silencioso — o CI (e qualquer
// script) consegue detectar a falha pelo ExitCode do processo, sem
// depender de uma janela que nunca teria aparecido.
function PrepareToInstall(var NeedsRestart: Boolean): String;
var
  BootstrapPath: String;
begin
  Result := '';
  NeedsRestart := False;

  ForceDirectories(ExpandConstant('{app}'));

  if PrecisaInstalarR() then
  begin
    if not InstalarR() then
    begin
      Result := 'Não consegui instalar o R. Veja o log em ' +
        ExpandConstant('{localappdata}\Trama\logs') + ' e tente rodar o Setup de novo.';
      Exit;
    end;
  end;

  ExtractTemporaryFile('bootstrap.R');
  BootstrapPath := ExpandConstant('{tmp}\bootstrap.R');

  if not RodarBootstrap(BootstrapPath) then
  begin
    Result := 'A instalação do trama falhou. Veja o log em ' +
      ExpandConstant('{localappdata}\Trama\logs') + '.';
    Exit;
  end;
end;
