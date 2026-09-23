# Checklist de release do instalador

Passos para publicar um `Trama-Setup.exe`/`install.sh` novo. Design:
`docs/plans/2026-09-23-instalador-windows-design.md`. Motor, telas e
instaladores: `tools/installer/`.

## Antes de marcar a tag

1. **Manifesto.** Atualize `tools/installer/release.json` (ou o `release.json`
   publicado nos Releases, se o processo real de publicação for manual):
   `trama` (a versão da release, `AAAA.MM[.patch]`), `r`, `cran_snapshot`
   (data ISO já disponível no [P3M](https://packagemanager.posit.co)),
   `core` (versões de `trama`, `trama.data`, `trama.view`,
   `trama.launcher`) e `collections`.
2. **Linha do R.** Se o campo `r` do manifesto mudou de linha (ex.:
   `4.5.x` → `4.6.x`), atualize `#define RVersion` **e**
   `#define RVersionLinha` no topo de
   `tools/installer/windows/Trama-Setup.iss`. Isso muda o `AppVersion` do
   Setup e faz ele baixar/instalar o R novo em máquinas limpas; sem esse
   bump, `Trama-Setup.exe` continua instalando a linha antiga do R e
   `bootstrap.R` falha na conferência de `getRversion()` contra o manifesto
   (mensagem clara, mas só depois de rodar a instalação inteira).
3. **r-universe.** Confira que `packages.json` em
   `uaipedro/uaipedro.r-universe.dev` (fonte da verdade — ver
   `tools/installer/r-universe/README.md`) lista todo pacote/coleção novo
   antes de referenciá-lo em `core`/`collections` do manifesto; senão a
   instalação real (fora do `--local` do CI) falha por pacote não
   encontrado.
4. **CI verde.** Rode `.github/workflows/installer.yml` (`workflow_dispatch`
   ou a própria tag) e confira os três jobs: `pacotes` (tarballs),
   `windows` e `linux` (instalação limpa + launcher respondendo em
   `127.0.0.1:8725`). Baixe o artefato `windows-e2e-logs` (ou
   `linux-e2e-logs`) e **abra o `bootstrap-*.log`**: confirme que é legível
   (sem caracteres truncados/mojibake) e que não há nenhuma linha indicando
   compilação a partir do código-fonte (o job já falha sozinho nesse caso,
   mas vale conferir o texto).

## Verificação manual (antes de publicar para o público)

Feita numa build candidata (tag ou artefato do `workflow_dispatch`), não a
cada commit.

1. **VirusTotal.** Suba o `Trama-Setup.exe` em virustotal.com e confira que
   nenhum motor relevante (a Microsoft em especial) acusa nada.
2. **VM Windows 11 limpa**, Defender ligado, **download pelo navegador**
   (não copiado por fora — é isso que marca o arquivo como "veio da
   internet" e ativa o SmartScreen de verdade em vez de pular a checagem):
   1. Instalar (aceitar o aviso azul do SmartScreen: "Mais informações" →
      "Executar assim mesmo").
   2. Abrir o launcher, instalar uma coleção.
   3. Fechar e reabrir o atalho "Trama".
   4. Conferir o **histórico de proteção** do Defender: nenhum item em
      quarentena, nenhum alerta além do próprio SmartScreen.
3. **Falso positivo.** Se algum antivírus acusar no passo 1, envie o hash
   pelo [portal de submissão da Microsoft](https://www.microsoft.com/wdsi/filesubmission)
   com a opção "Software developer".
4. **Usuário sem admin, nome com acento e espaço** (ex.:
   `C:\Users\João Silva`): instalar do zero (o Setup é `PrivilegesRequired=lowest`,
   não deve pedir elevação) e abrir o editor.
5. **Seguir só o site.** Sem conhecimento prévio do projeto, seguir
   `site/src/content/docs/por-dentro/instalacao.md` do início ao fim
   (baixar, instalar, abrir) — é o critério de pronto do design (alguém que
   não programa instala sozinho).
6. **SignPath.** Pendente (fila de aprovação do SignPath Foundation, ver o
   comentário em `.github/workflows/launcher-release.yml`). Quando a
   assinatura chegar, repetir o passo 1 (VirusTotal) na build assinada.

## Publicando

- Tag `trama-<versão>` (ex.: `trama-2026.10`) dispara `installer.yml`: os
  jobs `pacotes`/`windows`/`linux` rodam e, se passarem, `release` publica
  `Trama-Setup.exe`, `install.sh` e `release.json` numa GitHub Release
  marcada como latest.
- O manifesto publicado (`release.json` da Release) é o que
  `TRAMA_MANIFEST_URL` padrão aponta — conferir que ele é exatamente o que
  foi validado na verificação manual acima, não uma versão mais nova ainda
  não testada.
