#!/usr/bin/env bash
# install.sh — leva uma máquina Linux só com bash/curl/tar até o atalho
# "Trama" funcionando: lê o manifesto de release para saber qual R baixar
# (um build portátil da Posit, mesma fonte que
# tools/trama-cli/src/core/sources.ts usa para o R portátil do CLI), baixa
# bootstrap.R e o roda (ele instala o trama.launcher e a release do
# manifesto), e registra o atalho .desktop. Idempotente: rodar de novo
# atualiza (pula o download do R se a versão já estiver instalada, sempre
# roda o bootstrap de novo para pegar a release mais recente).
set -euo pipefail

# --- Configuração, tudo sobrescrevível por env var (usado nos testes/CI) --
TRAMA_HOME="${TRAMA_HOME:-$HOME/.local/share/trama}"

MANIFEST_URL="${TRAMA_MANIFEST_URL:-https://github.com/uaipedro/trama/releases/latest/download/release.json}"
REPO_RAW="${TRAMA_REPO_RAW:-https://raw.githubusercontent.com/uaipedro/trama/main}"
BOOTSTRAP_URL="${TRAMA_BOOTSTRAP_URL:-$REPO_RAW/tools/installer/bootstrap.R}"
ICON_URL="${TRAMA_ICON_URL:-$REPO_RAW/inst/www/icon.png}"
DESKTOP_TEMPLATE_URL="${TRAMA_DESKTOP_TEMPLATE_URL:-$REPO_RAW/tools/installer/linux/trama.desktop.in}"
DESKTOP_DIR="${TRAMA_DESKTOP_DIR:-$HOME/.local/share/applications}"

log() { printf '%s\n' "$*"; }
erro() { printf 'ERRO: %s\n' "$*" >&2; exit 1; }

command -v curl >/dev/null 2>&1 || erro "precisa do 'curl' instalado."
command -v tar >/dev/null 2>&1 || erro "precisa do 'tar' instalado."

mkdir -p "$TRAMA_HOME"

# --- 0. Versão do R: o manifesto (release.json, campo "r") é a fonte da
# verdade — é o mesmo manifesto que bootstrap.R vai ler de novo depois (a
# releitura aqui é só para saber qual R baixar antes dele existir).
# `TRAMA_R_VERSION` ainda sobrescreve, para overrides manuais/testes.
MANIFEST_LOCAL="$(mktemp)"
trap 'rm -f "$MANIFEST_LOCAL"' EXIT
if [ -f "$MANIFEST_URL" ]; then
  # Caminho local (testes/CI apontando um arquivo em vez de uma URL).
  cp "$MANIFEST_URL" "$MANIFEST_LOCAL"
else
  curl -fsSL "$MANIFEST_URL" -o "$MANIFEST_LOCAL" || erro "não consegui baixar o manifesto de release em $MANIFEST_URL"
fi
# Regex simples, só para o campo plano "r" do manifesto (não é um parser de
# JSON de verdade — não lida com aninhamento; bootstrap.R usa jsonlite para
# o resto).
MANIFEST_R="$(sed -n 's/.*"r"[[:space:]]*:[[:space:]]*"\([^"]*\)".*/\1/p' "$MANIFEST_LOCAL" | head -n1)"
[ -n "$MANIFEST_R" ] || erro "não consegui ler o campo 'r' do manifesto de release em $MANIFEST_URL."
rm -f "$MANIFEST_LOCAL"
trap - EXIT

# R portátil (build relocável da Posit, não um instalador): mesma URL que
# tools/trama-cli/src/core/sources.ts usa para "linux" (requer glibc >=
# 2.34, ou seja Ubuntu 22.04+/Debian 12+), com a versão do manifesto.
R_VERSION="${TRAMA_R_VERSION:-$MANIFEST_R}"
R_URL="${TRAMA_R_URL:-https://cdn.posit.co/r/manylinux_2_34/R-${R_VERSION}-manylinux_2_34.tar.gz}"

# --- 1. R portátil ----------------------------------------------------------
R_MARKER="$TRAMA_HOME/R/.trama-r-version"
if [ -x "$TRAMA_HOME/R/bin/Rscript" ] && [ -f "$R_MARKER" ] && [ "$(cat "$R_MARKER")" = "$R_VERSION" ]; then
  log "R $R_VERSION já instalado em $TRAMA_HOME/R, pulando download."
else
  log "Baixando R $R_VERSION..."
  R_TARBALL="$(mktemp)"
  trap 'rm -f "$R_TARBALL"' EXIT
  curl -fsSL "$R_URL" -o "$R_TARBALL" || erro "não consegui baixar o R em $R_URL"

  rm -rf "$TRAMA_HOME/R"
  mkdir -p "$TRAMA_HOME/R"
  log "Extraindo R..."
  # Os tarballs da Posit têm um único diretório de topo (ex.: "R-4.4.1");
  # --strip-components=1 achata isso para dentro de $TRAMA_HOME/R.
  tar -xzf "$R_TARBALL" -C "$TRAMA_HOME/R" --strip-components=1
  rm -f "$R_TARBALL"
  trap - EXIT

  [ -x "$TRAMA_HOME/R/bin/Rscript" ] || erro "o tarball do R não tinha bin/Rscript; algo mudou na build da Posit."
  echo "$R_VERSION" > "$R_MARKER"
fi

# --- 2. bootstrap.R (instala trama.launcher + a release do manifesto) ------
log "Instalando o trama..."
BOOTSTRAP_LOCAL="$TRAMA_HOME/bootstrap.R"
if [ -f "$BOOTSTRAP_URL" ]; then
  # Caminho local (testes/CI apontando um arquivo em vez de uma URL).
  cp "$BOOTSTRAP_URL" "$BOOTSTRAP_LOCAL"
else
  curl -fsSL "$BOOTSTRAP_URL" -o "$BOOTSTRAP_LOCAL" || erro "não consegui baixar bootstrap.R em $BOOTSTRAP_URL"
fi

"$TRAMA_HOME/R/bin/Rscript" "$BOOTSTRAP_LOCAL" || erro "a instalação do trama falhou (veja o motivo acima; logs em $TRAMA_HOME/logs quando existirem)."

# --- 3. Ícone ----------------------------------------------------------------
ICON_LOCAL="$TRAMA_HOME/trama.png"
if [ -f "$ICON_URL" ]; then
  cp "$ICON_URL" "$ICON_LOCAL"
else
  curl -fsSL "$ICON_URL" -o "$ICON_LOCAL" || log "aviso: não consegui baixar o ícone em $ICON_URL, seguindo sem ele."
fi

# --- 4. abrir.sh: lê a release atual do estado e sobe o launcher -----------
# Gerado (não baixado) porque referencia $TRAMA_HOME, que só é conhecido
# aqui. Mesma lógica do windows/abrir.R: acha a lib da release atual em
# estado.json e delega para trama.launcher::abrir().
cat > "$TRAMA_HOME/abrir.sh" <<ABRIR_SH
#!/usr/bin/env bash
set -euo pipefail
TRAMA_HOME="${TRAMA_HOME}"
ESTADO="\$TRAMA_HOME/estado.json"
ATUAL=""
if [ -f "\$ESTADO" ]; then
  # Regex simples, só para o campo plano "atual" do estado — não é um
  # parser de JSON de verdade, não lida com aninhamento nem escapes.
  ATUAL="\$(sed -n 's/.*"atual"[[:space:]]*:[[:space:]]*"\\([^"]*\\)".*/\\1/p' "\$ESTADO" | head -n1)"
fi

LIBPATHS_R=".libPaths()"
if [ -n "\$ATUAL" ] && [ -d "\$TRAMA_HOME/lib/\$ATUAL" ]; then
  LIBPATHS_R=".libPaths(c('\$TRAMA_HOME/lib/\$ATUAL', .libPaths()))"
fi

export TRAMA_HOME
exec "\$TRAMA_HOME/R/bin/Rscript" -e "\$LIBPATHS_R; trama.launcher::abrir()"
ABRIR_SH
chmod +x "$TRAMA_HOME/abrir.sh"

# --- 5. Atalho .desktop -----------------------------------------------------
mkdir -p "$DESKTOP_DIR"
TEMPLATE_LOCAL="$TRAMA_HOME/.trama.desktop.in"
if [ -f "$DESKTOP_TEMPLATE_URL" ]; then
  cp "$DESKTOP_TEMPLATE_URL" "$TEMPLATE_LOCAL"
else
  curl -fsSL "$DESKTOP_TEMPLATE_URL" -o "$TEMPLATE_LOCAL" || erro "não consegui baixar o modelo do atalho em $DESKTOP_TEMPLATE_URL"
fi
# Exec é quotado (o Desktop Entry Spec aceita aspas em "Exec", escapando
# aspas/backtick/$/\\ dentro delas — sem nenhum desses caracteres aqui,
# então a troca direta é segura). Icon é do tipo iconstring, que a spec NÃO
# deixa quotar: espaço vira o escape "\s" em vez de aspas.
TRAMA_HOME_ICON="${TRAMA_HOME// /\\s}"
sed \
  -e "s#__TRAMA_HOME__#$TRAMA_HOME#g" \
  -e "s#__TRAMA_HOME_ICON__#$TRAMA_HOME_ICON#g" \
  "$TEMPLATE_LOCAL" > "$DESKTOP_DIR/trama.desktop"
rm -f "$TEMPLATE_LOCAL"
chmod +x "$DESKTOP_DIR/trama.desktop"

log ""
log "Pronto! Procure Trama no menu de aplicativos."
