import { spawn } from "node:child_process";
import { readFileSync } from "node:fs";

const P3M_REPO = "https://packagemanager.posit.co/cran/latest";
const MAX_ERROR_TAIL_LINES = 40;

/**
 * URL do repo P3M pra instalar dependências (dplyr, ggplot2 etc, puxadas
 * via `dependencies = NA` em buildInstallScript).
 *
 * No Linux, a URL genérica só serve pacotes fonte — P3M decide
 * fonte-vs-binário pelo User-Agent, e o R portátil não se identifica como
 * nenhuma distro conhecida. Isso fazia pacotes com código compilado
 * (dplyr, ggplot2 e toda a árvore de deps deles) compilarem do zero a cada
 * instalação: minutos por pacote, inviável pra um instalador end-user.
 * Apontando pra URL com a distro (`__linux__/<codename>/`) explícita, o
 * mesmo endpoint devolve `Content-Type: binary/octet-stream` e o R instala
 * `*binary*` — verificado batendo install.packages("dplyr") direto: sem
 * `__linux__/noble/` compila (via gcc), com ele baixa binário e instala em
 * segundos. Detecta a distro via /etc/os-release (ID + VERSION_CODENAME,
 * como Ubuntu/Debian relatam); se o arquivo não existir ou a distro não
 * usar codename (RHEL/CentOS/etc), cai pra URL genérica — mesmo
 * comportamento (fonte) de antes, não piora nada.
 */
export function resolveRepoUrl(
  platform: NodeJS.Platform = process.platform,
  osReleasePath = "/etc/os-release"
): string {
  if (platform !== "linux") return P3M_REPO;
  try {
    const osRelease = readFileSync(osReleasePath, "utf8");
    const id = /^ID=(.*)$/m.exec(osRelease)?.[1]?.trim().replace(/^"|"$/g, "");
    const codename = /^VERSION_CODENAME=(.*)$/m.exec(osRelease)?.[1]?.trim().replace(/^"|"$/g, "");
    if (!id || !codename) return P3M_REPO;
    return `https://packagemanager.posit.co/cran/__linux__/${codename}/latest`;
  } catch {
    return P3M_REPO;
  }
}

/**
 * Gera o script R que instala pkgs (refs no formato do remotes, ex.
 * "owner/repo" ou "owner/repo/subdir") na biblioteca lib.
 *
 * Usa remotes::install_github(..., build = FALSE) em vez de pak::pak():
 * pak sempre monta um binário via `R CMD INSTALL --build`, que exige
 * Rtools no Windows mesmo pra pacotes 100% R sem código compilado — trama
 * e as coleções são assim. Verificado batendo nesse erro de verdade
 * (Rtools ausente) reproduzindo a instalação real do Windows via Wine
 * durante o desenvolvimento do launcher Go (ver
 * docs/plans/2026-09-18-launcher-fast-plan.md). `build = FALSE` faz um
 * install de fonte direto, sem passar pela etapa que dispara a checagem
 * de Rtools; confirmado que instala e carrega corretamente sem Rtools.
 *
 * `dependencies = NA` (não TRUE) evita puxar Suggests (testthat, pkgbuild
 * etc) que o usuário final não precisa.
 *
 * `force`: por padrão remotes pula a instalação se o DESCRIPTION já
 * presente na lib tiver o mesmo RemoteSha do HEAD atual — mas DESCRIPTION
 * é copiado logo no início da instalação, então uma instalação que ficou
 * pela metade (rede caiu, antivírus travou um arquivo no Windows etc) tem
 * o SHA "certo" registrado mesmo sem o pacote ter terminado de instalar.
 * Nesse caso o skip-by-SHA do remotes reproduz o mesmo pacote quebrado pra
 * sempre. Quem chama isso já sabe que o pacote está ausente/incompleto
 * (ver checkEnv em envcheck.ts) — passa `force = true` pra pular essa
 * checagem e reinstalar de verdade.
 *
 * `.libPaths(c(lib, ...))`: sem isso o remotes só enxerga a biblioteca do
 * R portátil, conclui que trama e todas as dependências (dplyr, shiny...)
 * estão ausentes e reinstala tudo a cada chamada — era o `trama update`
 * que "ficava instalando pra sempre". Com lib no caminho, o skip-by-SHA e a
 * checagem de dependências funcionam e um update sem novidade é imediato.
 */
export function buildInstallScript(
  pkgs: string[],
  lib: string,
  repoUrl: string = P3M_REPO,
  force = false
): string {
  const pkgList = pkgs.map((p) => JSON.stringify(p)).join(", ");
  return `options(repos = c(P3M = ${JSON.stringify(repoUrl)}))
lib <- ${JSON.stringify(lib)}
dir.create(lib, showWarnings = FALSE, recursive = TRUE)
.libPaths(c(lib, .libPaths()))
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
for (pkg in c(${pkgList})) {
  remotes::install_github(pkg, lib = lib, build = FALSE, upgrade = "never", dependencies = NA, force = ${force ? "TRUE" : "FALSE"})
}
`;
}

/**
 * Env do processo R com TAR corrigido: o Renviron do R portátil (build da
 * Posit) defaulta TAR pra /usr/bin/gtar, que só existe em macOS/BSD. Sem
 * isso, remotes::install_github quebra em qualquer Linux ao tentar
 * descompactar o tarball do GitHub (untar chama esse TAR via system()).
 * "tar" sem path resolve pelo PATH do processo — não hardcoda localização,
 * que varia entre distros (/bin/tar, /usr/bin/tar).
 */
export function rEnv(): NodeJS.ProcessEnv {
  if (process.platform === "win32") return process.env;
  return { ...process.env, TAR: process.env.TAR || "tar" };
}

/**
 * Roda `rscriptPath -e script`, capturando stdout+stderr. Se o processo
 * sair com erro, a mensagem inclui as últimas linhas de saída do R — sem
 * isso, uma falha (pacote não encontrado, erro de rede etc) vira só um
 * "exit code 1" sem pista nenhuma do que houve de verdade.
 */
export function runInstall(rscriptPath: string, script: string): Promise<void> {
  return new Promise((resolve, reject) => {
    const proc = spawn(rscriptPath, ["-e", script], { env: rEnv() });
    const tail: string[] = [];

    const onData = (chunk: Buffer) => {
      for (const line of chunk.toString("utf8").split(/\r?\n/)) {
        if (!line) continue;
        tail.push(line);
        if (tail.length > MAX_ERROR_TAIL_LINES) tail.shift();
      }
    };
    proc.stdout.on("data", onData);
    proc.stderr.on("data", onData);

    proc.on("error", reject);
    proc.on("close", (code) => {
      if (code === 0) {
        resolve();
      } else {
        reject(new Error(`Rscript saiu com código ${code}:\n${tail.join("\n")}`));
      }
    });
  });
}
