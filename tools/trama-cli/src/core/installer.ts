import { spawn } from "node:child_process";

const P3M_REPO = "https://packagemanager.posit.co/cran/latest";
const MAX_ERROR_TAIL_LINES = 40;

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
 */
export function buildInstallScript(pkgs: string[], lib: string): string {
  const pkgList = pkgs.map((p) => JSON.stringify(p)).join(", ");
  return `options(repos = c(P3M = ${JSON.stringify(P3M_REPO)}))
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
lib <- ${JSON.stringify(lib)}
dir.create(lib, showWarnings = FALSE, recursive = TRUE)
for (pkg in c(${pkgList})) {
  remotes::install_github(pkg, lib = lib, build = FALSE, upgrade = "never", dependencies = NA)
}
`;
}

/**
 * Roda `rscriptPath -e script`, capturando stdout+stderr. Se o processo
 * sair com erro, a mensagem inclui as últimas linhas de saída do R — sem
 * isso, uma falha (pacote não encontrado, erro de rede etc) vira só um
 * "exit code 1" sem pista nenhuma do que houve de verdade.
 */
export function runInstall(rscriptPath: string, script: string): Promise<void> {
  return new Promise((resolve, reject) => {
    const proc = spawn(rscriptPath, ["-e", script]);
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
