const P3M_REPO = "https://packagemanager.posit.co/cran/latest";

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
