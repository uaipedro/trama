// Package pakrunner gera e executa scripts R que instalam pacotes via
// remotes::install_github(), apontando para o P3M (Posit Package Manager)
// como repositório pras dependências de CRAN.
package pakrunner

import (
	"fmt"
	"strings"
)

// p3mRepo é o repositório P3M usado para instalação de pacotes binários.
const p3mRepo = "https://packagemanager.posit.co/cran/latest"

// BuildInstallScript gera o script R que instala pkgs (refs no formato do
// remotes, ex. "owner/repo" ou "owner/repo/subdir") na biblioteca lib.
//
// Usa remotes::install_github(..., build = FALSE) em vez de pak::pak():
// pak sempre tenta compilar um binário do pacote via `R CMD INSTALL
// --build`, o que exige Rtools no Windows mesmo pra pacotes 100% R sem
// código compilado (trama e as coleções são assim) — verificado batendo
// nesse erro de verdade (Rtools ausente) reproduzindo a instalação real do
// Windows via Wine. `build = FALSE` faz um install de fonte direto, sem
// passar pela etapa de build de binário que dispara a checagem de Rtools;
// confirmado que instala e carrega corretamente sem Rtools instalado.
func BuildInstallScript(pkgs []string, lib string) string {
	quoted := make([]string, len(pkgs))
	for i, p := range pkgs {
		quoted[i] = fmt.Sprintf("%q", p)
	}
	pkgList := strings.Join(quoted, ", ")

	return fmt.Sprintf(`options(repos = c(P3M = %q))
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
lib <- %q
dir.create(lib, showWarnings = FALSE, recursive = TRUE)
for (pkg in c(%s)) {
  remotes::install_github(pkg, lib = lib, build = FALSE, upgrade = "never", dependencies = TRUE)
}
`, p3mRepo, lib, pkgList)
}
