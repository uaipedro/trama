// Package pakrunner gera e executa scripts R que instalam pacotes via
// pak::pak(), apontando para o P3M (Posit Package Manager) como repositório.
package pakrunner

import (
	"fmt"
	"strings"
)

// p3mRepo é o repositório P3M usado para instalação de pacotes binários.
const p3mRepo = "https://packagemanager.posit.co/cran/latest"

// BuildInstallScript gera o script R que instala pkgs (refs no formato do
// pak, ex. "owner/repo" ou "owner/repo/subdir") na biblioteca lib.
func BuildInstallScript(pkgs []string, lib string) string {
	quoted := make([]string, len(pkgs))
	for i, p := range pkgs {
		quoted[i] = fmt.Sprintf("%q", p)
	}
	pkgList := strings.Join(quoted, ", ")

	return fmt.Sprintf(`options(repos = c(P3M = %q))
if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak")
}
lib <- %q
dir.create(lib, showWarnings = FALSE, recursive = TRUE)
pak::pak(c(%s), lib = lib)
`, p3mRepo, lib, pkgList)
}
