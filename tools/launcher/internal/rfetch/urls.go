package rfetch

import "fmt"

// Source descreve de onde baixar o R portátil e como validar o download.
type Source struct {
	URL      string
	Checksum string // sha256, preenchido a partir de um manifest versionado
}

// sources é populado a partir de um manifest mantido junto do launcher
// (docs/plans/2026-09-18-launcher-design.md § 3): URLs de build portátil do
// R por versão/SO/arch, com checksum sha256 pinado.
//
// Os builds vêm do projeto rstudio/r-builds publicados pela Posit (os mesmos
// usados internamente pelo Posit Workbench) — são builds relocáveis de fato,
// diferente dos instaladores/tarballs de fonte do CRAN.
var sources = map[string]map[string]map[string]Source{
	"4.4.1": {
		"windows": {
			"amd64": {
				URL:      "https://cdn.posit.co/r/windows/R-4.4.1-windows.zip",
				Checksum: "a82d78ef104d91f72570b7094cacd731fc4e5faced949add7628e0f7c081c351",
			},
		},
		"linux": {
			"amd64": {
				// Build manylinux_2_34: requer glibc >= 2.34 (Ubuntu 22.04+,
				// Debian 12+). Aceitável pro público-alvo da v1.
				URL:      "https://cdn.posit.co/r/manylinux_2_34/R-4.4.1-manylinux_2_34.tar.gz",
				Checksum: "23683241cd0c9035e38e00d548918a33a413372e8abdc1a388119b338e72053d",
			},
		},
	},
}

// ResolveSource retorna de onde baixar o R portátil pra essa versão/SO/arch.
func ResolveSource(goos, goarch, rVersion string) (Source, error) {
	byOS, ok := sources[rVersion]
	if !ok {
		return Source{}, fmt.Errorf("versão de R não catalogada: %s", rVersion)
	}
	byArch, ok := byOS[goos]
	if !ok {
		return Source{}, fmt.Errorf("SO não suportado na v1: %s", goos)
	}
	src, ok := byArch[goarch]
	if !ok {
		return Source{}, fmt.Errorf("arquitetura não suportada: %s/%s", goos, goarch)
	}
	return src, nil
}
