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
var sources = map[string]map[string]map[string]Source{
	"4.4.1": {
		"windows": {
			"amd64": {
				URL:      "https://cran.r-project.org/bin/windows/base/old/4.4.1/R-4.4.1-win.exe",
				Checksum: "TODO-preencher-com-sha256-real",
			},
		},
		"linux": {
			"amd64": {
				URL:      "https://cran.r-project.org/src/base/R-4/R-4.4.1.tar.gz",
				Checksum: "TODO-preencher-com-sha256-real",
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
