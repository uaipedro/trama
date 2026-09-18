package envcheck

import (
	"os"
	"path/filepath"
	"runtime"
)

// EnvStatus descreve o que já está instalado no diretório base do launcher.
type EnvStatus struct {
	RPortableInstalled bool
	TramaInstalled     bool
	RscriptPath        string
	LibPath            string
}

func rscriptName() string {
	if runtime.GOOS == "windows" {
		return "Rscript.exe"
	}
	return "Rscript"
}

// Status inspeciona base/r/<rVersion>/bin e base/lib pra determinar o que falta instalar.
func Status(base, rVersion string) EnvStatus {
	rscript := filepath.Join(base, "r", rVersion, "bin", rscriptName())
	lib := filepath.Join(base, "lib")

	st := EnvStatus{RscriptPath: rscript, LibPath: lib}

	if info, err := os.Stat(rscript); err == nil && !info.IsDir() {
		st.RPortableInstalled = true
	}

	descPath := filepath.Join(lib, "trama", "DESCRIPTION")
	if info, err := os.Stat(descPath); err == nil && !info.IsDir() {
		st.TramaInstalled = true
	}

	return st
}
