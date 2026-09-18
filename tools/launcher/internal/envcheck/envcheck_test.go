package envcheck

import (
	"os"
	"path/filepath"
	"testing"
)

func TestStatus_NoInstall(t *testing.T) {
	base := t.TempDir()
	st := Status(base, "4.4.1")
	if st.RPortableInstalled {
		t.Fatal("esperava RPortableInstalled = false em diretório vazio")
	}
	if st.TramaInstalled {
		t.Fatal("esperava TramaInstalled = false em diretório vazio")
	}
}

func TestStatus_RInstalledNoTrama(t *testing.T) {
	base := t.TempDir()
	binDir := filepath.Join(base, "r", "4.4.1", "bin")
	os.MkdirAll(binDir, 0o755)
	os.WriteFile(filepath.Join(binDir, rscriptName()), []byte(""), 0o755)

	st := Status(base, "4.4.1")
	if !st.RPortableInstalled {
		t.Fatal("esperava RPortableInstalled = true")
	}
	if st.TramaInstalled {
		t.Fatal("esperava TramaInstalled = false sem pacote na lib")
	}
}

func TestStatus_RAndTramaInstalled(t *testing.T) {
	base := t.TempDir()
	binDir := filepath.Join(base, "r", "4.4.1", "bin")
	os.MkdirAll(binDir, 0o755)
	os.WriteFile(filepath.Join(binDir, rscriptName()), []byte(""), 0o755)

	libDir := filepath.Join(base, "lib", "trama")
	os.MkdirAll(libDir, 0o755)
	os.WriteFile(filepath.Join(libDir, "DESCRIPTION"), []byte("Package: trama\n"), 0o644)

	st := Status(base, "4.4.1")
	if !st.RPortableInstalled || !st.TramaInstalled {
		t.Fatal("esperava R e trama instalados")
	}
}
