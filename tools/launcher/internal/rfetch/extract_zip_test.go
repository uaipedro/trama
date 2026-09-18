package rfetch

import (
	"archive/zip"
	"os"
	"path/filepath"
	"testing"
)

func makeZip(t *testing.T, files map[string]string) string {
	t.Helper()
	path := filepath.Join(t.TempDir(), "src.zip")
	f, err := os.Create(path)
	if err != nil {
		t.Fatal(err)
	}
	zw := zip.NewWriter(f)
	for name, content := range files {
		w, err := zw.Create(name)
		if err != nil {
			t.Fatal(err)
		}
		if _, err := w.Write([]byte(content)); err != nil {
			t.Fatal(err)
		}
	}
	if err := zw.Close(); err != nil {
		t.Fatal(err)
	}
	if err := f.Close(); err != nil {
		t.Fatal(err)
	}
	return path
}

// TestExtractZip_Basic mimetiza o formato do build real da Posit pro
// Windows: um único diretório de topo prefixado com "R-" (ex.:
// "R-4.4.1/"), diferente do Linux, que deve ser removido na extração.
func TestExtractZip_Basic(t *testing.T) {
	src := makeZip(t, map[string]string{"R-4.4.1/bin/Rscript.exe": "conteudo"})
	dest := t.TempDir()

	if err := ExtractZip(src, dest); err != nil {
		t.Fatal(err)
	}
	got, err := os.ReadFile(filepath.Join(dest, "bin", "Rscript.exe"))
	if err != nil || string(got) != "conteudo" {
		t.Fatalf("arquivo extraído incorreto: %v %q", err, got)
	}
}

func TestExtractZip_RejectsPathTraversal(t *testing.T) {
	src := makeZip(t, map[string]string{"R-4.4.1/../../etc/passwd": "malicioso"})
	dest := t.TempDir()

	if err := ExtractZip(src, dest); err == nil {
		t.Fatal("esperava rejeição de entrada com path traversal")
	}
}

func TestExtractZip_RejectsPathTraversalNoTopLevelPrefix(t *testing.T) {
	src := makeZip(t, map[string]string{"../../etc/passwd": "malicioso"})
	dest := t.TempDir()

	if err := ExtractZip(src, dest); err == nil {
		t.Fatal("esperava rejeição de entrada com path traversal")
	}
}
