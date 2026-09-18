package rfetch

import (
	"archive/tar"
	"bytes"
	"compress/gzip"
	"os"
	"path/filepath"
	"testing"
)

func makeTarGz(t *testing.T, files map[string]string) string {
	t.Helper()
	var buf bytes.Buffer
	gw := gzip.NewWriter(&buf)
	tw := tar.NewWriter(gw)
	for name, content := range files {
		hdr := &tar.Header{Name: name, Mode: 0o644, Size: int64(len(content))}
		tw.WriteHeader(hdr)
		tw.Write([]byte(content))
	}
	tw.Close()
	gw.Close()

	path := filepath.Join(t.TempDir(), "src.tar.gz")
	os.WriteFile(path, buf.Bytes(), 0o644)
	return path
}

func TestExtractTarGz_Basic(t *testing.T) {
	src := makeTarGz(t, map[string]string{"bin/Rscript": "conteudo"})
	dest := t.TempDir()

	if err := ExtractTarGz(src, dest); err != nil {
		t.Fatal(err)
	}
	got, err := os.ReadFile(filepath.Join(dest, "bin", "Rscript"))
	if err != nil || string(got) != "conteudo" {
		t.Fatalf("arquivo extraído incorreto: %v %q", err, got)
	}
}

func TestExtractTarGz_RejectsPathTraversal(t *testing.T) {
	src := makeTarGz(t, map[string]string{"../../etc/passwd": "malicioso"})
	dest := t.TempDir()

	if err := ExtractTarGz(src, dest); err == nil {
		t.Fatal("esperava rejeição de entrada com path traversal")
	}
}
