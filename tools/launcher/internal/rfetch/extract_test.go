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

// TestExtractTarGz_Basic mimetiza o formato do build real da Posit pro
// Linux: um único diretório de topo com o número da versão (ex.: "4.4.1/"),
// que deve ser removido na extração.
func TestExtractTarGz_Basic(t *testing.T) {
	src := makeTarGz(t, map[string]string{"4.4.1/bin/Rscript": "conteudo"})
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

// TestExtractTarGz_RejectsPathTraversalWithTopLevelPrefix garante que a
// remoção do diretório de topo não abre brecha pra escapar de dest: mesmo
// uma entrada que "parece" legítima (tem o prefixo esperado "4.4.1/") mas
// contém ".." depois disso deve ser rejeitada.
func TestExtractTarGz_RejectsPathTraversalWithTopLevelPrefix(t *testing.T) {
	src := makeTarGz(t, map[string]string{"4.4.1/../../etc/passwd": "malicioso"})
	dest := t.TempDir()

	if err := ExtractTarGz(src, dest); err == nil {
		t.Fatal("esperava rejeição de entrada com path traversal após remover o diretório de topo")
	}
}

// TestExtractTarGz_SkipsTopLevelDirEntry garante que a entrada do próprio
// diretório de topo (sem nada depois da "/") é ignorada sem erro.
func TestExtractTarGz_SkipsTopLevelDirEntry(t *testing.T) {
	var buf bytes.Buffer
	gw := gzip.NewWriter(&buf)
	tw := tar.NewWriter(gw)
	tw.WriteHeader(&tar.Header{Name: "4.4.1/", Typeflag: tar.TypeDir, Mode: 0o755})
	content := "conteudo"
	tw.WriteHeader(&tar.Header{Name: "4.4.1/bin/Rscript", Mode: 0o644, Size: int64(len(content))})
	tw.Write([]byte(content))
	tw.Close()
	gw.Close()

	path := filepath.Join(t.TempDir(), "src.tar.gz")
	os.WriteFile(path, buf.Bytes(), 0o644)

	dest := t.TempDir()
	if err := ExtractTarGz(path, dest); err != nil {
		t.Fatal(err)
	}
	got, err := os.ReadFile(filepath.Join(dest, "bin", "Rscript"))
	if err != nil || string(got) != content {
		t.Fatalf("arquivo extraído incorreto: %v %q", err, got)
	}
	if _, err := os.Stat(filepath.Join(dest, "4.4.1")); !os.IsNotExist(err) {
		t.Fatalf("diretório de topo não deveria ter sido recriado dentro de dest: %v", err)
	}
}
