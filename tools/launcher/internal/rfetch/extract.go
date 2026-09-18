package rfetch

import (
	"archive/tar"
	"compress/gzip"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
)

// stripTopLevel remove o primeiro componente de um caminho de entrada de
// arquivo (ex.: "4.4.1/bin/Rscript" -> "bin/Rscript", "R-4.4.1/bin/Rscript.exe"
// -> "bin/Rscript.exe"). Os dois builds portáteis (Linux e Windows) têm um
// único diretório de topo com nomes diferentes, então normalizamos aqui em
// vez de depender do nome do arquivo dentro do archive.
//
// Retorna o caminho restante e ok=false quando a entrada é o próprio
// diretório de topo (nada sobra após remover o primeiro componente) — nesse
// caso a entrada deve ser ignorada.
func stripTopLevel(name string) (string, bool) {
	name = strings.ReplaceAll(name, "\\", "/")
	idx := strings.IndexByte(name, '/')
	if idx < 0 {
		// Entrada sem separador: é o próprio diretório de topo (ou um
		// arquivo solto na raiz, que não esperamos nos builds reais).
		return "", false
	}
	rest := name[idx+1:]
	if rest == "" {
		return "", false
	}
	return rest, true
}

// safeJoin junta dest com o caminho (já sem o componente de topo) e garante
// que o resultado não escape de dest (zip slip).
func safeJoin(dest, rel string) (string, error) {
	target := filepath.Join(dest, rel)
	if !strings.HasPrefix(target, filepath.Clean(dest)+string(os.PathSeparator)) {
		return "", fmt.Errorf("entrada fora do diretório de destino: %s", rel)
	}
	return target, nil
}

// ExtractTarGz extrai src (.tar.gz) em dest, removendo o componente de
// diretório de topo de cada entrada (ver stripTopLevel) e rejeitando
// entradas que escapem de dest (zip slip).
func ExtractTarGz(src, dest string) error {
	f, err := os.Open(src)
	if err != nil {
		return err
	}
	defer f.Close()

	gr, err := gzip.NewReader(f)
	if err != nil {
		return err
	}
	defer gr.Close()

	tr := tar.NewReader(gr)
	for {
		hdr, err := tr.Next()
		if err == io.EOF {
			return nil
		}
		if err != nil {
			return err
		}

		rel, ok := stripTopLevel(hdr.Name)
		if !ok {
			continue
		}

		target, err := safeJoin(dest, rel)
		if err != nil {
			return err
		}

		switch hdr.Typeflag {
		case tar.TypeDir:
			if err := os.MkdirAll(target, 0o755); err != nil {
				return err
			}
		case tar.TypeReg:
			if err := os.MkdirAll(filepath.Dir(target), 0o755); err != nil {
				return err
			}
			out, err := os.OpenFile(target, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, os.FileMode(hdr.Mode))
			if err != nil {
				return err
			}
			if _, err := io.Copy(out, tr); err != nil {
				out.Close()
				return err
			}
			out.Close()
		}
	}
}
