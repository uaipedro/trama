package rfetch

import (
	"archive/zip"
	"io"
	"os"
	"path/filepath"
)

// ExtractZip extrai src (.zip) em dest, removendo o componente de
// diretório de topo de cada entrada (ver stripTopLevel) e rejeitando
// entradas que escapem de dest (zip slip). Usado pro build portátil de R
// do Windows, cujo top-level dir é "R-<versão>/" (prefixo diferente do
// Linux, que usa "<versão>/").
func ExtractZip(src, dest string) error {
	zr, err := zip.OpenReader(src)
	if err != nil {
		return err
	}
	defer zr.Close()

	for _, f := range zr.File {
		rel, ok := stripTopLevel(f.Name)
		if !ok {
			continue
		}

		target, err := safeJoin(dest, rel)
		if err != nil {
			return err
		}

		if f.FileInfo().IsDir() {
			if err := os.MkdirAll(target, 0o755); err != nil {
				return err
			}
			continue
		}

		if err := os.MkdirAll(filepath.Dir(target), 0o755); err != nil {
			return err
		}

		mode := f.Mode()
		if mode == 0 {
			mode = 0o644
		}
		out, err := os.OpenFile(target, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, mode)
		if err != nil {
			return err
		}

		rc, err := f.Open()
		if err != nil {
			out.Close()
			return err
		}

		if _, err := io.Copy(out, rc); err != nil {
			rc.Close()
			out.Close()
			return err
		}
		rc.Close()
		out.Close()
	}
	return nil
}
