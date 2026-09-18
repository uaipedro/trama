package rfetch

import (
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"io"
	"net/http"
	"os"
)

// Download baixa url pra dest, validando sha256 contra checksum. Tenta até
// maxAttempts vezes antes de desistir, apagando o arquivo parcial/corrompido
// entre tentativas.
func Download(url, checksum, dest string, maxAttempts int) error {
	var lastErr error
	for attempt := 1; attempt <= maxAttempts; attempt++ {
		if err := downloadOnce(url, dest); err != nil {
			lastErr = err
			continue
		}
		ok, err := verifyChecksum(dest, checksum)
		if err != nil {
			lastErr = err
			continue
		}
		if ok {
			return nil
		}
		os.Remove(dest)
		lastErr = fmt.Errorf("checksum não confere na tentativa %d", attempt)
	}
	return fmt.Errorf("falha ao baixar %s após %d tentativas: %w", url, maxAttempts, lastErr)
}

func downloadOnce(url, dest string) error {
	resp, err := http.Get(url)
	if err != nil {
		return err
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		return fmt.Errorf("status HTTP %d", resp.StatusCode)
	}

	f, err := os.Create(dest)
	if err != nil {
		return err
	}
	defer f.Close()

	_, err = io.Copy(f, resp.Body)
	return err
}

func verifyChecksum(path, want string) (bool, error) {
	f, err := os.Open(path)
	if err != nil {
		return false, err
	}
	defer f.Close()

	h := sha256.New()
	if _, err := io.Copy(h, f); err != nil {
		return false, err
	}
	got := hex.EncodeToString(h.Sum(nil))
	return got == want, nil
}
