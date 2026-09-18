package rfetch

import (
	"crypto/sha256"
	"encoding/hex"
	"net/http"
	"net/http/httptest"
	"path/filepath"
	"testing"
)

func TestFetch_ChecksumMismatchThenRetrySucceeds(t *testing.T) {
	good := []byte("conteudo-do-r-portatil")
	sum := sha256.Sum256(good)
	checksum := hex.EncodeToString(sum[:])

	attempt := 0
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		attempt++
		if attempt == 1 {
			w.Write([]byte("corrompido"))
			return
		}
		w.Write(good)
	}))
	defer srv.Close()

	dest := filepath.Join(t.TempDir(), "r.tar.gz")
	err := Download(srv.URL, checksum, dest, 2)
	if err != nil {
		t.Fatalf("esperava sucesso na 2ª tentativa, got %v", err)
	}
	if attempt != 2 {
		t.Fatalf("esperava 2 tentativas, got %d", attempt)
	}
}

func TestFetch_AllRetriesFail(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Write([]byte("sempre-corrompido"))
	}))
	defer srv.Close()

	dest := filepath.Join(t.TempDir(), "r.tar.gz")
	err := Download(srv.URL, "checksum-que-nunca-bate", dest, 2)
	if err == nil {
		t.Fatal("esperava erro após esgotar retries")
	}
}
