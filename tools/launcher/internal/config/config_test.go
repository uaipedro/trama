package config

import (
	"path/filepath"
	"testing"
)

func TestLoad_MissingFileReturnsDefaults(t *testing.T) {
	path := filepath.Join(t.TempDir(), "config.json")
	cfg, err := Load(path)
	if err != nil {
		t.Fatal(err)
	}
	if len(cfg.InstalledCollections) != 0 {
		t.Fatalf("esperava lista vazia, got %v", cfg.InstalledCollections)
	}
}

func TestSaveThenLoad_RoundTrips(t *testing.T) {
	path := filepath.Join(t.TempDir(), "config.json")
	cfg := Config{
		RVersion:             "4.4.1",
		InstalledCollections: []string{"trama.data", "trama.view", "trama.ml"},
	}
	if err := Save(path, cfg); err != nil {
		t.Fatal(err)
	}

	got, err := Load(path)
	if err != nil {
		t.Fatal(err)
	}
	if got.RVersion != cfg.RVersion || len(got.InstalledCollections) != 3 {
		t.Fatalf("round-trip incorreto: %+v", got)
	}
}
