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

func TestSave_CreatesMissingParentDir(t *testing.T) {
	// Regressão: numa máquina nova, o diretório base do launcher ainda não
	// existe quando Save é chamado pela primeira vez (ex.: após instalar
	// coleções). Save precisa criar o pai de path em vez de falhar com
	// ENOENT via os.WriteFile.
	base := t.TempDir()
	path := filepath.Join(base, ".trama-launcher", "config.json")

	cfg := Config{RVersion: "4.4.1"}
	if err := Save(path, cfg); err != nil {
		t.Fatalf("esperava sucesso mesmo com diretório pai inexistente, got %v", err)
	}

	got, err := Load(path)
	if err != nil {
		t.Fatal(err)
	}
	if got.RVersion != cfg.RVersion {
		t.Fatalf("round-trip incorreto: %+v", got)
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
