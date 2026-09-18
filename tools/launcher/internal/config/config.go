package config

import (
	"encoding/json"
	"os"
)

// Config é o estado persistido do launcher entre execuções.
type Config struct {
	RVersion             string   `json:"rVersion"`
	InstalledCollections []string `json:"installedCollections"`
	DefaultProjectDir    string   `json:"defaultProjectDir"`
}

// Load lê path; se o arquivo não existir, retorna um Config zero-value sem
// erro (primeira execução do launcher).
func Load(path string) (Config, error) {
	data, err := os.ReadFile(path)
	if os.IsNotExist(err) {
		return Config{}, nil
	}
	if err != nil {
		return Config{}, err
	}
	var cfg Config
	if err := json.Unmarshal(data, &cfg); err != nil {
		return Config{}, err
	}
	return cfg, nil
}

// Save grava cfg em path formatado (fácil de inspecionar/debugar à mão).
func Save(path string, cfg Config) error {
	data, err := json.MarshalIndent(cfg, "", "  ")
	if err != nil {
		return err
	}
	return os.WriteFile(path, data, 0o644)
}
