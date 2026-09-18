package rfetch

import "testing"

func TestResolveSource_Windows(t *testing.T) {
	src, err := ResolveSource("windows", "amd64", "4.4.1")
	if err != nil {
		t.Fatal(err)
	}
	if src.URL == "" || src.Checksum == "" {
		t.Fatal("esperava URL e checksum preenchidos")
	}
}

func TestResolveSource_Linux(t *testing.T) {
	src, err := ResolveSource("linux", "amd64", "4.4.1")
	if err != nil {
		t.Fatal(err)
	}
	if src.URL == "" || src.Checksum == "" {
		t.Fatal("esperava URL e checksum preenchidos")
	}
}

func TestResolveSource_Unsupported(t *testing.T) {
	if _, err := ResolveSource("darwin", "amd64", "4.4.1"); err == nil {
		t.Fatal("esperava erro pra plataforma não suportada na v1")
	}
}
