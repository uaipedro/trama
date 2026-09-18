package pakrunner

import (
	"context"
	"strings"
	"testing"
)

func TestParseProgress(t *testing.T) {
	input := strings.NewReader(strings.Join([]string{
		"! trama.data instalado",
		"i Baixando 3 pacotes",
		"x figsr falhou: pacote não encontrado",
		"",
	}, "\n"))

	var events []Event
	err := ParseProgress(input, func(e Event) { events = append(events, e) })
	if err != nil {
		t.Fatal(err)
	}

	if len(events) != 3 {
		t.Fatalf("esperava 3 eventos, got %d: %+v", len(events), events)
	}
	if events[0].Kind != EventDone || events[2].Kind != EventError {
		t.Fatalf("kinds inesperados: %+v", events)
	}
	if events[1].Kind != EventInfo {
		t.Fatalf("evento 1 deveria ser EventInfo: %+v", events[1])
	}
}

func TestRun(t *testing.T) {
	// Usa /bin/echo como stand-in pro Rscript pra verificar a canalização
	// (start do processo, pipe de stdout, chamada de onEvent, Wait).
	var events []Event
	err := Run(context.Background(), "/bin/echo", "! ok", func(e Event) { events = append(events, e) })
	if err != nil {
		t.Fatal(err)
	}
	if len(events) != 1 {
		t.Fatalf("esperava 1 evento, got %d: %+v", len(events), events)
	}
}
