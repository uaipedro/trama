package pakrunner

import (
	"bufio"
	"context"
	"io"
	"os/exec"
	"strings"
)

type EventKind int

const (
	EventInfo EventKind = iota
	EventDone
	EventError
)

// Event é uma linha de output do pak já classificada.
type Event struct {
	Kind EventKind
	Text string
}

// ParseProgress lê o output do pak linha a linha e chama onEvent pra cada
// linha não vazia, classificada pelo prefixo que o pak usa (! sucesso,
// i info, x erro).
func ParseProgress(r io.Reader, onEvent func(Event)) error {
	scanner := bufio.NewScanner(r)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line == "" {
			continue
		}
		kind := EventInfo
		switch {
		case strings.HasPrefix(line, "!"):
			kind = EventDone
		case strings.HasPrefix(line, "x"):
			kind = EventError
		}
		onEvent(Event{Kind: kind, Text: line})
	}
	return scanner.Err()
}

// Run executa rscriptPath -e script, transmitindo cada linha de
// stdout+stderr pra onEvent via ParseProgress. Bloqueia até o processo
// terminar ou ctx ser cancelado.
func Run(ctx context.Context, rscriptPath, script string, onEvent func(Event)) error {
	cmd := exec.CommandContext(ctx, rscriptPath, "-e", script)
	stdout, err := cmd.StdoutPipe()
	if err != nil {
		return err
	}
	cmd.Stderr = cmd.Stdout // pak escreve progresso em stderr também; unificar

	if err := cmd.Start(); err != nil {
		return err
	}
	if err := ParseProgress(stdout, onEvent); err != nil {
		return err
	}
	return cmd.Wait()
}
