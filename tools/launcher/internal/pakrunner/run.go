package pakrunner

import (
	"bufio"
	"context"
	"fmt"
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

// maxErrorTail é quantas linhas finais de output do R mantemos pra anexar
// à mensagem de erro se o processo sair com falha — sem isso, um erro do R
// (pacote não encontrado, falha de rede, etc.) vira só "exit status 1" pro
// usuário, sem pista nenhuma do que realmente deu errado.
const maxErrorTail = 20

// Run executa rscriptPath -e script, transmitindo cada linha de
// stdout+stderr pra onEvent via ParseProgress. Bloqueia até o processo
// terminar ou ctx ser cancelado. Se o processo sair com erro, a mensagem
// retornada inclui as últimas linhas de output do R, não só o exit status.
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

	var tail []string
	if err := ParseProgress(stdout, func(e Event) {
		tail = append(tail, e.Text)
		if len(tail) > maxErrorTail {
			tail = tail[len(tail)-maxErrorTail:]
		}
		onEvent(e)
	}); err != nil {
		return err
	}

	if err := cmd.Wait(); err != nil {
		if len(tail) > 0 {
			return fmt.Errorf("%w:\n%s", err, strings.Join(tail, "\n"))
		}
		return err
	}
	return nil
}
