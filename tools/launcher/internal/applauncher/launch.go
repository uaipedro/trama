package applauncher

import (
	"bytes"
	"context"
	"fmt"
	"net"
	"os"
	"os/exec"
	"runtime"
	"strconv"
	"sync"
	"time"
)

// OutputCapture coleta a saída combinada (stdout+stderr) de um processo de
// forma segura pra concorrência — lida enquanto o processo ainda roda, pra
// compor uma mensagem de erro útil se ele travar ou morrer antes da hora.
type OutputCapture struct {
	mu  sync.Mutex
	buf bytes.Buffer
}

func (c *OutputCapture) Write(p []byte) (int, error) {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.buf.Write(p)
}

func (c *OutputCapture) String() string {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.buf.String()
}

// WaitForPort tenta conectar em host:port até conseguir ou o timeout
// estourar, checando a cada 100ms.
func WaitForPort(host string, port int, timeout time.Duration) error {
	deadline := time.Now().Add(timeout)
	addr := net.JoinHostPort(host, strconv.Itoa(port))
	for time.Now().Before(deadline) {
		conn, err := net.DialTimeout("tcp", addr, 100*time.Millisecond)
		if err == nil {
			conn.Close()
			return nil
		}
		time.Sleep(100 * time.Millisecond)
	}
	return fmt.Errorf("timeout esperando %s responder", addr)
}

// OpenBrowser abre url no navegador padrão do SO.
func OpenBrowser(url string) error {
	var cmd *exec.Cmd
	switch runtime.GOOS {
	case "windows":
		cmd = exec.Command("rundll32", "url.dll,FileProtocolHandler", url)
	default:
		cmd = exec.Command("xdg-open", url)
	}
	return cmd.Start()
}

// buildTramaAppExpr monta a expressão R executada por StartTramaApp. Prepende
// libPath a .libPaths() explicitamente, em vez de depender de R_LIBS_USER/
// variáveis de ambiente, porque isso é determinístico e não depende de como
// o R do usuário (ou seu .Rprofile) trata o ambiente herdado do processo pai.
func buildTramaAppExpr(libPath, projectDir string) string {
	return fmt.Sprintf(`.libPaths(c(%q, .libPaths())); trama::tr_app(trama::tr_project(%q))`, libPath, projectDir)
}

// newTramaAppCmd monta o comando sem iniciá-lo. TRAMA_LAUNCHER=1 avisa o
// editor que ele foi aberto pelo launcher (e não por um console R), pra ele
// ajustar o que depende disso. O ambiente herdado segue inteiro.
func newTramaAppCmd(ctx context.Context, rscriptPath, libPath, projectDir string) *exec.Cmd {
	cmd := exec.CommandContext(ctx, rscriptPath, "-e", buildTramaAppExpr(libPath, projectDir))
	cmd.Env = append(os.Environ(), "TRAMA_LAUNCHER=1")
	return cmd
}

// StartTramaApp sobe `Rscript -e ".libPaths(...); trama::tr_app(trama::tr_project(projectDir))"`
// como processo filho e retorna o *exec.Cmd (já em execução) e a captura da
// sua saída, pro chamador decidir quando encerrar e diagnosticar falhas.
func StartTramaApp(ctx context.Context, rscriptPath, libPath, projectDir string) (*exec.Cmd, *OutputCapture, error) {
	cmd := newTramaAppCmd(ctx, rscriptPath, libPath, projectDir)
	capture := &OutputCapture{}
	cmd.Stdout = capture
	cmd.Stderr = capture
	if err := cmd.Start(); err != nil {
		return nil, nil, err
	}
	return cmd, capture, nil
}

// WaitForAppReady espera cmd abrir host:port ou terminar primeiro, o que vier
// antes. Sem isso, um R que crasha logo de cara (DLL faltando, erro de
// sintaxe, path inválido) só se manifestava como um timeout genérico depois
// de esperar o prazo inteiro, sem pista nenhuma do que houve — aqui, um
// processo que morre é detectado na hora, com a saída que ele produziu.
func WaitForAppReady(cmd *exec.Cmd, capture *OutputCapture, host string, port int, timeout time.Duration) error {
	exited := make(chan error, 1)
	go func() { exited <- cmd.Wait() }()

	portReady := make(chan error, 1)
	go func() { portReady <- WaitForPort(host, port, timeout) }()

	select {
	case err := <-portReady:
		if err != nil {
			return fmt.Errorf("%w:\n%s", err, capture.String())
		}
		return nil
	case err := <-exited:
		return fmt.Errorf("processo R encerrou antes da porta responder (%v):\n%s", err, capture.String())
	}
}
