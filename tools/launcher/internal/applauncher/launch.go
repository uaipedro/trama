package applauncher

import (
	"context"
	"fmt"
	"net"
	"os/exec"
	"runtime"
	"strconv"
	"time"
)

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

// StartTramaApp sobe `Rscript -e ".libPaths(...); trama::tr_app(trama::tr_project(projectDir))"`
// como processo filho e retorna o *exec.Cmd (já em execução) pro chamador
// decidir quando encerrar.
func StartTramaApp(ctx context.Context, rscriptPath, libPath, projectDir string) (*exec.Cmd, error) {
	expr := buildTramaAppExpr(libPath, projectDir)
	cmd := exec.CommandContext(ctx, rscriptPath, "-e", expr)
	if err := cmd.Start(); err != nil {
		return nil, err
	}
	return cmd, nil
}
