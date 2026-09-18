package applauncher

import (
	"net"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

func TestWaitForPort_Succeeds(t *testing.T) {
	ln, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	defer ln.Close()
	port := ln.Addr().(*net.TCPAddr).Port

	if err := WaitForPort("127.0.0.1", port, 2*time.Second); err != nil {
		t.Fatalf("esperava sucesso, got %v", err)
	}
}

func TestWaitForPort_TimesOut(t *testing.T) {
	// Porta improvável de estar em uso e que nunca vai abrir.
	err := WaitForPort("127.0.0.1", 65533, 200*time.Millisecond)
	if err == nil {
		t.Fatal("esperava timeout")
	}
}

func TestBuildTramaAppExpr_IncludesLibPath(t *testing.T) {
	expr := buildTramaAppExpr("/home/user/.trama-launcher/lib", "/home/user/.trama-launcher/projects/default")

	want := []string{
		`.libPaths(c("/home/user/.trama-launcher/lib", .libPaths()))`,
		`trama::tr_app(trama::tr_project("/home/user/.trama-launcher/projects/default"))`,
	}
	for _, w := range want {
		if !strings.Contains(expr, w) {
			t.Fatalf("expressão gerada não contém %q:\n%s", w, expr)
		}
	}

	// A chamada a .libPaths() precisa vir antes do uso de trama::, senão a
	// lib customizada não está no search path quando tr_app é resolvido.
	if idx := strings.Index(expr, ".libPaths"); idx == -1 || idx > strings.Index(expr, "trama::tr_app") {
		t.Fatalf(".libPaths() deveria vir antes de trama::tr_app na expressão: %s", expr)
	}
}

func TestWaitForAppReady_Succeeds(t *testing.T) {
	ln, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	defer ln.Close()
	port := ln.Addr().(*net.TCPAddr).Port

	cmd := exec.Command("sleep", "2")
	capture := &OutputCapture{}
	cmd.Stdout = capture
	cmd.Stderr = capture
	if err := cmd.Start(); err != nil {
		t.Fatal(err)
	}
	defer cmd.Process.Kill()

	if err := WaitForAppReady(cmd, capture, "127.0.0.1", port, 3*time.Second); err != nil {
		t.Fatalf("esperava sucesso, got %v", err)
	}
}

func TestWaitForAppReady_DetectsEarlyExitWithoutWaitingFullTimeout(t *testing.T) {
	// Stand-in de Rscript que morre na hora, imprimindo algo antes — simula
	// um R que crasha (DLL faltando, erro de sintaxe etc) logo de cara.
	fakeRscript := filepath.Join(t.TempDir(), "fake-rscript.sh")
	script := "#!/bin/sh\necho 'erro fatal: nao achou libfoo.so'\nexit 1\n"
	if err := os.WriteFile(fakeRscript, []byte(script), 0o755); err != nil {
		t.Fatal(err)
	}

	cmd := exec.Command(fakeRscript)
	capture := &OutputCapture{}
	cmd.Stdout = capture
	cmd.Stderr = capture
	if err := cmd.Start(); err != nil {
		t.Fatal(err)
	}

	start := time.Now()
	err := WaitForAppReady(cmd, capture, "127.0.0.1", 65533, 10*time.Second)
	elapsed := time.Since(start)

	if err == nil {
		t.Fatal("esperava erro")
	}
	if !strings.Contains(err.Error(), "libfoo.so") {
		t.Fatalf("erro deveria incluir a saída capturada do processo, got: %v", err)
	}
	if elapsed >= 3*time.Second {
		t.Fatalf("deveria detectar a saída antecipada rápido, não esperar perto do timeout de 10s; levou %v", elapsed)
	}
}
