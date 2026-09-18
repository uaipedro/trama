package applauncher

import (
	"net"
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
