package main

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"runtime"
	"time"

	"trama-launcher/internal/applauncher"
	"trama-launcher/internal/config"
	"trama-launcher/internal/envcheck"
	"trama-launcher/internal/pakrunner"
	"trama-launcher/internal/rfetch"
)

const rVersion = "4.4.1"

// shinyPort é a porta padrão do editor trama (tr_port_default(), R/app.R) —
// 8726 ("TRAM" no teclado do telefone), não a porta genérica do Shiny.
const shinyPort = 8726

// portTimeout é quanto tempo esperamos a porta do Shiny responder após subir
// o processo. Generoso de propósito: carregar shiny/httpuv/ragg num Windows
// "frio" (com antivírus escaneando cada DLL na primeira vez) pode passar
// fácil de 30s. Seguro aumentar porque WaitForAppReady detecta um processo
// que morre cedo na hora, sem esperar esse prazo inteiro.
const portTimeout = 90 * time.Second

// App struct
type App struct {
	ctx  context.Context
	base string // diretório base do launcher (R portátil, lib de pacotes, config.json)
}

// NewApp creates a new App application struct
func NewApp() *App {
	return &App{}
}

// startup is called when the app starts. Assinatura fixa pelo hook do Wails
// (OnStartup em main.go); resolvemos o diretório base aqui dentro em vez de
// recebê-lo como parâmetro.
func (a *App) startup(ctx context.Context) {
	a.ctx = ctx
	a.base = resolveBaseDir()
	if err := os.MkdirAll(a.base, 0o755); err != nil {
		// Não podemos abortar o startup (o hook do Wails não retorna erro),
		// e uma falha aqui só vai se manifestar de forma mais específica
		// quando uma operação concreta precisar do diretório (download,
		// gravação de config.json etc). Registramos pra facilitar debug.
		println("trama-launcher: falha ao criar diretório base", a.base, ":", err.Error())
	}
}

// resolveBaseDir determina o diretório de dados do launcher: R portátil,
// lib de pacotes instalados e config.json. Usamos ~/.trama-launcher, que
// funciona da mesma forma em Linux, macOS e Windows sem branch por GOOS.
func resolveBaseDir() string {
	home, err := os.UserHomeDir()
	if err != nil {
		// Fallback conservador: diretório de trabalho atual.
		return ".trama-launcher"
	}
	return filepath.Join(home, ".trama-launcher")
}

// CheckEnvironment expõe o status atual pro frontend decidir qual tela mostrar.
func (a *App) CheckEnvironment() envcheck.EnvStatus {
	return envcheck.Status(a.base, rVersion)
}

// EnsureRPortable baixa e extrai o R portátil se ainda não estiver presente.
func (a *App) EnsureRPortable() error {
	st := envcheck.Status(a.base, rVersion)
	if st.RPortableInstalled {
		return nil
	}
	src, err := rfetch.ResolveSource(runtime.GOOS, runtime.GOARCH, rVersion)
	if err != nil {
		return err
	}
	cacheDir := filepath.Join(a.base, "cache")
	if err := os.MkdirAll(cacheDir, 0o755); err != nil {
		return err
	}
	archive := filepath.Join(cacheDir, "r-download")
	if err := rfetch.Download(src.URL, src.Checksum, archive, 2); err != nil {
		return err
	}
	dest := filepath.Join(a.base, "r", rVersion)
	if runtime.GOOS == "windows" {
		return rfetch.ExtractZip(archive, dest)
	}
	return rfetch.ExtractTarGz(archive, dest)
}

// InstallCollections instala o núcleo + as coleções pedidas e atualiza o config.json.
func (a *App) InstallCollections(collections []string) error {
	st := envcheck.Status(a.base, rVersion)
	pkgs := []string{"uaipedro/trama", "uaipedro/trama/collections/trama.data", "uaipedro/trama/collections/trama.view"}
	for _, c := range collections {
		pkgs = append(pkgs, "uaipedro/trama/collections/"+c)
	}

	script := pakrunner.BuildInstallScript(pkgs, st.LibPath)
	if err := pakrunner.Run(a.ctx, st.RscriptPath, script, func(e pakrunner.Event) {
		// eventos vão pro log da UI via Wails events (ver Fase 6)
	}); err != nil {
		return err
	}

	cfgPath := filepath.Join(a.base, "config.json")
	cfg, _ := config.Load(cfgPath)
	cfg.RVersion = rVersion
	cfg.InstalledCollections = collections
	return config.Save(cfgPath, cfg)
}

// errTramaMissing é retornado quando os pacotes não estão mais em disco no
// momento de abrir o app, mesmo depois de uma instalação aparentemente bem
// sucedida — o suspeito mais comum é antivírus/Windows Defender colocando
// em quarentena os pacotes R recém-compilados/baixados, que costumam disparar
// falso positivo de heurística. Checamos isso ANTES de tentar subir o R, em
// vez de deixar o usuário ver um traceback críptico de "loadNamespace".
var errTramaMissing = errors.New(
	"os pacotes do trama não foram encontrados — algo os removeu depois da " +
		"instalação (suspeito comum: antivírus colocando em quarentena os " +
		"pacotes recém-instalados). Adicione uma exclusão pra pasta do " +
		"launcher no seu antivírus e instale as coleções de novo.",
)

// OpenTrama sobe o app e abre o navegador quando a porta responder.
func (a *App) OpenTrama() error {
	st := envcheck.Status(a.base, rVersion)
	if !st.TramaInstalled {
		return errTramaMissing
	}
	cfgPath := filepath.Join(a.base, "config.json")
	cfg, _ := config.Load(cfgPath)
	projectDir := cfg.DefaultProjectDir
	if projectDir == "" {
		projectDir = filepath.Join(a.base, "projects", "default")
	}

	cmd, capture, err := applauncher.StartTramaApp(a.ctx, st.RscriptPath, st.LibPath, projectDir)
	if err != nil {
		return err
	}
	if err := applauncher.WaitForAppReady(cmd, capture, "127.0.0.1", shinyPort, portTimeout); err != nil {
		return err
	}
	return applauncher.OpenBrowser(fmt.Sprintf("http://127.0.0.1:%d", shinyPort))
}
