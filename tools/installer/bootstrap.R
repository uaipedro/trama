#!/usr/bin/env Rscript
# bootstrap.R — o único script que os instaladores nativos rodam
# (Trama-Setup.exe no Windows via Rscript.exe, install.sh no Linux).
# Instala o trama.launcher e a release descrita no manifesto numa máquina
# que só tem o R (sem trama.launcher ainda), e sai. Toda a lógica de
# instalação de verdade mora em trama.launcher::tl_install_release(); este
# script só faz o suficiente para conseguir chamá-la: garante jsonlite,
# lê o manifesto, instala trama.launcher na lib da release e delega.
#
# `--local <dir>`: em vez de baixar de repositórios, instala jsonlite,
# trama.launcher e os pacotes do núcleo a partir de tarballs em <dir>
# (formato `<pacote>_<versão>.tar.gz`, o que `R CMD build` produz). Existe
# para o CI testar o caminho inteiro antes de o r-universe existir
# (task 4.1 do plano), e para quem quiser reproduzir isso localmente.

.bs_arg_local <- function(args) {
  i <- match("--local", args)
  if (is.na(i) || i == length(args)) return(NULL)
  args[[i + 1]]
}

local_dir <- .bs_arg_local(commandArgs(trailingOnly = TRUE))

# trama_home: calculado logo no início (não depende de rede nem do
# manifesto) só para já poder abrir o log antes de qualquer coisa que possa
# falhar — inclusive a instalação do jsonlite, o passo mais cedo que há.
trama_home <- if (.Platform$OS.type == "windows") {
  file.path(Sys.getenv("LOCALAPPDATA"), "Trama")
} else {
  h <- Sys.getenv("TRAMA_HOME", "")
  if (nzchar(h)) h else file.path(path.expand("~"), ".local", "share", "trama")
}

# --- Log: tudo que este script imprime (inclusive mensagens de erro) vai
# para TRAMA_HOME/logs/bootstrap-<data>.log desde a primeira linha — é
# esse arquivo que a mensagem de erro do Trama-Setup.iss aponta quando o
# bootstrap falha, então precisa existir mesmo quando a falha é bem no
# início (sem jsonlite, sem rede). `cat`/`print` vão para console E log
# (sink de output com split); `message()`/`warning()` (é como
# `install.packages()` relata a maioria dos problemas) não dá para "split"
# com `sink()` — R recusa —, então são espelhados manualmente por um
# `withCallingHandlers()` em volta do corpo do script, sem abafar o
# comportamento padrão (o console continua mostrando tudo em tempo real).
dir.create(file.path(trama_home, "logs"), recursive = TRUE, showWarnings = FALSE)
.bs_log_arquivo <- file.path(trama_home, "logs", sprintf("bootstrap-%s.log", format(Sys.time(), "%Y%m%d-%H%M%S")))
.bs_log_con <- file(.bs_log_arquivo, open = "wt")
sink(.bs_log_con, type = "output", split = TRUE)

# Conexão SEPARADA para as mensagens (aberta em modo "append" no mesmo
# arquivo): escrever direto na conexão que o `sink(split = TRUE)` acima já
# está usando ecoa de volta pro console (testado — vira mensagem
# duplicada), então as mensagens usam este segundo handle.
.bs_msg_con <- file(.bs_log_arquivo, open = "at")

.bs_log_linha <- function(prefixo, texto) {
  cat(sprintf("%s%s", prefixo, texto), file = .bs_msg_con)
  if (!grepl("\n$", texto)) cat("\n", file = .bs_msg_con)
  flush(.bs_msg_con)
}

.bs_fechar_log <- function() {
  sink(type = "output")
  close(.bs_log_con)
  close(.bs_msg_con)
}

.bs_erro <- function(...) {
  msg <- sprintf("ERRO: %s", sprintf(...))
  message(msg)
  .bs_fechar_log()
  quit(save = "no", status = 1)
}

.bs_tarball <- function(dir, pacote) {
  candidatos <- Sys.glob(file.path(dir, sprintf("%s_*.tar.gz", pacote)))
  if (!length(candidatos)) {
    .bs_erro("--local '%s' não tem um tarball de '%s' (esperava '%s_<versão>.tar.gz').",
             dir, pacote, pacote)
  }
  candidatos[[1]]
}

.bs_codename_linux <- function() {
  arquivo <- "/etc/os-release"
  if (!file.exists(arquivo)) return(NA_character_)
  linhas <- readLines(arquivo, warn = FALSE)
  alvo <- grep("^VERSION_CODENAME=", linhas, value = TRUE)
  if (!length(alvo)) return(NA_character_)
  gsub('^VERSION_CODENAME="?|"?$', "", alvo[[1]])
}

# Repositórios do manifesto + P3M preso no snapshot. Reimplementado aqui
# (é uma cópia de `tl_repos()`, ver R/manifest.R) porque, neste ponto do
# bootstrap, o trama.launcher ainda não está instalado — é ele quem
# normalmente calcula isso.
.bs_repos <- function(m) {
  codename <- if (.Platform$OS.type == "windows") NA_character_ else .bs_codename_linux()
  snapshot <- if (.Platform$OS.type == "windows" || is.na(codename) || !nzchar(codename)) {
    sprintf("https://packagemanager.posit.co/cran/%s", m$cran_snapshot)
  } else {
    sprintf("https://packagemanager.posit.co/cran/__linux__/%s/%s", codename, m$cran_snapshot)
  }
  repos_manifesto <- if (is.null(m$repos)) character(0) else unlist(m$repos, use.names = FALSE)
  c(repos_manifesto, P3M = snapshot)
}

.bs_so_binario <- function() {
  .Platform$OS.type == "windows" || identical(Sys.info()[["sysname"]], "Darwin")
}

# Compara `x.y.z` até `x.y` — mesma regra de tl_status()/troca_de_r: um
# patch novo do R (4.5.1 -> 4.5.2) não é uma linha diferente.
.bs_r_major_minor <- function(versao) {
  partes <- strsplit(as.character(versao), "\\.")[[1]]
  paste(partes[1:2], collapse = ".")
}

# --- Corpo do script, em volta de handlers que espelham message()/warning()
# (as de install.packages() inclusive) no log, sem abafar o console. ------
withCallingHandlers(
  {
    # --- 1. jsonlite: precisa dele para ler o manifesto. Se não estiver
    # disponível, instala numa lib temporária. Sem `--local`, usa o
    # espelho "latest" do P3M em vez do snapshot do manifesto: nesse ponto
    # o manifesto ainda não foi lido (é jsonlite quem lê JSON), e jsonlite
    # não precisa estar preso a uma data — só precisa ser binário.
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
      lib_tmp <- file.path(tempdir(), "trama-bootstrap-lib")
      dir.create(lib_tmp, recursive = TRUE, showWarnings = FALSE)
      .libPaths(c(lib_tmp, .libPaths()))

      if (!is.null(local_dir)) {
        utils::install.packages(.bs_tarball(local_dir, "jsonlite"),
                                 lib = lib_tmp, repos = NULL, type = "source")
      } else {
        if (.bs_so_binario()) {
          options(pkgType = "binary", install.packages.compile.from.source = "never")
        }
        utils::install.packages("jsonlite", lib = lib_tmp,
                                 repos = "https://packagemanager.posit.co/cran/latest",
                                 dependencies = NA)
      }

      if (!requireNamespace("jsonlite", quietly = TRUE)) {
        .bs_erro("Não consegui instalar o jsonlite; sem ele não dá para ler o manifesto de release.")
      }
    }

    # --- 2. Manifesto
    manifest_src <- Sys.getenv(
      "TRAMA_MANIFEST_URL",
      "https://github.com/uaipedro/trama/releases/latest/download/release.json"
    )
    m <- tryCatch(
      jsonlite::fromJSON(manifest_src, simplifyVector = FALSE),
      error = function(e) {
        .bs_erro("Não consegui ler o manifesto de release em '%s': %s", manifest_src, conditionMessage(e))
      }
    )
    faltando <- setdiff(c("trama", "r", "cran_snapshot", "core"), names(m))
    if (length(faltando)) {
      .bs_erro("Manifesto de release inválido: faltam as chaves obrigatórias: %s.", paste(faltando, collapse = ", "))
    }

    # R instalado precisa estar na mesma linha (major.minor) que o
    # manifesto exige — um R errado instalaria pacotes binários da ABI
    # errada, e o problema só apareceria depois, de um jeito confuso, ao
    # tentar carregar o pacote. Falha aqui, cedo e claro.
    if (!identical(.bs_r_major_minor(getRversion()), .bs_r_major_minor(m$r))) {
      .bs_erro(
        "R instalado (%s) não é a linha exigida pelo manifesto (%s). %s",
        as.character(getRversion()), m$r,
        if (.Platform$OS.type == "windows") {
          "Baixe o Trama-Setup.exe mais recente em github.com/uaipedro/trama/releases/latest."
        } else {
          "Rode o install.sh de novo para atualizar o R."
        }
      )
    }

    # --- 3. trama.launcher, na lib da própria release (mesma lib do
    # núcleo: é lá que `abrir()` espera encontrá-lo, via .libPaths()).
    lib_release <- file.path(trama_home, "lib", m$trama)
    dir.create(lib_release, recursive = TRUE, showWarnings = FALSE)
    .libPaths(c(lib_release, .libPaths()))

    if (!is.null(local_dir)) {
      utils::install.packages(.bs_tarball(local_dir, "trama.launcher"),
                               lib = lib_release, repos = NULL, type = "source", dependencies = NA)
    } else {
      repos <- .bs_repos(m)
      if (.bs_so_binario()) options(pkgType = "binary", install.packages.compile.from.source = "never")
      utils::install.packages("trama.launcher", lib = lib_release, repos = repos, dependencies = NA)
    }

    if (!dir.exists(file.path(lib_release, "trama.launcher"))) {
      .bs_erro("Não consegui instalar o trama.launcher. Veja acima o motivo do install.packages().")
    }

    # --- 4. A release inteira (núcleo + eventuais coleções). Sem
    # `--local`, delega para o motor de verdade, que já sabe rotacionar
    # libs antigas, gravar o estado só em caso de sucesso etc. Com
    # `--local`, o trama.launcher recém-instalado ainda não pode instalar
    # a si mesmo nem ler tarballs locais (isso é o próprio bootstrap.R
    # fazendo o papel do r-universe para o CI), então instala o resto do
    # núcleo manualmente e grava o estado do mesmo jeito que
    # `tl_install_release()` faria.
    if (is.null(local_dir)) {
      tryCatch(
        trama.launcher::tl_install_release(m, colecoes = character()),
        error = function(e) .bs_erro("Falhou a instalação da release: %s", conditionMessage(e))
      )
    } else {
      pacotes <- setdiff(names(m$core), "trama.launcher")
      for (p in pacotes) {
        utils::install.packages(.bs_tarball(local_dir, p),
                                 lib = lib_release, repos = NULL, type = "source", dependencies = NA)
      }
      todos <- names(m$core)
      faltando_pkgs <- todos[!vapply(todos, function(p) dir.exists(file.path(lib_release, p)), logical(1))]
      if (length(faltando_pkgs)) {
        .bs_erro("Falhou a instalação de: %s.", paste(faltando_pkgs, collapse = ", "))
      }

      estado_arquivo <- file.path(trama_home, "estado.json")
      anteriores <- character(0)
      if (file.exists(estado_arquivo)) {
        antigo <- tryCatch(jsonlite::fromJSON(estado_arquivo, simplifyVector = TRUE), error = function(e) NULL)
        if (!is.null(antigo) && !is.null(antigo$anteriores)) anteriores <- as.character(antigo$anteriores)
      }
      novo_estado <- list(atual = m$trama, anteriores = anteriores, colecoes = character(0), recentes = character(0))
      tmp <- tempfile(pattern = "estado-", tmpdir = trama_home, fileext = ".json.tmp")
      writeLines(jsonlite::toJSON(novo_estado, auto_unbox = TRUE), tmp)
      invisible(file.rename(tmp, estado_arquivo))
    }

    message(sprintf("trama %s instalado.", m$trama))
  },
  message = function(cond) .bs_log_linha("", conditionMessage(cond)),
  warning = function(cond) .bs_log_linha("Aviso: ", conditionMessage(cond))
)

.bs_fechar_log()
quit(save = "no", status = 0)
