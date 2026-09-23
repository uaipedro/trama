#' Tela de início do launcher, servida localmente por `abrir()`: versão,
#' atualização, coleções, utilitários e o botão "Abrir editor". Layout de
#' uma coluna, sem framework de CSS pesado — só os tokens de
#' `inst/www/trama.css` do trama (quando o pacote está instalado na sessão)
#' mais `inst/www/launcher.css` próprio.
#' @noRd
NULL

#' Caminho do `trama.css` do pacote `trama`, se ele estiver instalado na
#' sessão que serve o launcher; `NULL` senão. `system.file()` não lança erro
#' quando o pacote não existe, só devolve `""` — daí o teste com `nzchar()`.
#' @noRd
.tl_trama_css <- function() {
  caminho <- system.file("www", "trama.css", package = "trama")
  if (!nzchar(caminho)) return(NULL)
  caminho
}

#' Botão de ação com atributo `disabled` quando `desabilitado` é `TRUE` —
#' assim uma ação em andamento (dentro de `withProgress`) não pode ser
#' disparada de novo antes de terminar, sem depender de shinyjs (o pacote só
#' importa jsonlite, shiny, utils e tools).
#' @noRd
.tl_botao <- function(id, rotulo, ..., desabilitado = FALSE, classe = "tl-btn") {
  btn <- shiny::actionButton(id, rotulo, class = classe, ...)
  if (isTRUE(desabilitado)) btn <- shiny::tagAppendAttributes(btn, disabled = NA)
  btn
}

#' Pasta de projeto sugerida por padrão no campo "Abrir editor".
#' @noRd
.tl_projeto_padrao <- function() file.path(path.expand("~"), "Trama", "meu-fluxo")

#' Arquivo onde fica lembrada a última pasta de projeto usada, para o campo
#' "Abrir editor" já vir preenchido da próxima vez que a tela abrir.
#' @noRd
.tl_ultimo_projeto_arquivo <- function() file.path(tl_home(), "ultimo-projeto.txt")

#' @noRd
.tl_ultimo_projeto_ler <- function() {
  arquivo <- .tl_ultimo_projeto_arquivo()
  if (!file.exists(arquivo)) return(.tl_projeto_padrao())
  valor <- tryCatch(readLines(arquivo, n = 1, warn = FALSE), error = function(e) character(0))
  if (!length(valor) || !nzchar(valor)) return(.tl_projeto_padrao())
  valor
}

#' @noRd
.tl_ultimo_projeto_escrever <- function(dir) {
  dir.create(tl_home(), recursive = TRUE, showWarnings = FALSE)
  writeLines(dir, .tl_ultimo_projeto_arquivo())
}

#' Abre o editor num processo `Rscript` separado, com a lib da release atual
#' na frente do `.libPaths()`. Sem `callr`, para não acrescentar dependência
#' (decidido na task 2.1 do plano).
#' @noRd
.tl_abrir_editor <- function(lib, dir) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  cmd <- sprintf(
    ".libPaths(c(%s, .libPaths())); trama::tr_app(trama::tr_project(%s))",
    deparse(lib), deparse(dir)
  )
  rscript <- file.path(R.home("bin"), "Rscript")
  system2(rscript, c("-e", shQuote(cmd)), wait = FALSE)
  invisible(NULL)
}

#' UI da tela de início. Sem `fluidPage()`/bootstrap: um `tagList()` com
#' `head` e `body` próprios, para não puxar um framework de CSS pesado numa
#' tela de uma coluna.
#' @noRd
tl_ui <- function() {
  www_launcher <- system.file("www", package = "trama.launcher")
  if (nzchar(www_launcher)) shiny::addResourcePath("tl-www", www_launcher)

  trama_css <- .tl_trama_css()
  trama_www <- if (!is.null(trama_css)) dirname(trama_css) else NULL
  folhas <- list(shiny::tags$link(rel = "stylesheet", href = "tl-www/launcher.css"))
  icone <- NULL
  marca <- NULL
  if (!is.null(trama_www)) {
    shiny::addResourcePath("tl-trama-www", trama_www)
    folhas <- c(list(shiny::tags$link(rel = "stylesheet", href = "tl-trama-www/trama.css")), folhas)
    if (file.exists(file.path(trama_www, "icon.png"))) {
      icone <- shiny::tags$link(rel = "icon", href = "tl-trama-www/icon.png")
    }
    if (file.exists(file.path(trama_www, "marca.svg"))) {
      marca <- shiny::tags$img(class = "tl-marca", src = "tl-trama-www/marca.svg", alt = "Trama")
    }
  }

  shiny::tagList(
    shiny::tags$head(
      shiny::tags$meta(charset = "utf-8"),
      shiny::tags$title("Trama"),
      folhas,
      icone
    ),
    shiny::tags$body(
      shiny::div(
        class = "tl-app",
        shiny::div(class = "tl-header", marca, shiny::tags$h1("Trama")),
        shiny::div(
          class = "tl-secao",
          shiny::tags$h2("Versão"),
          shiny::uiOutput("tl_versao")
        ),
        shiny::div(
          class = "tl-secao",
          shiny::tags$h2("Coleções"),
          shiny::uiOutput("tl_colecoes")
        ),
        shiny::div(
          class = "tl-secao",
          shiny::tags$h2("Projetos"),
          shiny::uiOutput("tl_projetos"),
          shiny::div(
            class = "tl-linha",
            shiny::textInput("tl_novo_projeto_nome", "Nome do projeto novo", value = ""),
            shiny::uiOutput("tl_btn_novo_projeto")
          ),
          shiny::div(
            class = "tl-linha",
            shiny::textInput("tl_abrir_pasta", "Ou abrir uma pasta existente", value = ""),
            shiny::uiOutput("tl_btn_abrir_pasta")
          )
        ),
        shiny::div(
          class = "tl-secao",
          shiny::tags$h2("Abrir editor"),
          shiny::textInput("tl_projeto", "Pasta do projeto", value = .tl_ultimo_projeto_ler()),
          shiny::uiOutput("tl_btn_abrir")
        ),
        shiny::div(
          class = "tl-secao",
          shiny::tags$h2("Utilitários"),
          shiny::uiOutput("tl_utilitarios")
        )
      )
    )
  )
}

#' Server da tela de início. Separado do `shinyApp()` final para poder ser
#' testado com `shiny::testServer(tl_server, {...})`, sem subir o Shiny de
#' verdade (task 2.1 do plano).
#' @noRd
tl_server <- function(input, output, session) {
  m0 <- tl_manifest_fetch()
  manifesto <- shiny::reactiveVal(m0)
  status <- shiny::reactiveVal(tl_status(m = m0, s = tl_state_read()))
  ocupado <- shiny::reactiveVal(FALSE)
  sem_internet <- shiny::reactiveVal(is.null(m0) && !nzchar(status()$release_instalada))

  # Trava de verdade contra duplo clique: um flag simples no closure de
  # `tl_server`, checado de forma síncrona no primeiro passo de
  # `rodar_acao()`. `ocupado` (reactiveVal) só desabilita o botão na
  # próxima repintura — dois cliques rápidos ainda cabem antes dela rodar,
  # e são esses dois cliques que este flag barra (não reage, é lido e
  # escrito na hora, fora do ciclo reativo).
  ocupado_flag <- FALSE

  atualizar_status <- function() status(tl_status(m = manifesto(), s = tl_state_read()))

  # Roda `acao()` com `withProgress`, desabilitando os botões (via
  # `ocupado()`) e mostra o erro numa notificação com o caminho do log se
  # falhar. Sempre reatualiza o status ao final, com sucesso ou falha.
  rodar_acao <- function(titulo, acao) {
    if (isTRUE(ocupado_flag)) {
      shiny::showNotification("Aguarde a ação atual terminar.", type = "warning")
      return(invisible(NULL))
    }
    ocupado_flag <<- TRUE
    ocupado(TRUE)
    on.exit({
      ocupado_flag <<- FALSE
      ocupado(FALSE)
    }, add = TRUE)
    shiny::withProgress(message = titulo, value = 0, {
      tryCatch(
        acao(function(msg) shiny::incProgress(1 / 8, detail = msg)),
        error = function(e) {
          shiny::showNotification(conditionMessage(e), type = "error", duration = NULL)
        }
      )
    })
    atualizar_status()
  }

  # Primeira instalação: dispara sozinha quando a tela sobe sem release
  # nenhuma (em vez de abrir() instalar antes do runApp — offline nesse
  # ponto derrubava a sessão inteira antes de existir UI para avisar).
  # Sem internet, não tenta instalar: mostra a mensagem com o botão
  # "Tentar de novo" (`tl_tentar_instalar`, abaixo).
  tentar_instalar <- function() {
    m <- tl_manifest_fetch()
    manifesto(m)
    if (is.null(m)) {
      sem_internet(TRUE)
      return(invisible(NULL))
    }
    sem_internet(FALSE)
    rodar_acao("Instalando…", function(progresso) tl_install_release(m, progresso = progresso))
  }

  # Usa o `m0` já buscado na inicialização (acima) em vez de chamar
  # `tentar_instalar()` — que buscaria o manifesto de novo à toa.
  if (!nzchar(status()$release_instalada) && !is.null(m0)) {
    rodar_acao("Instalando…", function(progresso) tl_install_release(m0, progresso = progresso))
  }

  shiny::observeEvent(input$tl_tentar_instalar, tentar_instalar())

  output$tl_versao <- shiny::renderUI({
    st <- status()

    linhas <- lapply(seq_len(nrow(st$pacotes)), function(i) {
      p <- st$pacotes[i, ]
      shiny::tags$tr(
        shiny::tags$td(p$nome),
        shiny::tags$td(if (is.na(p$instalada)) "—" else p$instalada),
        shiny::tags$td(if (is.na(p$disponivel)) "—" else p$disponivel)
      )
    })
    tabela <- shiny::tags$table(
      class = "tl-tabela",
      shiny::tags$tr(shiny::tags$th("Pacote"), shiny::tags$th("Instalada"), shiny::tags$th("Disponível")),
      linhas
    )

    acao_versao <- if (!nzchar(st$release_instalada) && sem_internet()) {
      shiny::tagList(
        shiny::tags$p(class = "tl-aviso", "Sem internet para instalar. Verifique a conexão e tente de novo."),
        .tl_botao("tl_tentar_instalar", "Tentar de novo", desabilitado = ocupado())
      )
    } else if (isTRUE(st$troca_de_r)) {
      shiny::tags$p(
        class = "tl-aviso",
        sprintf("Esta versão do trama exige R %s. Baixe o novo instalador em ", st$r_exigido),
        shiny::tags$a(
          href = "https://github.com/uaipedro/trama/releases/latest",
          "github.com/uaipedro/trama/releases/latest"
        ), "."
      )
    } else if (!isTRUE(st$atualizar)) {
      if (nzchar(st$release_instalada)) shiny::tags$span(class = "tl-selo tl-selo-ok", "Atualizado") else NULL
    } else {
      rotulo <- if (nzchar(st$release_instalada)) {
        sprintf("Atualizar para %s", st$release_disponivel)
      } else {
        sprintf("Instalar %s", st$release_disponivel)
      }
      .tl_botao("tl_atualizar", rotulo, desabilitado = ocupado(), classe = "tl-btn tl-btn-primario")
    }

    shiny::tagList(
      shiny::tags$p(sprintf(
        "Release instalada: %s", if (nzchar(st$release_instalada)) st$release_instalada else "nenhuma"
      )),
      shiny::tags$p(sprintf(
        "R instalado: %s (exigido: %s)", st$r_instalado,
        if (is.na(st$r_exigido)) "—" else st$r_exigido
      )),
      tabela,
      acao_versao
    )
  })

  shiny::observeEvent(input$tl_atualizar, {
    m <- manifesto()
    if (is.null(m)) return(invisible(NULL))
    rodar_acao("Instalando…", function(progresso) tl_install_release(m, progresso = progresso))
  })

  output$tl_colecoes <- shiny::renderUI({
    st <- status()
    if (!nrow(st$colecoes)) {
      return(shiny::tags$p(class = "tl-dim", "Nenhuma coleção disponível nesta release."))
    }

    linhas <- lapply(seq_len(nrow(st$colecoes)), function(i) {
      c_ <- st$colecoes[i, ]
      requer <- if (length(c_$requires[[1]])) {
        shiny::tags$span(class = "tl-dim", sprintf(" (requer %s)", paste(c_$requires[[1]], collapse = ", ")))
      } else NULL
      botao <- if (isTRUE(c_$instalada)) {
        .tl_botao(sprintf("tl_remover_%s", c_$nome), "Remover", desabilitado = ocupado())
      } else if (isTRUE(c_$disponivel)) {
        .tl_botao(sprintf("tl_instalar_%s", c_$nome), "Instalar", desabilitado = ocupado())
      } else NULL
      shiny::tags$tr(
        shiny::tags$td(if (is.na(c_$titulo)) c_$nome else c_$titulo, requer),
        shiny::tags$td(botao)
      )
    })
    shiny::tags$table(class = "tl-tabela", linhas)
  })

  # Um observer por coleção do manifesto, criado uma vez na abertura da
  # sessão — as coleções de uma release não mudam durante a sessão, só o
  # que está instalado (e isso o `status()` já recalcula).
  m0 <- manifesto()
  for (nome in if (!is.null(m0)) names(m0$collections) else character(0)) {
    local({
      nm <- nome
      shiny::observeEvent(input[[sprintf("tl_instalar_%s", nm)]], {
        rodar_acao(
          sprintf("Instalando %s…", nm),
          function(progresso) tl_collection_add(manifesto(), nm, progresso = progresso)
        )
      })
      shiny::observeEvent(input[[sprintf("tl_remover_%s", nm)]], {
        rodar_acao(
          sprintf("Removendo %s…", nm),
          function(progresso) tl_collection_remove(nm, progresso = progresso)
        )
      })
    })
  }

  projetos <- shiny::reactiveVal(tl_projects())
  atualizar_projetos <- function() projetos(tl_projects())

  output$tl_projetos <- shiny::renderUI({
    pr <- projetos()
    if (!nrow(pr)) return(shiny::tags$p(class = "tl-dim", "Nenhum projeto ainda."))

    linhas <- lapply(seq_len(nrow(pr)), function(i) {
      p <- pr[i, ]
      rotulo <- if (isTRUE(p$aberto)) shiny::tags$span(class = "tl-dim", " (aberto)") else NULL
      shiny::tags$tr(
        shiny::tags$td(p$nome, rotulo),
        shiny::tags$td(shiny::tags$button(
          class = "tl-btn", disabled = if (ocupado()) NA else NULL,
          onclick = sprintf(
            "Shiny.setInputValue('tl_abrir_projeto', %s, {priority: 'event'})",
            jsonlite::toJSON(p$caminho, auto_unbox = TRUE)
          ),
          "Abrir"
        ))
      )
    })
    shiny::tags$table(class = "tl-tabela", linhas)
  })

  shiny::observeEvent(input$tl_abrir_projeto, {
    rodar_acao("Abrindo projeto…", function(progresso) {
      progresso("Abrindo…")
      tl_project_open(input$tl_abrir_projeto)
    })
    atualizar_projetos()
  })

  output$tl_btn_novo_projeto <- shiny::renderUI({
    .tl_botao("tl_novo_projeto", "Criar projeto", desabilitado = ocupado())
  })

  shiny::observeEvent(input$tl_novo_projeto, {
    tryCatch({
      tl_project_new(input$tl_novo_projeto_nome)
      atualizar_projetos()
      shiny::updateTextInput(session, "tl_novo_projeto_nome", value = "")
      shiny::showNotification(sprintf("Projeto '%s' criado.", input$tl_novo_projeto_nome), type = "message")
    }, error = function(e) shiny::showNotification(conditionMessage(e), type = "error"))
  })

  output$tl_btn_abrir_pasta <- shiny::renderUI({
    .tl_botao("tl_abrir_pasta_btn", "Abrir pasta…", desabilitado = ocupado())
  })

  shiny::observeEvent(input$tl_abrir_pasta_btn, {
    caminho <- input$tl_abrir_pasta
    if (!nzchar(caminho) || !dir.exists(caminho)) {
      shiny::showNotification("Pasta não encontrada.", type = "error")
      return(invisible(NULL))
    }
    rodar_acao("Abrindo projeto…", function(progresso) {
      progresso("Abrindo…")
      tl_project_open(caminho)
    })
    atualizar_projetos()
  })

  output$tl_btn_abrir <- shiny::renderUI({
    .tl_botao(
      "tl_abrir_editor", "Abrir editor",
      desabilitado = ocupado() || !nzchar(status()$release_instalada),
      classe = "tl-btn tl-btn-primario"
    )
  })

  shiny::observeEvent(input$tl_abrir_editor, {
    dir <- input$tl_projeto
    if (!nzchar(dir)) dir <- .tl_projeto_padrao()
    .tl_ultimo_projeto_escrever(dir)
    .tl_abrir_editor(tl_lib_dir(tl_state_read()$atual), dir)
    shiny::showNotification("Editor abrindo…", type = "message")
  })

  output$tl_utilitarios <- shiny::renderUI({
    shiny::tagList(
      .tl_botao("tl_reparar", "Reparar (reinstalar a versão atual)", desabilitado = ocupado()),
      .tl_botao(
        "tl_voltar_versao", "Voltar versão",
        desabilitado = ocupado() || !length(tl_state_read()$anteriores)
      ),
      .tl_botao("tl_abrir_logs", "Abrir pasta de logs", desabilitado = ocupado())
    )
  })

  shiny::observeEvent(input$tl_reparar, {
    m <- manifesto()
    if (is.null(m)) {
      shiny::showNotification(
        "Sem conexão para reparar: não foi possível ler o manifesto.",
        type = "error"
      )
      return(invisible(NULL))
    }
    rodar_acao("Reparando…", function(progresso) {
      tl_install_release(m, colecoes = tl_state_read()$colecoes, progresso = progresso)
    })
  })

  shiny::observeEvent(input$tl_voltar_versao, {
    rodar_acao("Voltando versão…", function(progresso) tl_rollback())
  })

  shiny::observeEvent(input$tl_abrir_logs, {
    dir.create(tl_log_dir(), recursive = TRUE, showWarnings = FALSE)
    utils::browseURL(tl_log_dir())
  })
}

#' Monta a `shinyApp` da tela de início.
#' @noRd
tl_app <- function() shiny::shinyApp(ui = tl_ui(), server = tl_server)
