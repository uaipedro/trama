#' Tela de início do launcher, servida localmente por `abrir()`: versão,
#' atualização, coleções, projetos e utilitários. A UI é um `htmlTemplate`
#' próprio (`inst/www/launcher.html` + `launcher.css` + `launcher.js`) — sem
#' widgets padrão do Shiny: as abas trocam no cliente, os dados chegam por
#' `session$sendCustomMessage()` e as ações disparam por
#' `Shiny.setInputValue(..., {priority: "event"})` nos MESMOS nomes de input
#' que o server já usava antes do redesenho (fase 2b do plano) — só a UI
#' mudou, o contrato com o server (e os testes de `tl_server()`) não.
#' @noRd
NULL

#' Pasta de projeto sugerida por padrão no campo "Nome do projeto novo".
#' @noRd
.tl_projeto_padrao <- function() file.path(path.expand("~"), "Trama", "meu-fluxo")

#' UI da tela de início: `htmlTemplate` de `inst/www/launcher.html`, que traz
#' `{{ headContent() }}` no `<head>` para as dependências do Shiny (preciso
#' mesmo sem widgets padrão: é o que dá `Shiny.setInputValue`,
#' `sendCustomMessage` e `showNotification`). `marca.svg`/`marca-min.svg`/
#' `icon.png` são copiados para o `inst/www` deste pacote (não lidos do
#' `trama` instalado): o launcher instala o `trama`, não pode depender dele
#' já existir na sessão.
#' @noRd
tl_ui <- function() {
  www_launcher <- system.file("www", package = "trama.launcher")
  if (nzchar(www_launcher)) shiny::addResourcePath("tl-www", www_launcher)
  shiny::htmlTemplate(file.path(www_launcher, "launcher.html"))
}

#' Versão de `r_exigido`/`r_instalado` como texto, para o payload JSON — sem
#' isso `getRversion()` (um objeto `R_system_version`) e `NA_character_`
#' viram tipos que o `jsonlite` não serializa do jeito esperado pelo JS.
#' @noRd
.tl_texto <- function(x) if (is.null(x) || (length(x) == 1 && is.na(x))) NULL else as.character(x)

#' Monta o payload de status enviado ao JS (`tl-status`): a mesma decisão
#' que antes vivia em `output$tl_versao` (selo/aviso/botão conforme
#' `atualizar`/`troca_de_r`/`sem_internet`), só que como dado em vez de UI já
#' desenhada — o `launcher.js` decide o HTML.
#' @noRd
.tl_status_payload <- function(st, ocupado, sem_internet, tem_anteriores) {
  pacotes <- lapply(seq_len(nrow(st$pacotes)), function(i) {
    p <- st$pacotes[i, ]
    list(nome = p$nome, instalada = .tl_texto(p$instalada), disponivel = .tl_texto(p$disponivel))
  })

  base <- list(
    release_instalada = st$release_instalada,
    release_disponivel = .tl_texto(st$release_disponivel),
    r_instalado = as.character(st$r_instalado),
    r_exigido = .tl_texto(st$r_exigido),
    ocupado = isTRUE(ocupado),
    tem_anteriores = isTRUE(tem_anteriores),
    pacotes = pacotes
  )

  if (!nzchar(st$release_instalada) && isTRUE(sem_internet)) {
    return(utils::modifyList(base, list(
      selo = "sem_internet",
      pill_texto = "Sem conexão",
      aviso = "Sem internet para instalar. Verifique a conexão e tente de novo.",
      acao = list(tipo = "botao", input = "tl_tentar_instalar", rotulo = "Tentar de novo")
    )))
  }

  if (isTRUE(st$troca_de_r)) {
    return(utils::modifyList(base, list(
      selo = "trocar_r",
      pill_texto = sprintf("R %s · trama %s · requer novo R", st$r_instalado, st$release_instalada),
      aviso = sprintf(
        "Esta versão do trama exige R %s. Baixe o novo instalador em:", .tl_texto(st$r_exigido)
      ),
      acao = list(
        tipo = "link", href = "https://github.com/uaipedro/trama/releases/latest",
        rotulo = "github.com/uaipedro/trama/releases/latest"
      )
    )))
  }

  if (!isTRUE(st$atualizar)) {
    selo <- if (nzchar(st$release_instalada)) "atualizado" else "nenhuma"
    texto <- if (nzchar(st$release_instalada)) {
      sprintf("R %s · trama %s · Atualizado", st$r_instalado, st$release_instalada)
    } else {
      "Nenhuma versão instalada"
    }
    return(utils::modifyList(base, list(selo = selo, pill_texto = texto, aviso = NULL, acao = NULL)))
  }

  rotulo <- if (nzchar(st$release_instalada)) {
    sprintf("Atualizar para %s", st$release_disponivel)
  } else {
    sprintf("Instalar %s", st$release_disponivel)
  }
  texto <- if (nzchar(st$release_instalada)) {
    sprintf("R %s · trama %s · Atualização disponível", st$r_instalado, st$release_instalada)
  } else {
    sprintf("trama %s disponível", st$release_disponivel)
  }
  utils::modifyList(base, list(
    selo = "desatualizado", pill_texto = texto, aviso = NULL,
    acao = list(tipo = "botao", input = "tl_atualizar", rotulo = rotulo)
  ))
}

#' Monta o payload de coleções enviado ao JS (`tl-colecoes`), a partir do
#' data.frame de `tl_status()`.
#' @noRd
.tl_colecoes_payload <- function(st) {
  if (!nrow(st$colecoes)) return(list())
  lapply(seq_len(nrow(st$colecoes)), function(i) {
    c_ <- st$colecoes[i, ]
    requer <- unlist(c_$requires[[1]], use.names = FALSE)
    list(
      nome = c_$nome,
      titulo = if (is.na(c_$titulo)) c_$nome else c_$titulo,
      instalada = isTRUE(c_$instalada),
      disponivel = isTRUE(c_$disponivel),
      requer = if (length(requer)) sprintf("Requer %s", paste(requer, collapse = ", ")) else NULL
    )
  })
}

#' Monta o payload de projetos enviado ao JS (`tl-projetos`), a partir do
#' data.frame de `tl_projects()`.
#' @noRd
.tl_projetos_payload <- function(pr) {
  if (!nrow(pr)) return(list())
  lapply(seq_len(nrow(pr)), function(i) {
    p <- pr[i, ]
    list(
      nome = p$nome, caminho = p$caminho, aberto = isTRUE(p$aberto),
      modificado = if (is.na(p$modificado)) "" else format(p$modificado, "%d/%m %H:%M")
    )
  })
}

#' Server da tela de início. Separado do `shinyApp()` final para poder ser
#' testado com `shiny::testServer(tl_server, {...})`, sem subir o Shiny de
#' verdade nem a UI em JS (task 2.1 do plano; os nomes de input testados —
#' `tl_atualizar`, `tl_instalar_<nome>`, `tl_remover_<nome>`,
#' `tl_voltar_versao`, `tl_reparar`, `tl_tentar_instalar` — continuam os
#' mesmos após o redesenho da fase 2b).
#' @noRd
tl_server <- function(input, output, session) {
  m0 <- tl_manifest_fetch()
  manifesto <- shiny::reactiveVal(m0)
  status <- shiny::reactiveVal(tl_status(m = m0, s = tl_state_read()))
  ocupado <- shiny::reactiveVal(FALSE)
  sem_internet <- shiny::reactiveVal(is.null(m0) && !nzchar(status()$release_instalada))
  projetos <- shiny::reactiveVal(tl_projects())

  # Trava de verdade contra duplo clique: um flag simples no closure de
  # `tl_server`, checado de forma síncrona no primeiro passo de
  # `rodar_acao()`. `ocupado` (reactiveVal) só desabilita o botão na
  # próxima repintura — dois cliques rápidos ainda cabem antes dela rodar,
  # e são esses dois cliques que este flag barra (não reage, é lido e
  # escrito na hora, fora do ciclo reativo).
  ocupado_flag <- FALSE

  # Ajuste residual da revisão da fase 2: depois da primeira instalação
  # nesta sessão do launcher, a lib da release atual entra na frente do
  # `.libPaths()` — sem isto, um `trama::` chamado na própria sessão do
  # launcher (não no processo do editor, que já recebe o `.libPaths` dele
  # por fora) não encontrava o pacote recém-instalado.
  atualizar_status <- function() {
    st <- tl_status(m = manifesto(), s = tl_state_read())
    status(st)
    if (nzchar(st$release_instalada)) {
      lib <- tl_lib_dir(st$release_instalada)
      if (!(lib %in% .libPaths())) .libPaths(c(lib, .libPaths()))
    }
  }

  # Roda `acao()` com `withProgress`, marca `ocupado` (o cliente desabilita
  # os botões via a classe `body.tl-ocupado`, ver launcher.css/js) e mostra
  # o erro numa notificação com o caminho do log se falhar. Sempre
  # reatualiza o status ao final, com sucesso ou falha.
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
  # nenhuma (em vez de abrir() instalar antes do runApp() — offline nesse
  # ponto derrubava a sessão inteira antes de existir UI para avisar).
  # Sem internet, não tenta instalar: o payload de status já chega com o
  # botão "Tentar de novo" (`tl_tentar_instalar`, abaixo).
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
  # `shiny::isolate()`: fora de `testServer()` (que embrulha tudo num
  # contexto reativo) esta checagem roda no corpo do server, sem contexto
  # reativo ativo — ler uma `reactiveVal` aqui sem `isolate()` lança "Operation
  # not allowed without an active reactive context" (só não aparecia nos
  # testes por causa desse embrulho do `testServer`).
  if (!nzchar(shiny::isolate(status())$release_instalada) && !is.null(m0)) {
    rodar_acao("Instalando…", function(progresso) tl_install_release(m0, progresso = progresso))
  }

  shiny::observeEvent(input$tl_tentar_instalar, tentar_instalar())

  shiny::observe({
    st <- status()
    session$sendCustomMessage("tl-status", .tl_status_payload(
      st, ocupado = ocupado(), sem_internet = sem_internet(),
      tem_anteriores = length(tl_state_read()$anteriores) > 0
    ))
  })

  shiny::observe({
    session$sendCustomMessage("tl-colecoes", .tl_colecoes_payload(status()))
  })

  shiny::observe({
    session$sendCustomMessage("tl-projetos", .tl_projetos_payload(projetos()))
  })

  shiny::observeEvent(input$tl_atualizar, {
    m <- manifesto()
    if (is.null(m)) return(invisible(NULL))
    rodar_acao("Instalando…", function(progresso) tl_install_release(m, progresso = progresso))
  })

  # Um observer por coleção do manifesto, criado uma vez na abertura da
  # sessão — as coleções de uma release não mudam durante a sessão, só o
  # que está instalado (e isso o `status()` já recalcula).
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

  atualizar_projetos <- function() projetos(tl_projects())

  shiny::observeEvent(input$tl_abrir_projeto, {
    rodar_acao("Abrindo projeto…", function(progresso) {
      progresso("Abrindo…")
      tl_project_open(input$tl_abrir_projeto)
    })
    atualizar_projetos()
  })

  shiny::observeEvent(input$tl_novo_projeto, {
    tryCatch({
      tl_project_new(input$tl_novo_projeto_nome)
      atualizar_projetos()
      shiny::showNotification(sprintf("Projeto '%s' criado.", input$tl_novo_projeto_nome), type = "message")
    }, error = function(e) shiny::showNotification(conditionMessage(e), type = "error"))
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
