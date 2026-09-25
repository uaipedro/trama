#' Transporte: UM canal pra cima, UM pra baixo.
#'
#' O Shiny é barramento, não lógica. Nenhuma regra de domínio, de documento ou
#' de execução mora aqui — decodifica, chama o motor, codifica de volta.
#'
#' Pra cima: `input$tr_op` com `{seq, base_rev, op}`.
#' Pra baixo: `sendCustomMessage("tr_event", ...)`.
#'
#' O front emite OPS, nunca o documento inteiro. No insumo o front empurrava o
#' grafo todo com debounce e o servidor diffava; havia ainda um segundo timer
#' pedindo execução em paralelo, sem garantia de ordem. Quando a ordem não
#' valia, a engine rodava contra o documento antigo, via que nada mudou,
#' servia do cache e o preview não atualizava — sem erro, em silêncio.
#' @export
tr_server <- function(project, flow = "main",
                      executor = tr_executor_sequential(), autosave = TRUE,
                      ctx_extra = NULL) {
  force(project); force(executor)

  .tr_warn_ctx_extra(ctx_extra)

  function(input, output, session) {
    # O projeto é reativo porque ele TROCA com o editor em pé (abrir/criar). O
    # registry dentro dele não muda — trocar coleção exigiria trocar o JS da
    # página, que só `tr_ui()` monta, uma vez. Ver `tr_project_at()`.
    rv_project <- shiny::reactiveVal(project)
    rv_flow    <- shiny::reactiveVal(flow)
    rv_doc     <- shiny::reactiveVal(tr_project_flow(project, flow))
    run_seq  <- shiny::reactiveVal(0L)
    # O log de undo é do SERVIDOR, e começa em `base_doc` (fixado no
    # tr_ready). Já foi do cliente, montado com os ecos: com o executor
    # sequencial o R bloqueia computando, as ops ficam em voo, e um Ctrl+Z
    # nesse intervalo tirava do log do cliente uma op que não era a última —
    # o servidor desfazia o passo errado e os dois logs divergiam. Aqui o log
    # só cresce com op APLICADA, na ordem em que foi aplicada, por construção.
    base_doc <- NULL
    log      <- list()

    send <- function(type, payload = list()) {
      session$sendCustomMessage("tr_event", c(list(type = type), payload))
    }

    # Falha de autosave NÃO pode ser silenciosa: disco cheio e permissão são
    # reais, e o usuário continuaria editando um documento que nunca é gravado.
    # `try(silent = TRUE)` era exatamente isso. O erro vira banner no front.
    save_now <- function(doc) {
      tryCatch(tr_project_save(rv_project(), doc, rv_flow()), error = function(e) {
        send("warning", list(message = paste0("Autosave falhou: ", conditionMessage(e))))
      })
    }

    # Estado do run em environment comum, NÃO reativo: `step()` roda dentro de
    # um callback do `later`, fora de qualquer contexto reativo do Shiny.
    exec <- new.env(parent = emptyenv()); exec$sched <- NULL; exec$logger <- NULL

    forward <- function(ev) {
      if (!is.null(exec$logger)) exec$logger$log(ev)
      ev$unit_type <- ev$type
      ev$type <- if (identical(ev$unit_type, "run_finished")) "run_finished" else "unit"
      session$sendCustomMessage("tr_event", ev)
    }

    # Bombeia o scheduler: um passo, e reagenda enquanto não acabou. É o que
    # deixa o processo do Shiny livre entre passos — a sessão responde a ops
    # (inclusive a que vai SUPERAR este run) enquanto o pool computa.
    pump <- function() {
      s <- exec$sched
      if (is.null(s) || s$finished()) return(invisible())
      tryCatch(s$step(), error = function(e) {
        send("warning", list(message = paste0("Falha no scheduler: ", conditionMessage(e))))
        exec$sched <- NULL
      })
      if (!is.null(exec$sched) && !exec$sched$finished()) later::later(pump, 0.05)
    }

    run_now <- function(doc) {
      run_seq(run_seq() + 1L)
      proj <- rv_project()
      # `tr_plan()` ABORTA em grafo malformado — ciclo, nó desconhecido, e as
      # recusas de região de fluxo (`.tr_stream_validate()`, chamada de dentro
      # do planejamento). Isso é o backstop CERTO (a maioria das recusas de
      # região agora também vira `problems` de `tr_doc_validate()`, alto e
      # cedo, antes de chegar aqui — mas `tr_error_cycle`,
      # `tr_error_unknown_node` e qualquer recusa que escapar da validação
      # continuam batendo aqui). Sem o `tryCatch`, o abort subia cru de dentro
      # de um `observeEvent` do Shiny: a sessão inteira parava de reagir a
      # QUALQUER evento seguinte — sem banner, sem pista — e o documento já
      # tinha sido salvo (autosave roda ANTES de `run_now`, em `tr_op`), então
      # reabrir o projeto batia no MESMO abort no observer de `ready`. Um
      # `to_stream` solto na tela virava projeto que não abre mais sem editar o
      # JSON à mão — é o Blocker 2 medido pelo revisor. Com o `tryCatch`, vira
      # aviso — o mesmo idioma de `avisar()` acima — e `exec$sched` do run
      # anterior (ou `NULL`, se este é o primeiro) fica como estava: a tela
      # continua reagindo, e dá pra apagar o card que quebrou o plano.
      plan <- tryCatch(
        tr_plan(doc, registry = proj$registry, store = proj$store, settings = proj$settings),
        error = avisar())
      if (is.null(plan)) return(invisible())
      # Quais nós formam cada região — a Fase 8 fecha o buraco herdado (todo
      # evento de unidade chega com `node` = primeiro colapso; membro interior
      # e segundo colapso em diante nunca recebiam nada). Mandado a CADA
      # `run_now`: o plano acabou de ser recalculado aqui mesmo, e a lista de
      # regiões pode ter mudado (nó entrou ou saiu da região) desde o run
      # anterior. `.tr_plan_regions()` só relê o plano já calculado — nenhuma
      # regra de região nova mora no barramento.
      #
      # ANTES de `tr_scheduler()`, não depois: o construtor do scheduler
      # classifica cada unidade e já emite `cached`/`failed`/`invalid`/
      # `blocked` SINCRONAMENTE por `on_event = forward` — o caso comum de
      # abrir um projeto cuja região está em cache. O handler de `regions` no
      # front só faz `bumpTick()`; não REPLAYA eventos de unidade já
      # recebidos. Com `regions` depois do scheduler (ordem medida com
      # `shiny::testServer`: `document / unit(cached,c1) / regions /
      # run_finished`), todo evento de unidade chegava ANTES de o front saber
      # que aquele nó pertence a uma região — a fonte, cada membro elevado e
      # todo colapso além do primeiro ficavam em branco. Mandar `regions`
      # primeiro garante que o front já sabe "isto é região" quando o
      # primeiro evento de unidade chega.
      send("regions", list(regions = .tr_plan_regions(plan)))
      # Run anterior ainda em voo: entrega. O que continua no plano novo é
      # adotado; o que saiu é cancelado. Isto SUBSTITUI o debounce de
      # recomputação: digitar 4, 40, 400 gera três planos e só o último vive.
      inherit <- list()
      if (!is.null(exec$sched) && !exec$sched$finished()) inherit <- exec$sched$handoff(unlist(plan$keys))
      if (!is.null(exec$logger)) exec$logger$close()
      exec$logger <- tr_run_logger(file.path(proj$root, ".trama", "runs"), as.character(run_seq()))
      # `ctx_extra` é AJUSTE DO RUN, fixado quando o editor sobe: quem abre o app
      # com uma região de dez mil pontos e acumulador grande passa
      # `checkpoint_every` aqui. Os botões de pause/step/tempo do front NÃO vêm
      # por aqui — são comando vivo, e vão pelo `control.json`
      # (`tr_stream_command()`). Misturar os dois faria o controle de velocidade
      # poder mexer no checkpoint sem ninguém pedir.
      exec$sched <- tr_scheduler(plan, proj$registry, proj$store, executor, on_event = forward,
                                 run_id = as.character(run_seq()), inherit = inherit,
                                 ctx_extra = ctx_extra)
      pump()
    }

    enviar_temas <- function() send("themes", .tr_settings_json(rv_project()$settings))

    # Quem abriu o editor decide o destino padrão de "Salvar como template": no
    # launcher não há projeto "de verdade" pro usuário, então a biblioteca
    # pessoal é o lugar natural; vindo do R, o projeto é o que ele versiona.
    origem <- function() if (nzchar(Sys.getenv("TRAMA_LAUNCHER"))) "launcher" else "r"

    # Erro de GESTO (caminho que não existe, coleção que falta, pasta ocupada)
    # não é bug do servidor: vira aviso na tela e o estado fica como estava. O
    # valor devolvido é como o chamador sabe que não deve seguir adiante.
    avisar <- function(valor = NULL) function(e) {
      send("warning", list(message = conditionMessage(e)))
      valor
    }

    # Trocar de projeto com o editor em pé. A ORDEM é o desenho inteiro:
    #
    # 1. valida (`.tr_check_project` recusa pasta sem manifesto; `tr_project_at`
    #    aborta nomeando a coleção que falta) ANTES de mexer em qualquer coisa —
    #    estado meio-trocado seria pior que não trocar;
    # 2. encerra o run em voo, como o fim de sessão já faz;
    # 3. re-aponta o store E a pasta de imagens. `addResourcePath` sobrescreve o
    #    mesmo prefixo, então as URLs de preview com arquivo e as do bloco de
    #    imagem passam a resolver no projeto novo sem recarregar a página.
    #    Limitação conhecida: o prefixo é do PROCESSO, não da sessão — com duas
    #    abas do mesmo app abertas, trocar de projeto numa re-aponta
    #    `trama-store` e `trama-imagens` da outra também, e lá os previews com
    #    arquivo e as imagens do documento passam a dar 404 em silêncio (imagem
    #    quebrada, sem erro). Aceitável porque trama é local-first e de um
    #    usuário só;
    # 4. zera `base_doc` e `log`: reaplicar o log de um projeto sobre o documento
    #    de outro é o que `.tr_undo_doc()` existe para impedir;
    # 5. volta para o flow padrão. Trocar de FLUXO é outro gesto, fora do escopo
    #    de trocar de projeto — e o projeto que entra pode nem ter o flow em que
    #    você estava, o que daria um documento vazio sem explicação.
    abrir <- function(path) {
      padrao <- "main"
      # Reabrir o projeto JÁ aberto rodava o GC sobre ele mesmo, zerava o log e
      # refazia o run: o documento sobrevivia (autosave), mas o Ctrl+Z evaporava
      # sem nenhum sinal. Só reconfirma onde o editor está. A comparação é entre
      # caminhos NORMALIZADOS porque o do cliente pode não estar.
      if (is.character(path) && length(path) == 1L && !is.na(path) &&
          identical(normalizePath(path, mustWork = FALSE), rv_project()$root)) {
        send("project", list(root = rv_project()$root, flow = rv_flow(), origem = origem()))
        return(invisible())
      }

      novo <- tryCatch({
        .tr_check_project(path)
        tr_project_at(path, rv_project()$registry)
      }, error = avisar())
      if (is.null(novo)) return(invisible())

      velho <- rv_project()
      if (!is.null(exec$sched) && !exec$sched$finished()) exec$sched$handoff(character())
      if (!is.null(exec$logger)) { exec$logger$close(); exec$logger <- NULL }
      exec$sched <- NULL
      # Mesmo tratamento do `onSessionEnded`: o GC do projeto que está sendo
      # largado não pode impedir a abertura do outro, mas calar disco cheio e
      # permissão deixaria o store crescendo sem que ninguém soubesse.
      tryCatch(tr_project_gc(velho),
               error = function(e) message("trama: GC falhou: ", conditionMessage(e)))

      shiny::addResourcePath("trama-store", novo$store$root)
      shiny::addResourcePath("trama-imagens", file.path(novo$root, "imagens"))
      rv_project(novo)
      rv_flow(padrao)

      doc <- tr_project_flow(novo, padrao)
      base_doc <<- doc
      log      <<- list()
      rv_doc(doc)
      send("project", list(root = novo$root, flow = padrao, origem = origem()))
      enviar_temas()
      send("document", list(doc = jsonlite::fromJSON(tr_doc_json(doc), simplifyVector = FALSE),
                            problems = tr_doc_validate(doc, novo$registry)))
      run_now(doc)
    }

    # O front avisa quando montou. Mandar catálogo/documento antes correria o
    # risco de a mensagem chegar sem handler registrado — perdida, sem replay.
    shiny::observeEvent(input$tr_ready, {
      send("catalog", list(catalog = tr_catalog(rv_project()$registry)))
      enviar_temas()
      send("project", list(root = rv_project()$root, flow = rv_flow(), origem = origem()))
      doc <- rv_doc()
      # O log de undo nasce AQUI: é este o documento sobre o qual as ops se
      # empilham, e é sobre ele que o undo reaplica o log.
      base_doc <<- doc
      log      <<- list()
      send("document", list(doc = jsonlite::fromJSON(tr_doc_json(doc), simplifyVector = FALSE),
                            problems = tr_doc_validate(doc, rv_project()$registry)))
      run_now(doc)
    }, once = TRUE)

    # O corpo de `tr_op` virou função porque inserir template também é uma op
    # comum: undo, `rev`, eco e re-execução têm de ser os mesmos de qualquer
    # gesto, e duplicar este caminho deixaria os dois divergirem.
    aplicar <- function(env) {
      res <- tr_submit(rv_doc(), env, rv_project()$registry)
      if (!isTRUE(res$ok)) {
        send("op_rejected", list(seq = env$seq, reason = res$reason, message = res$message,
                                 class = res$class, rev = rv_doc()$rev))
        # TODA recusa ressincroniza, e não só a de revisão defasada: o front
        # aplica alguns gestos otimisticamente (apagar em lote tira os cards da
        # tela antes do eco), e um batch recusado deixaria a tela mentindo
        # sobre um documento que não mudou.
        send("document", list(doc = jsonlite::fromJSON(tr_doc_json(rv_doc()),
                                                       simplifyVector = FALSE)))
        return(invisible())
      }
      rv_doc(res$doc)
      # A op NORMALIZADA (id/seed materializados) é a que entra no log: é o que
      # faz o replay reconstruir o mesmo documento, e não um nó de id novo.
      log[[length(log) + 1L]] <<- res$op
      send("op_applied", list(seq = res$seq, rev = res$rev, op = res$op,
                              semantic = res$semantic))

      # Op ESTRUTURAL manda o documento junto. O princípio que importa é o
      # front não empurrar o grafo inteiro PRA CIMA a cada tecla — devolver o
      # documento quando a estrutura muda é outra coisa: são poucos KB,
      # acontece por gesto do usuário (não por caractere), e evita o cliente
      # ter que reimplementar a semântica de cada op só pra saber desenhar o
      # resultado. Param, posição, tamanho e recolhimento continuam só no eco;
      # a lista de quem devolve o documento é `.tr_doc_echo_ops`, em
      # `R/document.R`.
      if (.tr_op_echoes_doc(res$op)) {
        send("document", list(doc = jsonlite::fromJSON(tr_doc_json(res$doc),
                                                       simplifyVector = FALSE)))
      }
      if (autosave) save_now(res$doc)
      if (isTRUE(res$semantic)) run_now(res$doc)
    }

    shiny::observeEvent(input$tr_op, aplicar(input$tr_op))

    # Colar/arrastar/importar template. O front só detectou a marca; parse,
    # remoção de dados e ids novos são daqui. Entra como um `batch` comum pelo
    # mesmo `aplicar()` de `tr_op`: um passo de undo, como colar nós copiados.
    # `base_rev` é o do servidor porque o pedido não é uma edição sobre a
    # revisão que o front via — é uma inserção, que vale sobre qualquer uma.
    shiny::observeEvent(input$tr_template_insert, {
      m <- input$tr_template_insert
      tpl <- tryCatch({
        if (!is.null(m$arquivo)) {
          # `arquivo` vem do painel e só pode ser um dos templates listados:
          # aceitar caminho qualquer faria deste input um leitor de arquivo
          # arbitrário da máquina.
          conhecidos <- vapply(tr_template_list(rv_project()$root, rv_project()$registry),
                               function(t) t$arquivo, "")
          alvo <- normalizePath(as.character(m$arquivo)[1], mustWork = FALSE)
          if (!alvo %in% conhecidos) rlang::abort("Template não encontrado entre os disponíveis.")
          tr_template_read(alvo)
        } else {
          tr_template_parse(m$conteudo)
        }
      }, error = avisar())
      if (is.null(tpl)) return(invisible())
      faltam <- setdiff(tpl$colecoes,
                        as.character(.tr_registry_packages(rv_project()$registry) %||% character()))
      if (length(faltam)) {
        send("warning", list(message = sprintf(
          "Este template usa %s, que não está carregado. Instale e reabra o projeto.",
          paste(faltam, collapse = ", "))))
        return(invisible())
      }
      num <- function(v) if (is.numeric(v) && length(v) == 1 && !is.na(v)) v else 0
      aplicar(list(seq = m$seq, base_rev = rv_doc()$rev,
                   op = tr_template_op(tpl, c(num(m$x), num(m$y)))))
    })

    # Destino "biblioteca"/"projeto" grava; "baixar"/"copiar" só devolve o
    # texto — o download e o clipboard são do navegador. `ids` nulo = flow todo.
    # Conflito de nome não é erro: vira pergunta no diálogo, que reenvia com
    # `overwrite`.
    shiny::observeEvent(input$tr_template_save, {
      m <- input$tr_template_save
      reg <- rv_project()$registry
      nome <- as.character(m$nome %||% "Template")[1]
      tpl <- tryCatch(tr_template(tr_doc_subset(rv_doc(), unlist(m$ids)), nome,
                                  as.character(m$descricao %||% "")[1], reg),
                      error = avisar())
      if (is.null(tpl)) return(invisible())
      destino <- as.character(m$destino %||% "")[1]
      if (destino %in% c("baixar", "copiar")) {
        send("template_json", list(seq = m$seq, acao = destino, nome = tpl$nome,
                                   texto = as.character(tr_template_json(tpl))))
        return(invisible())
      }
      ok <- tryCatch({
        tr_template_save(tpl, tr_template_dir(destino, rv_project()$root),
                         overwrite = isTRUE(m$overwrite))
        TRUE
      }, tr_error_template_exists = function(e) {
        send("template_conflict", list(seq = m$seq, nome = tpl$nome)); FALSE
      }, error = avisar(FALSE))
      if (isTRUE(ok)) {
        send("template_saved", list(seq = m$seq, nome = tpl$nome, destino = destino))
        send("templates", list(templates = tr_template_list(rv_project()$root, reg)))
      }
    })

    shiny::observeEvent(input$tr_template_list, {
      send("templates", list(templates = tr_template_list(rv_project()$root, rv_project()$registry)))
    })

    shiny::observeEvent(input$tr_rerun, { run_now(rv_doc()) })

    # Código é gerado no servidor porque só o registro conhece as funções R e
    # os adaptadores por trás de cada nó. O navegador continua responsável
    # apenas pelo download, como já faz para o JSON do flow.
    shiny::observeEvent(input$tr_export_code, {
      req <- input$tr_export_code
      formato <- if (identical(req$format, "quarto")) "quarto" else "r"
      titulo <- paste0(basename(rv_project()$root), " — ", rv_flow())
      code <- tryCatch(
        tr_export_code(rv_doc(), rv_project()$registry, format = formato, title = titulo),
        error = avisar()
      )
      if (!is.null(code)) send("export_code", list(format = formato, code = code))
    })

    # Comandar a região em voo: pause, um passo, velocidade, parar. Decodifica e
    # chama o motor, sem lógica própria — como o resto do arquivo.
    #
    # Não é op de documento, e é aí que está a decisão: não entra no log de undo,
    # não mexe em `rev` e não volta como `document`. É o precedente de
    # `sinks-ricos.md` ("rode este nó agora" é COMANDO, não op) — um Ctrl+Z
    # depois de pausar tem que desfazer a última EDIÇÃO, e um `rev` novo faria o
    # front ressincronizar o grafo por causa de um botão de velocidade.
    #
    # A `key` é a da UNIDADE, e o front já a tem: ela vem em todo evento de
    # unidade (`emit()`, em `scheduler.R`). Resolver o id do nó para a chave aqui
    # seria conhecimento de plano dentro do barramento.
    shiny::observeEvent(input$tr_stream_cmd, {
      m <- input$tr_stream_cmd
      tryCatch(tr_stream_command(rv_project()$store, m$key, m$cmd, tempo = m$tempo),
               error = avisar())
    })

    shiny::observeEvent(input$tr_undo, {
      # Undo por REPLAY do log sem a última op, sobre o `base_doc` desta sessão
      # e não sobre o vazio — ver `.tr_undo_doc()`. O valor do input não
      # carrega nada (é só um carimbo que muda); quem sabe o que desfazer é o
      # log daqui. Sem base (undo antes do ready) ou sem op nenhuma, não há o
      # que desfazer: ignora.
      if (is.null(base_doc) || length(log) == 0L) return(invisible())
      novo <- log[-length(log)]
      erro <- NULL
      doc <- tryCatch(.tr_undo_doc(base_doc, novo, rv_doc()$rev, rv_project()$registry),
                      error = function(e) { erro <<- conditionMessage(e); NULL })
      # Replay que falha NÃO encurta o log: o documento continua o de antes, e
      # o log tem que continuar descrevendo ele. E o usuário fica sabendo — um
      # Ctrl+Z que não faz nada, sem aviso, parece tecla quebrada.
      if (is.null(doc)) {
        send("warning", list(message = paste0("Não foi possível desfazer: ", erro)))
        return(invisible())
      }
      log <<- novo
      rv_doc(doc)
      send("document", list(doc = jsonlite::fromJSON(tr_doc_json(doc), simplifyVector = FALSE),
                            problems = tr_doc_validate(doc, rv_project()$registry)))
      if (autosave) save_now(doc)
      run_now(doc)
    })

    # `seq` pelo mesmo motivo de `tr_op`: `input$x` ignora valor idêntico
    # consecutivo, e entrar numa pasta, voltar e entrar de novo manda o mesmo
    # `path` duas vezes. Aqui isso é o caso comum, não a exceção.
    shiny::observeEvent(input$tr_browse, {
      l <- tryCatch(.tr_dir_listing(input$tr_browse$path), error = avisar())
      if (!is.null(l)) send("listing", l)
    })

    # Mesmo desenho de `tr_browse`/`listing`, sob demanda: o front pede quando
    # precisa (estado vazio de uma nota-imagem, ou ao abrir a ferramenta), em
    # vez do servidor empurrar a lista sem ser pedida — a pasta `imagens/` pode
    # crescer a qualquer momento fora do editor, e reler no gesto certo é mais
    # simples que inventar um observador de disco. `seq` é aceito mas ignorado
    # aqui, pelo mesmo motivo do `tr_browse`: pedir duas vezes a MESMA lista
    # (`input$x` não dispara em valor idêntico consecutivo) é o caso comum, não
    # a exceção — reabrir o mesmo bloco duas vezes manda o mesmo pedido.
    shiny::observeEvent(input$tr_list_imagens, {
      arquivos <- tryCatch(.tr_list_imagens(rv_project()$root), error = avisar())
      if (!is.null(arquivos)) send("imagens", list(files = as.list(arquivos)))
    })

    # Arquivo solto no CANVAS (CSV/JSON), pra virar nó de leitura. O conteúdo
    # já chega como TEXTO, mesmo motivo do `tr_project_import`: o front lê com
    # `FileReader`, o servidor nunca recebe caminho de disco do navegador. `id`
    # é do CLIENTE (gerado no drop) e só volta na resposta — é o que casa o
    # pedido com a posição/tipo de nó pendentes do lado de lá; este handler não
    # sabe nada de canvas, só grava bytes em `data/` e devolve o caminho.
    #
    # Colisão de nome NÃO sobrescreve sozinha: um segundo drop do mesmo
    # arquivo, ou um nome repetido por coincidência, apagaria dado de quem
    # nem sabe que houve conflito. `overwrite` é o cliente confirmando, depois
    # de perguntar ao usuário.
    shiny::observeEvent(input$tr_data_upload, {
      req <- input$tr_data_upload
      alvo <- tryCatch(.tr_data_upload_path(rv_project()$root, req$nome), error = avisar())
      if (is.null(alvo)) return(invisible())
      if (file.exists(alvo) && !isTRUE(req$overwrite)) {
        send("data_upload_conflict", list(id = req$id, nome = req$nome))
        return(invisible())
      }
      ok <- tryCatch({
        dir.create(dirname(alvo), recursive = TRUE, showWarnings = FALSE)
        writeLines(req$conteudo, alvo, useBytes = TRUE)
        TRUE
      }, error = avisar(FALSE))
      if (!isTRUE(ok)) return(invisible())
      send("data_upload_ok", list(id = req$id, path = paste0("data/", req$nome)))
    })

    # Salvar temas do painel. Recusa (tema inválido, disco) reenvia os temas
    # REAIS: o painel volta ao estado verdadeiro em vez de mostrar um tema que
    # não foi gravado. O front manda `seq` (ignorado aqui) pelo mesmo motivo do
    # `tr_browse`: salvar duas vezes igual não dispararia de novo. Tema entra
    # no hash, então quem aceitou re-roda — o GC e o próximo plano leem os
    # settings novos via `rv_project()`. A mensagem tem o MESMO formato que o
    # servidor manda (`temas`, `tema_padrao`, `marca`): um formato só evita o
    # front renomear campo na ida e na volta — foi assim que `padrao` escapou
    # antes.
    shiny::observeEvent(input$tr_themes, {
      m <- input$tr_themes
      raiz <- rv_project()$root
      # Um gesto do painel, dois verbos: os temas e a marca d'água. A marca é
      # conferida ANTES de qualquer escrita (e não só dentro do verbo que a
      # grava) porque ela é a SEGUNDA a escrever — recusá-la depois dos temas
      # deixaria gravada metade do que o usuário pediu. `marca` ausente é
      # recusa, como `tema_padrao`: assumir "ligada" religaria a marca de quem
      # acabou de desligá-la.
      #
      # Sobra a falha de DISCO entre as duas escritas, que dois verbos sobre o
      # mesmo arquivo não têm como evitar. Por isso a volta é sempre RELIDA do
      # manifesto, e não a lista devolvida pelo verbo: depois de uma recusa no
      # meio, o painel tem que mostrar o que está NO ARQUIVO — senão a chave
      # fica ligada na tela e desligada no disco.
      ok <- tryCatch({
        .tr_check_marca(m$marca)
        tr_project_set_themes(raiz, m$temas, m$tema_padrao)
        tr_project_set_marca(raiz, mostrar = m$marca)
        TRUE
      }, error = avisar(FALSE))
      # Falha na RELEITURA também vira aviso, e não silêncio: sem isto o painel
      # receberia de volta os settings velhos da memória sem uma palavra —
      # exatamente a discordância painel/disco que este bloco existe pra evitar.
      s <- tryCatch(.tr_settings_at(raiz), error = avisar())
      if (!is.null(s)) { p <- rv_project(); p$settings <- s; rv_project(p) }
      enviar_temas()
      if (isTRUE(ok)) run_now(rv_doc())
    })

    shiny::observeEvent(input$tr_project_open, abrir(input$tr_project_open$path))

    # Criar e abrir são um gesto só na tela, e dois passos aqui: o primeiro pode
    # falhar (pasta ocupada) sem que o segundo chegue a acontecer.
    shiny::observeEvent(input$tr_project_new, {
      req <- input$tr_project_new
      cols <- .tr_registry_packages(rv_project()$registry)
      raiz <- tryCatch({
        r <- .tr_project_path(req$path, req$nome)
        tr_project_new(r, cols %||% character())
        r
      }, error = avisar())
      if (!is.null(raiz)) abrir(raiz)
    })

    # Importar é "criar" + "gravar o flow recebido" antes de abrir — mesma
    # forma de `tr_project_new`, só que o `flows/main.json` não nasce vazio.
    # O conteúdo já chega como TEXTO (o front lê o arquivo com FileReader):
    # o servidor nunca recebe um caminho de disco do cliente pra isso, só
    # bytes — é a mesma razão do navegador de pastas em `.tr_dir_listing()`
    # existir (`webkitdirectory` não entrega path real).
    shiny::observeEvent(input$tr_project_import, {
      req <- input$tr_project_import
      raiz <- tryCatch({
        r <- .tr_project_path(req$path, req$nome)
        doc <- tr_doc_parse(req$conteudo)
        tr_project_new(r, .tr_registry_packages(rv_project()$registry) %||% character())
        tr_doc_write(doc, file.path(r, "flows", "main.json"))
        r
      }, error = avisar())
      if (!is.null(raiz)) abrir(raiz)
    })

    session$onSessionEnded(function() {
      if (!is.null(exec$sched) && !exec$sched$finished()) exec$sched$handoff(character())
      if (!is.null(exec$logger)) exec$logger$close()
      # `isolate` porque este callback roda fora de contexto reativo: ler o
      # reactiveVal cru aqui é erro, não valor velho.
      tryCatch(tr_project_gc(shiny::isolate(rv_project())),
               error = function(e) message("trama: GC falhou: ", conditionMessage(e)))
    })
  }
}
