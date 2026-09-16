# Comandos da região pelo STORE: pause, um passo, tempo, parar.
#
# É protocolo entre dois processos, com estado (pausado/rodando) e leitura por
# polling — o mesmo canal do progresso, na direção contrária. Todo teste aqui
# existe contra um modo de falha que não erra alto:
#
#   - o comando que o driver nunca lê (botão que não faz nada);
#   - o `step` que avança pra sempre porque o driver não lembra qual `seq` já
#     honrou (o "um passo" que vira "roda tudo");
#   - o laço pausado em espera APERTADA, queimando um núcleo por minutos;
#   - o `tempo` entrando na chave, que recomputaria o fluxo inteiro só por
#     alguém ter mexido na velocidade de assistir;
#   - e o pior: `stop` gravando artefato final. Parar não é terminar — o
#     histórico truncado ficaria cacheado sob uma chave VÁLIDA pra sempre, que é
#     o `tr_store_put_error` de erro falso da Fase 3 chegando por outra estrada.
#
# O executor sequencial roda a unidade no processo do coordenador, então quem
# escreve o comando "de fora" enquanto o laço anda só pode ser um nó de dentro
# do laço — é ele o segundo processo, aqui. É a mesma encenação de
# `test-stream-progress.R`, que espia o canal de dentro do laço pelo mesmo
# motivo.

controle_diario <- function() {
  e <- new.env(parent = emptyenv())
  e$vistos <- list()     # cada chamada do nó elevado, com o ponto que recebeu
  e$passo <- 0L          # quantas vezes o nó já rodou (o índice do passo)
  e$comandos <- list()   # passo (como texto) -> comando a emitir naquele passo
  e$store <- NULL; e$key <- NULL
  e
}

controle_collection <- function(e) {
  ponto <- function(...) tr_port("c/v", stream = TRUE, ...)
  tr_collection(
    id = "c", version = "1.0.0", label = "Controle de fluxo",
    types = list(tr_type("c/v", label = "Valor",
                         preview = function(x, ctx) tr_preview("c/v", data = list(v = x)))),
    nodes = list(
      tr_node("c/fonte", fn = function(n) as.list(seq_len(n)),
              outputs = list(out = ponto()), params = list(n = tr_param_int(10L)),
              description = "Emite n pontos: 1..n."),
      # O "outro processo": emite, de dentro do passo, o comando que o
      # coordenador emitiria. É o único jeito de exercitar o protocolo com o
      # executor sequencial, que bloqueia o coordenador enquanto a região roda.
      tr_node("c/manda", fn = function(x) {
                e$passo <- e$passo + 1L
                e$vistos <- c(e$vistos, list(x))
                cmd <- e$comandos[[as.character(e$passo)]]
                if (!is.null(cmd)) {
                  tr_stream_command(e$store, e$key, cmd$cmd, tempo = cmd$tempo)
                }
                x
              },
              inputs = list(x = "c/v"), outputs = list(out = "c/v"),
              description = "Passa o ponto adiante e emite o comando daquele passo."),
      # COM MEMÓRIA de propósito: sem estado no checkpoint, a retomada depois de
      # um `stop` produziria a média dos ÚLTIMOS pontos — plausível e errada.
      tr_node("c/media", fn = function(x) x,
              inputs = list(x = ponto()), outputs = list(out = ponto()),
              init = function() list(soma = 0, n = 0L),
              step = function(state, x) {
                state$soma <- state$soma + x; state$n <- state$n + 1L
                list(state = state, out = state$soma / state$n)
              },
              description = "Média corrente dos pontos."),
      tr_node("c/colapsa", fn = function(x) {
                data.frame(v = if (length(x)) vapply(x, as.numeric, 0) else numeric())
              },
              inputs = list(x = ponto()), outputs = list(out = "c/v"),
              description = "Monta o histórico do fluxo."),
      tr_node("c/mostra", fn = function(x) invisible(x), inputs = list(x = "c/v"),
              description = "Consome o histórico, fora da região.")
    ))
}

controle_registry <- function(e) {
  reg <- tr_registry(); tr_use(controle_collection(e), registry = reg); reg
}

# A região padrão destes testes: fonte -> manda -> media -> colapsa, e um
# consumidor FORA da região pra que o `stop` possa ser observado a jusante.
doc_controle <- function(reg, n = 10L) {
  tr_flow_doc(tr_flow(reg) |>
    tr_add("fo", "c/fonte", n = n) |>
    tr_add("ma", "c/manda", from = "fo") |>
    tr_add("ac", "c/media", from = "ma") |>
    tr_add("co", "c/colapsa", from = "ac") |>
    tr_add("mo", "c/mostra", from = "co"))
}

# Monta o cenário: plano, unidade da região, e o diário já sabendo a chave (é o
# que o nó de dentro precisa pra emitir comando como o coordenador emitiria).
cenario <- function(n = 10L, e = controle_diario()) {
  reg <- controle_registry(e); s <- tmp_store()
  doc <- doc_controle(reg, n)
  p <- tr_plan(doc, registry = reg, store = s)
  u <- p$units$co
  expect_equal(u$kind, "stream_region")
  e$store <- s; e$key <- u$key
  list(e = e, reg = reg, s = s, doc = doc, plan = p, u = u)
}

ctl_path  <- function(store, key) file.path(store$root, "stream", key, "control.json")
ckpt_path2 <- function(store, key) file.path(store$root, "stream", key, "ckpt.rds")

# --- O arquivo de controle ----------------------------------------------------

test_that("o comando atravessa o JSON com a FORMA certa: escalares, não listas de um", {
  # `write_json(auto_unbox = TRUE)` desembrulha vetor de comprimento 1 e joga
  # nome fora — é o comentário mais longo de `store.R`, e a Tarefa 5.1 caiu nele
  # de novo com o mapa `nodes`. Aqui os campos são escalares, que é a direção
  # segura; o teste fixa a forma pra que o driver do OUTRO processo não tenha que
  # adivinhar se `state` é texto ou lista de um texto.
  x <- cenario(n = 3L)
  dir.create(dirname(ctl_path(x$s, x$u$key)), recursive = TRUE, showWarnings = FALSE)
  expect_true(tr_stream_command(x$s, x$u$key, "pause"))

  c1 <- jsonlite::fromJSON(ctl_path(x$s, x$u$key), simplifyVector = FALSE)
  expect_type(c1$state, "character")
  expect_length(c1$state, 1L)
  expect_equal(c1$state, "paused")
  expect_type(c1$step_once, "logical")
  expect_length(c1$step_once, 1L)
  expect_false(c1$step_once)
  expect_equal(c1$seq, 1)
  expect_equal(c1$tempo, 0)

  # Cada comando anda o `seq`: é ele que faz o driver distinguir um `step` novo
  # de um `step` que ele já honrou.
  tr_stream_command(x$s, x$u$key, "step")
  c2 <- jsonlite::fromJSON(ctl_path(x$s, x$u$key), simplifyVector = FALSE)
  expect_equal(c2$state, "paused")
  expect_true(c2$step_once)
  expect_equal(c2$seq, 2)

  tr_stream_command(x$s, x$u$key, "tempo", tempo = 0.25)
  c3 <- jsonlite::fromJSON(ctl_path(x$s, x$u$key), simplifyVector = FALSE)
  expect_equal(c3$tempo, 0.25)
  # `tempo` não é play nem pause: o estado de quem estava pausado continua.
  expect_equal(c3$state, "paused")

  tr_stream_command(x$s, x$u$key, "play")
  c4 <- jsonlite::fromJSON(ctl_path(x$s, x$u$key), simplifyVector = FALSE)
  expect_equal(c4$state, "running")
  expect_false(c4$step_once)
  expect_equal(c4$tempo, 0.25)
})

test_that("comando para chave inexistente é no-op SILENCIOSO, não erro", {
  # A tela pode mandar o comando de uma região que já terminou (o evento `done`
  # e o clique do usuário se cruzam) ou de um run superado. Erro aqui viraria
  # banner vermelho por um botão que chegou tarde — e, pior, um `dir.create` de
  # cortesia encheria o store de diretórios de chaves que nunca rodaram.
  s <- tmp_store()
  expect_silent(r <- tr_stream_command(s, "chave-que-nunca-existiu", "pause"))
  expect_false(r)
  expect_false(dir.exists(file.path(s$root, "stream", "chave-que-nunca-existiu")))
  expect_false(tr_stream_command(s, NULL, "pause"))
  expect_false(tr_stream_command(s, "", "pause"))
})

test_that("comando desconhecido erra alto, nomeando os que existem", {
  # O barramento decodifica e chama o motor sem lógica própria, então quem sabe
  # quais comandos existem é este arquivo. Silêncio aqui daria um botão novo no
  # front que não faz nada e não reclama.
  x <- cenario(n = 3L)
  dir.create(dirname(ctl_path(x$s, x$u$key)), recursive = TRUE, showWarnings = FALSE)
  expect_error(tr_stream_command(x$s, x$u$key, "acelera"),
               class = "tr_error_stream_bad_command")
  # E comando AUSENTE também: a mensagem do front pode chegar sem o campo, e o
  # erro de R cru ("subscript out of bounds") num banner não diz o que faltava.
  expect_error(tr_stream_command(x$s, x$u$key, NULL),
               class = "tr_error_stream_bad_command")
})

test_that("controle ilegível conta como AUSENTE: a região roda como se não houvesse", {
  # Precedente de `.tr_ckpt_read()` e de `tr_store_handle()`: meio arquivo sob
  # uma chave válida não pode deixar uma região não-rodável. Uma região que se
  # recusa a rodar porque o botão de pausa escreveu um byte torto é pior que uma
  # pausa que não funciona.
  x <- cenario(n = 5L)
  dir.create(dirname(ctl_path(x$s, x$u$key)), recursive = TRUE, showWarnings = FALSE)
  writeLines("{isto não é json", ctl_path(x$s, x$u$key))

  expect_silent(.tr_run_unit(x$u, x$reg, x$s))
  hist <- tr_store_get(x$s, x$u$outputs$out, tr_get_type("c/v", x$reg))
  expect_equal(hist$v, cumsum(1:5) / seq_len(5))
})

# --- A espera do laço pausado -------------------------------------------------

test_that("pausado, o laço DORME em incrementos curtos até o `seq` mudar", {
  # As duas metades desta asserção são o teste inteiro: (a) não volta enquanto
  # está pausado — senão o botão não pausa nada; (b) volta DORMINDO, e não em
  # laço apertado — uma região pausada fica minutos assim, e espera apertada
  # queima um núcleo o tempo todo. `dorme` é argumento justamente pra que o
  # teste possa ser o outro processo: é ele quem, no terceiro cochilo, manda o
  # play que o coordenador mandaria.
  x <- cenario(n = 3L)
  dir.create(dirname(ctl_path(x$s, x$u$key)), recursive = TRUE, showWarnings = FALSE)
  tr_stream_command(x$s, x$u$key, "pause")

  cochilos <- numeric()
  dorme <- function(t) {
    cochilos <<- c(cochilos, t)
    if (length(cochilos) == 3L) tr_stream_command(x$s, x$u$key, "play")
  }
  g <- .tr_control_gate(x$s, x$u$key, 0L, dorme = dorme)

  expect_equal(g$acao, "segue")
  expect_equal(length(cochilos), 3L)          # esperou, e só saiu com o play
  expect_true(all(cochilos <= 0.05))          # em incrementos curtos
  expect_true(all(cochilos > 0))
})

test_that("o MESMO `seq` de `step` não é honrado duas vezes", {
  # Sem a memória do `seq` honrado, "um passo" vira "roda tudo": o driver leria
  # `step_once = TRUE` em todo passo seguinte e nunca mais pararia — com o
  # botão de pausa aceso na tela.
  x <- cenario(n = 3L)
  dir.create(dirname(ctl_path(x$s, x$u$key)), recursive = TRUE, showWarnings = FALSE)
  tr_stream_command(x$s, x$u$key, "step")     # seq = 1

  g1 <- .tr_control_gate(x$s, x$u$key, 0L, dorme = function(t) stop("não devia dormir"))
  expect_equal(g1$acao, "segue")
  expect_equal(g1$seq, 1)

  # Segunda passada com o `seq` já honrado: agora TEM que dormir.
  cochilos <- 0L
  dorme <- function(t) {
    cochilos <<- cochilos + 1L
    if (cochilos == 2L) tr_stream_command(x$s, x$u$key, "step")   # seq = 2
  }
  g2 <- .tr_control_gate(x$s, x$u$key, g1$seq, dorme = dorme)
  expect_equal(cochilos, 2L)
  expect_equal(g2$acao, "segue")
  expect_equal(g2$seq, 2)
})

# --- `step` e `stop` pelo driver ---------------------------------------------

test_that("`step` avança exatamente UM passo e volta a pausado", {
  # Dois comandos `step`, dois passos — e o terceiro passo não acontece porque
  # ninguém pediu. O `stop` no fim é só a saída do teste: sem ele o laço
  # dormiria pra sempre esperando o comando seguinte, que é exatamente o
  # comportamento que se quer.
  x <- cenario(n = 10L)
  dir.create(dirname(ctl_path(x$s, x$u$key)), recursive = TRUE, showWarnings = FALSE)
  tr_stream_command(x$s, x$u$key, "step")
  x$e$comandos <- list("1" = list(cmd = "step"), "2" = list(cmd = "stop"))

  expect_error(.tr_run_unit(x$u, x$reg, x$s), class = "tr_error_stream_stopped")
  expect_equal(length(x$e$vistos), 2L)
  expect_equal(unlist(x$e$vistos), 1:2)
})

test_that("`stop` deixa checkpoint válido e NÃO grava artefato final", {
  # A armadilha que esta asserção fecha: parar não é terminar. Um artefato final
  # aqui poria o histórico dos 3 primeiros pontos sob a chave VÁLIDA da região —
  # e `tr_plan()` o serviria do cache pra sempre, sem que nada errasse alto.
  x <- cenario(n = 10L)
  dir.create(dirname(ctl_path(x$s, x$u$key)), recursive = TRUE, showWarnings = FALSE)
  x$e$comandos <- list("3" = list(cmd = "stop"))

  expect_error(.tr_run_unit(x$u, x$reg, x$s), class = "tr_error_stream_stopped")

  # Nada sob a chave de saída: nem valor, nem erro.
  expect_null(tr_store_handle(x$s, x$u$outputs$out))
  expect_false(tr_store_has(x$s, x$u$outputs$out))

  # E o trabalho dos três passos está guardado, no último passo COMPLETO.
  ck <- readRDS(ckpt_path2(x$s, x$u$key))
  expect_equal(ck$i, 3L)
  expect_equal(ck$n, 10L)

  # O plano seguinte diz PENDENTE — nem cacheado, nem falho.
  p2 <- tr_plan(x$doc, registry = x$reg, store = x$s)
  expect_false(isTRUE(p2$units$co$cached))
  expect_false(isTRUE(p2$units$co$failed))
  expect_true("co" %in% names(tr_plan_pending(p2)))

  # E a retomada continua de onde parou, com a média corrente inteira: sete
  # passos novos, não dez, e o histórico IDÊNTICO ao do run sem interrupção.
  x$e$comandos <- list()
  unlink(ctl_path(x$s, x$u$key))
  antes <- length(x$e$vistos)
  .tr_run_unit(x$u, x$reg, x$s)
  expect_equal(length(x$e$vistos) - antes, 7L)
  hist <- tr_store_get(x$s, x$u$outputs$out, tr_get_type("c/v", x$reg))
  expect_equal(hist$v, cumsum(1:10) / seq_len(10))
})

test_that("`stop` no run completo: nada de handle de erro, e o jusante fica BLOQUEADO", {
  # A mesma armadilha pelo outro lado. Se o `stop` subisse como falha comum, o
  # `collect()` gravaria `tr_store_put_error` sob TODA chave de saída da região:
  # um erro FALSO cacheado sob chave válida, que nenhum `tr_plan()` recalcula.
  # E se subisse como sucesso sem handle, o jusante seria despachado, morreria
  # com "chave ausente" e o erro falso apareceria um salto adiante.
  x <- cenario(n = 10L)
  dir.create(dirname(ctl_path(x$s, x$u$key)), recursive = TRUE, showWarnings = FALSE)
  x$e$comandos <- list("3" = list(cmd = "stop"))

  evs <- list()
  res <- tr_run_plan(x$plan, x$reg, x$s, on_event = function(ev) evs[[length(evs) + 1L]] <<- ev)
  tipos <- function(no) vapply(Filter(function(v) identical(v$node, no), evs),
                               function(v) v$type, "")

  expect_true("cancelled" %in% tipos("co"))
  expect_false("failed" %in% tipos("co"))
  expect_true("blocked" %in% tipos("mo"))

  # Nem a região nem o jusante deixaram handle nenhum no store.
  expect_null(tr_store_handle(x$s, x$u$outputs$out))
  mo <- x$plan$units$mo
  expect_null(tr_store_handle(x$s, unlist(mo$outputs)[[1]]))
  expect_true(file.exists(ckpt_path2(x$s, x$u$key)))
})

test_that("comando velho da MESMA chave não pausa um run novo — e não apaga o checkpoint", {
  # Mesma classe do progresso órfão que a Tarefa 5.1 fechou no `dispatch()`: a
  # chave é determinística, e um run morto sem limpar deixa o `control.json` no
  # lugar. Sem a limpeza, o run novo nasceria pausado, o card ficaria parado e
  # não haveria erro nenhum pra investigar.
  #
  # E a limpeza tem que tirar SÓ o controle: apagar o diretório levaria o
  # checkpoint, e a retomada — a razão de o diretório existir — voltaria ao zero.
  x <- cenario(n = 10L)
  dir.create(dirname(ctl_path(x$s, x$u$key)), recursive = TRUE, showWarnings = FALSE)
  tr_stream_command(x$s, x$u$key, "pause")
  saveRDS(list(i = 5L, n = 10L,
               estado = list(ac = list(soma = sum(1:5), n = 5L)),
               hist = list(co = list(x = c(as.list(cumsum(1:5) / seq_len(5)),
                                           vector("list", 5L))))),
          ckpt_path2(x$s, x$u$key))

  tr_run_plan(x$plan, x$reg, x$s)

  # Rodou (não ficou pausado), retomou do passo 5 (cinco passos novos) e o
  # histórico é o inteiro.
  expect_equal(length(x$e$vistos), 5L)
  hist <- tr_store_get(x$s, x$u$outputs$out, tr_get_type("c/v", x$reg))
  expect_equal(hist$v, cumsum(1:10) / seq_len(10))
})

# --- `tempo` ------------------------------------------------------------------

test_that("`tempo` atrasa o passo e NÃO entra na chave da unidade", {
  # Decisão 9 em teste: cadência é estado de SESSÃO. Se `tempo` fosse param do
  # documento, mexer na velocidade de assistir mudaria a chave da região e
  # recomputaria o fluxo inteiro — o oposto do que o botão existe pra fazer.
  x <- cenario(n = 5L)
  chave_antes <- x$u$key
  dir.create(dirname(ctl_path(x$s, x$u$key)), recursive = TRUE, showWarnings = FALSE)
  tr_stream_command(x$s, x$u$key, "tempo", tempo = 0.03)

  t0 <- Sys.time()
  .tr_run_unit(x$u, x$reg, x$s)
  gasto <- as.numeric(Sys.time() - t0, units = "secs")
  expect_gte(gasto, 5 * 0.03)

  # A chave é a mesma depois, e o histórico é o mesmo de um run sem `tempo`.
  p2 <- tr_plan(x$doc, registry = x$reg, store = x$s)
  expect_identical(p2$units$co$key, chave_antes)
  hist <- tr_store_get(x$s, x$u$outputs$out, tr_get_type("c/v", x$reg))
  expect_equal(hist$v, cumsum(1:5) / seq_len(5))
})

test_that("`tempo` torto não derruba nem trava a região", {
  # Mesmo argumento do `checkpoint_every` torto: um botão de operação não pode
  # fazer uma região que roda hoje parar de rodar. E aqui há um segundo perigo —
  # `tempo` gigante dormiria o daemon por horas, com o card aceso e ninguém
  # entendendo por quê. O valor é aparado, não obedecido cegamente.
  expect_equal(.tr_control_tempo("depressa"), 0)
  expect_equal(.tr_control_tempo(NA), 0)
  expect_equal(.tr_control_tempo(-5), 0)
  expect_equal(.tr_control_tempo(Inf), 0)
  expect_equal(.tr_control_tempo(1e9), 5)      # aparado, não obedecido
  expect_equal(.tr_control_tempo(0.25), 0.25)

  # E pelo laço: com o valor torto no arquivo, a região roda como se não houvesse
  # `tempo` — nem erro, nem espera.
  for (torto in list("depressa", NA, -5, Inf)) {
    x <- cenario(n = 3L)
    dir.create(dirname(ctl_path(x$s, x$u$key)), recursive = TRUE, showWarnings = FALSE)
    jsonlite::write_json(list(state = "running", tempo = torto, step_once = FALSE, seq = 1L),
                         ctl_path(x$s, x$u$key), auto_unbox = TRUE, digits = NA)
    t0 <- Sys.time()
    .tr_run_unit(x$u, x$reg, x$s)
    expect_lt(as.numeric(Sys.time() - t0, units = "secs"), 5)
    hist <- tr_store_get(x$s, x$u$outputs$out, tr_get_type("c/v", x$reg))
    expect_equal(hist$v, cumsum(1:3) / seq_len(3))
  }
})

test_that("`tempo` e `publish_every` são botões DIFERENTES: o passo pedido publica", {
  # Conflatá-los seria bug nos dois sentidos. Aqui o que se mede é o sentido que
  # dói: com a cadência de publicação alta (o default estrangula), um `step`
  # pedido a dedo avançaria o ponto SEM publicar o parcial — o usuário aperta
  # "um passo", o número não muda na tela, e nada erra. O passo que o usuário
  # pediu é justamente a escrita que ele está esperando.
  x <- cenario(n = 10L)
  dir.create(dirname(ctl_path(x$s, x$u$key)), recursive = TRUE, showWarnings = FALSE)
  tr_stream_command(x$s, x$u$key, "step")
  x$e$comandos <- list("1" = list(cmd = "step"), "2" = list(cmd = "stop"))

  expect_error(.tr_run_unit(x$u, x$reg, x$s, ctx_extra = list(publish_every = 1000)),
               class = "tr_error_stream_stopped")

  # O progresso sobrevive ao `stop` (quem limpa é o `collect()` do scheduler), e
  # está no SEGUNDO passo: com o estrangulamento valendo, ele teria ficado no
  # primeiro — o piso de `-Inf` publica o passo 1 de qualquer jeito.
  p <- tr_progress(x$s, x$u$key)
  expect_equal(p$fraction, 0.2)
  expect_equal(p$nodes$ma$preview$data$v, 2)
})

# --- O barramento -------------------------------------------------------------

test_that("`tr_stream_cmd` chama o motor e NÃO toca no documento", {
  # Pause/step/tempo são COMANDO, não op de documento (o precedente de
  # `sinks-ricos.md`): não entram no log de undo, não mexem em `rev` e não
  # aparecem no documento. Um Ctrl+Z depois de pausar tem que desfazer a última
  # EDIÇÃO, não a pausa — e um `rev` novo faria o front ressincronizar o grafo
  # inteiro por causa de um botão de velocidade.
  root <- withr::local_tempdir("proj")
  tr_project_new(root, character())
  proj <- tr_project_at(root, tr_registry())

  shiny::testServer(tr_server(proj, autosave = FALSE), {
    msgs <- list()
    session$sendCustomMessage <- function(type, message) msgs[[length(msgs) + 1L]] <<- message
    session$setInputs(tr_ready = 1)

    rev_antes <- rv_doc()$rev
    log_antes <- length(session$env$log)
    # A chave viva: o diretório da região existe enquanto ela roda, e é ele que
    # faz o comando de chave morta ser no-op.
    dir.create(file.path(proj$store$root, "stream", "k-viva"), recursive = TRUE)

    msgs <- list()
    session$setInputs(tr_stream_cmd = list(seq = 1, key = "k-viva", cmd = "pause"))

    ctl <- jsonlite::fromJSON(file.path(proj$store$root, "stream", "k-viva", "control.json"),
                              simplifyVector = FALSE)
    expect_equal(ctl$state, "paused")
    expect_equal(rv_doc()$rev, rev_antes)
    expect_equal(length(session$env$log), log_antes)
    # Nem documento nem op de volta: o comando não é conteúdo do grafo.
    expect_false(any(vapply(msgs, function(m) m$type, "") %in% c("document", "op_applied")))

    # Comando torto vira aviso na tela, como todo erro de gesto do transporte —
    # não erro do servidor.
    msgs <- list()
    session$setInputs(tr_stream_cmd = list(seq = 2, key = "k-viva", cmd = "acelera"))
    expect_true("warning" %in% vapply(msgs, function(m) m$type, ""))
  })
})

test_that("`stop` com checkpoint_every = 0: a retomada USA o checkpoint que o stop gravou", {
  # A incoerência que este teste fecha, achada na revisão da Fase 5: o caminho
  # do `stop` grava um checkpoint mesmo com a válvula em 0 — de propósito, é o
  # trabalho que o usuário acabou de assistir acontecer. Mas a LEITURA estava
  # gateada pela mesma válvula, então o run seguinte, com o mesmo 0, jogava
  # aquele arquivo fora e refazia tudo. A escrita não comprava nada, e o
  # prejuízo caía no público exato da válvula: quem a liga é quem tem
  # acumulador grande e não quer pagar serialização periódica.
  #
  # O teste de `checkpoint_every = 0` em test-stream-driver.R NÃO pega isto: lá
  # o run morre por erro de passo, que não grava checkpoint nenhum, então uma
  # leitura desgateada também não encontra arquivo.
  zero <- list(checkpoint_every = 0)
  x <- cenario(n = 10L)
  dir.create(dirname(ctl_path(x$s, x$u$key)), recursive = TRUE, showWarnings = FALSE)
  x$e$comandos <- list("4" = list(cmd = "stop"))

  expect_error(.tr_run_unit(x$u, x$reg, x$s, ctx_extra = zero),
               class = "tr_error_stream_stopped")
  ck <- readRDS(ckpt_path2(x$s, x$u$key))
  expect_equal(ck$i, 4L)   # último passo COMPLETO antes do comando

  # Retomada com a MESMA válvula: sete passos novos, não dez.
  x$e$comandos <- list()
  unlink(ctl_path(x$s, x$u$key))
  antes <- length(x$e$vistos)
  .tr_run_unit(x$u, x$reg, x$s, ctx_extra = zero)
  expect_equal(length(x$e$vistos) - antes, 6L)
  hist <- tr_store_get(x$s, x$u$outputs$out, tr_get_type("c/v", x$reg))
  expect_equal(hist$v, cumsum(1:10) / seq_len(10))
})

test_that("`stop` no passo 1 não promete checkpoint que não existe", {
  # A mensagem dizia "o trabalho até o passo 0 está no checkpoint e o próximo
  # run retoma dali" — e no passo 1 não há passo COMPLETO, então não há arquivo.
  # Prometer retomada sem arquivo no disco é mandar o usuário esperar por algo
  # que não vai acontecer.
  # O comando tem que estar no disco ANTES do laço: emitido de dentro do passo
  # 1, o portão só o vê antes do passo 2, e aí já há um passo completo.
  x <- cenario(n = 10L)
  dir.create(dirname(ctl_path(x$s, x$u$key)), recursive = TRUE, showWarnings = FALSE)
  tr_stream_command(x$s, x$u$key, "stop")

  err <- expect_error(.tr_run_unit(x$u, x$reg, x$s), class = "tr_error_stream_stopped")
  expect_match(conditionMessage(err), "Nada havia a guardar")
  expect_no_match(conditionMessage(err), "está no checkpoint")
  expect_false(file.exists(ckpt_path2(x$s, x$u$key)))
})

test_that("nome QUASE certo em ctx_extra avisa; campo alheio passa calado", {
  # O único modo de falha calado que a porta dos ajustes introduziu:
  # `checkpoint_ever = 0` roda até o fim com o valor de fábrica e ninguém avisa.
  # Validar com lista branca fecharia a porta que `ctx_extra` existe pra abrir
  # (qualquer nó pode ler um campo próprio dali), então o meio é avisar só nos
  # dois campos de que o núcleo é dono, e na porta onde o erro foi digitado.
  expect_warning(.tr_warn_ctx_extra(list(checkpoint_ever = 0)),
                 class = "tr_warn_ctx_extra_typo")
  expect_warning(.tr_warn_ctx_extra(list(publish_ever = 1)),
                 class = "tr_warn_ctx_extra_typo")
  # Campo de um nó qualquer não é erro de digitação: passa sem ruído.
  expect_silent(.tr_warn_ctx_extra(list(meu_campo_de_dominio = 1)))
  expect_silent(.tr_warn_ctx_extra(list(publish_every = 1, checkpoint_every = 10)))
  expect_silent(.tr_warn_ctx_extra(NULL))
})
