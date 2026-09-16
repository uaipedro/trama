#' Comandar uma região em voo — pelo STORE, que já é o protocolo.
#'
#' A Decisão 10 fixou que comando vai pelo store, e não por socket. O caminho de
#' subida já tinha feito essa escolha (`.ctx$progress()` publica em
#' `<store>/progress/<chave>.json` e o `collect()` lê por polling); este arquivo
#' é a descida, no mesmo registro e sem maquinaria nova:
#'
#'   <store>/stream/<chave-da-unidade>/control.json
#'
#' No MESMO diretório do checkpoint (`.tr_stream_dir()`), porque é o mesmo
#' assunto: o estado em voo de uma unidade que dura minutos. Um segundo canal
#' custaria dependência nova, ordenação nova entre canais e um caminho a mais
#' pra morrer — e não funcionaria nos dois executores, que é a propriedade que
#' fez o store ganhar da porta `nanonext` no desenho.
#'
#' ## Um escritor só, e é por isso que existe `seq`
#'
#' O coordenador ESCREVE, o driver LÊ. O driver nunca escreve aqui, e isso não é
#' economia: com dois escritores, "consumir" um `step` (apagar o pedido depois de
#' honrá-lo) apagaria também o comando que o usuário mandou no mesmo intervalo, e
#' um clique sumiria sem rastro. Em vez disso, cada comando anda o `seq`, e o
#' driver LEMBRA qual `seq` já honrou — a memória do que foi consumido é de quem
#' lê, num lado só. Sem ela, `step_once = TRUE` seria lido de novo em cada passo
#' seguinte e "um passo" viraria "roda tudo" com o botão de pausa aceso.
#'
#' ## O que "pausar" pode significar em cada executor
#'
#' Com `tr_executor_pool()`, a região roda num daemon e o coordenador fica livre:
#' escrever o comando enquanto o laço anda é o caso normal, e é o que o front faz.
#'
#' Com `tr_executor_sequential()`, `submit()` roda a unidade no PRÓPRIO processo
#' do coordenador. Enquanto a região roda, não há ninguém pra escrever o arquivo
#' — pausa é inalcançável ali, por construção, e não por falta de código. O
#' driver lê o controle do mesmo jeito (o arquivo pode ter sido escrito por outra
#' sessão sobre o mesmo store, ou por um nó de dentro do laço, que é como os
#' testes encenam o segundo processo), mas em uso normal, no executor
#' sequencial, os botões não respondem. Está escrito aqui porque é exatamente o
#' tipo de coisa que alguém relata como bug.
#'
#' ## Comando não é op de documento
#'
#' Pause/step/tempo/stop não entram no log de undo, não mexem em `rev` e não
#' aparecem no documento — é o precedente de `sinks-ricos.md` ("rode este nó
#' agora" é COMANDO, não op). Por isso `tempo` é comando e não param: como param
#' entraria na chave da região (Decisão 9), e mexer na velocidade de assistir
#' recomputaria o fluxo inteiro.
#' @noRd
NULL

#' O incremento de espera do laço pausado.
#'
#' 0.05s é a cadência com que o coordenador já bombeia o scheduler
#' (`later::later(pump, 0.05)`), então um play notado um cochilo depois não é
#' perceptível — a latência de ponta a ponta continua sendo a do polling que já
#' existe.
#'
#' O que uma região PAUSADA custa: 20 leituras por segundo de um JSON de ~80
#' bytes no cache do sistema (~73µs cada, ou 0.15% de um núcleo), e nada de
#' espera apertada — que é a diferença que importa numa pausa de minutos, e o
#' motivo de a espera ser um `Sys.sleep` e não um `repeat` seco.
#'
#' A cancela do laço RODANDO não é estrangulada por relógio nenhum: o raciocínio
#' e os números estão em `stream-driver.R`, onde ela é chamada.
#' @noRd
.tr_control_poll <- 0.05

#' @noRd
.tr_control_path <- function(store, key) file.path(.tr_stream_dir(store, key), "control.json")

#' Manda um comando para a região que roda sob `key` (a chave da UNIDADE, que é
#' a que vem nos eventos do run).
#'
#' `cmd` é um de:
#'   "play"  — volta a andar sozinho
#'   "pause" — para entre passos e espera
#'   "step"  — um passo e volta a pausado
#'   "tempo" — muda o atraso por passo, sem mexer em pausado/rodando
#'   "stop"  — encerra o run desta região deixando checkpoint
#'
#' Chave sem diretório de região é NO-OP silencioso: o clique do usuário e o
#' evento `done` se cruzam, e comando de região que já terminou é normal, não
#' erro. Erro ali viraria banner vermelho por um botão que chegou tarde — e
#' criar o diretório "por cortesia" encheria o store de diretórios de chaves que
#' nunca rodaram. O diretório é criado pelo DRIVER quando a região começa: é ele
#' quem sabe que há laço pra comandar.
#' @export
tr_stream_command <- function(store, key, cmd, tempo = NULL) {
  if (length(key) != 1L || is.na(key) || !nzchar(key)) return(invisible(FALSE))
  if (!dir.exists(.tr_stream_dir(store, key))) return(invisible(FALSE))

  atual <- .tr_control_read(store, key) %||%
    list(state = "running", tempo = 0, step_once = FALSE, seq = 0L)
  # `cmd` conferido ANTES do `switch`: com `cmd` ausente ou vazio (mensagem do
  # front sem o campo), `as.character(cmd)[[1]]` sairia como "subscript out of
  # bounds" — erro de R cru num banner, sem uma palavra sobre o que faltava.
  if (!is.character(cmd) || length(cmd) != 1L || is.na(cmd)) {
    rlang::abort(paste0("Comando de região ausente ou malformado. Os comandos são play, ",
                        "pause, step, tempo e stop."),
                 class = "tr_error_stream_bad_command")
  }
  novo <- switch(
    cmd,
    play  = list(state = "running", step_once = FALSE),
    pause = list(state = "paused",  step_once = FALSE),
    step  = list(state = "paused",  step_once = TRUE),
    stop  = list(state = "stopped", step_once = FALSE),
    # `tempo` preserva o estado: mexer na velocidade de quem está pausado não
    # pode destravar o laço, senão o botão de velocidade seria também um play.
    tempo = list(state = atual$state, step_once = FALSE),
    rlang::abort(sprintf(
      "Comando de região desconhecido: '%s'. Os comandos são play, pause, step, tempo e stop.",
      paste(cmd, collapse = ", ")),
      class = "tr_error_stream_bad_command")
  )

  ctl <- list(state = novo$state,
              tempo = if (is.null(tempo)) atual$tempo else .tr_control_tempo(tempo),
              step_once = novo$step_once,
              # O `seq` anda em TODO comando, inclusive num "pause" repetido: é
              # ele que acorda o laço pausado, e é por ele que o driver sabe que
              # um `step` é novo.
              seq = as.integer(atual$seq) + 1L)
  dir.create(dirname(.tr_control_path(store, key)), recursive = TRUE, showWarnings = FALSE)
  .tr_atomic(store, .tr_control_path(store, key), function(tmp) {
    # `auto_unbox = TRUE` como no resto do pacote: os quatro campos são
    # escalares, que é a direção SEGURA do desembrulho (o estrago documentado no
    # comentário longo de `store.R` é com vetor nomeado, não com escalar). O
    # `test-stream-control.R` fixa a forma do outro lado do canal mesmo assim:
    # o driver que lê é outro processo, e adivinhar se `state` é texto ou lista
    # de um texto não é trabalho dele.
    jsonlite::write_json(ctl, tmp, auto_unbox = TRUE, digits = NA)
  })
  invisible(TRUE)
}

#' Apara o `tempo` recebido: segundos, entre 0 e 5.
#'
#' Valor torto (texto, NA, negativo, infinito) vale 0, e o teto existe porque
#' `tempo` gigante dormiria o daemon por horas com o card aceso e ninguém
#' entendendo por quê. Mesmo argumento do `checkpoint_every` torto do driver: um
#' botão de operação não pode fazer uma região que roda hoje parar de rodar —
#' nem virar um travamento indistinguível de nó lento.
#'
#' O teto é 5 segundos por passo, e não um minuto: acima de poucos segundos não
#' se está assistindo, e sim esperando — indistinguível de travamento, com o
#' daemon segurando uma vaga do pool o tempo todo. Quem quer olhar um ponto de
#' cada vez tem `step`, que é o botão certo pra isso e não tem teto nenhum.
#' @noRd
.tr_control_tempo <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  if (length(v) != 1L || is.na(v) || !is.finite(v) || v < 0) return(0)
  min(v, 5)
}

#' Lê o controle da chave, ou `NULL` se não houver um utilizável.
#'
#' Ilegível conta como AUSENTE, nunca como exceção — o precedente de
#' `.tr_ckpt_read()` e de `tr_store_handle()`. Uma região que se recusa a rodar
#' porque o botão de pausa escreveu meio arquivo (worker morto no meio da
#' escrita, disco cheio) é pior que uma pausa que não funciona: o custo de cair
#' aqui é um botão ignorado, e o de abortar seria o fluxo inteiro.
#'
#' Devolve sempre os quatro campos normalizados, pra que o laço não tenha que
#' saber que o JSON pode vir sem um deles.
#' @noRd
.tr_control_read <- function(store, key) {
  p <- .tr_control_path(store, key)
  if (!file.exists(p)) return(NULL)
  ctl <- tryCatch(jsonlite::fromJSON(p, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.list(ctl)) return(NULL)
  estado <- if (is.character(ctl$state) && length(ctl$state) == 1L) ctl$state else "running"
  if (!estado %in% c("running", "paused", "stopped")) estado <- "running"
  seq <- suppressWarnings(as.integer(ctl$seq %||% 0L))
  list(state = estado,
       tempo = .tr_control_tempo(ctl$tempo %||% 0),
       step_once = isTRUE(as.logical(ctl$step_once %||% FALSE)),
       seq = if (length(seq) != 1L || is.na(seq)) 0L else seq)
}

#' Apaga SÓ o controle, deixando o checkpoint onde está.
#'
#' Chamado pelo `dispatch()` do scheduler antes de submeter, pelo mesmo motivo
#' que o progresso órfão é apagado ali (Tarefa 5.1): a chave é determinística, e
#' um run morto sem limpar deixa o `control.json` no lugar — o run novo nasceria
#' pausado, com o card parado e nada pra investigar.
#'
#' E por que não `unlink(dir)`: o diretório também guarda o `ckpt.rds`, que é a
#' razão de ele existir. Apagar o diretório aqui jogaria fora a retomada em todo
#' despacho — o oposto da Tarefa 5.2.
#' @noRd
.tr_control_clear <- function(store, key) {
  unlink(.tr_control_path(store, key))
  invisible(TRUE)
}

#' A cancela ENTRE PASSOS: o que o laço faz antes de dar o passo `i`.
#'
#' Devolve `acao`:
#'   "segue" — dá o passo (rodando, ou um `step` recém-pedido)
#'   "para"  — encerra o run deixando checkpoint (ver o driver)
#'
#' Pausado sem `step` pendente, DORME em incrementos de `.tr_control_poll` e lê
#' de novo — espera apertada aqui queimaria um núcleo durante uma pausa que pode
#' durar minutos. `dorme` é argumento pra que o teste possa ser o outro processo:
#' num laço de um processo só, é dentro do cochilo que o comando de play tem que
#' aparecer.
#'
#' `seq_honrado` é o `seq` do último comando que este laço já obedeceu, e volta
#' atualizado: é a memória que faz `step` valer UM passo (ver o cabeçalho).
#'
#' `esperou` diz se o passo saiu de uma espera (pausa ou `step`). Quem usa é a
#' publicação do parcial: o passo que o usuário pediu a dedo é justamente a
#' escrita que ele está esperando, e o estrangulamento de cadência da Tarefa 5.1
#' — que existe contra escrita que ninguém lê — engoliria exatamente essa.
#' @noRd
.tr_control_gate <- function(store, key, seq_honrado, dorme = Sys.sleep) {
  # O caminho de sempre — nenhum comando jamais mandado nesta chave — custa um
  # `file.exists` (~2.7µs) e nada mais: é o que torna a leitura em todo passo
  # barata o bastante pra não precisar de estrangulamento (ver o driver).
  if (!file.exists(.tr_control_path(store, key))) {
    return(list(acao = "segue", tempo = 0, seq = seq_honrado, esperou = FALSE))
  }
  esperou <- FALSE
  repeat {
    ctl <- .tr_control_read(store, key)
    if (is.null(ctl)) {
      return(list(acao = "segue", tempo = 0, seq = seq_honrado, esperou = esperou))
    }
    if (identical(ctl$state, "stopped")) {
      return(list(acao = "para", tempo = ctl$tempo, seq = ctl$seq, esperou = esperou))
    }
    if (!identical(ctl$state, "paused")) {
      return(list(acao = "segue", tempo = ctl$tempo, seq = ctl$seq, esperou = esperou))
    }
    # Pausado. Um `step` só libera se for NOVO — `seq` diferente do último
    # honrado. Com a comparação ausente, o mesmo `step_once = TRUE` liberaria
    # todos os passos seguintes: "um passo" viraria "roda tudo".
    if (isTRUE(ctl$step_once) && !identical(ctl$seq, seq_honrado)) {
      return(list(acao = "segue", tempo = ctl$tempo, seq = ctl$seq, esperou = TRUE))
    }
    esperou <- TRUE
    dorme(.tr_control_poll)
  }
}
