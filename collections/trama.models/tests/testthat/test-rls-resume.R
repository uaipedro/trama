# Minor 10 da revisão da Fase 7: o round trip de nível 1 (`saveRDS`/`readRDS`,
# `test-rls.R`) prova que o ESTADO do RLS sobrevive ao disco. Não prova que o
# DRIVER o usa certo numa retomada de verdade — essa é a lacuna que o core já
# fecha para um nó de memória sintético (`test-stream-driver.R`, "run morto no
# passo 180 retoma do passo 100"). Aqui é a mesma forma, mas com o primeiro nó
# de memória REAL da coleção: se o checkpoint gravasse o estado errado (ou não
# gravasse), `models/rls` recomeçaria do zero em algum ponto do meio e o
# histórico sairia uma curva plausível — não uma que erra alto.
#
# `.tr_run_unit()` direto, como o core faz, e não `tr_run()`/`tr_value()`: o
# scheduler público trata uma chave já marcada `failed` como cache — a segunda
# chamada nem tentaria de novo — e o que está sob teste aqui é o driver, não o
# scheduler.

# Uma coleção só para este teste: um nó comum que produz 250 pontos fixos, e
# um nó de fluxo que morre no passo escolhido — o mesmo papel de `d/puro_morre`
# no core, adaptado para dados no formato que `models/rls` espera (`data/table`).
.tr_resume_test_collection <- function(e) {
  trama::tr_collection(
    id = "t_resume", version = "1.0.0", label = "Teste de retomada do RLS",
    nodes = list(
      trama::tr_node("t_resume/dados", fn = function() {
        set.seed(11)
        d <- data.frame(x = rnorm(250))
        d$y <- 1 - 2 * d$x + rnorm(250, sd = 0.2)
        d
      }, outputs = list(out = "data/table"),
         description = "250 pontos fixos, gerados uma vez — o nó comum de fora da região."),
      # Morre no passo `e$morre_em`, contando por CHAMADA (não pelo conteúdo do
      # ponto) — mesma escolha de `d/puro_morre`: `e$morre_em` é estado do
      # processo de teste, não param do grafo, então a chave da região não muda
      # entre o run que morre e o run que retoma.
      trama::tr_node("t_resume/morre", fn = function(dados) {
        e$vistos <- e$vistos + 1L
        if (!is.null(e$morre_em) && identical(e$vistos, e$morre_em)) {
          rlang::abort("worker morto", class = "tr_error_teste")
        }
        dados
      }, inputs = list(dados = trama::tr_port("data/table", stream = TRUE)),
         outputs = list(out = trama::tr_port("data/table", stream = TRUE)),
         description = "Passa o ponto adiante sem tocá-lo, e morre no ponto escolhido.")
    )
  )
}

.tr_resume_test_doc <- function(reg) {
  trama::tr_flow_doc(trama::tr_flow(reg) |>
    trama::tr_add("dados", "t_resume/dados") |>
    trama::tr_add("entra", "data/to_stream", lote = 1L, from = "dados") |>
    trama::tr_add("morre", "t_resume/morre", from = "entra") |>
    trama::tr_add("rls", "models/rls", resposta = "y", preditores = "x", from = "morre") |>
    trama::tr_add("sai", "data/from_stream", from = "rls"))
}

# `t_resume/dados` é um nó COMUM fora da região: seu artefato precisa existir
# no store ANTES de `.tr_run_unit()` da região, porque a região só LÊ as
# entradas externas do store (a mesma ordem de `test-stream-driver.R`, "entrada
# comum chega igual... e é lida do store UMA vez").
.tr_resume_test_run_dados <- function(doc, reg, store) {
  .tr_run_unit(tr_plan(doc, registry = reg, store = store)$units[["dados"]], reg, store)
}

.tr_resume_test_ckpt_path <- function(store, key) file.path(store$root, "stream", key, "ckpt.rds")

# `trama.models` não tem `tmp_store()` (é helper do core); um store próprio
# por chamada, do mesmo jeito que `rodar()` (`helper-models.R`) já faz.
.tr_resume_test_store <- function() trama::tr_store(tempfile("tr_resume_store"))

test_that("retomada de uma região com models/rls: morre no passo 180, retoma do checkpoint, histórico IDÊNTICO", {
  reg <- models_registry()
  e <- new.env(parent = emptyenv()); e$vistos <- 0L; e$morre_em <- NULL
  trama::tr_use(.tr_resume_test_collection(e), registry = reg)
  doc <- .tr_resume_test_doc(reg)

  # 1. A referência: sem interrupção, em store próprio.
  s0 <- .tr_resume_test_store()
  .tr_resume_test_run_dados(doc, reg, s0)
  u0 <- tr_plan(doc, registry = reg, store = s0)$units[["sai"]]
  .tr_run_unit(u0, reg, s0, ctx_extra = list(checkpoint_every = 50))
  ref <- trama::tr_store_get(s0, u0$outputs$out, trama::tr_get_type("data/table", reg))
  expect_equal(nrow(ref), 250L)

  # 2. O run que morre no ponto 180.
  s <- .tr_resume_test_store()
  .tr_resume_test_run_dados(doc, reg, s)
  u <- tr_plan(doc, registry = reg, store = s)$units[["sai"]]
  expect_identical(u$key, u0$key)  # mesma chave — é ela que acha o checkpoint
  e$vistos <- 0L; e$morre_em <- 180L
  expect_error(.tr_run_unit(u, reg, s, ctx_extra = list(checkpoint_every = 50)),
               class = "tr_error_stream_step")

  ckpt <- .tr_resume_test_ckpt_path(s, u$key)
  expect_true(file.exists(ckpt))
  expect_equal(readRDS(ckpt)$i, 150L)  # último múltiplo de 50 antes de 180

  # 3. A retomada, com a mesma chave — e de VERDADE: só os 100 pontos que
  # faltavam, não os 250 de novo.
  e$vistos <- 0L; e$morre_em <- NULL
  .tr_run_unit(u, reg, s, ctx_extra = list(checkpoint_every = 50))
  expect_equal(e$vistos, 100L)

  hist <- trama::tr_store_get(s, u$outputs$out, trama::tr_get_type("data/table", reg))
  expect_identical(hist, ref)

  # 4. Concluída, o diretório de checkpoint sai — o artefato final o substitui.
  expect_false(dir.exists(dirname(ckpt)))
})
