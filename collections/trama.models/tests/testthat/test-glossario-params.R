# Glossário de params (docs/glossario-parametros.md): `alfa` virou `confianca`
# e `coluna` virou `variavel`. Dois lados a travar: a conta com a confiança
# nova é a mesma do alfa antigo, e o fluxo salvo com o nome antigo abre migrado.

doc_minimo <- function(tipo, params) {
  list(nodes = list(n = list(type = tipo, params = params)), edges = list())
}

test_that("emmeans: confianca = 0.9 é o alfa = 0.1 do emmeans", {
  m <- milho_dbc()
  e <- tr_models_emmeans(m, "hibrido", confianca = 0.9)
  ref <- summary(emmeans::emmeans(m$ajuste, "hibrido"), level = 0.9)
  expect_equal(e$tabela$li, ref$lower.CL)
  expect_equal(e$tabela$ls, ref$upper.CL)
  expect_equal(e$alfa, 0.1)
  expect_match(e$nota, "a 10%", fixed = TRUE)
})

test_that("duncan: confianca = 0.9 é o alpha = 0.1 do agricolae", {
  m <- milho_dbc()
  d <- tr_models_duncan(m, "hibrido", confianca = 0.9)
  ref <- agricolae::duncan.test(m$ajuste, "hibrido", alpha = 0.1, console = FALSE)$groups
  expect_equal(d$tabela$grupo, trimws(as.character(ref$groups[match(levels(m$dados$hibrido), rownames(ref))])))
})

test_that("predict: confianca vai para o level do predict()", {
  mt <- ex("mtcars")
  r <- tr_models_predict(tr_models_lm(mt, formula = "mpg ~ wt"), mt, intervalo = "predicao", confianca = 0.9)
  ref <- stats::predict(stats::lm(mpg ~ wt, data = mt), mt, interval = "prediction", level = 0.9)
  expect_equal(r$li, unname(ref[, "lwr"]))
  expect_equal(r$ls, unname(ref[, "upr"]))
})

test_that("fluxos salvos com alfa/coluna abrem com confianca/variavel", {
  reg <- models_registry()
  mig <- function(tipo, params) trama::tr_doc_migrate(doc_minimo(tipo, params), reg)$nodes$n$params
  for (tipo in c("models/emmeans", "models/duncan")) {
    p <- mig(tipo, list(alfa = 0.1))
    expect_null(p$alfa)
    expect_equal(p$confianca, 0.9)
  }
  for (tipo in c("models/one_sample_t", "models/shapiro")) {
    p <- mig(tipo, list(coluna = "weight"))
    expect_null(p$coluna)
    expect_equal(p$variavel, "weight")
  }
})

test_that("os leitores de classificador da multi abrem como os blocos daqui", {
  # Declarado aqui porque o destino é daqui; a coleção de origem nem precisa
  # estar carregada para o id antigo migrar.
  reg <- models_registry()
  mig <- function(tipo, params) trama::tr_doc_migrate(doc_minimo(tipo, params), reg)$nodes$n
  expect_equal(mig("multi/classify", list(validacao = "cruzada")),
               list(type = "models/predict", params = list(validacao = "cruzada")))
  expect_equal(mig("multi/confusion", list())$type, "models/confusion")
  expect_equal(mig("multi/roc", list(validacao = "resubstituição"))$params$validacao, "resubstituição")
  # A multi/logistic_coefficients mostrava razões de chances: o nó que vem de
  # lá ganha `exponenciar`, mesmo salvo sem param nenhum; `nivel` vira confiança.
  rc <- mig("multi/logistic_coefficients", list(nivel = 0.9, escala = "desvio padrão"))
  expect_equal(rc$type, "models/coefficients")
  expect_equal(rc$params, list(escala = "desvio padrão", exponenciar = TRUE, confianca = 0.9))
  expect_equal(mig("multi/logistic_coefficients", list()),
               list(type = "models/coefficients", params = list(exponenciar = TRUE)))
  # Um models/coefficients nativo com escala não ganha exponenciar (num lm, ele
  # faria o card errar).
  expect_equal(mig("models/coefficients", list(escala = "desvio padrão"))$params,
               list(escala = "desvio padrão"))
  # A porta `novos` do classify é a `dados` do predict.
  doc <- list(nodes = list(t = list(type = "data/example", params = list()),
                           c = list(type = "multi/classify", params = list())),
              edges = list(list(from = list(node = "t", port = "out"), to = list(node = "c", port = "novos"))))
  expect_equal(trama::tr_doc_migrate(doc, reg)$edges[[1]]$to$port, "dados")
})
