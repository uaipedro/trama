# O guard mora no `store` porque ele é o funil único por onde todo valor entra
# no artefato — mesma lição que `data/table` e `view/plot` aprenderam.

test_that("store de series/ts recusa o que não é série univariada, classificado", {
  st <- series_ts_type()$store
  p <- tempfile(fileext = ".rds")
  for (x in list(1:10, tabela_mensal(), datasets::EuStockMarkets)) {
    err <- tryCatch(st(x, p), error = identity)
    expect_equal(class(err)[[1]], "tr_series_error_not_a_series")
  }
  expect_match(conditionMessage(tryCatch(st(datasets::EuStockMarkets, p), error = identity)),
               "DAX")
  expect_false(file.exists(p))
})

test_that("store/restore de série é identidade", {
  ty <- series_ts_type()
  f <- tempfile(fileext = ".rds")
  ty$store(serie_mensal(), f)
  expect_identical(ty$restore(f), serie_mensal())
})

test_that("resumo da série diz início, fim e frequência legíveis", {
  s <- series_ts_type()$summary(serie_mensal())
  expect_equal(s$inicio, "1949 jan")
  expect_equal(s$fim, "1960 dez")
  expect_equal(s$frequencia, 12)
  expect_equal(series_ts_type()$summary(serie_anual())$inicio, "1871")
})

test_that("adaptador série -> tabela: Date no mensal, inteiro no anual", {
  t1 <- .tr_series_tabela(serie_mensal())
  expect_equal(t1$tempo[[1]], as.Date("1949-01-01"))
  expect_equal(t1$tempo[[144]], as.Date("1960-12-01"))
  expect_equal(t1$valor[[1]], 112)
  t2 <- .tr_series_tabela(serie_anual())
  expect_identical(t2$tempo[[1]], 1871L)
  t3 <- .tr_series_tabela(datasets::UKgas)
  expect_equal(t3$tempo[[2]], as.Date("1960-04-01"))
})

test_that("preview da série é um PNG 2:1 pelo trama/image", {
  skip_if_not_installed("png")
  art <- series_ts_type()$preview(serie_mensal(), ctx_tmp())
  expect_equal(art$renderer, "trama/image")
  dims <- dim(png::readPNG(art$files$png))
  expect_equal(c(dims[[2]], dims[[1]]), c(1600, 800))
})

test_that("o rótulo de uma posição sai do calendário, nas três frequências", {
  # O `series/pettitt` devolve um ÍNDICE, e "30" não diz nada num relatório. O
  # helper mora em `tempo.R`, e não no bloco, porque o `series/zivot_andrews` vai
  # fazer a mesma pergunta para a quebra estrutural dele — duas cópias da regra
  # de ano divergiriam na primeira correção feita de um lado só.
  expect_equal(.tr_series_rotulo_em(serie_mensal(), 54), "1953 jun")
  expect_equal(.tr_series_rotulo_em(serie_mensal(), 1), "1949 jan")
  expect_equal(.tr_series_rotulo_em(serie_mensal(), 144), "1960 dez")
  expect_equal(.tr_series_rotulo_em(datasets::UKgas, 2), "1960 T2")
  expect_equal(.tr_series_rotulo_em(datasets::UKgas, 7), "1961 T3")
  expect_equal(.tr_series_rotulo_em(serie_anual(), 1), "1871")
  expect_equal(.tr_series_rotulo_em(serie_anual(), 30), "1900")
})

test_that("o ano do rótulo vem do arredondamento, e não do piso", {
  # A armadilha que `.tr_series_tempo()` documenta, agora pelo outro helper:
  # janeiro pode chegar como `1949.9999999`, e um `floor` o jogaria para o ano
  # anterior — o rótulo sairia "1949 jan" onde a série diz 1950, sem erro nenhum.
  # É por isso que a regra de ano é REUSADA daqui e não reescrita no bloco.
  x <- stats::ts(1:24, start = c(1950, 1), frequency = 12)
  expect_equal(.tr_series_rotulo_em(x, 1), "1950 jan")
  expect_equal(.tr_series_rotulo_em(x, 13), "1951 jan")
  # E a conferência contra a coluna `tempo`, que é a outra leitora da mesma
  # regra: as duas têm de concordar sobre o ano de todo mês da série.
  tt <- .tr_series_tabela(serie_mensal())
  rot <- vapply(seq_along(serie_mensal()), function(i) .tr_series_rotulo_em(serie_mensal(), i), "")
  expect_equal(substr(rot, 1, 4), format(tt$tempo, "%Y"))
})

test_that("tipo da regressão: guard, identidade, resumo e card de texto", {
  ty <- series_regression_type()
  r <- tr_series_regression(serie_mensal())
  f <- tempfile(fileext = ".rds")
  ty$store(r, f)
  expect_s3_class(ty$restore(f), "tr_series_reg")
  s <- ty$summary(r)
  expect_equal(s$grau, 1L)
  expect_true(s$sazonalidade)
  expect_gt(s$r2_ajustado, 0.8)
  expect_lt(s$p_valor_f, 0.001)
  expect_equal(ty$preview(r, ctx_tmp())$renderer, "trama/text")
  expect_error(ty$store(serie_mensal(), tempfile()),
               class = "tr_series_error_not_a_regression")
})

test_that("adaptadores da regressão: decomposição que fecha, e coeficientes em tabela", {
  r <- tr_series_regression(serie_mensal())
  d <- .tr_series_reg_decomp(r)
  expect_s3_class(d, "tr_series_decomp")
  expect_equal(d$metodo, "regressão")
  expect_equal(as.numeric(d$tendencia + d$sazonal + d$resto), as.numeric(serie_mensal()),
               tolerance = 1e-8)
  expect_gt(.tr_series_forca(d)[["sazonal"]], 0.5)
  tab <- .tr_series_reg_tabela(r)
  expect_equal(names(tab), c("termo", "estimativa", "erro_padrao", "estatistica_t", "p_valor"))
  expect_equal(tab$termo[[1]], "(Intercept)")
  expect_equal(nrow(tab), 13L)
  pares <- vapply(.tr_series_adapters(), function(a) paste(a$from, a$to), character(1))
  expect_true("series/regression data/table" %in% pares)
  expect_true("series/regression series/decomposition" %in% pares)
})

test_that("store de data/test recusa o que não tem a forma de um teste", {
  st <- trama::tr_get_type("data/test", series_registry())$store
  p <- tempfile(fileext = ".rds")
  for (x in list(1:10, tabela_mensal(), list(teste = "ADF"))) {
    err <- tryCatch(st(x, p), error = identity)
    expect_equal(class(err)[[1]], "tr_error_not_a_test")
  }
  expect_false(file.exists(p))
})

test_that("um teste da series sai no tipo único: card, store e adaptador", {
  reg <- series_registry()
  t <- tr_series_adf(stats::ts(cumsum(stats::rnorm(60))))
  ty <- trama::tr_get_type("data/test", reg)
  pv <- ty$preview(t, list())
  expect_equal(pv$renderer, "trama/test")
  expect_equal(names(pv$data$criticos), c("10%", "5%", "1%"))
  arq <- tempfile(fileext = ".rds"); ty$store(t, arq)
  expect_identical(ty$restore(arq), t)
  tb <- trama::tr_adapter_for("data/test", "data/table", reg)$fn(t)
  expect_s3_class(tb, "tbl_df"); expect_true(is.na(tb$p_valor))
})

test_that("testes da models e da series se empilham num quadro de colunas fixas", {
  skip_if_not_installed("trama.models")
  reg <- series_registry()
  ad <- trama::tr_adapter_for("data/test", "data/table", reg)$fn
  a <- ad(tr_series_ljung_box(serie_mensal()))
  b <- ad(trama.models::tr_models_shapiro(datasets::mtcars, "mpg"))
  q <- trama.data::tr_bind_rows(list(a, b))
  fixas <- c("teste", "h0", "rotulo_estat", "estatistica", "gl", "p_valor", "significancia",
             "valor_critico_5", "decisao_5", "conclusao", "efeito", "efeito_valor",
             "efeito_li_95", "efeito_ls_95", "nota", "fonte")
  # As fixas vêm primeiro e na mesma ordem nos dois; o que é próprio de cada
  # teste (`extra`) vem depois e fica NA na linha do outro.
  expect_equal(names(a)[seq_along(fixas)], fixas); expect_equal(names(b)[seq_along(fixas)], fixas)
  expect_equal(nrow(q), 2L); expect_equal(names(q)[seq_along(fixas)], fixas)
  expect_type(q$p_valor, "double"); expect_type(q$efeito_valor, "double")
})

test_that("adaptador teste -> tabela é uma linha, com a fonte", {
  t <- .tr_series_teste("ADF", "raiz unitária", -4.12, "t",
                        criticos = c(`10%` = -2.57, `5%` = -2.88, `1%` = -3.46),
                        sentido = "menor", conclusao_sim = "estacionária",
                        conclusao_nao = "não estacionária", fonte = "Dickey & Fuller (1979)")
  tb <- tabela_teste(t)
  expect_equal(nrow(tb), 1L)
  expect_true(all(c("teste", "h0", "estatistica", "p_valor", "valor_critico_5",
                    "decisao_5", "conclusao", "nota", "fonte") %in% names(tb)))
  expect_equal(tb$teste, "ADF")
  expect_equal(tb$valor_critico_5, -2.88)
})

test_that("extra vira coluna só quando existe", {
  base <- list("Pettitt", "homogêneos", 878, "K", p_valor = 1e-9,
               conclusao_sim = "há quebra", conclusao_nao = "não há", fonte = "Pettitt (1979)")
  sem <- tabela_teste(do.call(.tr_series_teste, base))
  com <- tabela_teste(do.call(.tr_series_teste,
                                          c(base, list(extra = list(ponto_de_mudanca = 30)))))
  expect_false("ponto_de_mudanca" %in% names(sem))
  expect_equal(com$ponto_de_mudanca, 30)
  # Os DOIS caminhos devolvem tibble. Sem o `as_tibble` de fora, o `cbind`
  # despacharia pro `cbind.data.frame` e a classe de saída do adaptador
  # passaria a depender da entrada — um adaptador com duas caras.
  expect_s3_class(sem, "tbl_df")
  expect_s3_class(com, "tbl_df")
})
