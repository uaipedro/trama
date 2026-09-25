test_that("lagarta: um painel por termo, intervalo pela variância condicional, faixa do desvio", {
  s <- tr_models_lmer(ex("sleepstudy"), formula = "Reaction ~ Days + (Days | Subject)")
  p <- tr_models_plot_caterpillar(s, intervalo = "± 2 EP")
  expect_s3_class(p, "ggplot")
  d <- p$data
  expect_equal(levels(d$termo), c("(Intercept)", "Days"))
  expect_equal(d$ls - d$condval, 2 * d$condsd)
  r <- as.data.frame(lme4::ranef(s$ajuste, condVar = TRUE))
  expect_equal(sort(d$condval), sort(r$condval))
  # A mesma ordem de sujeitos nos dois painéis.
  expect_equal(levels(d$grp), as.character(r$grp[r$term == "(Intercept)"][order(r$condval[r$term == "(Intercept)"])]))
  # A faixa usa o desvio padrão que o models/random_effects mostra.
  faixa <- p$layers[[1]]$data
  ve <- tr_models_random_effects(s)
  expect_equal(faixa$sd[[1]], ve$desvio_padrao[ve$componente == "(Intercept)"])
  expect_error(tr_models_plot_caterpillar(s, grupo = "Sujeito"), class = "tr_models_error_unknown_column")
  expect_error(tr_models_plot_caterpillar(milho_dbc()), class = "tr_models_error_not_applicable")
  expect_s3_class(tr_models_plot_caterpillar(
    tr_models_anova_split_plot(ex("aveia"), "producao", "variedade", "nitrogenio", "bloco")), "ggplot")
})

test_that("lagarta: IC usa a confiança (z = qnorm(1 - (1 - conf)/2))", {
  s <- tr_models_lmer(ex("sleepstudy"), formula = "Reaction ~ Days + (Days | Subject)")
  d <- tr_models_plot_caterpillar(s, intervalo = "IC", confianca = 0.9)$data
  expect_equal(d$ls - d$condval, stats::qnorm(0.95) * d$condsd)
  d <- tr_models_plot_caterpillar(s)$data
  expect_equal(d$ls - d$condval, stats::qnorm(0.975) * d$condsd)
  expect_error(tr_models_plot_caterpillar(s, intervalo = "IC 95%"))
})

test_that("lagarta: fluxo com o enum antigo abre com intervalo e confiança separados, uma vez só", {
  reg <- models_registry()
  doc <- trama::tr_doc_parse('{"format":1,"nodes":{
    "a":{"type":"models/plot_caterpillar","params":{"intervalo":"IC 90%"}},
    "b":{"type":"models/plot_caterpillar","params":{"intervalo":"± 1 EP"}}},"edges":[]}')
  doc <- trama::tr_doc_migrate(doc, reg)
  expect_equal(doc$nodes$a$params$intervalo, "IC")
  expect_equal(doc$nodes$a$params$confianca, 0.9)
  expect_equal(doc$nodes$b$params, list(intervalo = "± 1 EP"))
  attr(doc, "migrated") <- NULL
  expect_identical(trama::tr_doc_migrate(doc, reg), doc)
})

test_that("Duncan e Waller-Duncan batem com o agricolae", {
  m <- milho_dbc()
  ref <- agricolae::duncan.test(m$ajuste, "hibrido", console = FALSE)$groups
  d <- tr_models_duncan(m, "hibrido")
  expect_equal(d$tabela$grupo, trimws(as.character(ref$groups[match(levels(m$dados$hibrido), rownames(ref))])))
  expect_null(d$grade)
  refw <- agricolae::waller.test(m$ajuste, "hibrido", K = 100, console = FALSE)$groups
  w <- tr_models_waller_duncan(m, "hibrido")
  expect_equal(w$tabela$grupo, trimws(as.character(refw$groups[match(levels(m$dados$hibrido), rownames(refw))])))
  expect_match(w$nota, "diferença crítica", fixed = TRUE)
  # O card das médias e o adaptador valem para eles.
  expect_s3_class(tr_models_plot_means(d), "ggplot")
  expect_error(tr_models_pairwise(d), class = "tr_models_error_not_applicable")
})

test_that("na parcela subdividida cada fator usa o seu erro", {
  sp <- tr_models_anova_split_plot(ex("aveia"), "producao", "variedade", "nitrogenio", "bloco")
  q <- tr_models_anova_table(sp)$tabela
  v <- tr_models_duncan(sp, "variedade")
  expect_equal(unique(v$tabela$gl), q$gl[q$termo == "Resíduo (a)"])
  n <- tr_models_duncan(sp, "nitrogenio")
  expect_equal(unique(n$tabela$gl), q$gl[q$termo == "Resíduo (b)"])
  expect_error(tr_models_duncan(sp, "variedade, nitrogenio"), class = "tr_models_error_not_applicable")
  expect_error(tr_models_duncan(tr_models_glm(ex("InsectSprays"), "count", "spray"), "spray"),
               class = "tr_models_error_not_applicable")
  expect_error(tr_models_duncan(milho_dbc(), "variedade"), class = "tr_models_error_unknown_column")
})

# Grupos de referência tirados do pacote `ScottKnott` 1.4-0 (`SK()` com
# `sig.level = 0.05`), fixos aqui para o teste não depender dele. CRD1 e RCBD
# são os exemplos documentados do pacote (`?SK`), com os dados copiados.
test_that("Scott-Knott: mesmos grupos do pacote ScottKnott (DIC e DBC)", {
  crd1 <- data.frame(x = factor(rep(c("tr-1", "tr-2", "tr-3", "tr-4"), each = 6)),
                     y = c(58.81, 50.78, 49.32, 55.61, 49.47, 48.11, 61.98, 55.64, 65.13, 59.82, 54.67, 61.67,
                           59.52, 47.1, 44.19, 49.73, 62.81, 61.31, 45.74, 31.14, 32.84, 48.03, 39.38, 44.02))
  s <- tr_models_scott_knott(tr_models_anova_dic(crd1, "y", "x"), "x")
  expect_equal(s$tabela$grupo, c("a", "a", "a", "b"))
  rcbd <- data.frame(tra = factor(rep(LETTERS[1:5], each = 4)), blk = factor(rep(1:4, 5)),
                     y = c(143.17, 146.56, 143.51, 138.49, 138.75, 137.88, 146.42, 131.25, 139.86, 132.88,
                           136.74, 144.78, 151.4, 135.93, 137.16, 137.09, 154.3, 166.33, 152.49, 148.36))
  s <- tr_models_scott_knott(tr_models_anova_dbc(rcbd, "y", "tra", "blk"), "tra")
  expect_equal(s$tabela$grupo, c("b", "b", "b", "b", "a"))
  # Três grupos num DBC, e o DIC do InsectSprays.
  s <- tr_models_scott_knott(milho_dbc(), "hibrido")
  expect_equal(s$tabela$grupo, c("c", "c", "a", "c", "b"))
  expect_null(s$grade)
  expect_match(s$nota, "Scott-Knott a 5%", fixed = TRUE)
  expect_equal(s$tabela$gl, rep(12, 5))
  d <- tr_models_scott_knott(tr_models_anova_dic(ex("InsectSprays"), "count", "spray"), "spray")
  expect_equal(d$tabela$grupo, c("a", "a", "b", "b", "b", "a"))
  expect_s3_class(tr_models_plot_means(s), "ggplot")
})

test_that("Scott-Knott: a confiança muda o corte; erro certo na subdividida; recusas", {
  # O alfa sai da confiança: a 99,9% nenhum corte do milho passa, e as cinco
  # médias ficam num grupo só.
  g999 <- tr_models_scott_knott(milho_dbc(), "hibrido", confianca = 0.999)
  expect_equal(unique(g999$tabela$grupo), "a")
  expect_match(g999$nota, "0,1%", fixed = TRUE)
  # Com as médias bem separadas, cada uma vira o seu grupo.
  expect_equal(.tr_models_sk_grupos(c(1, 50, 100), 1, 20, 4, 0.05), c(3L, 2L, 1L))
  expect_equal(.tr_models_sk_grupos(c(10, 10.01, 10.02), 1, 20, 4, 0.05), c(1L, 1L, 1L))
  sp <- tr_models_anova_split_plot(ex("aveia"), "producao", "variedade", "nitrogenio", "bloco")
  q <- tr_models_anova_table(sp)$tabela
  expect_equal(unique(tr_models_scott_knott(sp, "nitrogenio")$tabela$gl), q$gl[q$termo == "Resíduo (b)"])
  expect_error(tr_models_scott_knott(tr_models_glm(ex("InsectSprays"), "count", "spray"), "spray"),
               class = "tr_models_error_not_applicable")
})

test_that("o eixo do gráfico de médias diz a origem e o nível real do intervalo", {
  m <- milho_dbc()
  eixo <- function(e) tr_models_plot_means(e)$labels$y
  # Agrupamentos do agricolae e o Scott-Knott usam as médias da tabela.
  expect_equal(eixo(tr_models_duncan(m, "hibrido", confianca = 0.9)), "producao (média e IC 90%)")
  expect_equal(eixo(tr_models_scott_knott(m, "hibrido")), "producao (média e IC 95%)")
  expect_equal(eixo(tr_models_waller_duncan(m, "hibrido")), "producao (média e IC 95%)")
  # O emmeans dá médias ajustadas, no nível pedido.
  expect_equal(eixo(tr_models_emmeans(m, "hibrido", confianca = 0.99)), "producao (média ajustada e IC 99%)")
})

test_that("Scott-Knott desbalanceado: s² = média de QM/rᵢ do grupo, como o pacote ScottKnott", {
  # DIC 6 × (3, 5, 4, 5, 5, 5). Referência: ScottKnott::SK(aov(y ~ t)) dá
  # F a, E b, C e D c, B d, A e. Com uma média harmônica única de r, E e F
  # ficavam juntos (lambda 4,95 contra 5,57 do pacote).
  d <- data.frame(t = rep(LETTERS[1:6], c(3, 5, 4, 5, 5, 5)),
                  y = c(9.715, 8.688, 9.609, 10.098, 11.851, 11.091, 10.601, 11.431,
                        11.737, 12.367, 13.707, 12.724, 12.781, 10.732, 12.618, 12.466,
                        11.4, 15.076, 15.159, 15.544, 15.705, 15.319, 16.309, 15.969,
                        16.353, 16.461, 15.901))
  s <- tr_models_scott_knott(tr_models_anova_dic(d, "y", "t"), "t")
  expect_equal(s$tabela$grupo, c("e", "d", "c", "c", "b", "a"))
})
