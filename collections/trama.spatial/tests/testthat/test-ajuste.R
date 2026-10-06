# Distâncias em metros, variância em (kg/ha)^2: os valores absolutos são da
# ordem de 1e5..1e7, então nada aqui compara com número fixo.
vp <- function(ds = "milho_pr", ...) tr_spatial_variogram(tr_spatial_example(ds), ...)

test_that("o ajuste sai com os parâmetros do contrato e todos positivos", {
  m <- tr_spatial_variogram_fit(vp())
  expect_s3_class(m, "tr_spatial_model")
  expect_equal(m$familia, "esferico")
  expect_equal(m$metodo, "WLS-Cressie")
  expect_gte(m$pepita, 0)
  expect_gt(m$contribuicao, 0)
  expect_gt(m$alcance, 0)
  expect_equal(m$patamar, m$pepita + m$contribuicao)
  expect_equal(m$grau_dependencia, m$pepita / m$patamar)
  expect_true(m$grau_dependencia >= 0 && m$grau_dependencia <= 1)
  expect_s3_class(m$variograma, "tr_spatial_variogram")
  expect_true(is.na(m$kappa))
})

test_that("as quatro famílias ajustam", {
  v <- vp()
  for (f in c("esferico", "exponencial", "gaussiano", "matern")) {
    m <- tr_spatial_variogram_fit(v, familia = f)
    expect_equal(m$familia, f)
    expect_gt(m$alcance, 0)
    expect_gt(m$alcance_pratico, 0)
  }
})

test_that("o kappa do Matérn é de fato usado, e kappa 0,5 reproduz o exponencial", {
  v <- vp()
  m5 <- tr_spatial_variogram_fit(v, familia = "matern", kappa = 0.5)
  ex <- tr_spatial_variogram_fit(v, familia = "exponencial")
  expect_equal(m5$alcance, ex$alcance, tolerance = 1e-6)
  expect_equal(m5$contribuicao, ex$contribuicao, tolerance = 1e-6)
  m2 <- tr_spatial_variogram_fit(v, familia = "matern", kappa = 2)
  expect_equal(m2$kappa, 2)
  expect_gt(abs(m2$alcance / m5$alcance - 1), 0.05)
})

# A confusão clássica da disciplina: phi NÃO é o alcance que se lê no gráfico.
test_that("o alcance prático segue a relação fechada de cada família", {
  v <- vp()
  e <- tr_spatial_variogram_fit(v, familia = "esferico")
  expect_equal(e$alcance_pratico, e$alcance, tolerance = 1e-9)
  x <- tr_spatial_variogram_fit(v, familia = "exponencial")
  expect_equal(x$alcance_pratico, 3 * x$alcance, tolerance = 1e-9)
  g <- tr_spatial_variogram_fit(v, familia = "gaussiano")
  expect_equal(g$alcance_pratico, sqrt(3) * g$alcance, tolerance = 1e-9)
})

test_that("pepita fixa em zero sai zero, e fixa num valor mantém o valor", {
  v <- vp()
  m <- tr_spatial_variogram_fit(v, pepita_fixa = TRUE, pepita_inicial = 0)
  expect_equal(m$pepita, 0, tolerance = 1e-12)
  expect_gt(m$contribuicao, 0)
  # A contribuição continua sendo estimada: `fit.sills = FALSE` a travaria junto.
  livre <- tr_spatial_variogram_fit(v)
  expect_gt(abs(m$contribuicao / livre$contribuicao - 1), 1e-3)
  p <- 3e5
  m2 <- tr_spatial_variogram_fit(v, pepita_fixa = TRUE, pepita_inicial = p)
  expect_equal(m2$pepita, p, tolerance = 1e-12)
})

test_that("os métodos clássicos rodam e dão parâmetros da mesma ordem", {
  v <- vp()
  ps <- vapply(c("WLS-Cressie", "WLS-np", "OLS"), function(mt) {
    tr_spatial_variogram_fit(v, metodo = mt)$alcance
  }, 0)
  expect_true(all(ps > 0))
  expect_lt(max(ps) / min(ps), 5)
})

test_that("o sqr é a soma de quadrados ponderada pelo método, recalculada pelo bloco", {
  v <- vp(); t <- v$tabela
  for (me in c("WLS-Cressie", "WLS-np", "OLS")) {
    m <- tr_spatial_variogram_fit(v, metodo = me)
    g <- gstat::variogramLine(.tr_spatial_vgm_model(m), dist_vector = t$u)$gamma
    w <- switch(me, `WLS-Cressie` = t$np / g^2, `WLS-np` = t$np, OLS = 1)
    expect_equal(m$sqr, sum(w * (t$gamma - g)^2), tolerance = 1e-9, info = me)
  }
})

test_that("o ajuste herda a nota do variograma", {
  v <- vp(pares_min = 400L)
  expect_match(v$nota, "ficaram de fora")
  expect_match(tr_spatial_variogram_fit(v)$nota, "ficaram de fora")
})

# O gstat devolve sill negativo CALADO.
test_that("ajuste com pepita ou contribuição negativa vira erro nomeado", {
  v <- vp()
  expect_error(
    tr_spatial_variogram_fit(v, familia = "gaussiano", pepita_inicial = -50,
                             contribuicao_inicial = -1, alcance_inicial = 1e-6),
    class = "tr_spatial_error_bad_fit")
})

test_that("a pepita negativa sozinha é recusada, e a mensagem diz como sair", {
  # milho_se, exponencial, WLS-np: o gstat devolve pepita negativa com sill e
  # alcance válidos, partindo de qualquer ponto da grade. Apagar a guarda da
  # pepita deixaria o modelo passar (a pepita seria cortada em zero, calada).
  v <- vp("milho_se")
  e <- expect_error(tr_spatial_variogram_fit(v, familia = "exponencial", metodo = "WLS-np"),
                    class = "tr_spatial_error_bad_fit")
  expect_match(conditionMessage(e), "pepita negativa")
  expect_match(conditionMessage(e), "pepita_fixa = TRUE", fixed = TRUE)
  # E o caminho indicado pela mensagem funciona.
  m <- tr_spatial_variogram_fit(v, familia = "exponencial", metodo = "WLS-np",
                                pepita_fixa = TRUE, pepita_inicial = 0)
  expect_equal(m$pepita, 0)
  expect_gt(m$contribuicao, 0)
})

test_that("contribuição não positiva sozinha é recusada", {
  # milho_se, exponencial, WLS-np, com os três chutes dados (então uma partida
  # só): o gstat devolve contribuição -5384 com alcance 129 km, plausível.
  # Apagar a guarda da contribuição deixaria este modelo passar.
  e <- expect_error(
    tr_spatial_variogram_fit(vp("milho_se"), familia = "exponencial", metodo = "WLS-np",
                             pepita_inicial = 2e5, contribuicao_inicial = 1e6,
                             alcance_inicial = 8e4),
    class = "tr_spatial_error_bad_fit")
  expect_match(conditionMessage(e), "contribuição não positiva")
  expect_no_match(conditionMessage(e), "alcance não positivo|fora da escala|pepita negativa")
})

test_that("alcance fora da escala dos dados é recusado", {
  # milho_pr, Matérn kappa 0,3: o gstat devolve alcance de ~14.500 km para uma
  # área de ~229 km, e sill e pepita positivos. Não é um modelo usável.
  e <- expect_error(tr_spatial_variogram_fit(vp(), familia = "matern", kappa = 0.3),
                    class = "tr_spatial_error_bad_fit")
  expect_match(conditionMessage(e), "fora da escala")
})

test_that("o exemplo milho_se ajusta com família e método padrão", {
  # O exemplo que a coleção entrega tem de funcionar na primeira tentativa. O
  # variograma é não monótono na origem, e o chute "pepita = gamma da 1a
  # classe" sozinho falhava: por isso as várias partidas.
  m <- tr_spatial_variogram_fit(vp("milho_se"))
  expect_gt(m$contribuicao, 0)
  v <- m$variograma$tabela
  expect_gte(m$alcance_pratico, min(v$u))
  expect_no_match(m$nota, "não chega a um patamar")
})

test_that("o ajuste é determinístico: mesma entrada, mesmo modelo, qualquer semente", {
  # A entrada importa: milho_pr, exponencial, WLS-Cressie tem 6 ajustes válidos
  # distintos, um por partida da grade. Num caso com uma só partida válida (como
  # milho_se esférico) a ordem das partidas não muda o resultado e o teste não
  # testa nada. Duas sementes diferentes pegam código que sorteie ou embaralhe
  # a ordem e devolva, por exemplo, a última partida válida. (Escolher por
  # `which.min` é equivalente por construção a qualquer ordem de busca, salvo
  # empate exato; nenhum teste "pega" isso, e não precisa.)
  v <- vp()
  set.seed(1); a <- tr_spatial_variogram_fit(v, familia = "exponencial")
  set.seed(987654); b <- tr_spatial_variogram_fit(v, familia = "exponencial")
  expect_identical(a$pepita, b$pepita)
  expect_identical(a$alcance, b$alcance)
  expect_identical(a$sqr, b$sqr)
})

test_that("nunca sai um modelo silenciosamente ruim: ou erro nomeado, ou ajuste válido", {
  # Propriedade sobre o exemplo pequeno e difícil (68 pontos), em toda
  # combinação de família e método. O que NÃO pode acontecer é devolver um
  # modelo singular, com sinal errado ou com alcance fora da escala dos dados.
  v <- vp("milho_se"); u <- v$tabela$u
  for (f in c("esferico", "exponencial", "gaussiano", "matern")) {
    for (me in c("WLS-Cressie", "WLS-np", "OLS")) {
      info <- paste(f, me)
      m <- tryCatch(tr_spatial_variogram_fit(v, familia = f, metodo = me),
                    tr_spatial_error = function(e) e)
      if (inherits(m, "tr_spatial_error")) {
        expect_true(inherits(m, "tr_spatial_error_bad_fit") ||
                      inherits(m, "tr_spatial_error_no_convergence"), info = info)
      } else {
        expect_s3_class(m, "tr_spatial_model")
        expect_gte(m$pepita, 0); expect_gt(m$contribuicao, 0)
        expect_gte(m$alcance_pratico, min(u))
        expect_lte(m$alcance_pratico, .TR_SPATIAL_PRATICO_MAX * max(u))
        expect_true(is.finite(m$sqr), info = info)
      }
    }
  }
})

test_that("família, método ou kappa inválido é erro de opção", {
  v <- vp()
  expect_error(tr_spatial_variogram_fit(v, familia = "batata"),
               class = "tr_spatial_error_bad_option")
  expect_error(tr_spatial_variogram_fit(v, metodo = "chute"),
               class = "tr_spatial_error_bad_option")
  for (k in c(-1, 0.05, 11)) {
    expect_error(tr_spatial_variogram_fit(v, familia = "matern", kappa = k),
                 class = "tr_spatial_error_bad_option")
  }
})

test_that("entrada que não é variograma é erro nomeado", {
  expect_error(tr_spatial_variogram_fit(list(a = 1)),
               class = "tr_spatial_error_not_a_variogram")
})

test_that("o adaptador para data/table dá uma linha por parâmetro, numérico, e distingue dois ajustes", {
  t <- .tr_spatial_modelo_tabela(tr_spatial_variogram_fit(vp()))
  expect_equal(names(t), c("parametro", "valor", "familia", "metodo", "kappa"))
  expect_equal(unique(t$familia), "esferico")
  expect_true(all(c("pepita", "contribuicao", "alcance", "alcance_pratico") %in% t$parametro))
  expect_type(t$valor, "double")
  # O uso que o adaptador existe para servir: empilhar duas famílias.
  v <- vp()
  dois <- rbind(t, .tr_spatial_modelo_tabela(tr_spatial_variogram_fit(v, familia = "exponencial")))
  expect_equal(nrow(unique(dois[dois$parametro == "alcance", c("familia", "metodo")])), 2L)
  mt <- .tr_spatial_modelo_tabela(tr_spatial_variogram_fit(v, familia = "matern", kappa = 2))
  expect_equal(unique(mt$kappa), 2)
})

test_that("o preview do tipo aguenta um modelo montado à mão, sem variograma", {
  m <- tr_spatial_variogram_fit(vp())
  m$variograma <- NULL
  p <- .tr_spatial_modelo_preview(m)
  expect_length(p$classes, 0L)
  expect_equal(p$alcance, m$alcance)
})

test_that("o tipo spatial/model recusa o que não é modelo e aceita o modelo", {
  ty <- spatial_model_type()
  expect_error(ty$store(list(a = 1), tempfile()), class = "tr_spatial_error_not_a_model")
  m <- tr_spatial_variogram_fit(vp())
  f <- tempfile(fileext = ".rds")
  ty$store(m, f)
  expect_equal(ty$restore(f)$alcance, m$alcance)
})

test_that("o vgm devolvido à krigagem leva os mesmos parâmetros", {
  m <- tr_spatial_variogram_fit(vp(), familia = "exponencial")
  g <- .tr_spatial_vgm_model(m)
  expect_equal(as.character(g$model), c("Nug", "Exp"))
  expect_equal(g$psill, c(m$pepita, m$contribuicao))
  expect_equal(g$range[[2]], m$alcance)
})

test_that("o nó roda no motor", {
  reg <- spatial_registry()
  fl <- trama::tr_flow(reg) |>
    trama::tr_add("p", "spatial/example", dataset = "milho_pr") |>
    trama::tr_add("v", "spatial/variogram", from = "p") |>
    trama::tr_add("m", "spatial/variogram_fit", familia = "exponencial", from = "v")
  m <- rodar(fl, "m")
  expect_s3_class(m, "tr_spatial_model")
  expect_equal(m$familia, "exponencial")
})
# ---- guardas que só mordem em situações montadas -------------------------------

test_that("fit singular numa partida só é recusado como não convergência", {
  # Com a grade de partidas outra partida sempre ganha; a guarda do `singular`
  # só morde quando as três iniciais são dadas (uma partida só). Estes números
  # estão com TODOS os dígitos de propósito: com 7 algarismos significativos o
  # gstat converge e o caso desaparece, e o teste deixaria de vigiar o que vigia.
  # NÃO arredonde.
  expect_error(
    tr_spatial_variogram_fit(vp("milho_se"),
                             pepita_inicial = 1267481.3676470588,
                             contribuicao_inicial = 1610110.0646837684,
                             alcance_inicial = 14664.784671143658),
    class = "tr_spatial_error_no_convergence")
})

test_that("o aviso de não convergência do gstat é lido, não engolido", {
  # milho_se, esférico, OLS: o gstat avisa "No convergence" e devolve um ajuste
  # com alcance de 15 km (e pepita negativa). Sem ler o aviso ele passaria.
  expect_error(tr_spatial_variogram_fit(vp("milho_se"), familia = "esferico", metodo = "OLS"),
               class = "tr_spatial_error_bad_fit")
})

test_that("alcance prático ABAIXO da menor distância é recusado", {
  # Variogramas sintéticos, gerados de uma exponencial com phi muito menor que a
  # primeira classe (10 km): a estrutura espacial está toda dentro da primeira
  # distância e o ajuste não a enxerga. O gstat devolve sinais válidos e
  # alcance prático menor que a 1a distância (gaussiano 9,1 km, esférico 2,0 km).
  u <- seq(10000, 100000, length.out = 10)
  sintetico <- function(phi) {
    w <- vp()
    g <- 1e5 + 9e5 * (1 - exp(-u / phi)) * (1 + c(0.01, -0.01)[1 + (seq_along(u) %% 2)])
    w$tabela <- tibble::tibble(u = u, gamma = g, np = 500L, direcao = NA_real_)
    w
  }
  e <- expect_error(tr_spatial_variogram_fit(sintetico(1500), familia = "gaussiano"),
                    class = "tr_spatial_error_bad_fit")
  expect_match(conditionMessage(e), "fora da escala")
  e <- expect_error(tr_spatial_variogram_fit(sintetico(3000), familia = "esferico"),
                    class = "tr_spatial_error_bad_fit")
  expect_match(conditionMessage(e), "fora da escala")
})

test_that("a escala dos dados se confere no alcance prático, não em phi", {
  # milho_pr, Matérn kappa 2,5, pepita fixa em zero: phi = 11,5 km, abaixo da 1a distância
  # (12,0 km), mas o alcance prático é ~5,9x phi, bem dentro da escala. O limite em
  # phi recusava este ajuste legítimo: era a confusão phi-versus-alcance prático
  # que o campo `alcance_pratico` existe para evitar.
  v <- vp()
  m <- tr_spatial_variogram_fit(v, familia = "matern", kappa = 2.5,
                                pepita_fixa = TRUE, pepita_inicial = 0)
  expect_lt(m$alcance, min(v$tabela$u))
  expect_gt(m$alcance_pratico, min(v$tabela$u))
})

test_that("alcance prático acima de 10x a maior distância é recusado", {
  # Os ajustes não identificados: o variograma não chega a patamar e o gstat
  # extrapola. milho_pr exponencial e Matérn 0,5 com WLS-np: ~17x (3.900 km numa
  # área de 229 km); cafe_mg Matérn 0,3 com OLS: ~13,6x.
  for (arg in list(list(familia = "exponencial", metodo = "WLS-np"),
                   list(familia = "matern", metodo = "WLS-np", kappa = 0.5))) {
    e <- expect_error(do.call(tr_spatial_variogram_fit, c(list(vp()), arg)),
                      class = "tr_spatial_error_bad_fit")
    expect_match(conditionMessage(e), "fora da escala")
  }
  expect_error(tr_spatial_variogram_fit(vp("cafe_mg"), familia = "matern", metodo = "OLS",
                                        kappa = 0.3),
               class = "tr_spatial_error_bad_fit")
})

test_that("entre 2x e 10x o ajuste vale, e a nota avisa que é extrapolação", {
  # milho_pr exponencial com WLS-Cressie (~6x): o caso de livro de variograma com
  # tendência. Recusar seria errado; calar também.
  m <- tr_spatial_variogram_fit(vp(), familia = "exponencial")
  L <- max(m$variograma$tabela$u)
  expect_gt(m$alcance_pratico, 2 * L)
  expect_lt(m$alcance_pratico, .TR_SPATIAL_PRATICO_MAX * L)
  expect_match(m$nota, "não chega a um patamar")
  expect_match(m$nota, "extrapolação")
  # Ajuste que chega ao patamar dentro das distâncias não leva o aviso.
  expect_no_match(tr_spatial_variogram_fit(vp(), familia = "esferico")$nota, "patamar")
  expect_no_match(tr_spatial_variogram_fit(vp("milho_se"))$nota, "patamar")
})


test_that("alcance prático indeterminado (NA) é recusa, não aprovação", {
  # O Matérn resolve o prático por `uniroot`, que pode falhar e devolver NA. Com
  # o prático trocado por NA, nenhum ajuste pode passar.
  local_mocked_bindings(.tr_spatial_alcance_pratico = function(...) NA_real_,
                         .package = "trama.spatial")
  e <- expect_error(tr_spatial_variogram_fit(vp()), class = "tr_spatial_error_bad_fit")
  expect_match(conditionMessage(e), "indeterminado")
})
