# Oráculos do ajuste. Cada um tem um papel diferente, e o que NÃO provam está
# dito aqui, porque um oráculo fraco com nome forte é pior que nenhum.
#
# 1. RELAÇÃO FECHADA do alcance prático, resolvendo gamma(h) = 0,95 * patamar
#    numericamente, em vez de confiar na constante tabelada. Vale para quem só
#    se aproxima do patamar (exponencial, gaussiano, Matérn).
#    O ESFÉRICO NÃO ENTRA nessa conta: ele ATINGE o patamar exatamente em phi
#    (medido: a raiz dos 95% cai em 0,8114 phi, não em phi), e o critério dos
#    95% só faz sentido para modelos que nunca chegam lá. O alcance prático do
#    esférico É phi, por definição do modelo.
# 2. MINIMIZAÇÃO. O modelo ajustado tem de ficar mais perto do variograma
#    empírico, no critério declarado de cada método, do que parâmetros
#    deliberadamente perturbados. O `sqr` do bloco tem de ser esse mesmo critério
#    recalculado, nos três métodos (o `SSErr` do gstat não serve: reaplica os pesos
#    a cada iteração e tem escala diferente por método).
# 3. geoR::variofit(weights = "cressie"). CONCORDÂNCIA DE ORDEM DE GRANDEZA, não
#    equivalência. Medido sobre o MESMO variograma empírico (que bate exato
#    entre os dois pacotes), o `fit.method = 2` do gstat e o `weights = "cressie"`
#    do geoR chegam a parâmetros até ~26% diferentes (milho_pr, alcance do
#    exponencial e do esférico): os dois minimizam critérios próximos mas não
#    idênticos, com otimizadores diferentes. Por isso a tolerância é 0,35 e o
#    teste existe para pegar erro grosseiro (parâmetro trocado, escala errada,
#    modelo errado), não para provar igualdade numérica.
# 4. Campo simulado de verdade conhecida.

gama_modelo <- function(h, pepita, contrib, alcance, vgm_id, kappa = 0.5) {
  gstat::variogramLine(gstat::vgm(psill = contrib, model = vgm_id, range = alcance,
                                  nugget = pepita, kappa = kappa),
                       dist_vector = h)$gamma
}

test_that("o alcance prático resolve gamma(h) = 0,95 do patamar (exponencial e gaussiano)", {
  # Dois conjuntos, com alcances diferentes: a relação é uma constante, e um
  # único conjunto não distingue constante certa de errada que coincida ali.
  for (ds in c("milho_pr", "cafe_mg")) {
    v <- tr_spatial_variogram(tr_spatial_example(ds))
    for (f in c("exponencial", "gaussiano")) {
      m <- tr_spatial_variogram_fit(v, familia = f)
      gama <- function(h) {
        switch(f,
          exponencial = m$pepita + m$contribuicao * (1 - exp(-h / m$alcance)),
          gaussiano   = m$pepita + m$contribuicao * (1 - exp(-(h / m$alcance)^2)))
      }
      alvo <- m$pepita + 0.95 * m$contribuicao
      raiz <- stats::uniroot(function(h) gama(h) - alvo,
                             interval = c(1e-9, 1000 * m$alcance), tol = 1e-10)$root
      # Tolerância 2e-3 e não 1e-3: 3 e sqrt(3) são os arredondamentos
      # convencionais (geoR, gstat, a literatura), e as raízes exatas dos 95% são
      # -log(0,05)=2,99573 e sqrt(-log(0,05))=1,73052. Medido: o erro relativo
      # da convenção é 1,4e-3 no exponencial e 7,4e-4 no gaussiano. Uma constante
      # errada (por exemplo 2 ou 1,5) erra por dezenas de por cento.
      expect_equal(m$alcance_pratico, raiz, tolerance = 2e-3, info = paste(ds, f))
    }
  }
})

test_that("o alcance prático do Matérn resolve 0,95 do patamar em kappas distintos", {
  # Contra `gstat::variogramLine`, que é independente da fórmula do bloco. Em
  # kappa = 0,5 o Matérn é o exponencial e qualquer constante de escala errada
  # (como o sqrt(2 kappa) da parametrização de Stein, que o brief trazia)
  # passa despercebida: por isso 0,3, 1, 2,5 e 10, onde a diferença aparece.
  for (k in c(0.3, 0.5, 1, 2.5, 10)) {
    for (a in c(1, 7, 12345)) {
      pr <- .tr_spatial_alcance_pratico("matern", a, k)
      g <- gama_modelo(pr, 0, 1, a, "Mat", k)
      expect_equal(g, 0.95, tolerance = 1e-6, info = paste("kappa", k, "alcance", a))
    }
  }
})

test_that("o alcance prático fechado vale para vários alcances, não só um", {
  for (a in c(0.5, 7, 12345)) {
    expect_equal(.tr_spatial_alcance_pratico("esferico", a), a)
    expect_equal(.tr_spatial_alcance_pratico("exponencial", a), 3 * a)
    expect_equal(.tr_spatial_alcance_pratico("gaussiano", a), sqrt(3) * a)
  }
})

test_that("o esférico atinge o patamar em phi, e é por isso que o critério dos 95% não vale", {
  v <- tr_spatial_variogram(tr_spatial_example("milho_pr"))
  m <- tr_spatial_variogram_fit(v, familia = "esferico")
  expect_equal(m$alcance_pratico, m$alcance)
  g <- gama_modelo(c(m$alcance, 2 * m$alcance), m$pepita, m$contribuicao, m$alcance, "Sph")
  expect_equal(g, rep(m$patamar, 2), tolerance = 1e-9)
  # A raiz dos 95% fica ANTES de phi (medido: ~0,81 phi), logo não é o alcance.
  raiz <- stats::uniroot(function(h) {
    gama_modelo(h, m$pepita, m$contribuicao, m$alcance, "Sph") - (m$pepita + 0.95 * m$contribuicao)
  }, interval = c(1e-6, m$alcance))$root
  expect_lt(raiz / m$alcance, 0.9)
})

test_that("o ajuste minimiza o critério declarado: perturbar os parâmetros piora", {
  vgm_id <- c(esferico = "Sph", exponencial = "Exp", gaussiano = "Gau")
  for (ds in c("milho_pr", "cafe_mg")) {
    v <- tr_spatial_variogram(tr_spatial_example(ds)); t <- v$tabela
    for (f in names(vgm_id)) {
      for (me in c("WLS-Cressie", "WLS-np", "OLS")) {
        # milho_pr exponencial com WLS-np é um ajuste não identificado (alcance
        # prático ~17x a maior distância) que o bloco recusa; ver test-ajuste.R.
        if (ds == "milho_pr" && f == "exponencial" && me == "WLS-np") {
          expect_error(tr_spatial_variogram_fit(v, familia = f, metodo = me),
                       class = "tr_spatial_error_bad_fit")
          next
        }
        m <- tr_spatial_variogram_fit(v, familia = f, metodo = me)
        crit <- function(p, c, a) {
          g <- gama_modelo(t$u, p, c, a, vgm_id[[f]])
          w <- switch(me, `WLS-Cressie` = t$np / g^2, `WLS-np` = t$np, OLS = 1)
          sum(w * (t$gamma - g)^2)
        }
        ajustado <- crit(m$pepita, m$contribuicao, m$alcance)
        info <- paste(ds, f, me)
        expect_lt(ajustado, crit(m$pepita, m$contribuicao, 2 * m$alcance), label = info)
        expect_lt(ajustado, crit(m$pepita, m$contribuicao, m$alcance / 2), label = info)
        expect_lt(ajustado, crit(m$pepita, 2 * m$contribuicao, m$alcance), label = info)
        expect_equal(ajustado, m$sqr, tolerance = 1e-9, label = info)
      }
    }
  }
})

test_that("WLS-Cressie concorda em ordem de grandeza com geoR::variofit (tolerância 0,35)", {
  skip_if_not_installed("geoR")
  # Ver o cabeçalho: concordância de ordem de grandeza, NÃO equivalência. O
  # chute do geoR é independente do ajuste do bloco (a heurística documentada),
  # para o geoR não herdar o ponto de chegada do gstat.
  casos <- c(exponencial = "exponential", esferico = "spherical", gaussiano = "gaussian")
  for (ds in c("milho_pr", "cafe_mg")) {
    v <- tr_spatial_variogram(tr_spatial_example(ds)); t <- v$tabela
    # Variograma montado à mão com as MESMAS classes e médias, para que a única
    # diferença em teste seja o algoritmo de ajuste.
    vg <- list(u = t$u, v = t$gamma, n = t$np, max.dist = max(t$u),
               output.type = "bin", estimator.type = "classical",
               direction = "omnidirectional")
    class(vg) <- "variogram"
    for (f in names(casos)) {
      m <- tr_spatial_variogram_fit(v, familia = f, metodo = "WLS-Cressie")
      o <- suppressWarnings(geoR::variofit(
        vg, ini.cov.pars = c(max(t$gamma) - t$gamma[[1]], max(t$u) / 3),
        nugget = t$gamma[[1]], cov.model = casos[[f]], weights = "cressie",
        messages = FALSE))
      info <- paste(ds, f)
      expect_equal(m$pepita, unname(o$nugget), tolerance = 0.35, info = info)
      expect_equal(m$contribuicao, unname(o$cov.pars[[1]]), tolerance = 0.35, info = info)
      expect_equal(m$alcance, unname(o$cov.pars[[2]]), tolerance = 0.35, info = info)
    }
  }
})

test_that("recupera os parâmetros de um campo simulado de verdade conhecida", {
  # Oráculo sintético: campo gaussiano com pepita 1, contribuição 9 e alcance 50
  # CONHECIDOS. Intervalo, não ponto: uma realização só não identifica o
  # parâmetro. O desenho importa: o domínio é 20x o alcance e o corte, 5x. Com o
  # corte curto (~2x o alcance) o alcance do exponencial é inidentificável (o
  # desenho antigo devolveu 65,6 contra 25). Semente 2, fixa.
  if (exists(".Random.seed", envir = globalenv())) {
    semente <- get(".Random.seed", envir = globalenv())
    on.exit(assign(".Random.seed", semente, envir = globalenv()), add = TRUE)
  }
  set.seed(2)
  n <- 400L; lado <- 1000
  d <- data.frame(x = runif(n, 0, lado), y = runif(n, 0, lado))
  dm <- as.matrix(stats::dist(d[, c("x", "y")]))
  d$z <- as.numeric(t(chol(9 * exp(-dm / 50) + diag(1, n))) %*% stats::rnorm(n))
  p <- tr_spatial_coordinates(d, "x", "y", "z")
  m <- tr_spatial_variogram_fit(
    tr_spatial_variogram(p, dist_max = 250, n_classes = 15L),
    familia = "exponencial", metodo = "WLS-Cressie")
  expect_gt(m$alcance, 0.7 * 50);       expect_lt(m$alcance, 1.4 * 50)
  expect_gt(m$contribuicao, 0.6 * 9);   expect_lt(m$contribuicao, 1.5 * 9)
  expect_lt(m$pepita, 4 * 1)
})
