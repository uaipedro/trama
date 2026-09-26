# Estrutura de cada delineamento, com oráculo: um pacote de referência
# (agricolae, FrF2, rsm, crossdes) ou a propriedade combinatória que define o
# delineamento. Tolerância: contagens são exatas; α e pontos do composto
# central, 1e-12.

test_that("DIC: cada tratamento r vezes, como no agricolae::design.crd", {
  p <- ds("dic", "trat: A, B, C, D, E", repeticoes = 3L, .seed = 7L)
  expect_equal(as.vector(table(p$unidades$trat)), rep(3L, 5))
  skip_if_not_installed("agricolae")
  o <- agricolae::design.crd(LETTERS[1:5], r = 3, seed = 7)$book
  expect_equal(nrow(o), nrow(p$unidades))
  expect_equal(as.vector(table(o[[3]])), as.vector(table(p$unidades$trat)))
})

test_that("DBC: cada tratamento uma vez por bloco (agricolae::design.rcbd dá o mesmo)", {
  p <- ds("dbc", "hibrido: 1, 2, 3, 4, 5", repeticoes = 6L, .seed = 3L)
  expect_true(all(contagem(p$unidades, "hibrido", "bloco") == 1L))
  skip_if_not_installed("agricolae")
  o <- agricolae::design.rcbd(1:5, r = 6, seed = 3)$book
  expect_true(all(table(o$block, o[[3]]) == 1L))
  expect_equal(p$analise$no, "models/anova_dbc")
})

test_that("DQL: cada tratamento uma vez por linha e por coluna", {
  for (sem in 1:5) {
    p <- ds("dql", "racao: A, B, C, D, E", .seed = sem)
    expect_true(all(contagem(p$unidades, "racao", "linha") == 1L))
    expect_true(all(contagem(p$unidades, "racao", "coluna") == 1L))
  }
  expect_error(ds("dql", "t: A, B"), class = "tr_experiments_error_no_residual")
  p <- ds("dql", "t: 8", .seed = 3L)
  expect_true(all(contagem(p$unidades, "t", "linha") == 1L))
  expect_true(all(contagem(p$unidades, "t", "coluna") == 1L))
  expect_length(p$avisos, 1L)
})

test_that("DQL: número de quadrados padrão igual à OEIS A000315", {
  n <- vapply(1:6, function(t) nrow(.tr_exp_padroes_latinos(t)), 0L)
  expect_equal(n, c(1L, 1L, 1L, 4L, 56L, 9408L))
  # Cada um é padrão (1ª linha e 1ª coluna em ordem) e latino, e não se repete.
  P <- .tr_exp_padroes_latinos(5)
  expect_equal(anyDuplicated(P), 0L)
  for (i in seq_len(nrow(P))) {
    M <- matrix(P[i, ], 5, byrow = TRUE)
    expect_true(all(M[1, ] == 1:5) && all(M[, 1] == 1:5) &&
                all(apply(M, 1, function(x) length(unique(x))) == 5L) &&
                all(apply(M, 2, function(x) length(unique(x))) == 5L))
  }
})

test_that("DQL 4 × 4: os 576 quadrados saem, com frequências de uniforme (qui-quadrado)", {
  # 576 = 4!·3!·4. Com 11520 sorteios, 20 esperados por quadrado; antes (só
  # permutações do cíclico) saíam 432 e a classe de Klein nunca.
  sorteia <- function(f, n) {
    trama.experiments:::.tr_exp_com_semente(20260925L, replicate(n, paste(f(), collapse = "")))
  }
  k <- sorteia(function() .tr_exp_quadrado_latino(4L), 11520L)
  tb <- table(k)
  expect_equal(length(tb), 576L)
  expect_gt(stats::chisq.test(as.vector(tb))$p.value, 0.001)
  # A cadeia de Jacobson & Matthews, usada de t = 7 em diante, conferida onde
  # dá para contar: 4 × 4, t³ = 64 passos.
  kj <- sorteia(function() .tr_exp_jm_latino(4L), 11520L)
  tj <- table(kj)
  expect_equal(length(tj), 576L)
  expect_gt(stats::chisq.test(as.vector(tj))$p.value, 0.001)
})

test_that("fatorial em DBC: cada combinação uma vez por bloco", {
  p <- ds("fatorial", "a: 1, 2; b: x, y, z", repeticoes = 4L, .seed = 2L)
  cmb <- paste(p$unidades$a, p$unidades$b)
  expect_true(all(table(p$unidades$bloco, cmb) == 1L))
  expect_equal(p$analise$params$fatores, "a, b")
})

test_that("confundimento: ABC constante no bloco, partição igual à do FrF2", {
  p <- ds("confundimento", "A: -, +; B: -, +; C: -, +; D: -, +", confundir = "ABC",
          repeticoes = 2L, .seed = 4L)
  u <- p$unidades
  x <- sapply(c("A", "B", "C", "D"), function(n) ifelse(u[[n]] == "+", 1, -1))
  abc <- x[, "A"] * x[, "B"] * x[, "C"]
  expect_true(all(tapply(abc, u$bloco, function(v) length(unique(v))) == 1L))
  expect_true(all(table(u$bloco) == 8L))
  skip_if_not_installed("FrF2")
  o <- suppressWarnings(FrF2::FrF2(16, 4, blocks = "ABC", randomize = FALSE, alias.block.2fis = TRUE))
  oo <- as.data.frame(o)
  chave <- function(d) apply(sapply(c("A", "B", "C", "D"), function(n) as.character(d[[n]])), 1, paste, collapse = "")
  norm <- function(k, b) sort(vapply(split(k, b), function(v) paste(sort(v), collapse = "|"), ""))
  ko <- chave(data.frame(sapply(oo[c("A", "B", "C", "D")], function(v) ifelse(as.character(v) == "1", "+", "-"))))
  r1 <- u$repeticao == "1"
  expect_equal(unname(norm(chave(u[r1, ]), droplevels(u$bloco[r1]))), unname(norm(ko, oo$Blocks)))
  expect_error(ds("confundimento", "A: -, +; B: -, +", confundir = "A"),
               class = "tr_experiments_error_bad_generator")
  q <- ds("confundimento", "A: -, +; B: -, +; C: -, +; D: -, +", confundir = "ABC; ABD")
  expect_setequal(q$extras$confundidos, c("ABC", "ABD", "CD"))
})

test_that("fracionado: relação de definição, resolução e matriz iguais às do FrF2", {
  p <- ds("fracionado", "A; B; C; D", geradores = "D = ABC", .seed = 9L)
  expect_equal(p$extras$relacao_definicao, "I = ABCD")
  expect_equal(p$extras$resolucao, 4L)
  u <- p$unidades
  expect_true(all(u$A * u$B * u$C * u$D == 1))
  q <- ds("fracionado", "A; B; C; D; E", geradores = "D = AB; E = AC")
  expect_equal(q$extras$relacao_definicao, "I = ABD = ACE = BCDE")
  expect_equal(q$extras$resolucao, 3L)
  skip_if_not_installed("FrF2")
  o <- as.data.frame(FrF2::FrF2(8, 4, generators = "ABC", randomize = FALSE))
  om <- sapply(o[c("A", "B", "C", "D")], function(v) as.numeric(as.character(v)))
  pm <- as.matrix(u[order(u$padrao), c("A", "B", "C", "D")])
  expect_equal(unname(pm), unname(om))
  o5 <- as.data.frame(FrF2::FrF2(8, 5, generators = c("AB", "AC"), randomize = FALSE))
  qm <- as.matrix(q$unidades[order(q$unidades$padrao), c("A", "B", "C", "D", "E")])
  expect_equal(unname(qm), unname(sapply(o5, function(v) as.numeric(as.character(v)))))
  expect_error(ds("fracionado", "A; B; C", geradores = "C = C"), class = "tr_experiments_error_bad_generator")
})

test_that("fracionado com níveis nomeados: coluna em −1/+1, nomes no metadado", {
  p <- ds("fracionado", "A: baixo, alto; B: x, y; C; D", geradores = "D = ABC")
  expect_true(is.numeric(p$unidades$A))
  expect_setequal(unique(p$unidades$A), c(-1, 1))
  expect_equal(p$extras$codificacao$menos1[1:2], c("baixo", "x"))
  expect_equal(p$extras$codificacao$mais1[1:2], c("alto", "y"))
  expect_equal(p$fatores$niveis[[which(p$fatores$nome == "A")]], c("baixo", "alto"))
})

test_that("composto central: pontos iguais aos do rsm::ccd (rotacional e face)", {
  skip_if_not_installed("rsm")
  for (k in 2:3) for (tipo in c("rotacional", "face")) {
    nomes <- paste0("x", seq_len(k))
    p <- ds("composto_central", paste(nomes, collapse = "; "), alfa = tipo, pontos_centrais = 3L)
    o <- as.data.frame(rsm::ccd(k, n0 = c(3, 0), alpha = if (tipo == "face") "faces" else "rotatable",
                                randomize = FALSE, oneblock = TRUE))
    chave <- function(d) sort(apply(round(as.matrix(d[nomes]), 10), 1, paste, collapse = ","))
    expect_equal(chave(p$unidades), chave(o), info = paste(k, tipo))
  }
  p <- ds("composto_central", "x1; x2")
  expect_equal(p$extras$alfa, sqrt(2), tolerance = 1e-12)
})

test_that("composto central com porção fatorial fracionada (k = 5, E = ABCD) igual ao rsm::ccd", {
  p <- ds("composto_central", "A; B; C; D; E", geradores = "E = ABCD", pontos_centrais = 4L)
  expect_equal(p$extras$alfa, 2, tolerance = 1e-12)
  expect_equal(p$extras$n_fatorial, 16L)
  expect_equal(nrow(p$unidades), 16L + 10L + 4L)
  expect_error(ds("composto_central", "A; B; C; D; E", geradores = "E = ABC"),
               class = "tr_experiments_error_bad_generator")
  skip_if_not_installed("rsm")
  o <- as.data.frame(rsm::ccd(~ A + B + C + D, E ~ A * B * C * D, n0 = c(4, 0), alpha = "rotatable",
                              randomize = FALSE, oneblock = TRUE))
  chave <- function(d) sort(apply(round(as.matrix(d[c("A", "B", "C", "D", "E")]), 10), 1, paste, collapse = ","))
  expect_equal(chave(p$unidades), chave(o))
})

test_that("parcela subdividida: A constante na parcela, B sorteado dentro dela", {
  p <- ds("parcela_subdividida", "irrig: b, a; var: A, B, C", repeticoes = 4L, .seed = 5L)
  u <- p$unidades
  expect_true(all(tapply(u$irrig, u$parcela, function(v) length(unique(v))) == 1L))
  expect_true(all(table(u$parcela, u$var) == 1L))
  expect_true(all(table(u$bloco, u$irrig) == 3L))
  # O sorteio da subparcela varia entre parcelas (não é a mesma ordem em todas).
  ordens <- tapply(as.character(u$var), u$parcela, paste, collapse = "")
  expect_gt(length(unique(ordens)), 1L)
  skip_if_not_installed("agricolae")
  o <- agricolae::design.split(1:2, 1:3, r = 4, serie = 0, seed = 5)$book
  expect_equal(nrow(o), nrow(u))
})

test_that("faixas: A constante na faixa horizontal, B na vertical, em cada bloco", {
  p <- ds("faixas", "a: 1, 2, 3; b: x, y", repeticoes = 3L, .seed = 1L)
  u <- p$unidades
  expect_true(all(tapply(u$a, interaction(u$bloco, u$faixa_linha), function(v) length(unique(v))) == 1L))
  expect_true(all(tapply(u$b, interaction(u$bloco, u$faixa_coluna), function(v) length(unique(v))) == 1L))
  expect_true(all(table(u$bloco, paste(u$a, u$b)) == 1L))
})

test_that("BIB: λ constante entre todos os pares, e os parâmetros do agricolae", {
  concorrencia <- function(u, t) {
    M <- table(u$bloco, u$t) > 0
    C <- crossprod(M * 1)
    C[upper.tri(C)]
  }
  p <- ds("bib", "t: 7", tamanho_bloco = 3L, .seed = 2L)
  expect_true(all(concorrencia(p$unidades) == 1))
  expect_equal(p$extras[c("b", "r", "lambda")], list(b = 7L, r = 3, lambda = 1))
  # b mínimo pelas condições necessárias (r, b inteiros, b ≥ t), que esses
  # BIBs atingem (Cochran & Cox 1957, cap. 11).
  casos <- list(c(t = 6, k = 3, b = 10, r = 5, l = 2), c(t = 9, k = 3, b = 12, r = 4, l = 1),
                c(t = 8, k = 4, b = 14, r = 7, l = 3), c(t = 10, k = 4, b = 15, r = 6, l = 2),
                c(t = 10, k = 6, b = 15, r = 9, l = 5))
  for (cs in casos) {
    q <- ds("bib", paste0("t: ", cs[["t"]]), tamanho_bloco = as.integer(cs[["k"]]), .seed = 5L)
    expect_true(all(concorrencia(q$unidades) == cs[["l"]]), info = cs[["t"]])
    expect_true(all(table(q$unidades$t) == cs[["r"]]), info = cs[["t"]])
    expect_true(all(table(q$unidades$bloco) == cs[["k"]]), info = cs[["t"]])
    expect_equal(unlist(q$extras[c("b", "r", "lambda")]), c(b = cs[["b"]], r = cs[["r"]], lambda = cs[["l"]]))
    expect_length(q$avisos, 0L)
  }
  expect_true(.tr_exp_bib_confere(.TR_EXP_BIB_TABELA[["10_4"]], 10L, 2L))
  skip_if_not_installed("agricolae")
  o <- suppressMessages(utils::capture.output(b <- agricolae::design.bib(1:7, 3, seed = 1)))
  expect_equal(unname(b$statistics$lambda), p$extras$lambda)
  expect_equal(unname(b$statistics$Efficiency), p$extras$eficiencia, tolerance = 1e-6)
})

test_that("medidas repetidas: o tratamento é do indivíduo, todos medidos em todos os tempos", {
  p <- ds("medidas_repetidas", "dieta: A, B, C", tempos = "0, 30, 60", repeticoes = 5L)
  u <- p$unidades
  expect_true(all(tapply(u$dieta, u$individuo, function(v) length(unique(v))) == 1L))
  expect_true(all(table(u$individuo, u$tempo) == 1L))
  expect_error(ds("medidas_repetidas", "dieta: A, B"), class = "tr_experiments_error_blank_param")
})

test_that("crossover: Williams igual ao crossdes e balanceado para o efeito residual", {
  for (t in 2:6) {
    W <- .tr_exp_williams(t)
    ant <- matrix(0L, t, t)
    for (i in seq_len(nrow(W))) for (j in 2:t) ant[W[i, j - 1], W[i, j]] <- ant[W[i, j - 1], W[i, j]] + 1L
    fora <- ant[row(ant) != col(ant)]
    expect_true(length(unique(fora)) == 1L, info = t)
    if (requireNamespace("crossdes", quietly = TRUE) && t > 2) {
      expect_equal(unname(W), unname(crossdes::williams(t)), info = t)
    }
  }
  p <- ds("crossover", "t: A, B, C", repeticoes = 2L)
  expect_equal(nrow(p$unidades), 6L * 2L * 3L)
  expect_true(all(table(p$unidades$individuo, p$unidades$t) == 1L))
})

test_that("crossover: coluna do efeito residual e fórmula com ela, t − 1 gl, posto completo", {
  p <- ds("crossover", "t: A, B, C, D", repeticoes = 2L, .seed = 6L)
  u <- as.data.frame(p$unidades)
  # No 1º período não há residual: a coluna leva a referência (o 1º nível).
  expect_true(all(u$residual[u$periodo == "1"] == "A"))
  expect_equal(levels(u$residual), c("A", "B", "C", "D"))
  for (i in which(u$periodo != "1")) expect_equal(as.character(u$residual[i]), as.character(u$t[i - 1L]))
  expect_match(p$analise$params$formula, "residual", fixed = TRUE)
  set.seed(1); u$y <- stats::rnorm(nrow(u))
  a <- stats::anova(stats::lm(y ~ individuo + periodo + t + residual, u))
  expect_equal(a["residual", "Df"], 3L)
  X <- stats::model.matrix(~ periodo + t + residual, u)
  expect_equal(qr(X)$rank, ncol(X))
  # Mesmo espaço ajustado do modelo com o nível "nenhum" no 1º período.
  nen <- factor(ifelse(u$periodo == "1", "nenhum", as.character(u$residual)), levels = c("nenhum", "A", "B", "C", "D"))
  expect_equal(stats::fitted(stats::lm(y ~ individuo + periodo + t + residual, u)),
               stats::fitted(stats::lm(y ~ individuo + periodo + t + nen, u)), tolerance = 1e-10)
  expect_error(ds("crossover", "residual: A, B"), class = "tr_experiments_error_reserved_name")
})

test_that("crossover: o lmer sugerido ajusta sem descartar coluna e recupera o residual simulado", {
  # λ = (0, 2, -1, 0.5) para A..D (A é a referência; 1º período sem residual).
  # 24 indivíduos, σ = 0,5: EP de cada diferença ≈ 0,17; tolerância 0,6 (> 3 EP).
  p <- ds("crossover", "t: A, B, C, D", repeticoes = 6L, .seed = 3L)
  p <- tr_experiments_effect(p, "intercepto", valor = 10)
  p <- tr_experiments_effect(p, "fixo", "residual", efeitos = "A = 0, B = 2, C = -1, D = 0.5")
  p <- tr_experiments_effect(p, "aleatorio", "individuo", sd = 1, .seed = 5L)
  y <- tr_experiments_error(p, sd = 0.5, .seed = 8L)
  u <- as.data.frame(y$unidades)
  expect_equal(u$.ef_residual[u$periodo == "1"], rep(0, sum(u$periodo == "1")))
  f <- stats::as.formula(y$analise$params$formula)
  msgs <- character()
  fit <- withCallingHandlers(lme4::lmer(f, data = u),
                             message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") },
                             warning = function(w) { msgs <<- c(msgs, conditionMessage(w)); invokeRestart("muffleWarning") })
  expect_false(any(grepl("rank deficient", msgs)))
  b <- lme4::fixef(fit)
  expect_lt(max(abs(b[c("residualB", "residualC", "residualD")] - c(2, -1, 0.5))), 0.6)
})

test_that("grupos: o mesmo DBC em cada local, com sorteio independente", {
  p <- ds("grupos", "t: A, B, C, D", locais = 3L, repeticoes = 3L, .seed = 11L)
  u <- p$unidades
  expect_true(all(table(interaction(u$local, u$bloco, drop = TRUE), u$t) == 1L))
  por_local <- tapply(as.character(u$t), u$local, paste, collapse = "")
  expect_gt(length(unique(por_local)), 1L)
  q <- ds("grupos", "a: 1, 2; b: x, y", locais = 3L)
  expect_match(q$analise$params$formula, "(1 | local:a:b)", fixed = TRUE)
  expect_match(q$analise$params$formula, "(1 | local:a)", fixed = TRUE)
})

test_that("reprodutível pela semente, sem mexer no RNG do usuário", {
  set.seed(123); antes <- stats::runif(1); set.seed(123)
  a <- ds("dbc", .seed = 42L)
  expect_equal(stats::runif(1), antes)
  b <- ds("dbc", .seed = 42L); c <- ds("dbc", .seed = 43L)
  expect_identical(a$unidades, b$unidades)
  expect_false(identical(a$unidades, c$unidades))
  expect_identical(tr_experiments_randomize(a, 42L)$unidades, a$unidades)
  expect_equal(a$semente, 42L)
})

test_that("validações: nomes reservados, fatores e níveis", {
  expect_error(ds("dbc", "bloco: A, B"), class = "tr_experiments_error_reserved_name")
  expect_error(ds("dbc", "a: 1, 2; b: 1, 2"), class = "tr_experiments_error_bad_factors")
  expect_error(ds("dbc", "a"), class = "tr_experiments_error_bad_factors")
  expect_error(ds("dic", "a: 1, 2", repeticoes = 1L), class = "tr_experiments_error_no_residual")
  expect_error(ds("xyz"), class = "tr_experiments_error_bad_option")
  expect_error(ds("dbc", ""), class = "tr_experiments_error_blank_param")
  expect_error(ds("dbc", covariaveis = "tratamento"), class = "tr_experiments_error_reserved_name")
  expect_error(ds("bib", "t: 30", tamanho_bloco = 7L), class = "tr_experiments_error_no_design")
  p <- ds("dbc", covariaveis = "peso_inicial")
  expect_true(all(is.na(p$unidades$peso_inicial)))
  expect_equal(p$fatores$mecanismo[p$fatores$nome == "peso_inicial"], "sem sorteio")
})

test_that("as quatro abas da vista desenham todas as estruturas", {
  casos <- list(list("dic"), list("dbc"), list("dql"), list("fatorial", "a: 1, 2; b: x, y"),
                list("confundimento", "A: -, +; B: -, +; C: -, +", confundir = "ABC"),
                list("fracionado", "A; B; C; D", geradores = "D = ABC"), list("composto_central", "x1; x2"),
                list("parcela_subdividida", "a: 1, 2; b: x, y, z"), list("faixas", "a: 1, 2; b: x, y"),
                list("bib", "t: 7"), list("medidas_repetidas", tempos = "0, 1"), list("crossover", "t: 3"),
                list("grupos"))
  for (cs in casos) {
    p <- do.call(ds, cs)
    for (aba in c("mapa", "hierarquia", "combinacoes", "ordem")) {
      g <- tr_experiments_view(p, aba)
      expect_s3_class(g, "ggplot")
      expect_no_error(ggplot2::ggplot_build(g), message = paste(cs[[1]], aba))
    }
  }
})
