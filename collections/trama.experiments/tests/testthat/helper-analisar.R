# Dados dos exemplos resolvidos usados como oráculo nos testes de análise.

# Montgomery (2017), exemplo 3.1: taxa de gravação (Å/min) por potência de RF
# (W), DIC com 5 réplicas. SQ tratamento = 66870,55 e SQ erro = 5339,20 no livro.
dados_gravacao <- function() {
  data.frame(potencia = factor(rep(c(160, 180, 200, 220), each = 5)),
             taxa = c(575, 542, 530, 539, 570, 565, 593, 590, 579, 610,
                      600, 651, 610, 637, 629, 725, 700, 715, 685, 710))
}

# Montgomery (2017), exemplo 6.1: 2² (concentração do reagente A, catalisador
# B), 3 réplicas. Totais (1) = 80, a = 100, b = 60, ab = 90.
dados_2k <- function() {
  # O nível alto é o SEGUNDO: é ele que leva o sinal + no contraste.
  data.frame(A = factor(rep(c("-", "+", "-", "+"), each = 3), levels = c("-", "+")),
             B = factor(rep(c("-", "-", "+", "+"), each = 3), levels = c("-", "+")),
             y = c(28, 25, 27, 36, 32, 32, 18, 19, 23, 31, 30, 29))
}

# Doses desigualmente espaçadas com réplicas desiguais (sem oráculo de livro:
# o oráculo é a identidade com a regressão em poly()).
dados_desiguais <- function() {
  set.seed(20260925)
  x <- c(0, 1, 3, 7, 15); r <- c(4, 3, 5, 2, 4)
  data.frame(dose = factor(rep(x, r)), y = 10 + 2 * log1p(rep(x, r)) + stats::rnorm(sum(r)))
}

split_aveia <- function() {
  trama.models::tr_models_anova_split_plot(trama.models::tr_models_example("aveia"), "producao",
                                           "variedade", "nitrogenio", "bloco")
}
