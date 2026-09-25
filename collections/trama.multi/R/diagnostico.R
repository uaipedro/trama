# Diagnósticos de antes de fatorar: a matriz tem correlação que valha fatorar
# (KMO e Bartlett)? Quantos fatores reter (análise paralela)?
#
# São fórmulas de livro, e ficam aqui em vez de virem do `psych` pelo mesmo
# motivo das rotações: meia dúzia de linhas cada, e a paridade com o `psych` é
# conferida nos testes.

#' A leitura de Kaiser (1974) para o KMO e o MSA.
#'
#' Os adjetivos são os dele ("marvelous", "meritorious", "middling", "mediocre",
#' "miserable", "unacceptable"), traduzidos: a escala é convenção, e citá-la
#' com os nomes de sempre deixa conferir contra qualquer livro.
#' @noRd
.tr_multi_leitura_kmo <- function(x) {
  as.character(cut(x, c(-Inf, .5, .6, .7, .8, .9, Inf), right = FALSE,
                   labels = c("inaceitável", "miserável", "medíocre", "mediano", "meritório",
                              "maravilhoso")))
}

#' KMO, MSA por variável e esfericidade de Bartlett.
#' @param dados tabela.
#' @param cols variáveis, separadas por vírgula; em branco, todas as numéricas.
#' @return tibble com `medida`, `variavel`, `valor`, `gl`, `p_valor`, `leitura`.
#' @export
tr_multi_kmo_bartlett <- function(dados, cols = "") {
  no <- "multi/kmo_bartlett"
  variaveis <- .tr_multi_variaveis(dados, cols)
  m <- .tr_multi_matriz(dados, variaveis, no)
  R <- .tr_multi_correlacao(m, no)
  n <- nrow(m)
  p <- ncol(R)

  # Correlações PARCIAIS a partir da inversa: o par (i, j) descontadas todas
  # as outras variáveis. Se as variáveis têm fatores comuns, a parcial é
  # pequena perto da simples (o fator comum explica a correlação), e o KMO,
  # que compara as duas, se aproxima de 1.
  Ri <- solve(R)
  parcial <- -Ri / sqrt(outer(diag(Ri), diag(Ri)))
  r2 <- R^2
  q2 <- parcial^2
  diag(r2) <- diag(q2) <- 0
  kmo <- sum(r2) / (sum(r2) + sum(q2))
  msa <- colSums(r2) / (colSums(r2) + colSums(q2))

  # Bartlett: H0 é a matriz de correlação ser a identidade. `ln|R|` é zero na
  # identidade e cai quanto mais correlação houver.
  qui2 <- -(n - 1 - (2 * p + 5) / 6) * log(det(R))
  gl <- p * (p - 1) / 2
  pv <- stats::pchisq(qui2, gl, lower.tail = FALSE)
  # "Não rejeita" não é "as variáveis são independentes": a leitura diz só o
  # que o teste permite dizer.
  leitura_b <- if (pv < 0.05) {
    "rejeita a esfericidade (5%): há correlação para fatorar"
  } else {
    "não rejeita a esfericidade (5%): sem evidência de correlação para fatorar"
  }

  tibble::tibble(
    medida = c("KMO global", rep("MSA", p), "Bartlett"),
    variavel = c(NA_character_, variaveis, NA_character_),
    valor = c(kmo, unname(msa), qui2),
    gl = c(NA_real_, rep(NA_real_, p), gl),
    p_valor = c(NA_real_, rep(NA_real_, p), pv),
    leitura = c(.tr_multi_leitura_kmo(c(kmo, msa)), leitura_b)
  )
}

#' Análise paralela de Horn: autovalores observados contra os de dados
#' aleatórios do mesmo tamanho.
#' @param dados tabela.
#' @param cols variáveis, separadas por vírgula; em branco, todas as numéricas.
#' @param repeticoes quantas tabelas aleatórias sortear.
#' @param percentil percentil dos autovalores aleatórios usado como limiar.
#' @param .seed semente (a do nó, passada pelo motor). `NULL` usa o RNG atual.
#' @return tibble com `posicao`, `autovalor_observado`, `autovalor_aleatorio`,
#'   `reter`.
#' @export
tr_multi_parallel <- function(dados, cols = "", repeticoes = 100L, percentil = 95L, .seed = NULL) {
  no <- "multi/parallel"
  repeticoes <- .tr_multi_int(repeticoes, "repeticoes", min = 10, max = 10000)
  percentil <- .tr_multi_int(percentil, "percentil", min = 50, max = 99)
  variaveis <- .tr_multi_variaveis(dados, cols)
  m <- .tr_multi_matriz(dados, variaveis, no)
  R <- .tr_multi_correlacao(m, no)
  n <- nrow(m)
  p <- ncol(R)
  observado <- eigen(R, symmetric = TRUE, only.values = TRUE)$values

  sortear <- function() {
    vapply(seq_len(repeticoes), function(i) {
      z <- matrix(stats::rnorm(n * p), n, p)
      eigen(stats::cor(z), symmetric = TRUE, only.values = TRUE)$values
    }, numeric(p))
  }
  # Com a semente do nó, o sorteio é o mesmo toda vez e não mexe no RNG de quem
  # chamou (o processo é o do app, e outro nó aleatório dependeria dele). Sem
  # semente, no console, usa o RNG corrente como qualquer função do R.
  aleatorios <- if (is.null(.seed)) sortear() else .tr_multi_com_semente(.seed, sortear())
  limiar <- apply(aleatorios, 1L, stats::quantile, probs = percentil / 100, names = FALSE)

  # Só a SEQUÊNCIA inicial conta: um sétimo autovalor que por acaso passa do
  # aleatório depois de o quinto não passar não é sétimo fator.
  reter <- cumprod(observado > limiar) == 1
  tibble::tibble(posicao = seq_len(p), autovalor_observado = observado,
                 autovalor_aleatorio = limiar, reter = reter)
}

#' Assimetria e curtose multivariadas de Mardia numa matriz.
#'
#' A covariância é a de divisor n, a do artigo (Mardia 1970, eq. 2.1-2.2) e a
#' do `MVN::mardia` (`use_population = TRUE`); o `psych::mardia` usa n − 1, e
#' os coeficientes dele saem menores por ((n − 1)/n)^3 e ^2. A correção de
#' amostra pequena da assimetria é o fator k de Mardia (1974), o mesmo do MVN e
#' do psych.
#' @noRd
.tr_multi_mardia_matriz <- function(x) {
  n <- nrow(x); p <- ncol(x)
  xc <- scale(x, center = TRUE, scale = FALSE)
  S <- crossprod(xc) / n
  D <- xc %*% solve(S, t(xc))
  b1 <- sum(D^3) / n^2
  b2 <- sum(diag(D)^2) / n
  gl <- p * (p + 1) * (p + 2) / 6
  k <- (p + 1) * (n + 1) * (n + 3) / (n * ((n + 1) * (p + 1) - 6))
  qui <- n * b1 / 6
  qui_k <- n * k * b1 / 6
  z <- (b2 - p * (p + 2)) * sqrt(n / (8 * p * (p + 2)))
  data.frame(
    medida = c("assimetria", "assimetria (amostra pequena)", "curtose"),
    n = n, coeficiente = c(b1, b1, b2), estatistica = c(qui, qui_k, z),
    gl = c(gl, gl, NA_real_),
    p_valor = c(stats::pchisq(qui, gl, lower.tail = FALSE),
                stats::pchisq(qui_k, gl, lower.tail = FALSE),
                2 * stats::pnorm(-abs(z))),
    stringsAsFactors = FALSE)
}

#' Teste de normalidade multivariada de Mardia (assimetria e curtose).
#' @param dados tabela.
#' @param cols variáveis, separadas por vírgula; em branco, todas as numéricas.
#' @param grupo coluna de grupo opcional: o teste é feito dentro de cada grupo,
#'   que é o que a discriminante e o M de Box supõem.
#' @param confianca nível de confiança da leitura (a decisão é a 1 − confianca).
#' @return tibble com `medida`, `n`, `coeficiente` (b1,p ou b2,p),
#'   `estatistica`, `gl`, `p_valor`, `leitura` (e o grupo na frente, se houver).
#' @export
tr_multi_mardia <- function(dados, cols = "", grupo = "", confianca = 0.95) {
  no <- "multi/mardia"
  confianca <- .tr_multi_num(confianca, "confianca", min = 0.5, max = 0.999)
  alfa <- 1 - confianca
  tem_grupo <- length(grupo) == 1L && !is.na(grupo) && nzchar(trimws(grupo))
  if (tem_grupo) {
    gr <- .tr_multi_grupos(dados, grupo, cols, no)
    X <- gr$X; g <- gr$g
  } else {
    X <- .tr_multi_matriz(dados, .tr_multi_variaveis(dados, cols), no)
    g <- factor(rep("todos", nrow(X)))
  }
  p <- ncol(X)
  # Com n = p + 1 os pontos formam um simplex e toda distância de Mahalanobis
  # vale o mesmo: b1 e b2 saem da conta de n e p, não dos dados.
  minimo <- p + 2L
  if (!tem_grupo && nrow(X) < minimo) {
    .tr_multi_abort("tr_multi_error_too_few_rows",
                    "'%s' precisa de pelo menos p + 2 = %d observações com %d variáveis, e a tabela tem %d.",
                    no, as.integer(minimo), p, nrow(X))
  }
  if (tem_grupo) {
    .tr_multi_grupo_minimo(g, minimo, no, sprintf(
      "Com %d variáveis o teste pede pelo menos p + 2 observações por grupo.", p))
  }
  partes <- lapply(levels(g), function(l) {
    x <- X[g == l, , drop = FALSE]
    S <- stats::cov(x)
    if (.tr_multi_cov_singular(S)) {
      .tr_multi_abort("tr_multi_error_singular_matrix",
                      paste0("'%s': a covariância%s é singular — alguma variável é combinação ",
                             "exata das outras ou constante. Tire a redundante."),
                      no, if (tem_grupo) sprintf(" do grupo '%s'", l) else "")
    }
    r <- .tr_multi_mardia_matriz(x)
    if (tem_grupo) r <- cbind(stats::setNames(data.frame(factor(l, levels(g))), gr$grupo), r)
    r
  })
  r <- do.call(rbind, partes)
  pct <- format(100 * alfa, decimal.mark = ",")
  r$leitura <- ifelse(r$p_valor < alfa,
                      sprintf("rejeita a normalidade multivariada (%s%%)", pct),
                      sprintf("não rejeita a normalidade multivariada (%s%%): sem evidência contra", pct))
  tibble::as_tibble(r)
}

.tr_multi_nos_diagnostico <- function() {
  P <- trama::tr_param
  list(
    trama::tr_node("multi/kmo_bartlett",
      pressupostos = .tr_multi_doc("multi/kmo_bartlett")$pressupostos,
      referencias = .tr_multi_doc("multi/kmo_bartlett")$referencias,
      fn = tr_multi_kmo_bartlett, label = "KMO e Bartlett",
      category = "multi_diagnostico", icon = trama::tr_icon("stethoscope"),
      description = "A matriz tem correlação para fatorar? KMO global, MSA por variável e esfericidade de Bartlett.",
      inputs = list(dados = "data/table"), outputs = list(out = "data/table"),
      params = list(cols = P("cols", "", label = "Variáveis", example = "ans1, ans2, ans3, soc1, soc2")),
      help = .tr_multi_ajuda(r"---[
Responde, antes de uma análise fatorial (`multi/factor_analysis`) ou de uma
PCA (`multi/pca`), se as variáveis têm correlação suficiente para valer a pena.

### KMO e MSA

O **KMO** (Kaiser-Meyer-Olkin) compara as correlações simples com as PARCIAIS
(o par descontadas todas as outras variáveis). Se há fatores comuns, eles
explicam as correlações, e as parciais ficam pequenas: o KMO vai para 1. Se as
correlações são de par em par, sem nada em comum, as parciais são tão grandes
quanto as simples e o KMO cai para 0,5 ou menos.

O **MSA** é o mesmo índice para UMA variável. É o que aponta o item que não
conversa com os outros — o candidato a sair antes de fatorar.

A leitura de Kaiser (1974):

| valor | leitura |
|---|---|
| ≥ 0,9 | maravilhoso |
| 0,8–0,9 | meritório |
| 0,7–0,8 | mediano |
| 0,6–0,7 | medíocre |
| 0,5–0,6 | miserável |
| < 0,5 | inaceitável |

### Esfericidade de Bartlett

Testa se a matriz de correlação é a IDENTIDADE (nenhuma correlação):
`χ² = −(n − 1 − (2p + 5)/6) · ln|R|`, com p(p − 1)/2 graus de liberdade.
Rejeitar é o esperado e o mínimo: diz que há ALGUMA correlação, e com n grande
rejeita quase sempre. O KMO é o que diz se há correlação BASTANTE. Não rejeitar
não prova que as variáveis são independentes; diz só que os dados não dão
evidência de correlação.
]---", r"---[
- **Variáveis** — as colunas a examinar, separadas por vírgula. Em branco,
  todas as numéricas.
]---", r"---[
Uma tabela (`data/table`) com uma linha `KMO global`, uma linha `MSA` por
variável e uma linha `Bartlett`. Colunas: `medida`, `variavel` (só nas linhas
MSA), `valor` (o índice, ou o χ²), `gl` e `p_valor` (só no Bartlett) e
`leitura`.
]---", r"---[
tr_flow(reg) |>
  tr_add("h", "multi/example", dataset = "harman_24_testes") |>
  tr_add("kmo", "multi/kmo_bartlett", from = "h")
]---", r"---[
`multi/parallel` para decidir quantos fatores; `multi/plot_correlation` para
ver a matriz; `multi/factor_analysis` para fatorar.
]---")),

    trama::tr_node("multi/mardia",
      pressupostos = .tr_multi_doc("multi/mardia")$pressupostos,
      referencias = .tr_multi_doc("multi/mardia")$referencias,
      fn = tr_multi_mardia, label = "Normalidade multivariada (Mardia)",
      category = "multi_diagnostico", icon = trama::tr_icon("stethoscope"),
      description = "Assimetria e curtose multivariadas de Mardia: as variáveis, juntas, são normais multivariadas? Por grupo, se houver.",
      inputs = list(dados = "data/table"), outputs = list(out = "data/table"),
      params = list(
        cols = P("cols", "", label = "Variáveis", example = "Sepal.Length, Sepal.Width, Petal.Length"),
        grupo = P("cols", "", label = "Grupo (opcional)", example = "Species"),
        confianca = trama::tr_param_num(0.95, min = 0.5, max = 0.999, label = "Confiança")),
      help = .tr_multi_ajuda(r"---[
Testa se as variáveis, JUNTAS, seguem uma normal multivariada — o que a
discriminante (`multi/discriminant`), o M de Box (`multi/box_m`), a fatorial
por máxima verossimilhança (`multi/factor_analysis`) e a esfericidade de
Bartlett (`multi/kmo_bartlett`) supõem. Conferir cada variável sozinha
(Shapiro-Wilk) é necessário mas não suficiente: variáveis normais uma a uma
podem não ser normais juntas.

### As duas medidas (Mardia, 1970)

Com dᵢⱼ = (xᵢ − x̄)ᵀ S⁻¹ (xⱼ − x̄) e S a covariância de divisor n:

- **assimetria** b₁,ₚ = Σᵢ Σⱼ dᵢⱼ³ / n², zero na normal; n·b₁,ₚ/6 é
  qui-quadrado com p(p + 1)(p + 2)/6 gl. A linha **amostra pequena** multiplica
  a estatística pelo fator k = (p + 1)(n + 1)(n + 3) / (n((n + 1)(p + 1) − 6))
  (Mardia, 1974), que deixa o nível mais perto do nominal com n pequeno (menos
  de 20, na regra do pacote MVN).
- **curtose** b₂,ₚ = Σᵢ dᵢᵢ² / n, igual a p(p + 2) na normal; o desvio
  padronizado (b₂,ₚ − p(p + 2)) / √(8p(p + 2)/n) é normal padrão (bilateral).

Rejeitar em qualquer das duas é evidência contra a normal multivariada.
Não rejeitar não prova normalidade: com poucos casos o teste tem pouco poder,
e com muitos rejeita desvios que não mudam a análise.

### Grupo

Com **grupo**, o teste é feito dentro de cada grupo (a discriminante e o M de
Box supõem normalidade DENTRO dos grupos, e não na tabela misturada — grupos
com médias diferentes formam uma mistura que não é normal).
]---", r"---[
- **Variáveis** — as colunas, separadas por vírgula. Em branco, todas as
  numéricas (menos o grupo).
- **Grupo (opcional)** — coluna que separa os grupos; em branco, a tabela toda.
- **Confiança** — a leitura rejeita quando p < 1 − confiança (padrão 0,95).
]---", r"---[
Uma tabela (`data/table`) com três linhas (por grupo, se houver): `assimetria`,
`assimetria (amostra pequena)` e `curtose`. Colunas: o grupo (se houver),
`medida`, `n`, `coeficiente` (b₁,ₚ ou b₂,ₚ), `estatistica` (χ² ou z), `gl`
(só nas assimetrias), `p_valor` e `leitura`.
]---", r"---[
tr_flow(reg) |>
  tr_add("i", "multi/example", dataset = "iris") |>
  tr_add("m", "multi/mardia", grupo = "Species", from = "i")
]---", r"---[
`multi/box_m` para as covariâncias;
`multi/discriminant` e `multi/logistic` (que não supõe normalidade).
]---")),

    trama::tr_node("multi/parallel", fn = tr_multi_parallel, label = "Análise paralela",
      category = "multi_diagnostico", icon = trama::tr_icon("dices"), stochastic = TRUE,
      description = "Quantos fatores reter: autovalores observados contra os de dados aleatórios (Horn).",
      inputs = list(dados = "data/table"), outputs = list(out = "data/table"),
      params = list(
        cols = P("cols", "", label = "Variáveis", example = "ans1, ans2, ans3, soc1, soc2"),
        repeticoes = trama::tr_param_int(100L, min = 10, max = 10000, label = "Repetições"),
        percentil = trama::tr_param_int(95L, min = 50, max = 99, label = "Percentil")),
      help = .tr_multi_ajuda(r"---[
Quantos fatores (ou componentes) reter? A análise paralela de Horn (1965)
responde comparando cada autovalor da matriz de correlação com o que se
obteria de dados SEM estrutura nenhuma: tabelas normais aleatórias com o mesmo
número de linhas e de colunas. Retém-se o fator enquanto o autovalor observado
passa do aleatório.

### Por que não "autovalor maior que 1"

A regra de Kaiser (reter autovalor > 1) vale para a população. Numa amostra
finita, mesmo dados puramente aleatórios têm os primeiros autovalores acima de
1 — com 24 variáveis e 145 linhas, o primeiro aleatório passa de 1,5. Por isso
a regra de Kaiser sugere fatores DEMAIS, tanto mais quanto mais variáveis e
menos observações. A análise paralela desconta exatamente esse acaso. Para
comparar, a contagem de Kaiser é o número de linhas com
`autovalor_observado > 1`.

### Como ler

A coluna `reter` marca a sequência INICIAL de posições em que o observado
passa do aleatório; a primeira que não passa encerra a contagem (uma posição
posterior que por acaso passe não conta). O número de `TRUE` é a sugestão.

É uma sugestão, não uma sentença: rode a análise fatorial com esse número e
com um a mais e um a menos, e fique com a solução que se interpreta. Os
autovalores são os da PCA (diagonal 1), a versão original de Horn, que tende a
sugerir o número certo de fatores bem definidos; fatores fracos podem ficar de
fora. No `harman_24_testes` de `multi/example` ela sugere 3, e o livro usa 4:
o quarto fator (memória) é o mais fraco — o caso em que vale rodar os dois e
ler as cargas.

### Semente

O nó é aleatório e mesmo assim reprodutível: usa a semente do card, gravada
no fluxo. O mesmo fluxo dá a mesma tabela em qualquer computador, e rodá-lo não
mexe no sorteio de outro nó.
]---", r"---[
- **Variáveis** — as colunas, separadas por vírgula. Em branco, todas as
  numéricas.
- **Repetições** — quantas tabelas aleatórias sortear. 100 basta para a
  decisão; mais deixa o limiar mais estável.
- **Percentil** — o limiar é este percentil dos autovalores aleatórios em cada
  posição. 95 é o critério conservador usual (Glorfeld, 1995); 50 é a média,
  a proposta original de Horn, e retém mais.
]---", r"---[
Uma tabela (`data/table`), uma linha por posição: `posicao`,
`autovalor_observado`, `autovalor_aleatorio` (o percentil escolhido) e
`reter`. Ligada a `view/line` com `x = posicao`, desenha o scree com a curva
aleatória.
]---", r"---[
tr_flow(reg) |>
  tr_add("q", "multi/example", dataset = "questionario") |>
  tr_add("pa", "multi/parallel", repeticoes = 50L, from = "q")
]---", r"---[
`multi/kmo_bartlett` antes; `multi/factor_analysis` com o número sugerido;
`multi/scree` para o gráfico dos autovalores de uma PCA.
]---"))
  )
}
