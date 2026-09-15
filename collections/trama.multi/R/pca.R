# Componentes principais: o ajuste, as tabelas que saem dele e os três
# gráficos que se leem juntos (scree, biplot, círculo de correlações).
#
# O ajuste é o `stats::prcomp`, e nada mais: é a decomposição em valores
# singulares da matriz centrada, numericamente melhor que o `princomp` (que
# faz `eigen` da covariância) e que devolve o mesmo resultado dos livros. O
# que esta coleção acrescenta é o que o `prcomp` deixa para quem usa — os nomes
# `CP1..CPk`, as correlações variável-componente, a tabela de variância — e as
# recusas: coluna de texto, faltante e variável constante viram card vermelho.

#' Percentual com vírgula, que é como a ajuda e o card falam.
#' @noRd
.tr_multi_pct <- function(x, digitos = 1L) {
  paste0(formatC(100 * x, format = "f", digits = digitos, decimal.mark = ","), "%")
}

#' Confere que o valor é mesmo o objeto da PCA, para o `fn` chamado no console.
#' @noRd
.tr_multi_pca_conferir <- function(pca) {
  .tr_multi_guard(pca, "tr_multi_pca", .TR_MULTI_CAMPOS_PCA, "tr_multi_error_not_a_pca",
                  "uma análise de componentes principais")
}

#' Componentes principais.
#'
#' `center` é SEMPRE ligado: PCA sem centrar decompõe o segundo momento em
#' torno do zero, e o primeiro "componente" vira a direção da média — nenhum
#' livro a chama de PCA. `padronizar` decide entre a matriz de correlação
#' (TRUE) e a de covariância (FALSE), e é a decisão que importa.
#' @param dados tabela.
#' @param cols variáveis, separadas por vírgula; em branco, todas as numéricas.
#' @param padronizar dividir cada variável pelo desvio padrão antes.
#' @return objeto `tr_multi_pca`.
#' @export
tr_multi_pca <- function(dados, cols = "", padronizar = TRUE) {
  variaveis <- .tr_multi_variaveis(dados, cols)
  m <- .tr_multi_matriz(dados, variaveis, "multi/pca")
  ajuste <- .tr_multi_ajustar(stats::prcomp(m, center = TRUE, scale. = isTRUE(padronizar)),
                              "multi/pca")
  # "CP" e não o "PC" do R: o card e a ajuda falam português, e o nome da
  # coluna é o que a pessoa digita no `view/points`.
  k <- ncol(ajuste$rotation)
  nomes <- paste0("CP", seq_len(k))
  colnames(ajuste$rotation) <- nomes
  colnames(ajuste$x) <- nomes
  .tr_multi_pca_obj(ajuste, dados, variaveis, isTRUE(padronizar))
}

#' A tabela de variância explicada.
#' @param pca objeto `tr_multi_pca`.
#' @return tibble `componente`, `autovalor`, `desvio`, `proporcao`, `acumulada`.
#' @export
tr_multi_pca_variance <- function(pca) {
  .tr_multi_pca_conferir(pca)
  sdev <- pca$ajuste$sdev
  # Na PCA não padronizada o autovalor é uma VARIÂNCIA na unidade dos dados
  # (Assault², em USArrests); só na padronizada ele se compara ao 1 de Kaiser.
  prop <- sdev^2 / sum(sdev^2)
  tibble::tibble(componente = colnames(pca$ajuste$rotation), autovalor = sdev^2, desvio = sdev,
                 proporcao = prop, acumulada = cumsum(prop))
}

#' Quantos componentes pedir: 0 é todos.
#' @noRd
.tr_multi_pca_k <- function(pca, componentes) {
  k <- ncol(pca$ajuste$rotation)
  n <- .tr_multi_int(componentes, "componentes", min = 0, max = k)
  if (n == 0L) k else n
}

#' Matriz p × k das correlações entre variável e componente.
#'
#' Padronizada, é `autovetor × desvio do componente`: a variável tem desvio 1.
#' Não padronizada, a mesma conta dá a COVARIÂNCIA variável-componente, e é
#' preciso dividir pelo desvio da variável — sem isso `Assault` teria "carga"
#' 80 no CP1 de USArrests, número que não se lê como nada. É `cor(X, escores)`
#' nos dois casos, que é o que o teste confere.
#' @noRd
.tr_multi_pca_cor <- function(pca) {
  a <- pca$ajuste
  r <- sweep(a$rotation, 2L, a$sdev[seq_len(ncol(a$rotation))], "*")
  if (!isTRUE(pca$padronizado)) {
    dp <- apply(as.matrix(as.data.frame(pca$dados)[, pca$variaveis, drop = FALSE]), 2L, stats::sd)
    r <- r / dp
  }
  r
}

#' As cargas: autovetores ou correlações variável-componente.
#' @param pca objeto `tr_multi_pca`.
#' @param tipo `"correlações"` ou `"autovetores"`.
#' @param componentes quantos componentes (os primeiros); 0 é todos.
#' @return tibble `variavel` + uma coluna por componente.
#' @export
tr_multi_pca_loadings <- function(pca, tipo = "correlações", componentes = 0L) {
  .tr_multi_pca_conferir(pca)
  tipo <- .tr_multi_enum(tipo, c("correlações", "autovetores"), "tipo")
  k <- .tr_multi_pca_k(pca, componentes)
  m <- if (tipo == "correlações") .tr_multi_pca_cor(pca) else pca$ajuste$rotation
  m <- m[, seq_len(k), drop = FALSE]
  tibble::as_tibble(data.frame(variavel = rownames(m), unname(m), stringsAsFactors = FALSE) |>
                      stats::setNames(c("variavel", colnames(m))))
}

#' Scree: a variância explicada por componente.
#'
#' Barras da proporção, linha da acumulada, no MESMO eixo (0 a 100%): as duas
#' perguntas que se fazem a um scree — onde está o cotovelo e quantos
#' componentes chegam a 80% — se leem sem trocar de escala. O eixo em
#' proporção, e não em autovalor, é o que deixa a mesma figura servir à PCA
#' não padronizada, cujo autovalor está na unidade² dos dados.
#'
#' O critério de Kaiser (autovalor > 1) só existe na padronizada, onde autovalor
#' 1 é "explica o que uma variável sozinha explica", ou seja proporção 1/p. A
#' linha sai nesse ponto, e as barras acima dela ganham a cor cheia.
#' @export
tr_multi_scree <- function(pca, aspecto = "16:9", tema = "padrão", titulo = "",
                           rotulo_x = "", rotulo_y = "", legenda = "direita") {
  v <- tr_multi_pca_variance(pca)
  v$componente <- factor(v$componente, levels = v$componente)
  p_vars <- length(pca$variaveis)
  kaiser <- isTRUE(pca$padronizado)
  v$grupo <- if (kaiser) ifelse(v$autovalor > 1, "autovalor > 1", "autovalor ≤ 1") else "proporção"
  cores <- c(`autovalor > 1` = .TR_MULTI_COR, `autovalor ≤ 1` = .TR_MULTI_CINZA,
             `proporção` = .TR_MULTI_COR)
  p <- ggplot2::ggplot(v, ggplot2::aes(x = .data[["componente"]])) +
    ggplot2::geom_col(ggplot2::aes(y = .data[["proporcao"]], fill = .data[["grupo"]]), width = .7) +
    ggplot2::geom_line(ggplot2::aes(y = .data[["acumulada"]], group = 1L), colour = .TR_MULTI_COR_2,
                       linewidth = .7) +
    ggplot2::geom_point(ggplot2::aes(y = .data[["acumulada"]]), colour = .TR_MULTI_COR_2, size = 2) +
    ggplot2::geom_text(ggplot2::aes(y = .data[["proporcao"]], label = .tr_multi_pct(.data[["proporcao"]], 0L)),
                       vjust = -.5, size = 3, colour = .TR_MULTI_CINZA,
                       # À esquerda do centro: no centro da barra fica o ponto
                       # da acumulada, que no CP1 é exatamente o topo dela.
                       position = ggplot2::position_nudge(x = -.2)) +
    ggplot2::scale_fill_manual(values = cores, name = NULL) +
    ggplot2::scale_y_continuous(labels = function(x) .tr_multi_pct(x, 0L), limits = c(0, 1.05),
                                breaks = seq(0, 1, .2)) +
    ggplot2::labs(x = "componente", y = "variância explicada (barras) e acumulada (linha)")
  if (kaiser) {
    p <- p + ggplot2::geom_hline(yintercept = 1 / p_vars, linetype = "dashed", colour = .TR_MULTI_CINZA) +
      ggplot2::annotate("text", x = nrow(v) + .45, y = 1 / p_vars, label = "Kaiser (autovalor 1)",
                        hjust = 1, vjust = -.5, size = 3, colour = .TR_MULTI_CINZA)
  }
  trama.view::tr_view_finish(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

#' Os dois componentes de um plano, conferidos.
#' @noRd
.tr_multi_pca_plano <- function(pca, x, y) {
  k <- ncol(pca$ajuste$rotation)
  x <- .tr_multi_int(x, "x", min = 1, max = k)
  y <- .tr_multi_int(y, "y", min = 1, max = k)
  if (x == y) {
    .tr_multi_abort("tr_multi_error_bad_option",
                    "Params 'x' e 'y': o plano precisa de dois componentes diferentes (veio CP%d nos dois).", x)
  }
  prop <- pca$ajuste$sdev^2 / sum(pca$ajuste$sdev^2)
  rot <- function(j) sprintf("CP%d (%s)", j, .tr_multi_pct(prop[[j]]))
  list(x = x, y = y, nx = paste0("CP", x), ny = paste0("CP", y), rx = rot(x), ry = rot(y))
}

#' Biplot: escores das observações e setas das variáveis no mesmo plano.
#'
#' As setas são as CORRELAÇÕES variável-componente, esticadas por um fator
#' único para caber na nuvem de escores. Fator único para todas: o
#' comprimento RELATIVO das setas e os ângulos entre elas continuam valendo, e é
#' isso que se lê. A escala absoluta das setas não tem unidade no eixo — para
#' ler o valor, o `multi/correlation_circle`.
#' @export
tr_multi_biplot <- function(pca, x = 1L, y = 2L, cor = "", rotulo = "", setas = TRUE,
                            aspecto = "1:1", tema = "padrão", titulo = "", rotulo_x = "",
                            rotulo_y = "", legenda = "direita") {
  .tr_multi_pca_conferir(pca)
  pl <- .tr_multi_pca_plano(pca, x, y)
  dados <- pca$dados
  cor <- if (length(cor) && !is.na(cor) && nzchar(trimws(cor))) .tr_multi_col(dados, cor, "cor") else NULL
  rotulo <- if (length(rotulo) && !is.na(rotulo) && nzchar(trimws(rotulo)))
    .tr_multi_col(dados, rotulo, "rotulo") else NULL
  d <- data.frame(ex = pca$ajuste$x[, pl$x], ey = pca$ajuste$x[, pl$y])
  if (!is.null(cor)) {
    # Texto vira fator para a legenda ser discreta; número fica contínuo, como
    # no `view/points`.
    v <- dados[[cor]]
    d$cor <- if (is.numeric(v)) v else factor(v)
  }
  if (!is.null(rotulo)) d$rotulo <- as.character(dados[[rotulo]])
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[["ex"]], y = .data[["ey"]])) +
    ggplot2::geom_hline(yintercept = 0, colour = .TR_MULTI_CINZA, linewidth = .3) +
    ggplot2::geom_vline(xintercept = 0, colour = .TR_MULTI_CINZA, linewidth = .3)
  p <- if (is.null(cor)) {
    p + ggplot2::geom_point(colour = .TR_MULTI_COR, size = 2, alpha = .8)
  } else {
    p + ggplot2::geom_point(ggplot2::aes(colour = .data[["cor"]]), size = 2, alpha = .85)
  }
  if (!is.null(rotulo)) {
    # Sem ggrepel: `check_overlap` descarta o rótulo que colidiria, em vez de
    # empilhar cinquenta nomes ilegíveis. O ponto continua lá.
    p <- p + ggplot2::geom_text(ggplot2::aes(label = .data[["rotulo"]]), size = 2.8, vjust = -.7,
                                colour = .TR_MULTI_CINZA, check_overlap = TRUE)
  }
  if (isTRUE(setas)) {
    r <- .tr_multi_pca_cor(pca)[, c(pl$x, pl$y), drop = FALSE]
    alcance <- max(abs(c(d$ex, d$ey)))
    fator <- .8 * alcance / max(sqrt(rowSums(r^2)))
    s <- data.frame(variavel = rownames(r), sx = r[, 1] * fator, sy = r[, 2] * fator)
    p <- p +
      ggplot2::geom_segment(data = s, ggplot2::aes(x = 0, y = 0, xend = .data[["sx"]], yend = .data[["sy"]]),
                            colour = .TR_MULTI_COR_2, linewidth = .6,
                            arrow = grid::arrow(length = grid::unit(.18, "cm"))) +
      # O rótulo nasce na ponta e cresce PARA FORA da seta (hjust pelo lado):
      # centrado, um nome longo como `expectativa_vida` passava da borda do
      # painel e saía cortado. O `expand_limits` reserva o espaço do texto,
      # estimado pelo número de letras — medido no card de `estados`.
      ggplot2::geom_text(data = s, ggplot2::aes(x = .data[["sx"]] * 1.05, y = .data[["sy"]] * 1.05,
                                                label = .data[["variavel"]],
                                                hjust = ifelse(.data[["sx"]] >= 0, 0, 1)),
                         colour = .TR_MULTI_COR_2, size = 3.2, fontface = "bold") +
      ggplot2::expand_limits(x = s$sx * 1.05 + sign(s$sx) * nchar(s$variavel) * alcance * .035)
  }
  p <- p + ggplot2::labs(x = pl$rx, y = pl$ry, colour = cor)
  trama.view::tr_view_finish(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

#' Círculo de correlações.
#'
#' Cada seta é uma variável; as coordenadas são as correlações dela com os dois
#' componentes. Como a soma dos quadrados das correlações de uma variável com
#' TODOS os componentes é 1, a seta nunca sai do círculo, e o quanto ela chega
#' perto dele é o quanto aquele plano a representa (o cos², na cor). Seta
#' curta é variável que o plano não mostra — o ângulo dela com as outras não
#' quer dizer nada.
#' @export
tr_multi_correlation_circle <- function(pca, x = 1L, y = 2L, aspecto = "1:1", tema = "padrão",
                                        titulo = "", rotulo_x = "", rotulo_y = "",
                                        legenda = "direita") {
  .tr_multi_pca_conferir(pca)
  pl <- .tr_multi_pca_plano(pca, x, y)
  r <- .tr_multi_pca_cor(pca)[, c(pl$x, pl$y), drop = FALSE]
  s <- data.frame(variavel = rownames(r), cx = r[, 1], cy = r[, 2], cos2 = rowSums(r^2))
  t <- seq(0, 2 * pi, length.out = 181L)
  circ <- data.frame(cx = cos(t), cy = sin(t))
  p <- ggplot2::ggplot(s, ggplot2::aes(x = .data[["cx"]], y = .data[["cy"]])) +
    ggplot2::geom_path(data = circ, colour = .TR_MULTI_CINZA, linewidth = .4) +
    ggplot2::geom_path(data = circ, ggplot2::aes(x = .data[["cx"]] / 2, y = .data[["cy"]] / 2),
                       colour = .TR_MULTI_CINZA, linewidth = .3, linetype = "dotted") +
    ggplot2::geom_hline(yintercept = 0, colour = .TR_MULTI_CINZA, linewidth = .3) +
    ggplot2::geom_vline(xintercept = 0, colour = .TR_MULTI_CINZA, linewidth = .3) +
    ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = .data[["cx"]], yend = .data[["cy"]],
                                       colour = .data[["cos2"]]),
                          linewidth = .7, arrow = grid::arrow(length = grid::unit(.2, "cm"))) +
    # Rótulo crescendo para fora da seta, como no biplot, e o limite do eixo X
    # alargado pelo nome mais comprido: centrado em ±1,12 com o eixo fixo em
    # ±1,25, qualquer nome de mais de seis letras saía cortado na borda.
    ggplot2::geom_text(ggplot2::aes(x = .data[["cx"]] * 1.05, y = .data[["cy"]] * 1.05,
                                    label = .data[["variavel"]], colour = .data[["cos2"]],
                                    hjust = ifelse(.data[["cx"]] >= 0, 0, 1)),
                       size = 3.2, show.legend = FALSE) +
    ggplot2::scale_colour_gradient(low = .TR_MULTI_CINZA, high = .TR_MULTI_COR_2, limits = c(0, 1),
                                   name = "cos²") +
    ggplot2::coord_equal(xlim = c(-1, 1) * (1.1 + max(nchar(s$variavel)) * .04),
                         ylim = c(-1.15, 1.15)) +
    ggplot2::labs(x = pl$rx, y = pl$ry)
  trama.view::tr_view_finish(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

#' Resumo do card da PCA.
#' @noRd
.tr_multi_pca_resumo <- function(x) {
  prop <- x$ajuste$sdev^2 / sum(x$ajuste$sdev^2)
  r <- list(variaveis = length(x$variaveis), observacoes = nrow(x$ajuste$x),
            padronizado = isTRUE(x$padronizado),
            variancia_cp1 = .tr_multi_pct(prop[[1]]),
            variancia_cp1_cp2 = if (length(prop) >= 2L) .tr_multi_pct(sum(prop[1:2])) else NA_character_)
  # Kaiser só na padronizada: na covariância o autovalor tem unidade, e
  # "maior que 1" dependeria de medir em metros ou em centímetros.
  if (isTRUE(x$padronizado)) r$autovalor_maior_que_1 <- sum(x$ajuste$sdev^2 > 1)
  r
}

#' Adaptador `multi/pca` → `data/table`: os escores ao lado do que não entrou.
#'
#' As colunas que NÃO foram variáveis (a espécie, o nome do estado) vêm antes,
#' e os escores `CP1..CPk` depois. É o que faz um `view/points` com `x = CP1`,
#' `y = CP2`, `cor = Species` ser o gráfico de escores sem `data/join` nenhum.
#' As variáveis originais ficam de fora: estão no `multi/pca_loadings` como
#' relação com os componentes, e aqui só confundiriam o `cols` em branco de um
#' nó seguinte.
#' @noRd
.tr_multi_pca_tabela <- function(x) {
  escores <- as.data.frame(x$ajuste$x)
  resto <- as.data.frame(x$dados)[, setdiff(names(x$dados), x$variaveis), drop = FALSE]
  # Coluna antiga chamada "CP1" que não foi variável (texto) cederia o nome
  # ao escore: duas colunas com o mesmo nome não existem num tibble.
  resto <- resto[, setdiff(names(resto), names(escores)), drop = FALSE]
  tibble::as_tibble(cbind(resto, escores))
}

.tr_multi_nos_pca <- function() {
  P <- trama::tr_param
  I <- trama::tr_param_int
  B <- trama::tr_param_bool
  E <- trama::tr_param_enum
  pca <- list(pca = "multi/pca")
  G <- list(out = "view/plot")
  list(
    trama::tr_node("multi/pca", fn = tr_multi_pca, label = "Componentes principais",
      category = "multi_pca", icon = trama::tr_icon("axis-3d"),
      description = "Resume variáveis correlacionadas em poucos componentes que não se correlacionam.",
      inputs = list(dados = "data/table"), outputs = list(out = "multi/pca"),
      params = list(cols = P("cols", "", label = "Variáveis", example = "Murder, Assault, UrbanPop, Rape"),
                    padronizar = B(TRUE, label = "Padronizar")),
      help = .tr_multi_ajuda(r"---[
A análise de componentes principais (PCA) troca p variáveis correlacionadas por
p novas variáveis — os COMPONENTES — que não se correlacionam entre si e vêm em
ordem de variância: o CP1 é a direção em que os dados mais variam, o CP2 a
maior entre as perpendiculares ao CP1, e assim por diante. Quando as variáveis
originais andam juntas, os primeiros dois ou três componentes carregam quase
toda a informação, e um gráfico de CP1 × CP2 mostra a tabela inteira num plano.

Cada componente é uma média ponderada das variáveis (centradas). Os pesos são
os AUTOVETORES; a variância de cada componente é o AUTOVALOR.

### Padronizar ou não

É a decisão que muda o resultado, e o exemplo é `USArrests`:

- **sem padronizar** (matriz de covariância), `Assault` — assaltos por 100 mil
  habitantes, na casa das centenas — tem variância milhares de vezes maior que
  `Murder`. O CP1 sai praticamente igual a `Assault` e explica ~97% da
  variância. Isso não diz nada sobre crime; diz que a unidade de `Assault` é
  pequena.
- **padronizando** (matriz de correlação), cada variável entra com variância 1,
  e o CP1 (~62%) passa a ser "criminalidade geral", com as três taxas de crime
  pesando parecido; o CP2 (~25%) é quase só urbanização.

Padronize quando as variáveis estão em unidades diferentes ou em escalas muito
diferentes — é o caso comum. Não padronize só quando todas estão na MESMA
unidade e a diferença de variância é informação (medidas em mm de um mesmo
animal, por exemplo).

### Cuidados

- O SINAL de cada componente é arbitrário: CP1 com todas as cargas negativas é
  o mesmo componente com todas positivas. Outro programa (ou outra versão do R)
  pode devolver o espelho. Leia a direção pelas cargas, não pelo sinal.
- Em branco, **Variáveis** usa todas as colunas numéricas. Um identificador
  numérico (código, ano) entraria como variável: nesse caso, liste as colunas.
- Faltantes e variáveis constantes param o nó com a coluna nomeada.
- PCA não é análise fatorial: não há modelo de erro nem rotação, e os
  componentes resumem a variância TOTAL, inclusive a que é só ruído de cada
  variável. Para fatores latentes, `multi/factor_analysis`.
]---", r"---[
- **Variáveis** — colunas numéricas, separadas por vírgula. Em branco, todas as
  numéricas da tabela.
- **Padronizar** — ligado, PCA na matriz de correlação (cada variável com
  variância 1); desligado, na de covariância. Os dados são sempre centrados.
]---", r"---[
Um objeto `multi/pca`. O card mostra a variância explicada (o scree). Ligado
direto a um nó de tabela, vira os escores: as colunas que não entraram na
análise seguidas de `CP1..CPk` — é assim que um `view/points` com `x = CP1`,
`y = CP2` e `cor = Species` desenha o gráfico de escores.
]---", r"---[
tr_flow(reg) |>
  tr_add("ua", "multi/example", dataset = "USArrests") |>
  tr_add("bruta", "multi/pca", padronizar = FALSE, from = "ua") |>
  tr_add("padr", "multi/pca", padronizar = TRUE, from = "ua")
]---", r"---[
`multi/scree` para decidir quantos componentes olhar; `multi/biplot` e
`multi/correlation_circle` para ler o que eles são; `multi/pca_loadings` e
`multi/pca_variance` para as tabelas; `multi/plot_correlation` antes de tudo.
]---")),

    trama::tr_node("multi/pca_variance", fn = tr_multi_pca_variance, label = "Variância explicada",
      category = "multi_pca", icon = trama::tr_icon("chart-bar-decreasing"),
      description = "Autovalor, proporção e proporção acumulada da variância de cada componente.",
      inputs = pca, outputs = list(out = "data/table"),
      params = list(),
      help = .tr_multi_ajuda(r"---[
A tabela por trás do scree: uma linha por componente, com quanto da variância
total ele carrega.

- `autovalor` — a variância do componente. Na PCA padronizada, a soma dos
  autovalores é o número de variáveis, e autovalor 1 é "explica o mesmo que uma
  variável original sozinha". Na não padronizada, está na unidade² dos dados.
- `desvio` — a raiz do autovalor (o `sdev` do `prcomp`).
- `proporcao` — autovalor ÷ soma dos autovalores. Soma 1.
- `acumulada` — a proporção dos k primeiros juntos.

### Quantos componentes guardar

Não há regra que valha sempre; as de uso comum, que se leem nesta tabela:

- **Kaiser** (só na padronizada): os de autovalor maior que 1. Tende a guardar
  componentes de mais quando há muitas variáveis.
- **Variância acumulada**: os que chegam a 70–90%, conforme o uso.
- **Cotovelo**: onde a proporção para de cair rápido (no `multi/scree`).
- **Análise paralela** (`multi/parallel`): os que explicam mais do que
  explicariam em dados sem correlação nenhuma. É a mais confiável das quatro.
]---", r"---[
Sem parâmetros.
]---", r"---[
Uma tabela (`data/table`) com `componente` (`CP1`, `CP2`, ...), `autovalor`,
`desvio`, `proporcao` e `acumulada`.
]---", r"---[
tr_flow(reg) |>
  tr_add("ua", "multi/example", dataset = "USArrests") |>
  tr_add("pca", "multi/pca", from = "ua") |>
  tr_add("var", "multi/pca_variance", from = "pca")
]---", r"---[
`multi/scree`, o mesmo em gráfico; `multi/parallel` para um critério melhor que
o de Kaiser.
]---")),

    trama::tr_node("multi/pca_loadings", fn = tr_multi_pca_loadings, label = "Cargas da PCA",
      category = "multi_pca", icon = trama::tr_icon("table-2"),
      description = "A relação de cada variável com cada componente: correlações ou autovetores.",
      inputs = pca, outputs = list(out = "data/table"),
      params = list(tipo = E("correlações", c("correlações", "autovetores"), label = "Tipo"),
                    componentes = I(0L, min = 0L, label = "Componentes")),
      help = .tr_multi_ajuda(r"---[
Uma linha por variável, uma coluna por componente. Há duas tabelas diferentes
com o nome de "cargas", e confundi-las é o erro mais comum ao ler PCA:

- **autovetores** — os PESOS: CP1 = a·(var1 centrada) + b·(var2 centrada) + ...
  Cada coluna tem soma dos quadrados 1. Servem para CALCULAR o escore; comparar
  um peso com outro só faz sentido na PCA padronizada.
- **correlações** — a correlação de cada variável com cada componente (os
  escores). Vão de −1 a 1 e se leem como qualquer correlação: 0,9 é "esta
  variável é quase o componente". A soma dos quadrados de uma LINHA, somando
  todos os componentes, é 1 — o quanto da variável os k primeiros componentes
  explicam é a soma dos quadrados até k.

Para NOMEAR um componente, use as correlações: procure as variáveis com valor
alto (em módulo) naquela coluna e o que elas têm em comum. Em USArrests
padronizado, `Murder`, `Assault` e `Rape` correlacionam fortemente com o CP1
("criminalidade") e `UrbanPop` com o CP2 ("urbanização").

Na PCA padronizada, a correlação é o autovetor vezes o desvio do componente.
Na não padronizada ela ainda é dividida pelo desvio da variável — a conta que
muitos textos esquecem, e sem a qual o número passa de 1.

O sinal de uma coluna inteira pode vir trocado: é o mesmo componente.
]---", r"---[
- **Tipo** — `correlações` (variável × componente, entre −1 e 1) ou
  `autovetores` (os pesos da combinação linear).
- **Componentes** — quantos dos primeiros componentes mostrar. 0 é todos.
]---", r"---[
Uma tabela (`data/table`) com `variavel` e as colunas `CP1..CPk`.
]---", r"---[
tr_flow(reg) |>
  tr_add("ua", "multi/example", dataset = "USArrests") |>
  tr_add("pca", "multi/pca", from = "ua") |>
  tr_add("cargas", "multi/pca_loadings", tipo = "correlações", componentes = 2L, from = "pca")
]---", r"---[
`multi/correlation_circle`, as mesmas correlações em gráfico; `multi/biplot`.
]---")),

    trama::tr_node("multi/scree", fn = tr_multi_scree, label = "Scree",
      category = "multi_pca", icon = trama::tr_icon("chart-no-axes-combined"),
      description = "Variância explicada por componente, com a acumulada e a linha de Kaiser.",
      inputs = pca, outputs = G,
      params = .tr_multi_props(.aspecto = "16:9"),
      help = .tr_multi_ajuda(r"---[
O gráfico para decidir QUANTOS componentes olhar. Cada barra é a proporção da
variância total que um componente explica; a linha rosa é a acumulada. As duas
estão no mesmo eixo, de 0 a 100%.

### Como ler

- **Cotovelo**: o ponto em que as barras param de cair depressa e viram uma
  rampa baixa. Os componentes antes do cotovelo são estrutura; os depois,
  quase sempre ruído.
- **Acumulada**: onde a linha cruza 80% (ou o patamar que o seu uso pede) é
  quantos componentes resumem a tabela a esse ponto.
- **Kaiser** (só na PCA padronizada): a linha tracejada fica em 1/p, que é a
  proporção de um autovalor 1 — o que uma variável original explica sozinha. As
  barras acima dela saem na cor cheia. Com muitas variáveis o critério guarda
  componentes de mais; confira com `multi/parallel`.

Na PCA não padronizada não há linha de Kaiser: o autovalor está na unidade dos
dados, e "maior que 1" dependeria da unidade escolhida. Um primeiro componente
com 97%, nesse caso, costuma ser só a variável de maior escala — veja a ajuda
de `multi/pca`.
]---", r"---[
Só os de aparência, abaixo.
]---", r"---[
Um gráfico (`view/plot`). No console, um ggplot com a tabela de
`multi/pca_variance` em `$data`. É também o que aparece no card do `multi/pca`.
]---", r"---[
tr_flow(reg) |>
  tr_add("ua", "multi/example", dataset = "USArrests") |>
  tr_add("pca", "multi/pca", from = "ua") |>
  tr_add("scree", "multi/scree", from = "pca")
]---", r"---[
`multi/pca_variance`, a mesma informação em tabela; `multi/parallel`.
]---", grafico = TRUE)),

    trama::tr_node("multi/biplot", fn = tr_multi_biplot, label = "Biplot",
      category = "multi_pca", icon = trama::tr_icon("chart-scatter"),
      description = "Observações e variáveis no plano de dois componentes.",
      inputs = pca, outputs = G,
      params = .tr_multi_props(
        x = I(1L, min = 1L, label = "Componente X"),
        y = I(2L, min = 1L, label = "Componente Y"),
        cor = P("cols", "", label = "Cor por", example = "Species"),
        rotulo = P("cols", "", label = "Rótulo", example = "nome"),
        setas = B(TRUE, label = "Setas das variáveis"),
        .aspecto = "1:1"),
      help = .tr_multi_ajuda(r"---[
O biplot põe DUAS coisas no mesmo plano de componentes: um ponto por
observação (os escores) e uma seta por variável.

### Como ler os pontos

- Pontos próximos são observações parecidas em todas as variáveis ao mesmo
  tempo (no quanto este plano as representa).
- Com **Cor por** apontando um grupo (`Species` na iris), dá para ver se os
  grupos se separam sem que a PCA saiba deles — na iris, o CP1 sozinho quase
  separa as espécies.
- O eixo diz quanto da variância o componente explica. Um plano com 50% deixa
  metade da história de fora: dois pontos juntos aqui podem estar longe no CP3.

### Como ler as setas

- A direção da seta é para onde a variável cresce no plano.
- Setas com ângulo pequeno: variáveis positivamente correlacionadas; a 90°,
  não correlacionadas; em sentidos opostos, negativamente correlacionadas.
- Seta longa: variável bem representada no plano. Seta curta: o plano quase
  não a mostra, e o ângulo dela não vale leitura.
- Uma observação na direção de uma seta tem valor alto naquela variável.

As setas são as correlações variável-componente esticadas por UM fator comum,
só para caberem na nuvem: os comprimentos se comparam entre si, mas não se lê
um valor no eixo. O valor está no `multi/correlation_circle`.

### Cuidados

- O sinal dos componentes é arbitrário: o gráfico pode sair espelhado em outro
  programa, e é o mesmo gráfico.
- A proporção padrão é 1:1 de propósito: esticar um eixo muda os ângulos, e os
  ângulos são a leitura.
- **Rótulo** com muitas observações: os rótulos que se sobreporiam são omitidos
  (o ponto fica).
]---", r"---[
- **Componente X** e **Componente Y** — os números dos componentes do plano
  (1 e 2 por padrão). Têm de ser diferentes.
- **Cor por** — coluna da tabela ORIGINAL, que pode não ter entrado na PCA.
  Texto ou fator dá cores discretas; número, degradê. Em branco, uma cor só.
- **Rótulo** — coluna com o nome de cada observação (`nome` em USArrests). Em
  branco, sem rótulos.
- **Setas das variáveis** — desenhar ou não as setas.
]---", r"---[
Um gráfico (`view/plot`). No console, um ggplot com os escores em `$data`.
]---", r"---[
tr_flow(reg) |>
  tr_add("iris", "multi/example", dataset = "iris") |>
  tr_add("pca", "multi/pca", from = "iris") |>
  tr_add("bi", "multi/biplot", cor = "Species", from = "pca")
]---", r"---[
`multi/correlation_circle` para ler as setas com escala; `multi/pca_loadings`;
`view/points` para o gráfico de escores sem setas, ligado direto no `multi/pca`.
]---", grafico = TRUE)),

    trama::tr_node("multi/correlation_circle", fn = tr_multi_correlation_circle,
      label = "Círculo de correlações",
      category = "multi_pca", icon = trama::tr_icon("orbit"),
      description = "Correlação de cada variável com dois componentes, dentro do círculo unitário.",
      inputs = pca, outputs = G,
      params = .tr_multi_props(
        x = I(1L, min = 1L, label = "Componente X"),
        y = I(2L, min = 1L, label = "Componente Y"),
        .aspecto = "1:1"),
      help = .tr_multi_ajuda(r"---[
Cada seta é uma variável, com a ponta em (correlação com o componente X,
correlação com o componente Y). É a metade "variáveis" do biplot, mas com
escala: o eixo vai de −1 a 1 e se lê o valor.

### Como ler

- A seta nunca sai do círculo de raio 1: a soma dos quadrados das correlações
  de uma variável com TODOS os componentes é 1.
- O quanto a ponta chega perto do círculo é o quanto o plano representa a
  variável (o cos², que é a cor: a soma dos quadrados das duas coordenadas).
  Uma seta dentro do círculo pontilhado (raio 0,5) tem menos de 25% da
  variância mostrada aqui.
- Duas setas LONGAS com ângulo pequeno são variáveis correlacionadas; opostas,
  correlacionadas negativamente; a 90°, independentes. Para setas curtas o
  ângulo não diz nada — a variável mora em outro plano.
- Nomear o componente: veja que setas estão perto do eixo dele, e de que lado.

A leitura só vale sem distorção, por isso a figura é sempre quadrada e com a
mesma escala nos dois eixos. O sinal de cada componente é arbitrário: um eixo
inteiro espelhado é a mesma solução.
]---", r"---[
- **Componente X** e **Componente Y** — os números dos componentes do plano.
  Têm de ser diferentes; o plano CP1 × CP3 às vezes mostra a variável que o
  CP1 × CP2 esconde.
]---", r"---[
Um gráfico (`view/plot`). No console, um ggplot com `variavel`, `cx`, `cy` e
`cos2` em `$data`.
]---", r"---[
tr_flow(reg) |>
  tr_add("ua", "multi/example", dataset = "USArrests") |>
  tr_add("pca", "multi/pca", from = "ua") |>
  tr_add("circ", "multi/correlation_circle", x = 1L, y = 2L, from = "pca")
]---", r"---[
`multi/pca_loadings` com tipo `correlações`, os mesmos números em tabela;
`multi/biplot`.
]---", grafico = TRUE))
  )
}
