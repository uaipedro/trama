# O mapa de calor da matriz de correlação: o primeiro gráfico antes de
# qualquer técnica da coleção.
#
# PCA e análise fatorial só têm o que resumir se as variáveis se
# correlacionam, e o mapa mostra isso de relance — blocos de cor forte são
# grupos de variáveis que andam juntas, e cada bloco é um candidato a
# componente ou a fator. Uma tabela de 24 × 24 números não se lê; o mesmo
# número em cor, ORDENADO para os blocos ficarem contíguos, se lê.

#' A ordem das variáveis por agrupamento hierárquico em 1 − |r|.
#'
#' Módulo, e não r: uma variável invertida (o `soc5` do questionário, carga
#' −0,60) pertence ao bloco do fator dela, e com 1 − r ela iria parar do outro
#' lado do mapa. Ligação média porque a completa quebra blocos grandes e a
#' simples encadeia tudo num bloco só.
#' @noRd
.tr_multi_ordem_correlacao <- function(r) {
  if (ncol(r) < 3L) return(colnames(r))
  h <- stats::hclust(stats::as.dist(1 - abs(r)), method = "average")
  colnames(r)[h$order]
}

#' Mapa de calor da matriz de correlação.
#' @param dados tabela.
#' @param cols variáveis; em branco, todas as numéricas.
#' @param ordenar reordenar as variáveis por agrupamento, para os blocos
#'   aparecerem.
#' @param valores escrever o valor de r em cada casa.
#' @export
tr_multi_plot_correlation <- function(dados, cols = "", ordenar = TRUE, valores = TRUE,
                                      aspecto = "1:1", tema = "padrão", titulo = "",
                                      rotulo_x = "", rotulo_y = "", legenda = "direita") {
  variaveis <- .tr_multi_variaveis(dados, cols)
  m <- .tr_multi_matriz(dados, variaveis, "multi/plot_correlation")
  # Sem `.tr_multi_correlacao()`: a matriz singular é justamente algo que se
  # quer VER aqui (um r = 1 fora da diagonal), e não um motivo para não desenhar.
  r <- stats::cor(m)
  ordem <- if (isTRUE(ordenar)) .tr_multi_ordem_correlacao(r) else colnames(r)
  p_vars <- length(ordem)
  d <- expand.grid(v1 = ordem, v2 = ordem, stringsAsFactors = FALSE)
  d$r <- r[cbind(d$v1, d$v2)]
  d$v1 <- factor(d$v1, levels = ordem)
  # Eixo Y invertido para a diagonal descer da esquerda para a direita, como a
  # matriz impressa: a primeira variável fica no alto.
  d$v2 <- factor(d$v2, levels = rev(ordem))
  # Casa escura com texto claro e casa clara com texto escuro: as cores das
  # casas são opacas, então o contraste não depende do tema do gráfico.
  d$texto <- ifelse(abs(d$r) > .6, "#ffffff", "#111827")
  d$rotulo <- formatC(d$r, format = "f", digits = 2, decimal.mark = ",")
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[["v1"]], y = .data[["v2"]], fill = .data[["r"]])) +
    ggplot2::geom_tile(colour = NA) +
    ggplot2::scale_fill_gradient2(low = "#2563eb", mid = "#f3f4f6", high = "#dc2626", midpoint = 0,
                                  limits = c(-1, 1), name = "r") +
    ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(angle = 45)) +
    ggplot2::coord_equal() +
    ggplot2::labs(x = NULL, y = NULL)
  if (isTRUE(valores)) {
    # Tamanho que cai com o número de variáveis: 24 × 24 casas com texto de
    # 4 mm viram borrão.
    tam <- max(1.6, min(4, 26 / p_vars))
    p <- p + ggplot2::geom_text(ggplot2::aes(label = .data[["rotulo"]], colour = .data[["texto"]]),
                                size = tam) +
      ggplot2::scale_colour_identity()
  }
  trama.view::tr_view_finish(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

#' A matriz como tabela: `variavel` e uma coluna por variável.
#' @noRd
.tr_multi_matriz_tabela <- function(M) {
  tibble::as_tibble(cbind(data.frame(variavel = rownames(M), stringsAsFactors = FALSE),
                          as.data.frame(M, row.names = NULL)))
}

#' Matriz de correlação ou de covariância, como tabela.
#'
#' Existe ao lado do mapa de calor porque o mapa é para LER e esta é para
#' LEVAR: exportar num `data/write_csv`, citar num relatório, conferir uma casa
#' com mais casas decimais do que o gráfico escreve.
#'
#' Covariância só de Pearson, e é recusa, não troca silenciosa: o `cov()` do R
#' aceita `method = "kendall"` e devolve um número, mas é covariância dos
#' POSTOS, sem unidade nenhuma das variáveis — ninguém que pede "covariância"
#' quer aquilo.
#'
#' Com grupo, sai uma matriz por grupo empilhada, com a coluna `grupo` na
#' frente. A matriz COMBINADA dentro dos grupos (a que a discriminante linear e
#' o M de Box usam) vem por último, e só em Pearson: combinar as somas de
#' produtos cruzados é conta de momentos, e não existe para postos.
#' @param dados tabela.
#' @param cols variáveis; em branco, todas as numéricas (menos o grupo).
#' @param matriz `"correlação"` ou `"covariância"`.
#' @param metodo `"pearson"`, `"spearman"` ou `"kendall"`.
#' @param grupo coluna do grupo; em branco, uma matriz só.
#' @export
tr_multi_correlation_matrix <- function(dados, cols = "", matriz = "correlação",
                                        metodo = "pearson", grupo = "") {
  no <- "multi/correlation_matrix"
  matriz <- .tr_multi_enum(matriz, c("correlação", "covariância"), "matriz")
  metodo <- .tr_multi_enum(metodo, c("pearson", "spearman", "kendall"), "metodo")
  if (matriz == "covariância" && metodo != "pearson") {
    .tr_multi_abort("tr_multi_error_bad_option",
                    paste0("'%s': covariância só existe em Pearson. Com '%s' o resultado seria a ",
                           "covariância dos postos, sem a unidade das variáveis. Use matriz = ",
                           "'correlação' para %s."), no, metodo, metodo)
  }
  com_grupo <- nzchar(trimws(as.character(grupo)))
  gcol <- if (com_grupo) .tr_multi_col(dados, grupo, "grupo") else character()
  variaveis <- .tr_multi_variaveis(dados, cols, excluir = gcol)
  if (com_grupo && gcol %in% variaveis) {
    .tr_multi_abort("tr_multi_error_bad_option",
                    "'%s': a coluna do grupo (%s) não pode estar também entre as variáveis.", no, gcol)
  }
  X <- .tr_multi_matriz(dados, variaveis, no)
  calcula <- function(m) {
    if (matriz == "covariância") stats::cov(m) else stats::cor(m, method = metodo)
  }
  if (!com_grupo) return(.tr_multi_matriz_tabela(calcula(X)))

  g <- dados[[gcol]]
  if (anyNA(g)) {
    .tr_multi_abort("tr_multi_error_missing_values",
                    "'%s' não aceita faltantes, e %d linha(s) não têm grupo (coluna: %s). Ligue um 'data/drop_na' antes.",
                    no, sum(is.na(g)), gcol)
  }
  g <- droplevels(as.factor(g))
  .tr_multi_grupo_minimo(g, 3L, no, "Uma matriz por grupo pede ao menos 3 observações em cada.")
  blocos <- lapply(levels(g), function(l) {
    m <- X[g == l, , drop = FALSE]
    # Variável constante DENTRO de um grupo passa pelo `.tr_multi_matriz` (que
    # olha a tabela inteira) e faria o `cor()` devolver NA com um aviso que o
    # card não mostra. Aqui vira erro nomeando o grupo e a variável.
    dp <- apply(m, 2L, stats::sd)
    if (any(dp == 0)) {
      .tr_multi_abort("tr_multi_error_constant_variable",
                      "'%s': variável constante dentro do grupo '%s': %s.",
                      no, l, paste(variaveis[dp == 0], collapse = ", "))
    }
    cbind(tibble::tibble(grupo = l), .tr_multi_matriz_tabela(calcula(m)))
  })
  if (metodo == "pearson") {
    S <- .tr_multi_sscp(X, g)$W / (nrow(X) - nlevels(g))
    M <- if (matriz == "covariância") S else stats::cov2cor(S)
    blocos <- c(blocos, list(cbind(tibble::tibble(grupo = "combinada"), .tr_multi_matriz_tabela(M))))
  }
  tibble::as_tibble(do.call(rbind, blocos))
}

.tr_multi_nos_correlacao <- function() {
  list(
    trama::tr_node("multi/correlation_matrix", fn = tr_multi_correlation_matrix,
      label = "Matriz de correlação",
      category = "multi_diagnostico", icon = trama::tr_icon("table-2"),
      description = "Matriz de correlação ou de covariância como tabela, geral ou por grupo.",
      inputs = list(dados = "data/table"), outputs = list(out = "data/table"),
      params = list(
        cols = trama::tr_param_col("", label = "Variáveis", role = "numerica", multi = TRUE, example = "alcool, fenois_totais, flavonoides"),
        matriz = trama::tr_param_enum("correlação", c("correlação", "covariância"), label = "Matriz"),
        metodo = trama::tr_param_enum("pearson", c("pearson", "spearman", "kendall"), label = "Método"),
        grupo = trama::tr_param_col("", label = "Grupo", role = "categorica", example = "cultivar", suggest = FALSE)),
      help = .tr_multi_ajuda(r"---[
A matriz de correlação (ou de covariância) entre as variáveis, como TABELA: uma
linha e uma coluna por variável, com o nome da linha em `variavel`. É a matriz
que as técnicas da coleção usam por dentro, posta à vista para exportar num
`data/write_csv`, filtrar na coleção `data` ou conferir com todas as casas.

Para LER a matriz, o `multi/plot_correlation` é melhor: com mais de meia dúzia de
variáveis, uma tabela de números não se varre com o olho.

### Correlação ou covariância

- **correlação** — sem unidade, de −1 a 1. É a matriz da PCA padronizada e da
  análise fatorial.
- **covariância** — na unidade das variáveis (o produto delas). É a matriz da
  PCA não padronizada, e é por ela que uma variável de escala grande domina.
  A diagonal é a variância de cada uma.

### Método

- **pearson** — relação linear, a de todas as técnicas desta coleção.
- **spearman** — Pearson nos postos: mede relação monótona e resiste a ponto
  extremo.
- **kendall** — concordância de pares; mais lento, bom para amostra pequena ou
  escala ordinal curta (itens de 1 a 5).

Covariância só existe com **pearson**: com os outros métodos o nó para em
vermelho, porque a covariância dos postos não tem a unidade das variáveis.

### Por grupo

Com **Grupo** preenchido, sai uma matriz por grupo, empilhadas, e a coluna
`grupo` na frente. Cada grupo precisa de pelo menos 3 observações, e uma
variável constante dentro de um grupo para o nó nomeando os dois.

Em **pearson** vem ainda um último bloco, `combinada`: a matriz DENTRO dos
grupos, somando as variações de cada um em torno da própria média (divisor
n − g). É a matriz que a discriminante linear inverte e que o `multi/box_m`
compara com as de cada grupo. Com spearman ou kendall esse bloco não sai, porque
combinar grupos é conta de momentos e não existe para postos.

Comparar a matriz de cada grupo com a combinada é o jeito de VER o que o M de Box
testa: se elas diferem muito, a discriminante quadrática faz mais sentido.

### Faltantes

Este nó não aceita faltantes, como toda a coleção: o nó para dizendo quantas
linhas têm, em vez de calcular cada casa com um número diferente de
observações. Ligue um `data/drop_na` antes.
]---", r"---[
- **Variáveis** — colunas numéricas, separadas por vírgula. Em branco, todas as
  numéricas (menos a do grupo).
- **Matriz** — `correlação` ou `covariância`.
- **Método** — `pearson`, `spearman` ou `kendall`.
- **Grupo** — coluna que separa os grupos. Em branco, uma matriz só.
]---", r"---[
Uma tabela (`data/table`): `variavel` e uma coluna por variável. Com grupo, a
coluna `grupo` na frente e um bloco de linhas por grupo, mais o bloco
`combinada` em Pearson.
]---", r"---[
tr_flow(reg) |>
  tr_add("v", "multi/example", dataset = "vinhos") |>
  tr_add("cov", "multi/correlation_matrix", matriz = "covariância", grupo = "cultivar", from = "v")
]---", r"---[
`multi/plot_correlation` para ver a matriz em cor; `multi/box_m` para testar se
as covariâncias dos grupos são iguais; `multi/pca` e `multi/factor_analysis`,
que partem desta matriz.
]---")),


    trama::tr_node("multi/plot_correlation", fn = tr_multi_plot_correlation,
      label = "Mapa de correlações",
      category = "multi_diagnostico", icon = trama::tr_icon("grid-3x3"),
      description = "Mapa de calor da matriz de correlação, ordenado para mostrar os blocos.",
      inputs = list(dados = "data/table"), outputs = list(out = "view/plot"),
      params = .tr_multi_props(
        cols = trama::tr_param_col("", label = "Variáveis", role = "numerica", multi = TRUE, example = "ans1, ans2, soc1, soc2"),
        ordenar = trama::tr_param_bool(TRUE, label = "Ordenar por agrupamento"),
        valores = trama::tr_param_bool(TRUE, label = "Mostrar valores"),
        .aspecto = "1:1"),
      help = .tr_multi_ajuda(r"---[
A matriz de correlação de Pearson entre as variáveis, em cor: vermelho é
correlação positiva, azul é negativa, e quanto mais forte a cor, mais perto de
±1. A diagonal é sempre 1 (a variável com ela mesma).

É o gráfico para olhar ANTES de uma PCA ou de uma análise fatorial. As duas
técnicas resumem o que as variáveis têm em COMUM; se quase tudo aqui é cinza
claro, não há o que resumir, e o resultado vai ser um componente por variável.

### Como ler

- **Blocos** de cor forte ao longo da diagonal são grupos de variáveis que
  andam juntas. Cada bloco costuma virar um fator (ou pesar num mesmo
  componente). No `questionario`, com a ordenação ligada, aparecem três blocos:
  ansiedade, sociabilidade e organização.
- **Azul dentro de um bloco** é item invertido: `soc5` ("prefiro ficar
  sozinho") correlaciona negativamente com os outros itens de sociabilidade, e
  mesmo assim pertence a eles.
- **Casa fora da diagonal muito perto de ±1** é variável redundante (um total,
  uma mesma medida em duas unidades): a análise fatorial vai recusar a matriz
  singular.
- **Variável sem cor com ninguém** não compartilha nada com as outras; numa
  análise fatorial ela terá comunalidade baixa.

### Ordenar

Com **Ordenar por agrupamento**, as variáveis são reordenadas por um
agrupamento hierárquico (ligação média) na distância 1 − |r|: as que se
correlacionam forte — em qualquer sinal — ficam vizinhas, e os blocos
aparecem. Desligado, a ordem é a das colunas da tabela, que é a certa quando a
ordem já significa algo (itens na ordem do questionário).

### Cuidados

- Pearson mede relação LINEAR. Uma relação curva forte pode sair perto de
  zero; na dúvida, olhe o disperso das duas no `view/points`.
- Um ponto extremo pode criar (ou destruir) uma correlação sozinho.
- O mapa não diz se a correlação é significativa. Para saber se a matriz
  inteira serve para fatorar, `multi/kmo_bartlett`.
]---", r"---[
- **Variáveis** — colunas numéricas, separadas por vírgula. Em branco, todas as
  numéricas.
- **Ordenar por agrupamento** — reordenar para os blocos ficarem contíguos.
- **Mostrar valores** — escrever o r em cada casa. Com muitas variáveis o texto
  fica pequeno; desligue para ver só as cores.
]---", r"---[
Um gráfico (`view/plot`). No console, um ggplot com `v1`, `v2` e `r` em `$data`.
Faltantes e variáveis constantes param o nó com a coluna nomeada.
]---", r"---[
tr_flow(reg) |>
  tr_add("q", "multi/example", dataset = "questionario") |>
  tr_add("mapa", "multi/plot_correlation", ordenar = TRUE, valores = FALSE, from = "q")
]---", r"---[
`multi/kmo_bartlett` para saber se a matriz serve para fatorar; `multi/pca` e
`multi/factor_analysis` para resumir os blocos.
]---", grafico = TRUE))
  )
}
