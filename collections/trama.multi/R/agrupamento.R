# Agrupamento: dissimilaridade entre indivíduos, agrupamento hierárquico e
# k-means, dendrograma e o método de otimização de Tocher.
#
# Em agrárias é a análise de DIVERSIDADE GENÉTICA: genótipos medidos em vários
# caracteres, uma matriz de distância entre eles (euclidiana padronizada ou D²
# de Mahalanobis), UPGMA com a correlação cofenética no rodapé e o Tocher ao
# lado. As peças são separadas porque a mesma matriz alimenta as duas técnicas
# de agrupamento, e refazer a distância para cada uma seria o convite para
# compará-las em matrizes diferentes sem perceber.
#
# Tudo em `stats` (e `cluster::daisy` para Gower, que vem com o R): o Tocher
# são vinte linhas e fica à mão, conferido contra um exemplo resolvido nos testes.

.TR_MULTI_DISTANCIAS <- c("euclidiana padronizada", "euclidiana", "mahalanobis", "gower")
.TR_MULTI_LIGACOES <- c(UPGMA = "average", Ward.D2 = "ward.D2", completo = "complete",
                        simples = "single")

# ---- O tipo `multi/dist` --------------------------------------------------------
#
# Tipo próprio, e não `data/table`: a matriz carrega o MÉTODO (Ward sobre D² de
# Mahalanobis não é o mesmo que sobre euclidiana, e o card do agrupamento diz
# qual foi) e a tabela de origem (o agrupamento devolve os dados com a coluna do
# grupo mesmo quando partiu da matriz). Numa tabela larga de n × n os dois se
# perderiam. O adaptador para `data/table` dá a matriz larga para exportar.

.TR_MULTI_CAMPOS_DIST <- c("d", "metodo", "variaveis", "dados", "rotulos")

#' Monta o objeto da matriz de distância.
#'
#' - `d`: o `dist` do R, com `Labels` = `rotulos`.
#' - `metodo`: um de `.TR_MULTI_DISTANCIAS`.
#' - `variaveis`: as colunas que entraram; `dados`: a tabela inteira.
#' - `rotulos`: o nome de cada linha (a coluna `rotulo`, ou `1..n`).
#' @noRd
.tr_multi_dist_obj <- function(d, metodo, variaveis, dados, rotulos) {
  structure(list(d = d, metodo = metodo, variaveis = variaveis, dados = dados, rotulos = rotulos),
            class = "tr_multi_dist")
}

multi_dist_type <- function() {
  trama::tr_type(
    "multi/dist", version = 1L, label = "Matriz de distância", color = "#fb923c", ext = "rds",
    store = function(x, path) {
      .tr_multi_guard(x, "tr_multi_dist", .TR_MULTI_CAMPOS_DIST, "tr_multi_error_not_a_dist",
                      "uma matriz de distância")
      saveRDS(x, path, compress = FALSE)
    },
    restore = function(path) readRDS(path),
    summary = function(x) {
      v <- as.vector(x$d)
      list(metodo = x$metodo, individuos = length(x$rotulos), variaveis = length(x$variaveis),
           minima = min(v), maxima = max(v))
    },
    # O card é o MAPA DE CALOR, ordenado pelo UPGMA para os grupos aparecerem
    # como blocos escuros na diagonal — é a primeira leitura da diversidade.
    preview = function(x, ctx) trama.view::tr_view_render(.tr_multi_mapa_distancia(x), ctx)
  )
}

.TR_MULTI_CAMPOS_CLUSTER <- c("metodo", "arvore", "kmeans", "grupos", "k", "cofenetica",
                              "distancia")

#' Monta o objeto do agrupamento.
#'
#' - `metodo`: `"UPGMA"`, `"Ward.D2"`, `"completo"`, `"simples"` ou `"k-means"`.
#' - `arvore`: o `hclust` (NULL no k-means); `kmeans`: o `kmeans` (NULL no
#'   hierárquico).
#' - `grupos`: inteiro por linha, na ordem dos dados; `k`: quantos grupos.
#' - `cofenetica`: correlação entre a distância original e a cofenética (NA no
#'   k-means, que não tem árvore).
#' - `distancia`: o `multi/dist` de onde partiu (montado aqui quando veio da
#'   tabela), que carrega os dados e os rótulos.
#' @noRd
.tr_multi_cluster_obj <- function(...) {
  structure(list(...)[.TR_MULTI_CAMPOS_CLUSTER], class = "tr_multi_cluster")
}

multi_cluster_type <- function() {
  trama::tr_type(
    "multi/cluster", version = 1L, label = "Agrupamento", color = "#34d399", ext = "rds",
    store = function(x, path) {
      .tr_multi_guard(x, "tr_multi_cluster", .TR_MULTI_CAMPOS_CLUSTER,
                      "tr_multi_error_not_a_cluster", "um agrupamento")
      saveRDS(x, path, compress = FALSE)
    },
    restore = function(path) readRDS(path),
    summary = function(x) {
      list(metodo = x$metodo, distancia = x$distancia$metodo, grupos = x$k,
           tamanhos = paste(tabulate(x$grupos, x$k), collapse = ", "),
           cofenetica = if (is.na(x$cofenetica)) NA_real_ else round(x$cofenetica, 3))
    },
    # O card do hierárquico é o DENDROGRAMA com o corte; o do k-means, que não
    # tem árvore, os grupos no plano das duas primeiras componentes.
    preview = function(x, ctx) trama.view::tr_view_render(tr_multi_plot_dendrogram(x), ctx)
  )
}

#' Adaptador `multi/dist` → `data/table`: a matriz larga, `rotulo` na frente.
#' @noRd
.tr_multi_dist_tabela <- function(x) {
  M <- as.matrix(x$d)
  tibble::as_tibble(cbind(data.frame(rotulo = x$rotulos, stringsAsFactors = FALSE),
                          as.data.frame(M, row.names = NULL, optional = TRUE)))
}

#' O nome da coluna nova sem pisar numa existente.
#' @noRd
.tr_multi_nome_livre <- function(nome, existentes) {
  if (!nome %in% existentes) return(nome)
  i <- 2L
  while (paste0(nome, "_", i) %in% existentes) i <- i + 1L
  paste0(nome, "_", i)
}

#' Adaptador `multi/cluster` → `data/table`: os dados com a coluna `grupo`.
#'
#' É o que leva o resultado adiante — colorir um `view/points`, fazer a média
#' dos caracteres por grupo num `data/group_summarise`. O grupo sai como fator (G1,
#' G2...) porque número de grupo não é medida: um `view/points` com `cor = grupo`
#' pintaria um degradê.
#' @noRd
.tr_multi_cluster_tabela <- function(x) {
  d <- as.data.frame(x$distancia$dados)
  nome <- .tr_multi_nome_livre("grupo", names(d))
  d[[nome]] <- factor(paste0("G", x$grupos), levels = paste0("G", seq_len(x$k)))
  tibble::as_tibble(d)
}

# ---- Distância ---------------------------------------------------------------------

#' Os rótulos das linhas: a coluna `rotulo`, ou a posição.
#'
#' Rótulo repetido é erro: dois genótipos "BRS 1" no dendrograma não se
#' distinguem, e o `as.matrix(dist)` indexaria o primeiro nas duas vezes.
#' @noRd
.tr_multi_rotulos <- function(dados, rotulo, no) {
  if (!nzchar(trimws(as.character(rotulo)[1L] |> .tr_multi_ou("")))) return(as.character(seq_len(nrow(dados))))
  col <- .tr_multi_col(dados, rotulo, "rotulo")
  r <- as.character(dados[[col]])
  if (anyNA(r) || anyDuplicated(r)) {
    .tr_multi_abort("tr_multi_error_bad_option",
                    "'%s': a coluna de rótulos (%s) precisa de um nome por linha, sem faltante nem repetido%s.",
                    no, col, if (anyDuplicated(r)) sprintf(" (repetido: %s)", r[anyDuplicated(r)]) else "")
  }
  r
}

.tr_multi_ou <- function(a, b) if (is.null(a) || !length(a) || is.na(a[[1]])) b else a

#' A matriz de distância como função de console.
#' @noRd
.tr_multi_calcula_dist <- function(dados, cols, metodo, rotulo, no) {
  rotulos <- .tr_multi_rotulos(dados, rotulo, no)
  rcol <- if (nzchar(trimws(as.character(rotulo)[1L] |> .tr_multi_ou("")))) trimws(rotulo) else character()
  if (metodo == "gower") {
    # Gower aceita coluna de texto e fator: é para isso que existe (caracteres
    # qualitativos e quantitativos juntos, como cor da flor e altura).
    nomes <- .tr_multi_split(cols)
    if (!length(nomes)) nomes <- setdiff(names(dados), rcol)
    faltam <- setdiff(nomes, names(dados))
    if (length(faltam)) {
      .tr_multi_abort("tr_multi_error_unknown_column",
                      "Param 'cols': coluna inexistente: %s. Disponíveis: %s.",
                      paste(faltam, collapse = ", "), paste(names(dados), collapse = ", "))
    }
    X <- as.data.frame(dados)[, nomes, drop = FALSE]
    X[] <- lapply(X, function(v) if (is.character(v) || is.logical(v)) factor(v) else v)
    incompletas <- !stats::complete.cases(X)
    if (any(incompletas)) {
      .tr_multi_abort("tr_multi_error_missing_values",
                      "'%s' não aceita faltantes, e %d linha(s) têm. Ligue um 'data/drop_na' antes.",
                      no, sum(incompletas))
    }
    d <- stats::as.dist(as.matrix(cluster::daisy(X, metric = "gower")))
    variaveis <- nomes
  } else {
    variaveis <- .tr_multi_variaveis(dados, cols, minimo = 1L, excluir = rcol)
    X <- .tr_multi_matriz(dados, variaveis, no)
    d <- switch(metodo,
      "euclidiana" = stats::dist(X),
      "euclidiana padronizada" = stats::dist(scale(X)),
      "mahalanobis" = {
        # D² entre pares: (xᵢ − xⱼ)' S⁻¹ (xᵢ − xⱼ), com S a covariância das
        # linhas. Pela Cholesky S = R'R, é a euclidiana AO QUADRADO em X R⁻¹ —
        # sem inverter S nem laço sobre os n² pares. Sai o D² (e não a raiz)
        # porque é ele que as teses de melhoramento reportam e agrupam.
        if (length(variaveis) > 1L) .tr_multi_correlacao(X, no)
        stats::dist(X %*% solve(chol(stats::cov(X))))^2
      })
  }
  attr(d, "Labels") <- rotulos
  .tr_multi_dist_obj(d, metodo, variaveis, dados, rotulos)
}

#' Matriz de dissimilaridade entre as linhas da tabela.
#' @param dados tabela.
#' @param cols variáveis; em branco, todas as numéricas (na Gower, todas).
#' @param metodo `"euclidiana padronizada"`, `"euclidiana"`, `"mahalanobis"`
#'   (D²) ou `"gower"`.
#' @param rotulo coluna com o nome de cada linha; em branco, o número da linha.
#' @return objeto `tr_multi_dist` (tipo `multi/dist`).
#' @export
tr_multi_distance <- function(dados, cols = "", metodo = "euclidiana padronizada", rotulo = "") {
  metodo <- .tr_multi_enum(metodo, .TR_MULTI_DISTANCIAS, "metodo")
  .tr_multi_calcula_dist(dados, cols, metodo, rotulo, "multi/distance")
}

#' O mapa de calor da matriz, ordenado pelo UPGMA.
#' @noRd
.tr_multi_mapa_distancia <- function(x) {
  M <- as.matrix(x$d)
  n <- nrow(M)
  ordem <- if (n >= 3L) x$rotulos[stats::hclust(x$d, "average")$order] else x$rotulos
  d <- expand.grid(i = ordem, j = ordem, stringsAsFactors = FALSE)
  d$v <- M[cbind(match(d$i, x$rotulos), match(d$j, x$rotulos))]
  d$i <- factor(d$i, levels = ordem)
  d$j <- factor(d$j, levels = rev(ordem))
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[["i"]], y = .data[["j"]], fill = .data[["v"]])) +
    ggplot2::geom_tile(colour = NA) +
    # Escuro = PERTO: o bloco escuro na diagonal é o grupo de parecidos.
    ggplot2::scale_fill_gradient(low = "#1e3a8a", high = "#f3f4f6",
                                 name = if (x$metodo == "mahalanobis") "D²" else "d") +
    ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(angle = 90)) +
    ggplot2::coord_equal() +
    ggplot2::labs(x = NULL, y = NULL, subtitle = sprintf("Distância %s, %d indivíduos", x$metodo, n))
  # Com muitos indivíduos os nomes viram borrão: some o texto, fica a cor.
  if (n > 40L) p <- p + ggplot2::theme(axis.text = ggplot2::element_blank())
  trama.view::tr_view_finish(p, "1:1", "padrão", "", "", "", "direita")
}

# ---- Agrupamento ---------------------------------------------------------------------

#' Agrupamento hierárquico ou k-means.
#' @param dados tabela (ligue esta OU a distância).
#' @param distancia matriz de `multi/distance` (ligue esta OU os dados).
#' @param cols variáveis quando parte da tabela; em branco, as numéricas.
#' @param padronizar padronizar as variáveis (média 0, desvio 1) quando parte da
#'   tabela.
#' @param rotulo coluna com o nome de cada linha, quando parte da tabela.
#' @param metodo `"UPGMA"`, `"Ward.D2"`, `"completo"`, `"simples"` ou `"k-means"`.
#' @param grupos quantos grupos formar (k do corte, ou do k-means).
#' @param .seed semente (a do nó). `NULL` usa o RNG atual.
#' @return objeto `tr_multi_cluster` (tipo `multi/cluster`).
#' @export
tr_multi_cluster <- function(dados = NULL, distancia = NULL, cols = "", padronizar = TRUE,
                             rotulo = "", metodo = "UPGMA", grupos = 3L, .seed = NULL) {
  no <- "multi/cluster"
  metodo <- .tr_multi_enum(metodo, c(names(.TR_MULTI_LIGACOES), "k-means"), "metodo")
  if (is.null(dados) == is.null(distancia)) {
    .tr_multi_abort("tr_multi_error_bad_input",
                    "'%s': ligue a tabela (entrada 'dados') OU uma matriz de 'multi/distance' (entrada 'distancia'), uma das duas.",
                    no)
  }
  if (!is.null(distancia)) {
    .tr_multi_guard(distancia, "tr_multi_dist", .TR_MULTI_CAMPOS_DIST, "tr_multi_error_not_a_dist",
                    "uma matriz de distância")
    if (metodo == "k-means") {
      # O k-means move CENTRÓIDES no espaço das variáveis; uma matriz de
      # distância não tem espaço onde pôr um centróide.
      .tr_multi_abort("tr_multi_error_bad_option",
                      "'%s': o k-means precisa das variáveis, e não de uma matriz de distância. Ligue a tabela em 'dados', ou escolha um método hierárquico.",
                      no)
    }
    dist <- distancia
  } else {
    dist <- .tr_multi_calcula_dist(dados, cols, if (isTRUE(padronizar)) "euclidiana padronizada" else "euclidiana",
                                   rotulo, no)
  }
  n <- length(dist$rotulos)
  if (n < 3L) {
    .tr_multi_abort("tr_multi_error_too_few_rows", "'%s' precisa de pelo menos 3 indivíduos, e há %d.", no, n)
  }
  k <- .tr_multi_int(grupos, "grupos", min = 1, max = n)

  if (metodo == "k-means") {
    X <- .tr_multi_matriz(dist$dados, dist$variaveis, no)
    if (isTRUE(padronizar)) X <- scale(X)
    # 25 partidas: o k-means acha um ótimo LOCAL, e uma partida só muda de
    # resposta a cada semente. Com 25 a melhor quase sempre é a mesma.
    ajusta <- function() stats::kmeans(X, centers = k, nstart = 25L, iter.max = 100L)
    km <- .tr_multi_ajustar(if (is.null(.seed)) ajusta() else .tr_multi_com_semente(.seed, ajusta()), no)
    # Grupos renumerados pela ordem em que aparecem nas linhas: sem isso o G1
    # de uma semente é o G3 da outra, com a mesma partição.
    g <- match(km$cluster, unique(km$cluster))
    return(.tr_multi_cluster_obj(metodo = metodo, arvore = NULL, kmeans = km, grupos = g, k = k,
                                 cofenetica = NA_real_, distancia = dist))
  }

  # O `ward.D2` eleva a distância ao quadrado por dentro; sobre o D² de
  # Mahalanobis isso daria D⁴. Para Ward entra a raiz (D, a distância
  # euclidiana nos dados transformados); os outros métodos usam o D², como nas
  # teses. A cofenética compara com a matriz que de fato entrou.
  dd <- if (metodo == "Ward.D2" && dist$metodo == "mahalanobis") sqrt(dist$d) else dist$d
  h <- stats::hclust(dd, method = .TR_MULTI_LIGACOES[[metodo]])
  # Correlação cofenética: quanto a árvore preserva as distâncias originais.
  # Acima de 0,7 a árvore é considerada boa representação (Sokal e Rohlf 1962),
  # e é o número que toda tese de diversidade põe abaixo do dendrograma.
  cof <- stats::cor(as.vector(stats::cophenetic(h)), as.vector(dd))
  g <- stats::cutree(h, k = k)
  g <- match(g, unique(g[h$order]))  # G1 é o grupo da esquerda do dendrograma
  .tr_multi_cluster_obj(metodo = metodo, arvore = h, kmeans = NULL, grupos = unname(g), k = k,
                        cofenetica = cof, distancia = dist)
}

#' As coordenadas dos segmentos de um `hclust`, com o grupo de cada um.
#'
#' Feito à mão (e não `ggdendro`) para não trazer dependência por quarenta
#' linhas. Um ramo leva a cor do grupo quando todas as folhas abaixo dele são
#' do mesmo grupo; os ramos acima do corte ficam cinza.
#' @noRd
.tr_multi_segmentos <- function(h, g) {
  n <- length(h$order)
  pos <- integer(n); pos[h$order] <- seq_len(n)
  xs <- numeric(n - 1L); gs <- integer(n - 1L)
  seg <- vector("list", n - 1L)
  no_info <- function(i) {
    if (i < 0) list(x = pos[-i], h = 0, g = g[-i]) else list(x = xs[i], h = h$height[i], g = gs[i])
  }
  for (i in seq_len(n - 1L)) {
    a <- no_info(h$merge[i, 1L]); b <- no_info(h$merge[i, 2L])
    xs[i] <- (a$x + b$x) / 2
    gs[i] <- if (!is.na(a$g) && !is.na(b$g) && a$g == b$g) a$g else NA_integer_
    hh <- h$height[i]
    seg[[i]] <- data.frame(x = c(a$x, b$x, a$x), xend = c(a$x, b$x, b$x),
                           y = c(a$h, b$h, hh), yend = c(hh, hh, hh),
                           g = c(a$g, b$g, gs[i]))
  }
  do.call(rbind, seg)
}

#' Dendrograma com o corte e as cores dos grupos.
#' @param agrupamento saída de `multi/cluster`.
#' @param grupos quantos grupos colorir; 0 usa os do agrupamento.
#' @param horizontal deitar a árvore (rótulos na vertical à esquerda), melhor
#'   com muitos indivíduos.
#' @inheritParams trama.view::tr_view_finish
#' @return ggplot.
#' @export
tr_multi_plot_dendrogram <- function(agrupamento, grupos = 0L, horizontal = FALSE, aspecto = "16:9",
                                     tema = "padrão", titulo = "", rotulo_x = "", rotulo_y = "",
                                     legenda = "direita") {
  no <- "multi/plot_dendrogram"
  .tr_multi_guard(agrupamento, "tr_multi_cluster", .TR_MULTI_CAMPOS_CLUSTER,
                  "tr_multi_error_not_a_cluster", "um agrupamento")
  x <- agrupamento
  if (is.null(x$arvore)) return(.tr_multi_plot_kmeans(x, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda))
  h <- x$arvore
  n <- length(h$order)
  k <- .tr_multi_int(grupos, "grupos", min = 0, max = n)
  g <- if (k == 0L) x$grupos else {
    gg <- stats::cutree(h, k = k); match(gg, unique(gg[h$order]))
  }
  if (k == 0L) k <- x$k
  s <- .tr_multi_segmentos(h, g)
  s$grupo <- factor(ifelse(is.na(s$g), NA, paste0("G", s$g)), levels = paste0("G", seq_len(k)))
  folhas <- data.frame(x = seq_len(n), rotulo = x$distancia$rotulos[h$order])
  # O corte passa no meio do vão entre a altura que junta k grupos em k − 1 e a
  # anterior: é a linha que, cruzada, dá exatamente os k grupos coloridos.
  alturas <- sort(h$height)
  corte <- if (k > 1L) mean(alturas[c(n - k, n - k + 1L)]) else NA_real_
  # Os ramos acima do corte (sem grupo) numa camada CINZA à parte: com o NA na
  # escala de cor, `na.translate = FALSE` os apagaria junto com a legenda.
  aes_seg <- ggplot2::aes(x = .data[["x"]], xend = .data[["xend"]], y = .data[["y"]], yend = .data[["yend"]])
  p <- ggplot2::ggplot(s[!is.na(s$grupo), ]) +
    ggplot2::geom_segment(mapping = aes_seg, data = s[is.na(s$grupo), ], colour = .TR_MULTI_CINZA,
                          linewidth = .5, lineend = "square") +
    ggplot2::geom_segment(ggplot2::aes(x = .data[["x"]], xend = .data[["xend"]], y = .data[["y"]],
                                       yend = .data[["yend"]], colour = .data[["grupo"]]),
                          linewidth = .5, lineend = "square") +
    ggplot2::scale_colour_discrete(name = "Grupo", drop = FALSE) +
    ggplot2::scale_x_continuous(breaks = folhas$x, labels = folhas$rotulo, expand = ggplot2::expansion(add = .6)) +
    ggplot2::labs(x = NULL, y = if (x$distancia$metodo == "mahalanobis") "D²" else "Distância",
                  subtitle = sprintf("%s · %s · cofenética %s", x$metodo, x$distancia$metodo,
                                     formatC(x$cofenetica, format = "f", digits = 3, decimal.mark = ",")))
  if (!is.na(corte)) {
    p <- p + ggplot2::geom_hline(yintercept = corte, linetype = "dashed", colour = .TR_MULTI_CINZA)
  }
  if (isTRUE(horizontal)) p <- p + ggplot2::coord_flip() + ggplot2::scale_y_reverse()
  p <- trama.view::tr_view_finish(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
  # O tema vem DEPOIS do finish, que reescreve o tema inteiro: os nomes das
  # folhas em pé (senão se sobrepõem) e sem a grade no eixo das folhas, que
  # com cinquenta indivíduos vira listra.
  folha <- if (isTRUE(horizontal)) "y" else "x"
  p <- p + if (isTRUE(horizontal)) {
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(), panel.grid.minor.y = ggplot2::element_blank())
  } else {
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = .5),
                   panel.grid.major.x = ggplot2::element_blank(), panel.grid.minor.x = ggplot2::element_blank())
  }
  if (n > 60L) p <- p + do.call(ggplot2::theme, stats::setNames(list(ggplot2::element_blank()), paste0("axis.text.", folha)))
  p
}

#' O k-means não tem árvore: os grupos no plano CP1 × CP2 das variáveis que
#' entraram (padronizadas como no agrupamento).
#' @noRd
.tr_multi_plot_kmeans <- function(x, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda) {
  X <- .tr_multi_matriz(x$distancia$dados, x$distancia$variaveis, "multi/plot_dendrogram")
  pc <- stats::prcomp(X, scale. = TRUE)
  sc <- pc$x[, seq_len(min(2L, ncol(pc$x))), drop = FALSE]
  if (ncol(sc) < 2L) sc <- cbind(sc, 0)
  prop <- pc$sdev^2 / sum(pc$sdev^2)
  d <- data.frame(CP1 = sc[, 1], CP2 = sc[, 2], grupo = factor(paste0("G", x$grupos),
                                                                levels = paste0("G", seq_len(x$k))))
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[["CP1"]], y = .data[["CP2"]], colour = .data[["grupo"]])) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(colour = "Grupo", x = sprintf("CP1 (%s)", .tr_multi_pct(prop[1])),
                  y = if (length(prop) > 1L) sprintf("CP2 (%s)", .tr_multi_pct(prop[2])) else "",
                  subtitle = sprintf("k-means com %d grupos, no plano das componentes principais", x$k))
  trama.view::tr_view_finish(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

# ---- Tocher -------------------------------------------------------------------------

#' Otimização de Tocher (Rao 1952), como em Cruz, Regazzi e Carneiro.
#'
#' θ é a MAIOR das menores distâncias (a distância de cada indivíduo ao vizinho
#' mais próximo, e dessas a maior). Um grupo começa pelo par mais próximo entre
#' os que sobram e recebe, um de cada vez, o indivíduo com a menor soma de
#' distâncias aos membros, enquanto essa soma dividida pelo número de membros
#' (a distância MÉDIA do candidato ao grupo) não passar de θ. Quando ninguém
#' cabe, o grupo fecha e outro começa. Quem sobra sozinho é grupo de um.
#' @return lista com `grupos` (inteiro por indivíduo) e `theta`.
#' @noRd
.tr_multi_tocher <- function(D) {
  n <- nrow(D)
  diag(D) <- Inf
  theta <- max(apply(D, 1L, min))
  g <- rep(NA_integer_, n)
  livres <- seq_len(n)
  atual <- 0L
  while (length(livres)) {
    atual <- atual + 1L
    if (length(livres) == 1L) { g[livres] <- atual; break }
    sub <- D[livres, livres, drop = FALSE]
    par <- livres[arrayInd(which.min(sub), dim(sub))]
    # Par de abertura mais distante que θ não forma grupo: se nem os dois mais
    # próximos que sobraram cabem no critério, ninguém mais cabe, e cada um
    # vira grupo de um (Rao 1952; o `tocher()` "original" do biotools). Juntá-los
    # mesmo assim criava grupos com distância interna acima do próprio limite.
    if (D[par[1], par[2]] > theta) {
      g[livres] <- atual + seq_along(livres) - 1L
      break
    }
    membros <- par
    livres <- setdiff(livres, par)
    while (length(livres)) {
      soma <- colSums(D[membros, livres, drop = FALSE])
      j <- which.min(soma)
      if (soma[[j]] / length(membros) > theta) break
      membros <- c(membros, livres[[j]])
      livres <- livres[-j]
    }
    g[membros] <- atual
  }
  list(grupos = g, theta = theta)
}

#' Agrupamento de Tocher sobre uma matriz de distância.
#' @param distancia matriz de `multi/distance`.
#' @return tibble com uma linha por grupo: `grupo`, `n`, `membros`,
#'   `distancia_media` (média das distâncias entre os membros) e `theta`.
#' @export
tr_multi_tocher <- function(distancia) {
  no <- "multi/tocher"
  .tr_multi_guard(distancia, "tr_multi_dist", .TR_MULTI_CAMPOS_DIST, "tr_multi_error_not_a_dist",
                  "uma matriz de distância")
  D <- as.matrix(distancia$d)
  if (nrow(D) < 3L) {
    .tr_multi_abort("tr_multi_error_too_few_rows", "'%s' precisa de pelo menos 3 indivíduos, e há %d.",
                    no, nrow(D))
  }
  r <- .tr_multi_tocher(D)
  ks <- sort(unique(r$grupos))
  tibble::tibble(
    grupo = paste0("G", ks),
    n = vapply(ks, function(k) sum(r$grupos == k), 0L),
    membros = vapply(ks, function(k) paste(distancia$rotulos[r$grupos == k], collapse = ", "), ""),
    # Grupo de um não tem distância interna: NA, e não 0, que leria "idênticos".
    distancia_media = vapply(ks, function(k) {
      i <- which(r$grupos == k)
      if (length(i) < 2L) NA_real_ else mean(D[i, i][upper.tri(D[i, i])])
    }, 0),
    theta = r$theta)
}

# ---- Nós ---------------------------------------------------------------------------

.tr_multi_nos_agrupamento <- function() {
  P <- trama::tr_param
  TB <- "data/table"
  list(
    trama::tr_node("multi/distance", fn = tr_multi_distance,
      pressupostos = .tr_multi_doc("multi/distance")$pressupostos,
      referencias = .tr_multi_doc("multi/distance")$referencias, label = "Matriz de distância",
      category = "multi_agrupamento", icon = trama::tr_icon("ruler"),
      description = "Dissimilaridade entre as linhas: euclidiana, padronizada, D² de Mahalanobis ou Gower.",
      inputs = list(dados = TB), outputs = list(out = "multi/dist"),
      params = list(
        cols = trama::tr_param_col("", label = "Variáveis", role = "numerica", multi = TRUE, example = "Murder, Assault, UrbanPop, Rape"),
        metodo = trama::tr_param_enum("euclidiana padronizada", .TR_MULTI_DISTANCIAS, label = "Distância"),
        rotulo = trama::tr_param_col("", label = "Rótulo das linhas", role = "qualquer", suggest = FALSE, example = "nome")),
      help = .tr_multi_ajuda(r"---[
A distância (dissimilaridade) entre cada par de LINHAS da tabela — genótipos,
cultivares, acessos de um banco de germoplasma, municípios. Quanto maior, mais
diferentes os dois indivíduos nos caracteres medidos. É o ponto de partida do
`multi/cluster` e do `multi/tocher`, e o estudo de diversidade genética começa
aqui.

### Qual distância

- **euclidiana padronizada** (padrão) — cada variável vira média 0 e desvio 1
  antes: todas pesam igual, qualquer que seja a unidade. É a escolha quando os
  caracteres têm escalas diferentes (altura em cm, produção em kg/ha).
- **euclidiana** — nas unidades originais: a variável de escala grande domina.
  Só faz sentido com variáveis na mesma unidade.
- **mahalanobis** — o D² generalizado, (xᵢ − xⱼ)' S⁻¹ (xᵢ − xⱼ), com S a
  covariância entre as variáveis nas próprias linhas. Desconta a CORRELAÇÃO: dois
  caracteres que medem quase a mesma coisa não contam duas vezes. Sai como
  **D²** (e não a raiz), que é o que se reporta em melhoramento. Em experimento com
  repetições, o livro usa a covariância RESIDUAL; aqui é a das médias, então
  ligue a tabela de médias por genótipo e leia como D² descritivo.
- **gower** — para caracteres MISTOS: numéricos, fatores e texto juntos (cor da
  flor, hábito de crescimento e altura). Cada variável contribui de 0 a 1, e a
  distância é a média. Em branco, entram todas as colunas (menos o rótulo).

### Rótulo

A coluna com o nome de cada linha (o genótipo), escrita no mapa e no
dendrograma. Precisa de um nome por linha, sem repetir: com repetições no
experimento, resuma antes (média por genótipo, na coleção de dados) e ligue as médias.

### O card

O mapa de calor da matriz, com as linhas ordenadas pelo UPGMA: blocos ESCUROS
ao longo da diagonal são grupos de indivíduos parecidos.
]---", r"---[
- **Variáveis** — colunas numéricas, separadas por vírgula. Em branco, todas as
  numéricas (na Gower, todas as colunas).
- **Distância** — `euclidiana padronizada`, `euclidiana`, `mahalanobis` ou
  `gower`.
- **Rótulo das linhas** — coluna com o nome de cada linha. Em branco, o número
  da linha.
]---", r"---[
Uma matriz de distância (`multi/dist`). Ligada numa entrada de tabela, vira a
matriz larga: `rotulo` e uma coluna por indivíduo. Faltantes param o nó
(ligue um `data/drop_na`); o Mahalanobis recusa covariância singular.
]---", r"---[
tr_flow(reg) |>
  tr_add("usa", "multi/example", dataset = "USArrests") |>
  tr_add("d", "multi/distance", metodo = "mahalanobis", rotulo = "nome", from = "usa")
]---", r"---[
`multi/cluster` para o agrupamento hierárquico; `multi/tocher` para o método de
otimização; `multi/correlation_matrix` para a matriz entre VARIÁVEIS, e não
entre linhas.
]---")),

    trama::tr_node("multi/cluster", fn = tr_multi_cluster,
      pressupostos = .tr_multi_doc("multi/cluster")$pressupostos,
      referencias = .tr_multi_doc("multi/cluster")$referencias, label = "Agrupamento",
      category = "multi_agrupamento", icon = trama::tr_icon("network"),
      description = "Agrupamento hierárquico (UPGMA, Ward, completo, simples) ou k-means, com corte em k grupos.",
      inputs = list(dados = trama::tr_port(TB, required = FALSE),
                    distancia = trama::tr_port("multi/dist", required = FALSE)),
      outputs = list(out = "multi/cluster"),
      params = list(
        cols = trama::tr_param_col("", label = "Variáveis", role = "numerica", multi = TRUE, example = "Murder, Assault, UrbanPop, Rape"),
        padronizar = trama::tr_param_bool(TRUE, label = "Padronizar"),
        rotulo = trama::tr_param_col("", label = "Rótulo das linhas", role = "qualquer", suggest = FALSE, example = "nome"),
        metodo = trama::tr_param_enum("UPGMA", c(names(.TR_MULTI_LIGACOES), "k-means"), label = "Método"),
        grupos = trama::tr_param_int(3L, min = 1L, label = "Grupos")),
      help = .tr_multi_ajuda(r"---[
Junta os indivíduos em grupos de parecidos. Ligue UMA das duas entradas:

- **dados** — a tabela; a distância é a euclidiana nas **Variáveis**
  (padronizadas, com **Padronizar** ligado).
- **distancia** — uma matriz do `multi/distance`, para usar Mahalanobis ou
  Gower. Aí **Variáveis**, **Padronizar** e **Rótulo** são ignorados: valem os
  da matriz.

### Métodos hierárquicos

Juntam, passo a passo, os dois grupos mais próximos, até sobrar um; a árvore
(dendrograma) é o card. Diferem em como medir a distância entre GRUPOS:

- **UPGMA** (ligação média) — a média das distâncias entre os membros (sobre
  Mahalanobis, a média dos D², como nas teses). O mais
  usado em diversidade genética, e o que costuma dar a maior correlação
  cofenética.
- **Ward.D2** — junta os grupos que menos aumentam a soma de quadrados dentro
  deles; grupos compactos e de tamanhos parecidos. Supõe distância euclidiana:
  sobre Mahalanobis usa a raiz do D² (D), porque o Ward já eleva ao quadrado;
  sobre Gower é só aproximado, porque a Gower não é euclidiana.
- **completo** — a MAIOR distância entre membros: grupos compactos.
- **simples** — a MENOR distância (vizinho mais próximo): encadeia, e serve
  mais para achar indivíduos isolados do que para formar grupos.

### Correlação cofenética

A correlação entre a distância original de cada par e a altura em que o par
se junta na árvore. Mede quanto o dendrograma é fiel à matriz: acima de 0,7 é o
limiar usual (Sokal e Rohlf, 1962). Vai no subtítulo do card e no resumo.

### Grupos

**Grupos** é o k do corte: a linha tracejada do dendrograma passa na altura
que deixa exatamente k grupos, e cada linha da tabela ganha o seu. Escolher k
é olhar o dendrograma — um corte onde os ramos são LONGOS separa grupos
nítidos.

### k-means

Com **k-means**, os grupos saem da minimização da soma de quadrados dentro
deles, com k fixado de antemão, 25 partidas e a semente do nó (resultado
reprodutível). Só a partir da tabela. Não há árvore: o card mostra os grupos no
plano das duas primeiras componentes principais.
]---", r"---[
- **Variáveis** — colunas numéricas (quando parte da tabela). Em branco, todas
  as numéricas.
- **Padronizar** — média 0 e desvio 1 antes (quando parte da tabela).
- **Rótulo das linhas** — coluna com o nome de cada linha (quando parte da
  tabela).
- **Método** — `UPGMA`, `Ward.D2`, `completo`, `simples` ou `k-means`.
- **Grupos** — quantos grupos formar.
]---", r"---[
Um agrupamento (`multi/cluster`), com o dendrograma de card. Ligado numa
entrada de tabela, vira os dados com a coluna `grupo` (G1, G2...; G1 é o da
esquerda do dendrograma) — pronta para um `view/points` com `cor = grupo` ou um
resumo das médias por grupo na coleção de dados.
]---", r"---[
tr_flow(reg) |>
  tr_add("usa", "multi/example", dataset = "USArrests") |>
  tr_add("d", "multi/distance", rotulo = "nome", from = "usa") |>
  tr_add("ag", "multi/cluster", metodo = "UPGMA", grupos = 4L, from = "d")
]---", r"---[
`multi/distance` para escolher a distância; `multi/plot_dendrogram` para
recolorir com outro número de grupos; `multi/tocher` para o método de
otimização na mesma matriz.
]---")),

    trama::tr_node("multi/plot_dendrogram", role = "leitura", fn = tr_multi_plot_dendrogram,
      label = "Dendrograma",
      category = "multi_agrupamento", icon = trama::tr_icon("git-fork"),
      description = "Dendrograma do agrupamento, com a linha de corte e os ramos coloridos por grupo.",
      inputs = list(agrupamento = "multi/cluster"), outputs = list(out = "view/plot"),
      params = .tr_multi_props(
        grupos = trama::tr_param_int(0L, min = 0L, label = "Grupos (0 = os do agrupamento)"),
        horizontal = trama::tr_param_bool(FALSE, label = "Horizontal"),
        .aspecto = "16:9"),
      help = .tr_multi_ajuda(r"---[
A árvore do agrupamento hierárquico. Cada folha é um indivíduo; a ALTURA em que
dois ramos se juntam é a distância entre eles (no UPGMA, a média entre os
membros). Os ramos abaixo do corte levam a cor do grupo, e a linha tracejada é
o corte.

É o card do `multi/cluster`; este bloco existe para experimentar outro número
de **Grupos** sem refazer o agrupamento, e para deitar a árvore quando os
nomes são muitos.

### Como ler

- Ramos LONGOS logo abaixo do corte: grupos bem separados, corte bom.
- Folhas que se juntam muito alto, sozinhas: indivíduos divergentes, os
  candidatos a genitores em cruzamentos para ampliar a variabilidade.
- O subtítulo traz a correlação cofenética: abaixo de 0,7 a árvore distorce a
  matriz, e as alturas não devem ser lidas ao pé da letra.

Num k-means não há árvore: o gráfico mostra os grupos no plano das duas
primeiras componentes principais.
]---", r"---[
- **Grupos** — quantos grupos colorir. 0 usa os do agrupamento.
- **Horizontal** — deitar a árvore, com os nomes na vertical à esquerda.
]---", r"---[
Um gráfico (`view/plot`).
]---", r"---[
tr_flow(reg) |>
  tr_add("usa", "multi/example", dataset = "USArrests") |>
  tr_add("ag", "multi/cluster", rotulo = "nome", metodo = "Ward.D2", from = "usa") |>
  tr_add("dend", "multi/plot_dendrogram", grupos = 4L, horizontal = TRUE, from = "ag")
]---", r"---[
`multi/cluster` para o agrupamento; `multi/tocher` para outro método na mesma
matriz.
]---", grafico = TRUE)),

    trama::tr_node("multi/tocher", fn = tr_multi_tocher,
      pressupostos = .tr_multi_doc("multi/tocher")$pressupostos,
      referencias = .tr_multi_doc("multi/tocher")$referencias, label = "Tocher",
      category = "multi_agrupamento", icon = trama::tr_icon("boxes"),
      description = "Método de otimização de Tocher sobre uma matriz de distância: grupos e distância média.",
      inputs = list(distancia = "multi/dist"), outputs = list(out = TB),
      params = list(),
      help = .tr_multi_ajuda(r"---[
O método de otimização de Tocher (Rao, 1952), o agrupamento mais usado nos
estudos de diversidade genética no Brasil, ao lado do UPGMA. Não forma árvore:
dá os grupos direto, com o critério de que a distância MÉDIA dentro de cada
grupo não passe de um limite, θ.

### O algoritmo

1. θ é a maior das menores distâncias: para cada indivíduo, a distância ao
   vizinho mais próximo; θ é a maior delas.
2. O primeiro grupo começa pelo par mais próximo da matriz.
3. Entra no grupo o indivíduo com a menor soma de distâncias aos membros, se
   essa soma dividida pelo número de membros (a distância média dele ao grupo)
   não passar de θ. Repete-se até ninguém caber.
4. O grupo fecha, e o próximo começa pelo par mais próximo entre os que
   sobraram. Se a distância desse par já passa de θ, ninguém mais forma
   grupo: cada restante vira um grupo de um, e o método termina.

É o procedimento de Cruz, Regazzi e Carneiro (Modelos biométricos aplicados ao
melhoramento genético), o mesmo do programa Genes.

### Como ler

Os primeiros grupos são os maiores e os de indivíduos mais parecidos; os
últimos costumam ser de um ou dois indivíduos divergentes. Cruzamentos entre
genótipos de grupos DIFERENTES são os que prometem mais variabilidade.

Tocher e UPGMA na mesma matriz costumam concordar nos grupos grandes; onde
discordam, o dendrograma (`multi/plot_dendrogram`) mostra o porquê.
]---", r"---[
Nenhum: tudo vem da matriz de distância ligada.
]---", r"---[
Uma tabela (`data/table`) com uma linha por grupo: `grupo` (G1, G2...), `n`,
`membros` (os rótulos, separados por vírgula), `distancia_media` (média das
distâncias entre os membros; vazia em grupo de um) e `theta`, o limite usado.
]---", r"---[
tr_flow(reg) |>
  tr_add("usa", "multi/example", dataset = "USArrests") |>
  tr_add("d", "multi/distance", metodo = "mahalanobis", rotulo = "nome", from = "usa") |>
  tr_add("toc", "multi/tocher", from = "d")
]---", r"---[
`multi/distance` para a matriz; `multi/cluster` para o UPGMA na mesma matriz.
]---"))
  )
}
