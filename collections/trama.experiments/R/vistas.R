# As vistas do plano: um bloco só (`experiments/view`), com a aba como param.
#
# Um verbo, um bloco: as quatro abas leem o MESMO plano e respondem à mesma
# pergunta ("o que foi sorteado, e onde?") por quatro lados. Quatro nós
# fariam a paleta crescer sem ensinar nada a mais.

.TR_EXP_ABAS <- c("mapa", "hierarquia", "combinacoes", "ordem", "componentes")
.TR_EXP_POR <- c("tratamento", "unidade")

#' Nomes dos fatores de papel tratamento, na ordem declarada.
#' @noRd
.tr_exp_trat <- function(plano) {
  f <- plano$fatores
  nm <- f$nome[f$papel == "tratamento" & f$nome %in% names(plano$unidades)]
  # A sequência do crossover é sorteada ao indivíduo, mas o que se aplica em
  # cada período é o tratamento: o mapa pinta o tratamento.
  setdiff(nm, "sequencia")
}

#' Desenha uma aba do plano.
#' @param plano objeto `tr_experiments_plan`.
#' @param aba `mapa`, `hierarquia`, `combinacoes`, `ordem` ou `componentes`.
#' @param por na aba `componentes`: `tratamento` (média de cada termo por
#'   combinação de tratamentos) ou `unidade`.
#' @param aspecto,tema,titulo,rotulo_x,rotulo_y,legenda cosméticos da `view`.
#' @return um ggplot.
#' @export
tr_experiments_view <- function(plano, aba = "mapa", por = "tratamento", aspecto = "16:9", tema = "padrão", titulo = "",
                                rotulo_x = "", rotulo_y = "", legenda = "direita") {
  .tr_exp_plano_conferir(plano)
  aba <- .tr_exp_enum(aba, .TR_EXP_ABAS, "aba")
  p <- switch(aba, mapa = .tr_exp_vista_mapa(plano), hierarquia = .tr_exp_vista_hierarquia(plano),
              combinacoes = .tr_exp_vista_combinacoes(plano), ordem = .tr_exp_vista_ordem(plano),
              componentes = .tr_exp_vista_componentes(plano, .tr_exp_enum(por, .TR_EXP_POR, "por")))
  if (!nzchar(titulo)) titulo <- plano$rotulo
  trama.view::tr_view_finish(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

.tr_exp_vista_mapa <- function(plano) {
  u <- plano$unidades; g <- plano$geometria$posicoes
  trat <- .tr_exp_trat(plano)
  d <- data.frame(linha = g$linha, coluna = g$coluna, tratamento = .tr_exp_rotulo_trat(u, trat))
  # Com muitas combinações a legenda vira ruído; o rótulo na célula basta.
  cores <- length(unique(d$tratamento)) <= 24L
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[["coluna"]], y = .data[["linha"]])) +
    (if (cores) ggplot2::geom_tile(ggplot2::aes(fill = .data[["tratamento"]]), colour = "white", linewidth = .6)
     else ggplot2::geom_tile(fill = "grey88", colour = "white", linewidth = .6)) +
    ggplot2::geom_text(ggplot2::aes(label = .data[["tratamento"]]), size = 3) +
    ggplot2::scale_y_reverse(breaks = NULL) + ggplot2::scale_x_continuous(breaks = NULL) +
    ggplot2::coord_equal() +
    ggplot2::labs(x = plano$geometria$eixo_coluna, y = plano$geometria$eixo_linha,
                  fill = paste(trat, collapse = " × "))
  if ("bloco" %in% names(u) && plano$geometria$tipo == "campo") {
    # O contorno de cada bloco: é o que mostra o controle local no croqui.
    b <- do.call(rbind, lapply(split(data.frame(g, bloco = u$bloco, local = if ("local" %in% names(u)) u$local else 1),
                                     list(if ("local" %in% names(u)) u$local else 1, u$bloco), drop = TRUE),
                               function(x) data.frame(xmin = min(x$coluna) - .5, xmax = max(x$coluna) + .5,
                                                      ymin = min(x$linha) - .5, ymax = max(x$linha) + .5)))
    p <- p + ggplot2::geom_rect(data = b, ggplot2::aes(xmin = .data[["xmin"]], xmax = .data[["xmax"]],
                                                       ymin = .data[["ymin"]], ymax = .data[["ymax"]]),
                                inherit.aes = FALSE, fill = NA, colour = "grey20", linewidth = .8)
  }
  p
}

.tr_exp_vista_hierarquia <- function(plano) {
  h <- plano$hierarquia; f <- plano$fatores
  nh <- nrow(h)
  niveis <- data.frame(x = 0, y = -seq_len(nh), texto = sprintf("%s  (%d)", h$nivel, h$n))
  # Cada fator ao lado do nível em que é aplicado, com o mecanismo do sorteio.
  # Fator sem sorteio sai em cinza: ele não recebe leitura causal.
  alvo <- match(f$unidade, h$nivel)
  alvo[is.na(alvo)] <- nh
  f$y <- -alvo
  f$ord <- stats::ave(f$y, f$y, FUN = seq_along)
  f$sorteado <- ifelse(f$mecanismo == "sem sorteio", "sem sorteio", "sorteado")
  f$texto <- ifelse(f$mecanismo == "sem sorteio",
                    sprintf("%s · %s · sem sorteio", f$nome, f$papel),
                    sprintf("%s · %s · %s em: %s", f$nome, f$papel, f$mecanismo, f$escopo))
  f$x <- 1
  f$y <- f$y - (f$ord - 1) * .28
  ggplot2::ggplot() +
    ggplot2::geom_segment(data = data.frame(y = -seq_len(max(1L, nh - 1L)))[seq_len(nh - 1L), , drop = FALSE],
                          ggplot2::aes(x = 0, xend = 0, y = .data[["y"]] - .15, yend = .data[["y"]] - .85),
                          arrow = ggplot2::arrow(length = ggplot2::unit(.15, "cm"))) +
    ggplot2::geom_label(data = niveis, ggplot2::aes(x = .data[["x"]], y = .data[["y"]], label = .data[["texto"]]),
                        fill = .TR_EXP_COR, colour = "white", fontface = "bold") +
    ggplot2::geom_text(data = f, ggplot2::aes(x = .data[["x"]], y = .data[["y"]], label = .data[["texto"]],
                                              colour = .data[["sorteado"]]), hjust = 0, size = 3.2) +
    ggplot2::scale_colour_manual(values = c(sorteado = "grey10", "sem sorteio" = .TR_EXP_CINZA)) +
    ggplot2::scale_x_continuous(limits = c(-.6, 4), breaks = NULL) +
    ggplot2::scale_y_continuous(limits = c(-nh - .8, -.4), breaks = NULL) +
    ggplot2::labs(x = NULL, y = NULL, colour = NULL)
}

.tr_exp_vista_combinacoes <- function(plano) {
  u <- plano$unidades; trat <- .tr_exp_trat(plano)
  niv <- lapply(stats::setNames(trat, trat), function(nm) {
    v <- u[[nm]]; if (is.factor(v)) levels(v) else sort(unique(v))
  })
  # A grade COMPLETA dos níveis, contra a qual as réplicas são contadas: no
  # fracionado e no BIB, as células vazias são o próprio desenho.
  todas <- expand.grid(niv, stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
  chave_u <- do.call(paste, c(lapply(trat, function(nm) as.character(u[[nm]])), sep = "\r"))
  chave_t <- do.call(paste, c(lapply(trat, function(nm) as.character(todas[[nm]])), sep = "\r"))
  todas$n <- as.integer(table(factor(chave_u, levels = chave_t)))
  x <- trat[[1]]
  todas$x <- factor(as.character(todas[[x]]), levels = as.character(niv[[x]]))
  todas$y <- if (length(trat) > 1L) {
    do.call(paste, c(lapply(trat[-1], function(nm) as.character(todas[[nm]])), sep = ":"))
  } else "réplicas"
  todas$rot <- ifelse(todas$n == 0L, "vazia", as.character(todas$n))
  ggplot2::ggplot(todas, ggplot2::aes(x = .data[["x"]], y = .data[["y"]])) +
    ggplot2::geom_tile(ggplot2::aes(fill = .data[["n"]]), colour = "white", linewidth = .6) +
    ggplot2::geom_text(ggplot2::aes(label = .data[["rot"]]), size = 3.2) +
    ggplot2::scale_fill_gradient(low = "grey92", high = .TR_EXP_COR, limits = c(0, NA)) +
    ggplot2::labs(x = x, y = if (length(trat) > 1L) paste(trat[-1], collapse = " : ") else NULL, fill = "réplicas")
}

.tr_exp_vista_ordem <- function(plano) {
  u <- plano$unidades; trat <- .tr_exp_trat(plano)
  d <- data.frame(ordem = u$ordem, tratamento = .tr_exp_rotulo_trat(u, trat))
  ggplot2::ggplot(d, ggplot2::aes(x = .data[["ordem"]], y = .data[["tratamento"]])) +
    ggplot2::geom_path(ggplot2::aes(group = 1), colour = .TR_EXP_CINZA, linewidth = .3) +
    ggplot2::geom_point(colour = .TR_EXP_COR, size = 2.2) +
    ggplot2::labs(x = "ordem de execução", y = paste(trat, collapse = " : "))
}

#' A resposta decomposta: uma barra empilhada por termo (espelho do
#' `series/component`). O intercepto sai da pilha — é a mesma constante em
#' toda barra e esconderia o resto — e vai para o subtítulo. O ponto é a
#' resposta menos o intercepto: a soma da pilha, quando não há unidade perdida.
#' @noRd
.tr_exp_vista_componentes <- function(plano, por) {
  u <- plano$unidades
  cols <- grep("^\\.ef_", names(u), value = TRUE)
  if (!length(cols)) {
    .tr_experiments_abort("tr_experiments_error_bad_option",
                          "Aba 'componentes': o plano ainda não tem termos. Ligue 'experiments/effect' antes da vista.")
  }
  interc <- vapply(plano$termos, function(t) if (t$tipo == "intercepto") t$coluna else NA_character_, "")
  interc <- interc[!is.na(interc)]
  base <- if (length(interc)) rowSums(as.matrix(u[, interc, drop = FALSE])) else rep(0, nrow(u))
  pilha <- setdiff(cols, interc)
  if (!length(pilha)) pilha <- cols
  resp <- plano$resposta$nome
  grupo <- if (por == "unidade") factor(u$ordem) else factor(.tr_exp_rotulo_trat(u, .tr_exp_trat(plano)))
  longo <- do.call(rbind, lapply(pilha, function(cc) {
    data.frame(x = levels(grupo), termo = sub("^\\.ef_", "", cc),
               valor = as.numeric(tapply(u[[cc]], grupo, mean)), stringsAsFactors = FALSE)
  }))
  longo$x <- factor(longo$x, levels = levels(grupo))
  longo$termo <- factor(longo$termo, levels = sub("^\\.ef_", "", pilha))
  p <- ggplot2::ggplot(longo, ggplot2::aes(x = .data[["x"]], y = .data[["valor"]])) +
    ggplot2::geom_col(ggplot2::aes(fill = .data[["termo"]]), width = .8) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey30", linewidth = .3)
  if (!is.null(resp) && plano$resposta$distribuicao == "normal") {
    pt <- data.frame(x = levels(grupo), valor = as.numeric(tapply(u[[resp]] - base, grupo, mean, na.rm = TRUE)))
    pt$x <- factor(pt$x, levels = levels(grupo))
    p <- p + ggplot2::geom_point(data = pt, shape = 21, fill = "white", colour = "grey10", size = 2)
  }
  sub <- c(if (length(interc)) sprintf("intercepto = %s (fora da pilha)", format(signif(mean(base), 4))),
           if (!is.null(resp) && plano$resposta$distribuicao != "normal") "escala do preditor linear (ligação)",
           if (por == "tratamento") "média por combinação de tratamentos" else NULL,
           if (!is.null(resp) && plano$resposta$distribuicao == "normal") sprintf("ponto: %s − intercepto", resp))
  p + ggplot2::labs(x = if (por == "unidade") "unidade (ordem)" else paste(.tr_exp_trat(plano), collapse = " : "),
                    y = "contribuição", fill = "termo", subtitle = paste(sub, collapse = " · "))
}
