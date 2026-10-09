# A krigagem, sobre `gstat::krige`.
#
# Mapeamento conferido na documentacao do gstat em 2026-10-06:
#   ordinaria -> formula z ~ 1, SEM beta
#   simples   -> formula z ~ 1, COM beta = media conhecida
# A saida traz `var1.pred` e `var1.var`.
#
# Condicionamento: as duas formulas sao `z ~ 1`, sem termo de coordenada no lado
# direito. O problema que a tendencia polinomial tem em UTM cru (quadrados de
# ~1e12, ver variograma.R) nao existe aqui; distancias e covariancias sao
# lineares na escala. Se a krigagem universal entrar, ela precisa da mesma
# centragem do variograma.
#
# A grade nasce aqui e nao num bloco proprio: ela e consequencia da borda e de
# um parametro de resolucao, nao uma decisao analitica autonoma.

# Valores do param `tipo`. A universal e a de indicadora entram como valores
# novos desta lista, sem bloco novo.
.TR_SPATIAL_TIPOS_KRIG <- c("ordinaria", "simples")

#' Valida as opcoes da krigagem. Roda ANTES da grade e do motor.
#' @noRd
.tr_spatial_krig_opcoes <- function(tipo, media, vizinhos_max, dist_max) {
  if (!is.character(tipo) || length(tipo) != 1L || !tipo %in% .TR_SPATIAL_TIPOS_KRIG) {
    .tr_spatial_abort("tr_spatial_error_bad_option", sprintf(
      "Tipo: escolha um de %s.", paste(.TR_SPATIAL_TIPOS_KRIG, collapse = ", ")))
  }
  # O gstat ACEITA a simples sem beta e resolve com media zero, em silencio.
  if (identical(tipo, "simples") &&
      (!is.numeric(media) || length(media) != 1L || !is.finite(media))) {
    .tr_spatial_abort("tr_spatial_error_no_mean", paste(
      "Krigagem simples supõe a média da população CONHECIDA, e ela não foi informada.",
      "Preencha 'Média', ou use a krigagem ordinária, que a estima a partir dos dados."))
  }
  # NA puro e logico; o campo vazio do card chega assim.
  vazio <- function(x) length(x) == 1L && is.na(x)
  positivo <- function(x, inteiro) {
    is.numeric(x) && length(x) == 1L && is.finite(x) && x > 0 &&
      (!inteiro || (x >= 1 && x == round(x)))
  }
  if (!vazio(vizinhos_max) && !positivo(vizinhos_max, TRUE)) {
    .tr_spatial_abort("tr_spatial_error_bad_option",
      "Vizinhos: use um inteiro a partir de 1, ou deixe vazio para krigar com todos.")
  }
  if (!vazio(dist_max) && !positivo(dist_max, FALSE)) {
    .tr_spatial_abort("tr_spatial_error_bad_option",
      "Raio: use um número positivo, ou deixe vazio para krigar com todos.")
  }
  invisible(tipo)
}

#' Grade de predição: retângulo que cobre os pontos e a borda, recortado na borda.
#'
#' O retângulo é a união da extensão dos pontos com a da borda. Se fosse só o da
#' borda, uma borda em escala errada (quilômetros contra metros) produziria uma
#' grade completa e plausível, na escala errada, em vez de uma grade vazia.
#'
#' @param pontos um objeto espacial (`spatial/points`).
#' @param resolucao pontos no lado maior.
#' @return data.frame com as duas colunas de coordenada.
#' @export
tr_spatial_grid <- function(pontos, resolucao = 60L) {
  .tr_spatial_pontos_conferir(pontos)
  if (!is.numeric(resolucao) || length(resolucao) != 1L || !is.finite(resolucao) ||
      resolucao < 5 || resolucao > 500 || resolucao != round(resolucao)) {
    .tr_spatial_abort("tr_spatial_error_bad_option",
      "Resolucao: use um inteiro entre 5 e 500.")
  }
  resolucao <- as.integer(resolucao)
  xs <- pontos$coords[, 1]; ys <- pontos$coords[, 2]
  if (!is.null(pontos$borda)) { xs <- c(xs, pontos$borda[, 1]); ys <- c(ys, pontos$borda[, 2]) }
  lx <- diff(range(xs)); ly <- diff(range(ys))
  maior <- max(lx, ly)
  nx <- if (lx > 0) max(2L, as.integer(round(resolucao * lx / maior))) else 1L
  ny <- if (ly > 0) max(2L, as.integer(round(resolucao * ly / maior))) else 1L
  g <- expand.grid(seq(min(xs), max(xs), length.out = nx),
                   seq(min(ys), max(ys), length.out = ny))
  names(g) <- pontos$coord_cols
  if (!is.null(pontos$borda)) {
    pol <- sf::st_sfc(sf::st_polygon(list(pontos$borda)))
    pts <- sf::st_as_sf(g, coords = pontos$coord_cols)
    g <- g[lengths(sf::st_intersects(pts, pol)) > 0L, , drop = FALSE]
  }
  if (!nrow(g)) {
    .tr_spatial_abort("tr_spatial_error_empty_grid", paste(
      "A grade ficou vazia depois do recorte na borda: nenhuma célula caiu dentro dela.",
      "Confira se a borda está na mesma unidade e na mesma ordem de colunas que as coordenadas."))
  }
  rownames(g) <- NULL
  g
}

#' O texto da vizinhança, como aparece na nota e na mensagem de erro.
#'
#' Vive aqui, e não dentro de `tr_spatial_kriging`, porque a guarda da superfície
#' vazia é levantada em `tr_spatial_kriging_em` e precisa da mesma frase.
#' @noRd
.tr_spatial_viz_texto <- function(vizinhos_max, dist_max) {
  if (is.na(vizinhos_max) && is.na(dist_max)) return("global")
  paste(c(if (!is.na(vizinhos_max)) sprintf("até %d vizinhos", as.integer(vizinhos_max)),
          if (!is.na(dist_max)) sprintf("raio de %g", as.numeric(dist_max))), collapse = ", ")
}

#' Número arredondado, no separador de milhar brasileiro, sem notação científica.
#' @noRd
.tr_spatial_num <- function(x) {
  format(round(as.numeric(x)), big.mark = ".", decimal.mark = ",", scientific = FALSE,
         trim = TRUE)
}

#' Distância típica (mediana) ao vizinho mais próximo entre os pontos amostrais.
#'
#' Serve à mensagem de erro: sem a escala dos dados a pessoa não sabe que raio
#' pôr, e o raio está na unidade das coordenadas, que varia por conjunto. Numa
#' amostra de até 500 pontos, porque a matriz de distâncias é quadrática e o
#' número aqui é só ordem de grandeza.
#' @noRd
.tr_spatial_dist_vizinho <- function(coords) {
  co <- as.matrix(coords)
  if (nrow(co) > 500L) co <- co[sample.int(nrow(co), 500L), , drop = FALSE]
  d <- as.matrix(stats::dist(co))
  diag(d) <- Inf
  stats::median(apply(d, 1L, min))
}

#' Krigagem em pontos arbitrários. É aqui que a conta acontece.
#'
#' `novos` traz as duas colunas de coordenada, pelo nome quando ambas existem e
#' pela posição quando não.
#' @noRd
tr_spatial_kriging_em <- function(pontos, modelo, novos, tipo = "ordinaria",
                                  media = NA, vizinhos_max = NA, dist_max = NA) {
  .tr_spatial_pontos_conferir(pontos)
  .tr_spatial_modelo_conferir(modelo)
  .tr_spatial_krig_opcoes(tipo, media, vizinhos_max, dist_max)
  cx <- pontos$coord_cols[[1]]; cy <- pontos$coord_cols[[2]]
  novos <- as.data.frame(novos)
  if (all(c(cx, cy) %in% names(novos))) {
    novos <- novos[, c(cx, cy), drop = FALSE]
  } else {
    novos <- novos[, 1:2, drop = FALSE]
    names(novos) <- c(cx, cy)
  }
  # Nomes internos: a coluna da variavel pode ter nome nao sintatico.
  d <- data.frame(.z_ = pontos$dados[[pontos$variavel]],
                  .x_ = pontos$coords[, 1], .y_ = pontos$coords[, 2])
  nd <- data.frame(.x_ = novos[[1]], .y_ = novos[[2]])
  args <- list(formula = .z_ ~ 1, locations = ~ .x_ + .y_, data = d, newdata = nd,
               model = .tr_spatial_vgm_model(modelo), debug.level = 0)
  if (identical(tipo, "simples")) args$beta <- as.numeric(media)
  if (!is.na(vizinhos_max)) args$nmax <- as.integer(vizinhos_max)
  if (!is.na(dist_max)) args$maxdist <- as.numeric(dist_max)
  k <- do.call(gstat::krige, args)
  v <- as.numeric(k$var1.var)
  ok <- is.finite(v)
  # Variancia negativa e sintoma de modelo nao definido positivo. Deixar passar
  # produz NaN na raiz, longe da causa. O limite acompanha a escala do modelo:
  # um valor absoluto seria ruido de arredondamento para dados em kg2/ha2 e
  # erro grosseiro para dados em escala unitaria.
  if (any(v[ok] < -1e-8 * (modelo$pepita + modelo$contribuicao))) {
    .tr_spatial_abort("tr_spatial_error_negative_variance", paste(
      "A variância de krigagem saiu negativa em alguma célula:",
      "o modelo ajustado não é definido positivo nesta configuração.",
      "Troque a família (o gaussiano sem pepita é o caso mais comum)",
      "ou acrescente um efeito pepita."))
  }
  v[ok] <- pmax(v[ok], 0)
  pred <- as.numeric(k$var1.pred)
  # Superficie 100% NA NAO e resultado. O mapa desenha os tiles com na.rm =
  # TRUE, entao ela sai como painel vazio: um mapa plausivel, de uma cor so, sem
  # informacao nenhuma. Achado de teste humano no editor, com raio de 1 metro em
  # coordenadas UTM. NA PARCIAL continua nota, porque e legitimo na borda.
  if (length(pred) && all(is.na(pred))) {
    .tr_spatial_abort("tr_spatial_error_empty_surface", sprintf(paste(
      "Nenhuma das %s células recebeu predição: a vizinhança (%s) exclui todos os pontos.",
      "Nestes dados o vizinho mais próximo está a %s (mediana) e o alcance do modelo é %s,",
      "na unidade das coordenadas. Use um raio dessa ordem de grandeza,",
      "ou deixe-o vazio para krigar com todos os pontos."),
      .tr_spatial_num(length(pred)), .tr_spatial_viz_texto(vizinhos_max, dist_max),
      .tr_spatial_num(.tr_spatial_dist_vizinho(pontos$coords)),
      .tr_spatial_num(modelo$alcance)))
  }
  data.frame(novos, predito = pred, variancia = v, erro_padrao = sqrt(v))
}

#' De onde vêm os pontos da krigagem.
#'
#' O modelo já carrega os pontos com que foi ajustado. Exigir os pontos de novo
#' permitiria ajustar num conjunto e krigar noutro sem reclamação, que é
#' resultado errado e plausível. Então: sem `pontos`, usa os do modelo; com
#' `pontos` e com pontos no modelo, confere que são os MESMOS DADOS (variável,
#' colunas de coordenada, coordenadas e valores, não identidade de objeto);
#' sem nenhum dos dois (modelo montado à mão), pede para conectar.
#' @noRd
.tr_spatial_krig_pontos <- function(pontos, modelo) {
  do_modelo <- modelo$variograma$pontos
  if (is.null(pontos)) {
    if (is.null(do_modelo)) {
      .tr_spatial_abort("tr_spatial_error_no_points", paste(
        "A krigagem precisa dos pontos, e este modelo não traz os seus",
        "(foi montado sem variograma empírico). Conecte os pontos na entrada 'pontos'."))
    }
    .tr_spatial_pontos_conferir(do_modelo)
    return(do_modelo)
  }
  .tr_spatial_pontos_conferir(pontos)
  if (!is.null(do_modelo)) {
    .tr_spatial_pontos_iguais(pontos, do_modelo)
  }
  pontos
}

#' Compara DADOS, não objetos, e sem depender da ordem das linhas: a krigagem não
#' depende dela, e recusar o mesmo conjunto com as linhas embaralhadas seria
#' acusar de erro um fluxo certo. As duas listas vão para uma ordem canônica
#' (x, depois y, depois o valor) antes de comparar.
#' @noRd
.tr_spatial_pontos_iguais <- function(pontos, do_modelo) {
  motivo <- NULL
  canon <- function(p) {
    co <- unname(as.matrix(p$coords)); z <- as.numeric(p$dados[[p$variavel]])
    o <- order(co[, 1], co[, 2], z)
    list(co = co[o, , drop = FALSE], z = z[o])
  }
  if (!identical(pontos$variavel, do_modelo$variavel)) {
    motivo <- sprintf("a variável é '%s' nos pontos e '%s' no modelo",
                      pontos$variavel, do_modelo$variavel)
  } else if (!identical(as.character(pontos$coord_cols), as.character(do_modelo$coord_cols))) {
    motivo <- "as colunas de coordenada são outras"
  } else if (!identical(dim(pontos$coords), dim(do_modelo$coords))) {
    motivo <- sprintf("há %d pontos e o modelo foi ajustado com %d",
                      nrow(pontos$coords), nrow(do_modelo$coords))
  } else {
    a <- canon(pontos); b <- canon(do_modelo)
    if (!isTRUE(all.equal(a$co, b$co, tolerance = 0))) {
      motivo <- "as coordenadas dos pontos não são as do modelo"
    } else if (!isTRUE(all.equal(a$z, b$z, tolerance = 0))) {
      motivo <- "os valores da variável não são os do modelo"
    }
  }
  if (!is.null(motivo)) {
    # A frase sobre releitura so cabe quando a diferenca esta nos numeros: com
    # outra variavel ou outro tamanho, mandaria procurar erro de precisao onde
    # o erro e de ligacao.
    numerico <- grepl("coordenadas dos pontos|valores da variável", motivo)
    .tr_spatial_abort("tr_spatial_error_points_mismatch", paste0(
      "Os pontos conectados não são os dados com que o modelo foi ajustado: ", motivo, ". ",
      "A ordem das linhas não importa. ",
      if (numerico) paste0("Os números precisam ser idênticos: dados relidos de um arquivo, ",
                           "ou arredondados, já não são os mesmos. ") else "",
      "Krigar outro conjunto com este modelo dá um mapa plausível e errado. ",
      "Ajuste o modelo nestes pontos, ou desconecte a entrada 'pontos' para usar os do modelo."))
  }
  invisible(TRUE)
}

#' Krigagem numa grade que cobre a área.
#'
#' @param pontos um objeto espacial (`spatial/points`), opcional: sem ele, a
#'   krigagem usa os pontos com que o modelo foi ajustado. Se vierem os dois e
#'   não forem os mesmos dados, é erro.
#' @param modelo um modelo ajustado (`spatial/model`).
#' @param tipo `"ordinaria"` ou `"simples"`.
#' @param media a média conhecida; obrigatória na simples.
#' @param resolucao pontos no lado maior da grade.
#' @param vizinhos_max,dist_max vizinhança local; `NA` kriga globalmente.
#' @return uma superfície predita (`spatial/surface`).
#' @export
tr_spatial_kriging <- function(pontos = NULL, modelo, tipo = "ordinaria", media = NA,
                               resolucao = 60L, vizinhos_max = NA, dist_max = NA) {
  .tr_spatial_modelo_conferir(modelo)
  pontos <- .tr_spatial_krig_pontos(pontos, modelo)
  .tr_spatial_krig_opcoes(tipo, media, vizinhos_max, dist_max)
  g <- tr_spatial_grid(pontos, resolucao)
  grade <- tr_spatial_kriging_em(pontos, modelo, g, tipo, media, vizinhos_max, dist_max)
  viz <- .tr_spatial_viz_texto(vizinhos_max, dist_max)
  nota <- paste(c(modelo$nota, sprintf("Grade de %d células (resolução %d).",
                                       nrow(grade), as.integer(resolucao))), collapse = " ")
  nota <- trimws(nota)
  sem <- sum(is.na(grade$predito))
  if (sem > 0L) {
    nota <- paste(c(nota, sprintf(paste(
      "%d de %d células ficaram sem predição: nenhum ponto dentro da vizinhança (%s).",
      "Aumente o raio ou deixe-o vazio."), sem, nrow(grade), viz)), collapse = " ")
  }
  superficie <- structure(list(
    grade = tibble::as_tibble(grade), tipo = tipo, modelo = modelo, pontos = pontos,
    borda = pontos$borda, resolucao = as.integer(resolucao), vizinhanca = viz,
    variavel = pontos$variavel, unidade = pontos$unidade, nota = nota),
    class = "tr_spatial_surface")
  # A krigagem é do gstat (o `tr_ref` comum da coleção declara o variograma,
  # que não entra aqui). O recorte pela borda com o sf é geometria, não conta.
  .tr_spatial_ferramentas(superficie, "gstat::krige")
}
