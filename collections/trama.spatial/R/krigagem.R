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

# Valores do param `tipo`.
#
# A INDICADORA nao entra aqui: ela precisa do variograma DO INDICADOR, e sai
# pelo bloco `spatial/indicator`, antes do variograma. O comentario original
# previa um valor de `tipo` para ela, e estava errado -- com um valor de tipo o
# bloco receberia um modelo ajustado a variavel continua e krigaria indicador
# com ele, em silencio.
.TR_SPATIAL_TIPOS_KRIG <- c("ordinaria", "simples", "universal")

# Tendencias que a universal aceita. `constante` nao e uma delas: universal com
# tendencia constante E a ordinaria, e oferecer as duas como se fossem
# diferentes confundiria em vez de ajudar.
.TR_SPATIAL_TENDENCIAS_KRIG <- c("1a ordem", "2a ordem", "covariavel")

#' Colunas da tendencia polinomial, CENTRADAS E PADRONIZADAS.
#'
#' A geometria segue crua: so a base da tendencia e centrada. Em UTM cru a 2a
#' ordem e mal condicionada nos DOIS pacotes -- medido em 2026-10-09: o
#' `geoR::krige.conv` fica computacionalmente singular (condicao reciproca
#' 7,3e-17) e o `gstat` difere 3,7e-4 do centrado. A krigagem com tendencia e
#' invariante a reparametrizacao linear da base, entao a versao centrada e a
#' mesma conta, melhor condicionada.
#'
#' O centro e a escala sao SEMPRE os do conjunto de ajuste: se cada ponta
#' centrasse pela propria media, a base mudaria entre ajuste e predicao e a
#' predicao sairia errada sem erro nenhum.
#'
#' @param coords matriz n x 2 dos pontos de ajuste.
#' @param alvo matriz onde avaliar; `NULL` usa `coords`.
#' @param tendencia `1a ordem` ou `2a ordem`.
#' @noRd
.tr_spatial_tendencia_colunas <- function(coords, alvo = NULL,
                                          tendencia = "1a ordem") {
  co <- as.matrix(coords)
  centro <- colMeans(co)
  escala <- apply(co, 2, stats::sd)
  if (any(!is.finite(escala)) || any(escala <= 0)) {
    .tr_spatial_abort("tr_spatial_error_bad_coords",
      "Uma das coordenadas nao varia: nao ha como ajustar tendencia nela.")
  }
  m <- as.matrix(alvo %||% co)
  x <- (m[, 1] - centro[[1]]) / escala[[1]]
  y <- (m[, 2] - centro[[2]]) / escala[[2]]
  d <- data.frame(.tx = x, .ty = y)
  if (identical(tendencia, "2a ordem")) {
    d$.tx2 <- x^2; d$.ty2 <- y^2; d$.txy <- x * y
  }
  d
}

#' A formula do `gstat` para a krigagem, conforme tipo e tendencia.
#' @noRd
.tr_spatial_krig_formula <- function(tipo, tendencia, covariaveis) {
  if (!identical(tipo, "universal")) return(stats::as.formula(".z_ ~ 1"))
  rhs <- switch(tendencia,
    "1a ordem" = ".tx + .ty",
    "2a ordem" = ".tx + .ty + I(.tx^2) + I(.ty^2) + I(.tx * .ty)",
    "covariavel" = paste(sprintf("`%s`", covariaveis), collapse = " + "))
  stats::as.formula(paste(".z_ ~", rhs))
}

#' Valida as opcoes da krigagem. Roda ANTES da grade e do motor.
#' @noRd
.tr_spatial_krig_opcoes <- function(tipo, media, vizinhos_max, dist_max,
                                    tendencia = "constante") {
  if (!is.character(tipo) || length(tipo) != 1L || !tipo %in% .TR_SPATIAL_TIPOS_KRIG) {
    .tr_spatial_abort("tr_spatial_error_bad_option", sprintf(
      "Tipo: escolha um de %s.", paste(.TR_SPATIAL_TIPOS_KRIG, collapse = ", ")))
  }
  if (identical(tipo, "universal") &&
      (!is.character(tendencia) || length(tendencia) != 1L ||
       !tendencia %in% .TR_SPATIAL_TENDENCIAS_KRIG)) {
    .tr_spatial_abort("tr_spatial_error_bad_option", sprintf(paste(
      "Tendencia: a krigagem universal precisa de uma destas: %s.",
      "Com tendencia constante, a krigagem universal E a ordinaria -- escolha",
      "o tipo 'ordinaria'."), paste(.TR_SPATIAL_TENDENCIAS_KRIG, collapse = ", ")))
  }
  # O gstat ACEITA a simples sem beta e resolve com media zero, em silencio.
  if (identical(tipo, "simples") &&
      (!is.numeric(media) || length(media) != 1L || !is.finite(media))) {
    .tr_spatial_abort("tr_spatial_error_no_mean", paste(
      "Krigagem simples supõe a média da população CONHECIDA, e ela não foi informada.",
      "Preencha 'Média', ou use a krigagem ordinária, que a estima a partir dos dados."))
  }
  .tr_spatial_krig_vizinhanca(vizinhos_max, dist_max)
  invisible(tipo)
}

#' A grade trazida pelo usuario, validada contra os pontos.
#'
#' O KED exige a covariavel EXAUSTIVA, conhecida em toda celula. Quem nao tem
#' esse dado nao roda KED e recebe erro dizendo o que falta: a alternativa seria
#' interpolar a covariavel por dentro, e ai o erro-padrao do mapa ficaria
#' subestimado sem dizer, porque o erro da interpolacao nao se propaga.
#'
#' As colunas de coordenada tem de ter os MESMOS nomes declarados no
#' `spatial/coordinates`: a grade vem de outro caminho (uma tabela lida de
#' arquivo), e casar coluna por posicao trocaria leste por norte em silencio.
#' @noRd
.tr_spatial_grade_externa <- function(grade, pontos) {
  g <- as.data.frame(grade)
  faltam <- setdiff(pontos$coord_cols, names(g))
  if (length(faltam)) {
    .tr_spatial_abort("tr_spatial_error_missing_drift", sprintf(paste(
      "A grade ligada nao tem a coluna %s. Ela precisa das mesmas colunas de",
      "coordenada dos pontos (%s). Tem: %s."),
      paste(faltam, collapse = ", "),
      paste(pontos$coord_cols, collapse = ", "),
      paste(names(g), collapse = ", ")))
  }
  ruins <- vapply(pontos$coord_cols, function(cc) sum(!is.finite(g[[cc]])), 0L)
  if (any(ruins > 0L)) {
    .tr_spatial_abort("tr_spatial_error_bad_coords", sprintf(
      "A grade ligada tem %d celula(s) com coordenada faltante ou infinita.",
      sum(ruins)))
  }
  if (!nrow(g)) {
    .tr_spatial_abort("tr_spatial_error_empty_grid",
      "A grade ligada nao tem nenhuma linha.")
  }
  g
}

#' Valida a vizinhança (`vizinhos_max`, `dist_max`).
#'
#' Extraída de `.tr_spatial_krig_opcoes()` porque a validação cruzada usa a
#' mesma vizinhança sem ter `tipo` nem `media`.
#' @noRd
.tr_spatial_krig_vizinhanca <- function(vizinhos_max, dist_max) {
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
  invisible(NULL)
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
                                  media = NA, vizinhos_max = NA, dist_max = NA,
                                  tendencia = "constante") {
  .tr_spatial_pontos_conferir(pontos)
  .tr_spatial_modelo_conferir(modelo)
  .tr_spatial_krig_opcoes(tipo, media, vizinhos_max, dist_max, tendencia)
  cx <- pontos$coord_cols[[1]]; cy <- pontos$coord_cols[[2]]
  novos_todo <- as.data.frame(novos)
  if (all(c(cx, cy) %in% names(novos_todo))) {
    novos <- novos_todo[, c(cx, cy), drop = FALSE]
  } else {
    novos <- novos_todo[, 1:2, drop = FALSE]
    names(novos) <- c(cx, cy)
  }
  # Nomes internos: a coluna da variavel pode ter nome nao sintatico.
  d <- data.frame(.z_ = pontos$dados[[pontos$variavel]],
                  .x_ = pontos$coords[, 1], .y_ = pontos$coords[, 2])
  nd <- data.frame(.x_ = novos[[1]], .y_ = novos[[2]])
  if (identical(tipo, "universal")) {
    if (identical(tendencia, "covariavel")) {
      cov <- pontos$covariaveis
      if (!length(cov)) {
        .tr_spatial_abort("tr_spatial_error_blank_param", paste(
          "Tendencia por covariavel, mas o objeto espacial nao declara nenhuma.",
          "Volte ao bloco Coordenadas e preencha 'Covariaveis'."))
      }
      faltam <- setdiff(cov, names(novos_todo))
      if (length(faltam)) {
        .tr_spatial_abort("tr_spatial_error_missing_drift", sprintf(paste(
          "A deriva externa precisa da covariavel conhecida em TODA celula onde",
          "se prediz, e falta %s na grade. Ligue na porta 'grade' uma tabela com",
          "as colunas %s e %s, ou use tendencia de 1a ou 2a ordem, que so precisa",
          "das coordenadas."), paste(faltam, collapse = ", "),
          paste(pontos$coord_cols, collapse = " e "),
          paste(cov, collapse = " e ")))
      }
      ruins <- vapply(cov, function(cc) sum(!is.finite(novos_todo[[cc]])), 0L)
      if (any(ruins > 0L)) {
        .tr_spatial_abort("tr_spatial_error_missing_drift", sprintf(paste(
          "A covariavel %s tem %d celula(s) sem valor na grade. A deriva externa",
          "nao prediz onde a covariavel falta: recorte a grade, ou preencha-a."),
          paste(names(ruins)[ruins > 0L], collapse = ", "), sum(ruins)))
      }
      for (cc in cov) {
        d[[cc]] <- pontos$dados[[cc]]
        nd[[cc]] <- novos_todo[[cc]]
      }
    } else {
      d <- cbind(d, .tr_spatial_tendencia_colunas(pontos$coords,
                                                  tendencia = tendencia))
      nd <- cbind(nd, .tr_spatial_tendencia_colunas(
        pontos$coords, alvo = as.matrix(novos), tendencia = tendencia))
    }
  }
  args <- list(formula = .tr_spatial_krig_formula(tipo, tendencia,
                                                  pontos$covariaveis),
               locations = ~ .x_ + .y_, data = d, newdata = nd,
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
  pred <- .tr_spatial_recorta_indicador(pred, pontos)
  out <- data.frame(novos, predito = as.numeric(pred), variancia = v,
                    erro_padrao = sqrt(v))
  if (!is.null(attr(pred, "n_recortadas"))) {
    attr(out, "n_recortadas") <- attr(pred, "n_recortadas")
  }
  out
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
tr_spatial_kriging <- function(pontos = NULL, modelo, grade = NULL,
                               tipo = "ordinaria", media = NA,
                               resolucao = 60L, vizinhos_max = NA, dist_max = NA,
                               tendencia = "constante") {
  .tr_spatial_modelo_conferir(modelo)
  pontos <- .tr_spatial_krig_pontos(pontos, modelo)
  .tr_spatial_krig_opcoes(tipo, media, vizinhos_max, dist_max, tendencia)
  # Grade ligada VENCE a resolucao: ela e a unica forma de a deriva externa ter
  # a covariavel em toda celula, e tambem serve a quem quer predizer em pontos
  # escolhidos em vez de numa grade regular.
  externa <- !is.null(grade)
  g <- if (externa) .tr_spatial_grade_externa(grade, pontos) else
    tr_spatial_grid(pontos, resolucao)
  grade_pred <- tr_spatial_kriging_em(pontos, modelo, g, tipo, media,
                                      vizinhos_max, dist_max, tendencia)
  viz <- .tr_spatial_viz_texto(vizinhos_max, dist_max)
  nota <- paste(c(modelo$nota,
                  if (identical(tipo, "universal"))
                    sprintf("Krigagem universal, tend\u00eancia de %s.", tendencia),
                  # Tendência fora da universal não faz nada, e o param persiste
                  # no documento quando se troca o Tipo: dizer que foi ignorada
                  # é mais honesto que calar.
                  if (!identical(tipo, "universal") &&
                      !identical(tendencia, "constante"))
                    sprintf(paste("A tend\u00eancia '%s' foi IGNORADA: s\u00f3 a krigagem",
                                  "universal a usa."), tendencia),
                  if (externa && !is.null(pontos$borda))
                    paste("A grade ligada n\u00e3o \u00e9 recortada na borda: o contorno",
                          "aparece no mapa, mas a predi\u00e7\u00e3o sai onde a grade",
                          "pedir."),
                  if (externa)
                    sprintf(paste("Grade ligada na porta, com %d células: a",
                                  "resolução é ignorada."), nrow(grade_pred))
                  else
                    sprintf("Grade de %d células (resolução %d).",
                            nrow(grade_pred), as.integer(resolucao))),
                collapse = " ")
  nota <- trimws(nota)
  sem <- sum(is.na(grade_pred$predito))
  if (sem > 0L) {
    nota <- paste(c(nota, sprintf(paste(
      "%d de %d células ficaram sem predição: nenhum ponto dentro da vizinhança (%s).",
      "Aumente o raio ou deixe-o vazio."), sem, nrow(grade_pred), viz)), collapse = " ")
  }
  rec <- attr(grade_pred, "n_recortadas")
  if (!is.null(pontos$indicador)) {
    nota <- paste(c(nota, sprintf(paste(
      "O predito é a PROBABILIDADE de %s %s %s, recortada em [0, 1]%s."),
      pontos$indicador$variavel_original, pontos$indicador$sentido,
      format(signif(pontos$indicador$corte, 6)),
      if (!is.null(rec) && rec > 0L)
        sprintf(" (%d célula(s) precisaram do recorte)", rec) else "")),
      collapse = " ")
  }
  structure(list(
    grade = tibble::as_tibble(grade_pred), tipo = tipo,
    indicador = pontos$indicador, n_recortadas = rec, modelo = modelo, pontos = pontos,
    borda = pontos$borda, resolucao = as.integer(resolucao), vizinhanca = viz,
    variavel = pontos$variavel, unidade = pontos$unidade, nota = nota),
    class = "tr_spatial_surface")
}
