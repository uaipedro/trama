# Validação cruzada da krigagem, sobre `gstat::krige.cv`.
#
# `nfold = n` é leave-one-out: cada ponto é predito pelos outros. Oráculo:
# `geoR::xvalid`, que reproduz a 1,07e-14 quando se força o MESMO modelo nos
# dois (medido 2026-10-09); as quatro métricas saíram iguais em 10 casas.
#
# MSDR (mean squared deviation ratio) é a média de (resíduo / erro-padrão)^2, e
# é a métrica que olha a VARIÂNCIA: perto de 1 o erro-padrão do mapa está
# calibrado; muito acima, o mapa é otimista (promete precisão que não tem);
# muito abaixo, pessimista. RMSE sozinho não diz nada sobre o mapa de
# erro-padrão, que é metade do que a krigagem entrega.

.TR_SPATIAL_CAMPOS_VALID <- c("tabela", "metricas", "metodo", "dobras", "semente",
                              "modelo", "pontos", "nota")
.TR_SPATIAL_METODOS_VALID <- c("leave-one-out", "k dobras")

#' As quatro métricas da validação cruzada.
#' @noRd
.tr_spatial_metricas <- function(observado, predito, variancia) {
  if (any(!is.finite(variancia)) || any(variancia <= 0)) {
    .tr_spatial_abort("tr_spatial_error_bad_fit", paste(
      "A variância de krigagem saiu zero, negativa ou ausente em algum ponto, e",
      "o MSDR não existe nesse caso. Acontece com pepita zero, quando o ponto",
      "predito coincide com um amostral: use um modelo com pepita, ou k dobras."))
  }
  res <- observado - predito
  list(me = mean(res), rmse = sqrt(mean(res^2)),
       msdr = mean(res^2 / variancia),
       correlacao = stats::cor(observado, predito))
}

#' O MSDR em palavras, para a nota do card.
#' @noRd
.tr_spatial_msdr_texto <- function(msdr) {
  if (msdr > 1.3) "o erro-padrão do mapa é otimista: o erro real é maior"
  else if (msdr < 0.7) "o erro-padrão do mapa é pessimista: o erro real é menor"
  else "o erro-padrão do mapa está calibrado"
}

#' Validação cruzada da krigagem.
#'
#' @param pontos um objeto espacial (`spatial/points`).
#' @param modelo um modelo ajustado (`spatial/model`).
#' @param metodo `leave-one-out` ou `k dobras`.
#' @param dobras número de dobras, quando o método é `k dobras`.
#' @param semente semente da partição em dobras.
#' @param vizinhos_max,dist_max vizinhança, como na krigagem.
#' @return um objeto de validação (`spatial/validation`).
#' @export
tr_spatial_validation <- function(pontos, modelo, metodo = "leave-one-out",
                                  dobras = 10L, semente = NA,
                                  vizinhos_max = NA, dist_max = NA) {
  .tr_spatial_pontos_conferir(pontos)
  .tr_spatial_modelo_conferir(modelo)
  if (!is.character(metodo) || length(metodo) != 1L ||
      !metodo %in% .TR_SPATIAL_METODOS_VALID) {
    .tr_spatial_abort("tr_spatial_error_bad_option", sprintf(
      "Método: escolha um de %s.",
      paste(.TR_SPATIAL_METODOS_VALID, collapse = ", ")))
  }
  .tr_spatial_krig_vizinhanca(vizinhos_max, dist_max)
  n <- nrow(pontos$dados)
  nfold <- if (identical(metodo, "leave-one-out")) n else {
    k <- suppressWarnings(as.integer(dobras))
    if (length(k) != 1L || is.na(k) || k < 2L) {
      .tr_spatial_abort("tr_spatial_error_bad_option", paste(
        "Dobras: use um inteiro a partir de 2. Com uma dobra nada é deixado de",
        "fora, e a validação não valida nada."))
    }
    if (k > n) {
      .tr_spatial_abort("tr_spatial_error_bad_option", sprintf(paste(
        "Dobras: %d é mais que os %d pontos amostrais. Use no máximo %d, ou",
        "escolha leave-one-out, que é a validação com %d dobras."), k, n, n, n))
    }
    k
  }
  if (!is.na(semente)) set.seed(as.integer(semente))
  d <- sf::st_as_sf(as.data.frame(pontos$dados), coords = pontos$coord_cols,
                    crs = if (inherits(pontos$crs, "crs")) pontos$crs else NA,
                    remove = FALSE)
  d$.z <- pontos$dados[[pontos$variavel]]
  args <- list(formula = stats::as.formula(".z ~ 1"), locations = d,
               model = .tr_spatial_vgm_model(modelo), nfold = nfold,
               verbose = FALSE)
  if (!is.na(vizinhos_max)) args$nmax <- as.integer(vizinhos_max)
  if (!is.na(dist_max)) args$maxdist <- as.numeric(dist_max)
  cv <- do.call(gstat::krige.cv, args)
  res <- as.numeric(cv$observed) - as.numeric(cv$var1.pred)
  # As colunas de coordenada vêm da matriz `coords`, que já carrega os nomes
  # declarados no `spatial/coordinates`.
  tab <- tibble::as_tibble(cbind(
    as.data.frame(pontos$coords),
    data.frame(observado = as.numeric(cv$observed),
               predito = as.numeric(cv$var1.pred),
               variancia = as.numeric(cv$var1.var), residuo = res,
               z = res / sqrt(as.numeric(cv$var1.var)),
               dobra = as.integer(cv$fold))))
  met <- .tr_spatial_metricas(tab$observado, tab$predito, tab$variancia)
  nota <- paste(
    sprintf("%s, %d pontos, vizinhança %s.", metodo, n,
            .tr_spatial_viz_texto(vizinhos_max, dist_max)),
    sprintf("MSDR %.3f: %s.", met$msdr, .tr_spatial_msdr_texto(met$msdr)))
  structure(list(tabela = tab, metricas = met, metodo = metodo,
                 dobras = as.integer(nfold),
                 semente = if (is.na(semente)) NA_real_ else as.numeric(semente),
                 modelo = modelo, pontos = pontos, nota = nota),
            class = "tr_spatial_validation")
}

.tr_spatial_valid_conferir <- function(x) {
  .tr_spatial_guard(x, "tr_spatial_validation", .TR_SPATIAL_CAMPOS_VALID,
                    "tr_spatial_error_not_validation", "uma validação cruzada")
}

#' Validação -> tabela: o por-ponto, que é o que se filtra e se desenha.
#' @noRd
.tr_spatial_valid_tabela <- function(x) {
  .tr_spatial_valid_conferir(x)
  as.data.frame(x$tabela)
}

#' O que o card da validação mostra: obs x pred, e as quatro métricas.
#' @noRd
.tr_spatial_valid_preview <- function(x) {
  t <- x$tabela
  list(variavel = x$pontos$variavel, unidade = .tr_spatial_nulo(x$pontos$unidade),
       metodo = x$metodo, dobras = x$dobras, n = nrow(t),
       me = signif(x$metricas$me, 6), rmse = signif(x$metricas$rmse, 6),
       msdr = signif(x$metricas$msdr, 6),
       correlacao = signif(x$metricas$correlacao, 6),
       pontos = lapply(seq_len(nrow(t)), function(i) {
         list(obs = t$observado[[i]], pred = t$predito[[i]], z = t$z[[i]])
       }),
       nota = .tr_spatial_nulo(x$nota))
}

spatial_validation_type <- function() {
  .tr_spatial_rds_type("spatial/validation", "Validação", "#b45309",
                       .tr_spatial_valid_conferir,
                       function(x, ctx) trama::tr_preview(
                         "spatial/validation", data = .tr_spatial_valid_preview(x)),
                       report = tr_spatial_report_validation)
}
