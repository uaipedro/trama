# O tema dos gráficos do documento: claro, com as cores do `_brand.yml`.
#
# O padrão do projeto costuma ser o tema escuro do editor, que num relatório
# impresso ou numa página branca fica errado. Decisão da visão (fase 3): no
# documento, o "padrão" do projeto é trocado por um tema claro derivado do
# `_brand.yml` do documento — fundo branco (ou o do modelo, se for claro),
# texto, eixos e grade da cor do texto, e a paleta começando pelas cores do
# modelo que se leem sobre o fundo.
#
# A troca é nos settings do projeto, como `tr_export_code()` faz no Quarto
# exportado: card com o tema "padrão" segue o documento; card com tema
# escolhido à mão fica com o que escolheu. O tema entra na chave do store, então
# o gráfico do documento é calculado (e guardado) à parte do gráfico do
# editor, e trocar as cores do `_brand.yml` deixa as regiões desatualizadas.

.TD_TEMA_NOME <- "trama-documento"
# A paleta do tema `claro` embutido no núcleo (`R/theme.R`), para completar a
# do modelo até 8 cores.
.TD_PALETA_CLARA <- c("#2563eb", "#d97706", "#059669", "#dc2626", "#7c3aed", "#0891b2", "#db2777", "#65a30d")

#' Settings do projeto para o documento da pasta: o "padrão" vira o tema
#' claro do `_brand.yml`.
#' @noRd
.td_settings_documento <- function(settings, pasta_doc) {
  if (is.null(settings)) return(settings)
  # `_brand.yml` fora do formato (editado à mão) não pode parar o documento:
  # vale o tema claro de sempre.
  settings$temas[[.TD_TEMA_NOME]] <- tryCatch(.td_tema_documento(.td_brand_ler(pasta_doc)),
                                              error = function(e) .td_tema_documento(list()))
  settings$tema_padrao <- .TD_TEMA_NOME
  settings
}

#' Campos de tema do núcleo (`trama::tr_theme()`), na ordem e nos tipos que
#' ele normaliza, a partir do `_brand.yml`.
#' @noRd
.td_tema_documento <- function(brand) {
  fundo <- .td_brand_cor(brand, "background")
  if (is.null(fundo) || .td_luminancia(fundo) < 0.6) fundo <- "#ffffff"
  texto <- .td_brand_cor(brand, "foreground")
  if (is.null(texto) || .td_contraste(texto, fundo) < 4.5) texto <- "#1f2937"
  cores <- unlist(lapply(c("primary", "secondary", "tertiary", "success", "info", "warning", "danger"),
                         function(k) .td_brand_cor(brand, k)))
  # Cor que não se lê sobre o fundo (o azul-gelo de uma marca, por exemplo)
  # não serve de série.
  cores <- Filter(function(cr) .td_contraste(cr, fundo) >= 2.5, cores)
  paleta <- unique(c(cores, .TD_PALETA_CLARA))[1:8]
  fonte <- .td_brand_fontes(brand)$texto %||% ""
  list(base = "minimal", tamanho = 13, fonte = .td_familia(fonte),
       fundo = fundo, texto = texto, eixos = .td_misturar(texto, fundo, 0.25),
       grade = .td_misturar(texto, fundo, 0.88), paleta = paleta, continua = "viridis")
}

#' A família genérica que o ggplot entende.
#' @noRd
.td_familia <- function(f) {
  if (grepl("mono|code|courier|consol", f, ignore.case = TRUE)) return("mono")
  if (grepl("sans", f, ignore.case = TRUE)) return("sans")
  if (grepl("serif|times|georgia|garamond|merriweather|lora|libertin|palatino|cambria|book", f, ignore.case = TRUE)) return("serif")
  "sans"
}

.td_rgb <- function(hex) strtoi(substring(hex, c(2L, 4L, 6L), c(3L, 5L, 7L)), 16L)

#' Luminância relativa (WCAG), como `.tr_export_luminancia` do núcleo.
#' @noRd
.td_luminancia <- function(hex) {
  v <- .td_rgb(hex) / 255
  v <- ifelse(v <= 0.03928, v / 12.92, ((v + 0.055) / 1.055)^2.4)
  sum(c(0.2126, 0.7152, 0.0722) * v)
}

.td_contraste <- function(a, b) {
  l <- sort(c(.td_luminancia(a), .td_luminancia(b)), decreasing = TRUE)
  (l[[1]] + 0.05) / (l[[2]] + 0.05)
}

#' `a` misturada com `b` (`t` = quanto de `b`).
#' @noRd
.td_misturar <- function(a, b, t) {
  v <- round((1 - t) * .td_rgb(a) + t * .td_rgb(b))
  sprintf("#%02x%02x%02x", v[[1]], v[[2]], v[[3]])
}
