# Camadas: blocos que recebem um gráfico e devolvem o MESMO gráfico com algo a
# mais — uma linha de referência, a reta ajustada, uma anotação.
#
# É a resposta controlada ao "um ggplot que vai somando", sem trazer a
# gramática inteira para o canvas: cada camada é UMA pergunta de quem lê o
# gráfico (onde está o limite? que reta passa aqui? o que é aquele ponto?), e
# não um `geom_*` genérico com vinte params.
#
# Nenhuma camada declara os seis cosméticos, e isso é o desenho, não omissão.
# O gráfico de entrada já saiu do funil (`.tr_view_acabar()`): tema, rótulos,
# legenda e o atributo `tr_view_dim` já estão nele, e somar uma camada a um
# ggplot preserva os três (medido: o atributo sobrevive ao `+`, e a camada nova
# pega as cores de `element_geom` do tema já aplicado). Um segundo tema aqui
# seria um segundo lugar para decidir a aparência — o mesmo motivo por que o
# `view/save` também não o tem. Quem quer outro tema troca no gráfico de origem.
#
# Painel (`view/combine`) é RECUSADO na entrada, e não "aplicado ao último
# painel" como o `+` do patchwork faria: a camada iria parar num painel que o
# usuário não escolheu, calada. A camada entra ANTES do painel, no gráfico que
# ela anota.
#
# Facetas e escala log do gráfico de entrada são respeitadas porque a camada é
# somada ao objeto, e não redesenhada: linha e faixa sem a coluna do painel
# aparecem em todos os painéis; a reta ajustada leva a coluna do painel e cai
# em cada um; valores de referência passam pela escala como qualquer dado.

.TR_VIEW_REFERENCIAS <- c("horizontal", "vertical", "diagonal")
.TR_VIEW_ESTILOS_LINHA <- c("tracejada", "contínua", "pontilhada")
.TR_VIEW_AJUSTES <- c("linear", "quadrática", "loess")

# O texto das camadas tem o tamanho do TEMA (80% do texto base), e não um
# número fixo em mm: assim o `view/save`, que leva o texto a `texto_pt`
# pontos na figura gravada, leva a equação e a anotação junto. Com tamanho
# fixo, num painel de 170 mm a equação saía maior que o eixo e cortada.
.TR_VIEW_TAMANHO_TEXTO <- rlang::expr(ggplot2::from_theme(fontsize * .8))

#' A entrada de toda camada: um ggplot, e não um painel.
#' @noRd
.tr_view_camada_entrada <- function(grafico, no) {
  if (!inherits(grafico, "ggplot")) {
    rlang::abort(sprintf("'%s' recebe um gráfico, e chegou um objeto '%s'.", no, class(grafico)[[1]]),
                 class = "tr_view_error_not_a_plot")
  }
  if (inherits(grafico, "patchwork")) {
    rlang::abort(sprintf(paste0("'%s' não se aplica a um painel ('view/combine'): num painel a camada ",
                                "iria para um gráfico só, sem você escolher qual. Ligue a camada no ",
                                "gráfico antes do painel."), no),
                 class = "tr_view_error_panel")
  }
  grafico
}

#' Números digitados: "20", "10; 20; 30", "2,5". Vírgula é decimal, ";" separa.
#'
#' O param é texto porque número do card é um só, e a referência pede vários
#' (limites inferior e superior, as três doses). Número que já vem numérico,
#' do console, passa direto.
#' @noRd
.tr_view_numeros <- function(x, param, vazio = FALSE) {
  if (is.numeric(x)) {
    v <- as.numeric(x)
  } else {
    txt <- trimws(strsplit(paste(as.character(x), collapse = ";"), ";", fixed = TRUE)[[1]])
    txt <- txt[nzchar(txt)]
    if (!length(txt)) {
      if (vazio) return(numeric())
      rlang::abort(sprintf("Param '%s': preencha um número (vários, separados por ';').", param),
                   class = "tr_view_error_blank_param")
    }
    v <- suppressWarnings(as.numeric(gsub(",", ".", txt, fixed = TRUE)))
    if (anyNA(v)) {
      rlang::abort(sprintf("Param '%s': '%s' não é número. Use vírgula para decimal e ';' entre valores (ex.: 2,5; 10).",
                           param, paste(txt[is.na(v)], collapse = "', '")),
                   class = "tr_view_error_not_numeric")
    }
  }
  if (!length(v) && !vazio) {
    rlang::abort(sprintf("Param '%s': preencha um número.", param), class = "tr_view_error_blank_param")
  }
  if (any(!is.finite(v))) {
    rlang::abort(sprintf("Param '%s': precisa ser número finito.", param), class = "tr_view_error_not_numeric")
  }
  v
}

.tr_view_um_numero <- function(x, param, vazio = FALSE) {
  v <- .tr_view_numeros(x, param, vazio)
  if (length(v) > 1L) {
    rlang::abort(sprintf("Param '%s': aqui cabe um número só, e vieram %d.", param, length(v)),
                 class = "tr_view_error_bad_option")
  }
  v
}

# ---- Referência --------------------------------------------------------------

#' Linha(s) de referência, e a faixa entre dois valores.
#' @param grafico um ggplot (não um painel).
#' @param tipo `"horizontal"` (y = valor), `"vertical"` (x = valor) ou
#'   `"diagonal"` (y = valor + inclinacao·x).
#' @param valor um ou vários números, separados por `;`, com vírgula decimal.
#'   Na diagonal, o intercepto.
#' @param inclinacao a inclinação da diagonal; 1 com valor 0 é a reta y = x.
#' @param ate vazio desliga; preenchido, sombreia a faixa entre `valor` (um só)
#'   e `ate`. Não vale na diagonal.
#' @param texto escrito junto da linha; vários separados por `;`, um por valor.
#' @param estilo `"tracejada"`, `"contínua"` ou `"pontilhada"`.
#' @return o ggplot com a referência.
#' @export
tr_reference <- function(grafico, tipo = "horizontal", valor = "0", inclinacao = 1, ate = "",
                         texto = "", estilo = "tracejada") {
  p <- .tr_view_camada_entrada(grafico, "view/reference")
  tipo <- .tr_view_escolha(tipo, "tipo", .TR_VIEW_REFERENCIAS)
  estilo <- .tr_view_escolha(estilo, "estilo", .TR_VIEW_ESTILOS_LINHA)
  lt <- switch(estilo, tracejada = "dashed", `contínua` = "solid", pontilhada = "dotted")
  v <- .tr_view_numeros(valor, "valor")
  fim <- .tr_view_um_numero(ate, "ate", vazio = TRUE)
  rotulos <- trimws(strsplit(paste(as.character(texto), collapse = ";"), ";", fixed = TRUE)[[1]])
  if (length(rotulos) && !length(rotulos) %in% c(1L, length(v))) {
    rlang::abort(sprintf("Param 'texto': %d textos para %d valores. Use um texto só, ou um por valor, separados por ';'.",
                         length(rotulos), length(v)), class = "tr_view_error_bad_option")
  }
  rotulos <- rep_len(rotulos, if (length(rotulos)) length(v) else 0L)

  if (length(fim)) {
    if (tipo == "diagonal") {
      rlang::abort("Param 'ate': a faixa vale para referência horizontal ou vertical, não para a diagonal.",
                   class = "tr_view_error_bad_option")
    }
    if (length(v) != 1L) {
      rlang::abort("Param 'ate': a faixa vai de UM valor até 'ate'; deixe um número só em 'valor'.",
                   class = "tr_view_error_bad_option")
    }
    # A faixa vai ANTES das linhas e com `-Inf`/`Inf` na outra direção: ocupa
    # o painel inteiro em qualquer escala, e sem a coluna de painel cai em
    # todos. `from_theme(accent)` é a cor de destaque do tema já aplicado.
    lim <- sort(c(v, fim))
    faixa <- if (tipo == "horizontal") data.frame(xmin = -Inf, xmax = Inf, ymin = lim[[1]], ymax = lim[[2]])
             else data.frame(xmin = lim[[1]], xmax = lim[[2]], ymin = -Inf, ymax = Inf)
    p <- p + ggplot2::geom_rect(
      data = faixa, inherit.aes = FALSE, alpha = .15,
      ggplot2::aes(xmin = .data[["xmin"]], xmax = .data[["xmax"]], ymin = .data[["ymin"]],
                   ymax = .data[["ymax"]], fill = ggplot2::from_theme(accent)))
  }

  if (tipo == "diagonal" && (length(.tr_view_escala_log(p, "x")) || length(.tr_view_escala_log(p, "y")))) {
    # Com log, a reta a + b·x do ggplot é traçada na escala TRANSFORMADA, e a
    # "y = x" viraria outra curva sem aviso. Melhor recusar que desenhar errado.
    rlang::abort("Param 'tipo': a diagonal não vale num gráfico com eixo em log; desligue o log no gráfico de origem.",
                 class = "tr_view_error_bad_option")
  }
  p <- p + switch(tipo,
    horizontal = ggplot2::geom_hline(yintercept = v, linetype = lt, linewidth = .6),
    vertical = ggplot2::geom_vline(xintercept = v, linetype = lt, linewidth = .6),
    diagonal = ggplot2::geom_abline(intercept = v, slope = .tr_view_um_numero(inclinacao, "inclinacao"),
                                    linetype = lt, linewidth = .6))

  quais <- which(nzchar(rotulos))
  if (length(quais)) {
    # O texto encosta na ponta direita (horizontal) ou no topo (vertical), por
    # dentro do painel — `Inf` é a borda em qualquer escala e em todo painel.
    # Na diagonal, na ponta direita da faixa do X do gráfico, calculada na
    # escala de desenho: é onde a reta sai do painel.
    txt <- switch(tipo,
      horizontal = data.frame(x = Inf, y = v[quais], rotulo = rotulos[quais], hj = 1.05, vj = -.4, ang = 0),
      vertical = data.frame(x = v[quais], y = Inf, rotulo = rotulos[quais], hj = 1.1, vj = -.5, ang = 90),
      diagonal = {
        fx <- ggplot2::layer_scales(p)$x
        xm <- if (!is.null(fx) && fx$is_discrete() == FALSE) max(fx$get_limits()) else NA_real_
        if (is.na(xm)) {
          rlang::abort("Param 'texto': a diagonal só leva texto num eixo X numérico.",
                       class = "tr_view_error_not_numeric")
        }
        data.frame(x = xm, y = v[quais] + .tr_view_um_numero(inclinacao, "inclinacao") * xm,
                   rotulo = rotulos[quais], hj = 1, vj = -.4, ang = 0)
      })
    p <- p + ggplot2::geom_text(
      data = txt, inherit.aes = FALSE,
      ggplot2::aes(x = .data[["x"]], y = .data[["y"]], label = .data[["rotulo"]],
                   hjust = .data[["hj"]], vjust = .data[["vj"]], angle = .data[["ang"]],
                   size = !!.TR_VIEW_TAMANHO_TEXTO))
  }
  p
}

# ---- Reta ajustada -----------------------------------------------------------

#' O nome da coluna por trás de uma estética, ou NULL.
#'
#' Os gráficos da `view` mapeiam sempre `.data[["nome"]]` (ver
#' `.tr_view_mapa()`), e é isso que torna a leitura de volta segura. Estética
#' calculada (`after_stat`, uma expressão) não é coluna, e devolve NULL.
#' @noRd
.tr_view_coluna_de <- function(q, data) {
  if (is.null(q)) return(NULL)
  e <- rlang::quo_get_expr(q)
  nome <- if (is.symbol(e)) as.character(e)
          else if (is.call(e) && identical(e[[1]], quote(`[[`)) && identical(e[[2]], quote(.data)) &&
                   is.character(e[[3]])) e[[3]]
          else NULL
  if (!is.null(nome) && nome %in% names(data)) nome else NULL
}

#' Transformação da escala de um eixo: `"log-10"` ou NULL (identidade).
#' @noRd
.tr_view_escala_log <- function(p, eixo) {
  s <- p$scales$get_scales(eixo)
  nm <- if (is.null(s)) NULL else (s$trans %||% s$transformation)$name
  if (identical(nm, "log-10")) "log-10" else NULL
}

#' Coeficiente formatado como a `models` escreve a equação: 4 algarismos
#' significativos, vírgula decimal. Local, e não importado: a `models` depende
#' da `view`, e não o contrário, e o formatador de lá não é exportado.
#' @noRd
.tr_view_fmt <- function(x, digitos = 4L) {
  formatC(signif(x, digitos), format = "fg", digits = digitos, decimal.mark = ",", flag = "#") |>
    trimws() |> sub(pattern = ",$", replacement = "")
}

#' "ŷ = 1,23 + 0,45·x - 0,0021·x²   R² = 0,752".
#' @noRd
.tr_view_equacao <- function(fit, grau, nx, ny) {
  b <- unname(stats::coef(fit))
  # Hífen, e não o sinal de menos (U+2212) que a `models` usa: a fonte do
  # tema desenha o U+2212 como um traço minúsculo no PNG (medido), e o
  # intercepto negativo já sai com hífen do `formatC`.
  termo <- function(v, suf) sprintf(" %s %s%s", if (v < 0) "-" else "+", .tr_view_fmt(abs(v)), suf)
  pot <- c(sprintf("·%s", nx), sprintf("·%s²", nx))
  eq <- paste0(ny, " = ", .tr_view_fmt(b[[1]]),
               paste(vapply(seq_len(grau), function(j) termo(b[[j + 1L]], pot[[j]]), ""), collapse = ""))
  sprintf("%s   R² = %s", eq, .tr_view_fmt(summary(fit)$r.squared, 3L))
}

#' Reta ou curva ajustada sobre um disperso (ou linha).
#'
#' Os dados e as colunas vêm do PRÓPRIO gráfico de entrada (`p$data` e o
#' mapeamento `x`/`y`), e não de uma segunda entrada de tabela: a reta é
#' sempre a do que está desenhado, sem como ligar a tabela errada.
#'
#' O ajuste é feito aqui, e não por `geom_smooth`, por causa da equação: o
#' número escrito no gráfico tem de ser o da linha desenhada, e os dois saem
#' do mesmo `lm`. Um ajuste por painel (facetas) e, com `por_cor`, por grupo
#' de cor. Com eixo em log, o ajuste é na escala desenhada (log10), como o
#' `geom_smooth` faria, e a equação diz `log(x)`.
#'
#' A faixa é o intervalo de CONFIANÇA da média (`predict(..., interval =
#' "confidence")`), não o de predição: diz onde está a reta, não onde cairá
#' a próxima observação. No loess, ± t·erro-padrão com os gl do loess.
#' @param grafico um ggplot com `x` e `y` numéricos mapeados em colunas.
#' @param metodo `"linear"`, `"quadrática"` ou `"loess"`.
#' @param intervalo desenhar a faixa do intervalo de confiança.
#' @param confianca o nível do intervalo.
#' @param equacao escrever equação e R² (linear e quadrática).
#' @param por_cor uma linha por grupo de cor, se o gráfico tem cor discreta.
#' @return o ggplot com a curva; o atributo `tr_view_ajustes` guarda os
#'   coeficientes e o R² de cada grupo.
#' @export
tr_fit_line <- function(grafico, metodo = "linear", intervalo = TRUE, confianca = 0.95,
                        equacao = TRUE, por_cor = TRUE) {
  p <- .tr_view_camada_entrada(grafico, "view/fit_line")
  metodo <- .tr_view_escolha(metodo, "metodo", .TR_VIEW_AJUSTES)
  conf <- suppressWarnings(as.numeric(confianca))
  if (length(conf) != 1L || is.na(conf) || conf <= 0 || conf >= 1) {
    rlang::abort(sprintf("Param 'confianca': precisa ser um número entre 0 e 1 (ex.: 0,95), não '%s'.",
                         paste(confianca, collapse = ", ")), class = "tr_view_error_bad_option")
  }
  dados <- p$data
  mapa <- p$mapping
  # Estética declarada só na camada (e não no ggplot) também conta: o
  # primeiro `geom` que a declara é o gráfico.
  for (l in p$layers) for (k in names(l$mapping)) if (is.null(mapa[[k]])) mapa[[k]] <- l$mapping[[k]]
  if (!is.data.frame(dados)) {
    rlang::abort("'view/fit_line' precisa de um gráfico feito a partir de uma tabela (um Disperso, uma Linha).",
                 class = "tr_view_error_not_numeric")
  }
  nx <- .tr_view_coluna_de(mapa$x, dados); ny <- .tr_view_coluna_de(mapa$y, dados)
  if (is.null(nx) || is.null(ny) || !is.numeric(dados[[nx]]) || !is.numeric(dados[[ny]])) {
    rlang::abort(paste0("'view/fit_line' ajusta Y em função de X, e o gráfico de entrada não tem X e Y ",
                        "numéricos vindos de colunas. Use-a sobre um Disperso ou uma Linha de duas medidas."),
                 class = "tr_view_error_not_numeric")
  }
  cor <- .tr_view_coluna_de(mapa$colour, dados)
  if (!isTRUE(por_cor) || is.null(cor) || is.numeric(dados[[cor]])) cor <- NULL
  facetas <- c(p$facet$params$facets, p$facet$params$rows, p$facet$params$cols)
  facetas <- unique(unlist(lapply(facetas, .tr_view_coluna_de, data = dados)))
  chaves <- c(facetas, cor)

  lx <- .tr_view_escala_log(p, "x"); ly <- .tr_view_escala_log(p, "y")
  tx <- if (is.null(lx)) identity else log10
  ty <- if (is.null(ly)) identity else log10
  inv <- if (is.null(ly)) identity else function(v) 10^v
  rx <- if (is.null(lx)) "x" else "log(x)"; ry <- if (is.null(ly)) "ŷ" else "log(ŷ)"
  grau <- switch(metodo, linear = 1L, `quadrática` = 2L, loess = NA_integer_)
  minimo <- if (is.na(grau)) 6L else grau + 2L

  d <- data.frame(x = tx(dados[[nx]]), y = ty(dados[[ny]]))
  for (k in chaves) d[[k]] <- dados[[k]]
  d <- d[stats::complete.cases(d) & is.finite(d$x) & is.finite(d$y), , drop = FALSE]
  grupos <- if (length(chaves)) split(d, d[chaves], drop = TRUE) else list(tudo = d)

  curvas <- list(); textos <- list(); ajustes <- list()
  for (g in names(grupos)) {
    dg <- grupos[[g]]
    if (nrow(dg) < minimo || length(unique(dg$x)) < (if (is.na(grau)) 4L else grau + 1L)) {
      rlang::abort(sprintf(paste0("'view/fit_line': o grupo '%s' tem %d ponto(s), e a curva %s precisa de pelo menos %d, ",
                                  "com X distintos. Desligue 'Uma por cor', filtre o grupo ou use a linear."),
                           g, nrow(dg), metodo, minimo), class = "tr_view_error_too_few")
    }
    grade <- data.frame(x = seq(min(dg$x), max(dg$x), length.out = 100L))
    if (is.na(grau)) {
      fit <- stats::loess(y ~ x, data = dg)
      pr <- stats::predict(fit, grade, se = TRUE)
      q <- stats::qt((1 + conf) / 2, pr$df)
      ajuste <- data.frame(fit = pr$fit, lwr = pr$fit - q * pr$se.fit, upr = pr$fit + q * pr$se.fit)
    } else {
      # `I(x^2)` e não `poly()`: os coeficientes do polinômio ortogonal não são
      # os da equação que se escreve, e a equação é o ponto do param.
      fit <- if (grau == 1L) stats::lm(y ~ x, data = dg) else stats::lm(y ~ x + I(x^2), data = dg)
      ajuste <- as.data.frame(stats::predict(fit, grade, interval = "confidence", level = conf))
      ajustes[[g]] <- list(coeficientes = stats::coef(fit), r2 = summary(fit)$r.squared)
      textos[[g]] <- .tr_view_equacao(fit, grau, rx, ry)
    }
    cg <- data.frame(x = grade$x, .fit = ajuste$fit, .lwr = ajuste$lwr, .upr = ajuste$upr, .grupo = g)
    for (k in chaves) cg[[k]] <- dg[[k]][[1]]
    curvas[[g]] <- cg
  }
  cv <- do.call(rbind, curvas)
  # De volta à unidade dos dados: a escala do gráfico transforma de novo na
  # hora de desenhar, e assim a curva cai onde os pontos estão.
  if (!is.null(lx)) cv$x <- 10^cv$x
  cv$.fit <- inv(cv$.fit); cv$.lwr <- inv(cv$.lwr); cv$.upr <- inv(cv$.upr)
  names(cv)[names(cv) == "x"] <- ".x"

  # Expressão, e não valor: `from_theme()` só se resolve DENTRO do desenho,
  # com o tema já aplicado; avaliada aqui, `accent` não existe.
  acento <- rlang::expr(ggplot2::from_theme(accent))
  if (isTRUE(intervalo)) {
    m <- if (is.null(cor)) ggplot2::aes(x = .data[[".x"]], ymin = .data[[".lwr"]], ymax = .data[[".upr"]],
                                        group = .data[[".grupo"]], fill = !!acento)
         else ggplot2::aes(x = .data[[".x"]], ymin = .data[[".lwr"]], ymax = .data[[".upr"]],
                           group = .data[[".grupo"]], fill = .data[[!!cor]])
    p <- p + ggplot2::geom_ribbon(data = cv, mapping = m, inherit.aes = FALSE, alpha = .2,
                                  show.legend = FALSE)
  }
  m <- if (is.null(cor)) ggplot2::aes(x = .data[[".x"]], y = .data[[".fit"]], group = .data[[".grupo"]],
                                      colour = !!acento)
       else ggplot2::aes(x = .data[[".x"]], y = .data[[".fit"]], group = .data[[".grupo"]],
                         colour = .data[[!!cor]])
  # Sem cor por grupo, a cor da curva é constante e não merece legenda; com
  # ela, a curva entra na chave da legenda do grupo, que é a leitura certa.
  p <- p + ggplot2::geom_line(data = cv, mapping = m, inherit.aes = FALSE, linewidth = .9,
                              show.legend = if (is.null(cor)) FALSE else NA)

  if (isTRUE(equacao) && length(textos)) {
    # Uma linha de texto por grupo, empilhadas no canto superior esquerdo de
    # cada painel; com cor, na cor do grupo, que é a legenda da equação.
    eq <- do.call(rbind, lapply(names(textos), function(g) {
      r <- curvas[[g]][1L, c(chaves, ".grupo"), drop = FALSE]; r$.rotulo <- textos[[g]]; r
    }))
    eq$.vj <- if (length(facetas)) stats::ave(seq_len(nrow(eq)), eq[facetas], FUN = seq_along) else seq_len(nrow(eq))
    eq$.vj <- 1.3 + 1.8 * (eq$.vj - 1)
    m <- ggplot2::aes(x = -Inf, y = Inf, label = .data[[".rotulo"]], vjust = .data[[".vj"]],
                      size = !!.TR_VIEW_TAMANHO_TEXTO)
    if (!is.null(cor)) m$colour <- rlang::quo(.data[[!!cor]])
    p <- p + ggplot2::geom_text(data = eq, mapping = m, inherit.aes = FALSE, hjust = -.04,
                                show.legend = FALSE)
  }
  attr(p, "tr_view_ajustes") <- ajustes
  p
}

# ---- Anotação ----------------------------------------------------------------

#' Texto numa coordenada do gráfico, com seta opcional.
#' @param grafico um ggplot (não um painel).
#' @param x,y onde o texto fica, nas unidades dos eixos (vírgula decimal).
#' @param texto o que escrever.
#' @param seta_x,seta_y para onde a seta aponta; os dois vazios, sem seta.
#' @return o ggplot anotado.
#' @export
tr_annotate <- function(grafico, x = "", y = "", texto = "", seta_x = "", seta_y = "") {
  p <- .tr_view_camada_entrada(grafico, "view/annotate")
  tx <- trimws(paste(as.character(texto), collapse = " "))
  if (!nzchar(tx)) rlang::abort("Param 'texto': preencha o que escrever.", class = "tr_view_error_blank_param")
  px <- .tr_view_um_numero(x, "x"); py <- .tr_view_um_numero(y, "y")
  sx <- .tr_view_um_numero(seta_x, "seta_x", vazio = TRUE)
  sy <- .tr_view_um_numero(seta_y, "seta_y", vazio = TRUE)
  if (length(sx) != length(sy)) {
    rlang::abort("Params 'seta_x' e 'seta_y': a seta precisa dos dois, ou de nenhum.",
                 class = "tr_view_error_bad_option")
  }
  # Sem coluna de painel nos dados da camada, a anotação cai em TODOS os
  # painéis. A seta sai do texto e termina no ponto, e vai antes para o texto
  # ficar por cima. O texto é `geom_text` de uma linha, e não `annotate()`,
  # só para poder levar o tamanho do tema (ver `.TR_VIEW_TAMANHO_TEXTO`).
  if (length(sx)) {
    p <- p + ggplot2::annotate("segment", x = px, y = py, xend = sx, yend = sy, linewidth = .5,
                               arrow = grid::arrow(length = grid::unit(2.5, "mm"), type = "closed"))
  }
  p + ggplot2::geom_text(data = data.frame(x = px, y = py, rotulo = tx), inherit.aes = FALSE,
                         ggplot2::aes(x = .data[["x"]], y = .data[["y"]], label = .data[["rotulo"]],
                                      size = !!.TR_VIEW_TAMANHO_TEXTO))
}

# ---- Nós ---------------------------------------------------------------------

.tr_view_nos_camadas <- function(P, G) {
  camada <- function(id, fn, label, desc, icone, params, help) {
    trama::tr_node(id, fn = fn, label = label, category = "camadas", description = desc,
                   icon = trama::tr_icon(icone), inputs = list(grafico = G), outputs = list(out = G),
                   params = params, help = help)
  }
  list(
    camada("view/reference", tr_reference, "Linha de referência",
      "Acrescenta ao gráfico linhas de referência (limite, meta, y = x) e uma faixa sombreada.",
      "separator-horizontal",
      list(
        tipo = trama::tr_param_enum("horizontal", .TR_VIEW_REFERENCIAS, label = "Tipo"),
        valor = P("text", "0", label = "Valor", example = "10; 20"),
        inclinacao = trama::tr_param_num(1, label = "Inclinação"),
        ate = P("text", "", label = "Faixa até", example = "25"),
        texto = P("text", "", label = "Texto", example = "limite legal"),
        estilo = trama::tr_param_enum("tracejada", .TR_VIEW_ESTILOS_LINHA, label = "Estilo")),
      "## Descrição

Acrescenta a um gráfico uma ou mais linhas de referência — o limite legal, a
meta, a média histórica, a reta y = x de um observado contra previsto — e,
opcionalmente, uma faixa sombreada entre dois valores. O gráfico que entra sai
igual, com a linha por cima: tema, proporção e rótulos são os dele.

Com painéis (**Painéis por**), a referência aparece em todos. Com eixo em log,
o valor é dado na unidade dos dados, e a linha cai no lugar certo.

Um painel (`view/combine`) não é aceito: ligue a referência no gráfico, antes
do painel.

## Parâmetros

- **Tipo** — `horizontal` (y = valor), `vertical` (x = valor) ou `diagonal`
  (y = valor + inclinação·x).
- **Valor** — um número, ou vários separados por `;` (`10; 20`). Vírgula é
  decimal: `2,5`. Na diagonal, o intercepto.
- **Inclinação** — só na diagonal; `1` com valor `0` é a reta y = x.
- **Faixa até** — vazio desliga; preenchido, sombreia a faixa entre **Valor**
  (um só) e este número. Não vale na diagonal.
- **Texto** — escrito junto da linha; vários separados por `;`, um por valor.
- **Estilo** — `tracejada` (padrão), `contínua` ou `pontilhada`.

## Valor

O mesmo gráfico (`view/plot`), com a referência. No console,
`tr_reference(p, \"horizontal\", \"10; 20\", texto = \"mín.; máx.\")`.

## Exemplos

```r
tr_flow(reg) |>
  tr_add(\"ler\", \"data/example\", dataset = \"mtcars\") |>
  tr_add(\"g\", \"view/points\", x = \"wt\", y = \"mpg\", from = \"ler\") |>
  tr_add(\"ref\", \"view/reference\", valor = \"20\", ate = \"25\",
         texto = \"faixa-alvo\", from = \"g\")
```

## Veja também

`view/fit_line` para a reta ajustada aos pontos; `view/annotate` para marcar
um ponto; `view/combine` depois das camadas."),

    camada("view/fit_line", tr_fit_line, "Reta ajustada",
      "Acrescenta a reta (ou curva) ajustada aos pontos, com IC e equação.",
      "trending-up",
      list(
        metodo = trama::tr_param_enum("linear", .TR_VIEW_AJUSTES, label = "Método"),
        intervalo = trama::tr_param_bool(TRUE, label = "Intervalo"),
        confianca = trama::tr_param_num(0.95, min = 0.5, max = 0.999, step = 0.01, label = "Confiança (IC)"),
        equacao = trama::tr_param_bool(TRUE, label = "Equação e R²"),
        por_cor = trama::tr_param_bool(TRUE, label = "Uma por cor")),
      "## Descrição

Acrescenta a um Disperso (ou a uma Linha) a reta ou curva ajustada aos
pontos, com a faixa do intervalo de confiança e, na linear e na quadrática, a
equação e o R² escritos no canto do gráfico. Os dados são os do PRÓPRIO
gráfico de entrada: o X e o Y dele, e nenhuma tabela a mais.

Com painéis, há um ajuste por painel, e cada equação fica no seu. Com cor
por grupo e **Uma por cor** ligado, uma curva por cor, na cor do grupo. Com
eixo em log, o ajuste é na escala desenhada (log10), e a equação diz `log(x)`.

A faixa é o intervalo de confiança da MÉDIA — onde está a reta —, e não o de
predição, que diria onde cai a próxima observação e é bem mais largo.

Para o ajuste com teste, resíduos e pressupostos, use a coleção de modelos; a
reta aqui é para VER.

## Parâmetros

- **Método** — `linear` (padrão), `quadrática` (ŷ = a + b·x + c·x²) ou
  `loess` (regressão local, sem equação).
- **Intervalo** — a faixa do intervalo de confiança. Ligado por padrão.
- **Confiança (IC)** — o nível da faixa, padrão 0,95.
- **Equação e R²** — escreve a equação com vírgula decimal, como nas teses.
- **Uma por cor** — com cor por grupo no gráfico, uma curva por grupo;
  desligado, uma curva para todos os pontos.

## Valor

O mesmo gráfico (`view/plot`), com a curva. No console,
`tr_fit_line(p, \"quadrática\")`; os coeficientes ficam em
`attr(saida, \"tr_view_ajustes\")`.

## Exemplos

```r
tr_flow(reg) |>
  tr_add(\"ler\", \"data/example\", dataset = \"mtcars\") |>
  tr_add(\"g\", \"view/points\", x = \"wt\", y = \"mpg\", painel = \"am\", from = \"ler\") |>
  tr_add(\"reta\", \"view/fit_line\", metodo = \"linear\", from = \"g\")
```

## Veja também

`view/points` com **Tendência** para a reta sem equação; `models/lm` e
`models/plot_regression` para a regressão com inferência; `view/reference`
para linhas fixas."),

    camada("view/annotate", tr_annotate, "Anotação",
      "Escreve um texto numa coordenada do gráfico, com seta opcional.",
      "message-square-text",
      list(
        x = P("text", "", label = "X", example = "3,5"),
        y = P("text", "", label = "Y", example = "30"),
        texto = P("text", "", label = "Texto", example = "outlier"),
        seta_x = P("text", "", label = "Seta até X", example = "5,4"),
        seta_y = P("text", "", label = "Seta até Y", example = "10,4")),
      "## Descrição

Escreve um texto num ponto do gráfico — o nome de um outlier, o início de um
tratamento, uma observação — e, se quiser, uma seta do texto até outro ponto.
As coordenadas são nas unidades dos eixos. Com painéis, a anotação aparece em
todos.

## Parâmetros

- **X**, **Y** — onde fica o texto, na unidade dos eixos. Vírgula é decimal.
  Só para eixos numéricos.
- **Texto** — o que escrever. Obrigatório.
- **Seta até X**, **Seta até Y** — para onde a seta aponta. Os dois vazios,
  sem seta; um só preenchido para o nó.

## Valor

O mesmo gráfico (`view/plot`), anotado. No console,
`tr_annotate(p, 3.5, 30, \"outlier\", seta_x = 5.4, seta_y = 10.4)`.

## Exemplos

```r
tr_flow(reg) |>
  tr_add(\"ler\", \"data/example\", dataset = \"mtcars\") |>
  tr_add(\"g\", \"view/points\", x = \"wt\", y = \"mpg\", from = \"ler\") |>
  tr_add(\"nota\", \"view/annotate\", x = \"4\", y = \"30\", texto = \"carros leves\",
         seta_x = \"2\", seta_y = \"32\", from = \"g\")
```

## Veja também

`view/labels` para escrever o nome de TODOS os pontos; `view/reference` para
linhas e faixas.")
  )
}
