# Médias ajustadas (`emmeans`), as letras e as comparações.
#
# Médias AJUSTADAS, e não as médias da tabela: no desbalanceado, e em todo
# modelo com covariável ou bloco, a média crua de um tratamento carrega o
# efeito dos blocos em que ele caiu. É a média que o SAS chama de LSMEANS, e é
# a que o teste de Tukey compara de verdade.

.TR_MODELS_AJUSTES <- c("tukey", "bonferroni", "holm", "sidak", "nenhum")

.tr_models_ajuste_r <- function(ajuste) if (ajuste == "nenhum") "none" else ajuste

#' O modelo que o `emmeans` sabe usar, com a tabela ao lado.
#' @noRd
.tr_models_modelo_emm <- function(fit) {
  if (fit$classe == "split") fit$aux_misto else fit$ajuste
}

#' Letras de médias (algoritmo de inserir e absorver, Piepho 2004).
#'
#' Médias que compartilham uma letra não diferem ao nível escolhido. Começa com
#' uma coluna em que todos têm a letra; para cada par que difere, toda coluna
#' que contém os dois é partida em duas, cada uma sem um deles; no fim, coluna
#' contida em outra é absorvida. As letras seguem a ordem das médias, da maior
#' para a menor — "a" é o grupo da maior média, que é como as tabelas de
#' experimentação escrevem.
#' @param medias vetor numérico.
#' @param difere matriz lógica simétrica k × k (TRUE = o par difere).
#' @return vetor de letras, na ordem de `medias`.
#' @noRd
.tr_models_letras <- function(medias, difere) {
  k <- length(medias)
  if (k == 1L) return("a")
  ord <- order(medias, decreasing = TRUE)
  difere <- difere[ord, ord, drop = FALSE]
  cols <- list(rep(TRUE, k))
  for (i in seq_len(k - 1L)) for (j in (i + 1L):k) {
    if (!isTRUE(difere[i, j])) next
    novas <- list()
    for (cc in cols) {
      if (cc[i] && cc[j]) {
        a <- cc; a[i] <- FALSE
        b <- cc; b[j] <- FALSE
        novas <- c(novas, list(a, b))
      } else novas <- c(novas, list(cc))
    }
    # Absorver: duplicatas saem, e coluna contida em outra também.
    novas <- unique(novas)
    contida <- vapply(seq_along(novas), function(x) {
      any(vapply(seq_along(novas), function(y) y != x && all(novas[[x]] <= novas[[y]]), TRUE))
    }, TRUE)
    cols <- novas[!contida]
  }
  # Ordena as colunas pela primeira média (da maior) que as tem.
  primeira <- vapply(cols, function(cc) which(cc)[[1]], 1L)
  cols <- cols[order(primeira)]
  alfabeto <- c(letters, LETTERS, paste0(letters, "'"))
  let <- vapply(seq_len(k), function(i) {
    paste(alfabeto[which(vapply(cols, function(cc) cc[i], TRUE))], collapse = "")
  }, "")
  let[order(ord)]
}

#' A matriz "difere" de uma grade sem condição, a partir dos pares do `emmeans`.
#'
#' Os contrastes `pairwise` saem na ordem do `combn` (1-2, 1-3, ..., 2-3...), o
#' que dispensa ler o NOME do contraste — nome que quebra quando um nível tem
#' " - " dentro, como "0 - 10 cm".
#' @noRd
.tr_models_difere <- function(grade, ajuste, alfa) {
  k <- nrow(summary(grade))
  m <- matrix(FALSE, k, k)
  if (k < 2L) return(m)
  pp <- summary(emmeans::contrast(grade, "pairwise", by = NULL, adjust = .tr_models_ajuste_r(ajuste)))
  pares <- utils::combn(k, 2L)
  m[t(pares)] <- pp$p.value < alfa
  m | t(m)
}

#' Padroniza as colunas do `summary.emmGrid`.
#'
#' O nome da coluna da média muda com a escala (`emmean`, `response`, `rate`,
#' `prob`), e o do intervalo com os gl (`lower.CL`, `asymp.LCL`). Pela posição
#' e por padrão, e não por nome fixo, para servir a todo modelo.
#' @noRd
.tr_models_emm_tabela <- function(s, fatores) {
  s <- as.data.frame(s)
  est <- setdiff(names(s), c(fatores, "SE", "df"))[[1]]
  li <- grep("LCL|lower", names(s), value = TRUE)[[1]]
  ls <- grep("UCL|upper", names(s), value = TRUE)[[1]]
  out <- s[, fatores, drop = FALSE]
  out$media <- s[[est]]; out$erro_padrao <- s$SE; out$gl <- s$df
  out$li <- s[[li]]; out$ls <- s[[ls]]
  out
}

#' Médias ajustadas, com letras.
#' @param modelo objeto `tr_models_fit`.
#' @param especs fatores das médias, separados por vírgula (`"tratamento"`, ou
#'   `"A, B"` para as combinações).
#' @param por fatores de condição: as médias e as letras são feitas DENTRO de
#'   cada nível deles (o desdobramento da interação).
#' @param ajuste correção das comparações que fazem as letras.
#' @param alfa nível das letras.
#' @param escala `"resposta"` ou `"ligação"` (só muda algo no GLM).
#' @return objeto `tr_models_emm`.
#' @export
tr_models_emmeans <- function(modelo, especs = "", por = "", ajuste = "tukey", alfa = 0.05,
                              escala = "resposta") {
  .tr_models_fit_conferir(modelo)
  no <- "models/emmeans"
  ajuste <- .tr_models_enum(ajuste, .TR_MODELS_AJUSTES, "ajuste")
  alfa <- .tr_models_num(alfa, "alfa", min = 0.001, max = 0.5)
  escala <- .tr_models_enum(escala, c("resposta", "ligação"), "escala")
  esp <- .tr_models_cols(modelo$dados, especs, "especs", minimo = 1L, maximo = 3L)
  cond <- .tr_models_cols(modelo$dados, por, "por", minimo = 0L, maximo = 2L)
  for (v in c(esp, cond)) {
    if (!is.factor(modelo$dados[[v]])) {
      .tr_models_abort("tr_models_error_not_applicable",
                       paste0("'%s': '%s' é numérica no modelo, e médias ajustadas são por nível de ",
                              "fator. Nos blocos de ANOVA isso é automático; no 'models/lm', escreva ",
                              "'factor(%s)' na fórmula ou converta a coluna antes."), no, v, v)
    }
  }
  if (length(intersect(esp, cond))) {
    .tr_models_abort("tr_models_error_bad_option", "'%s': a mesma coluna está em 'especs' e em 'por'.", no)
  }
  aj <- .tr_models_modelo_emm(modelo)
  args <- list(aj, specs = esp, by = if (length(cond)) cond else NULL, data = modelo$dados)
  if (modelo$classe %in% c("lmer", "split")) args$lmer.df <- "satterthwaite"
  # No GLS, Satterthwaite explícito (é o padrão do emmeans hoje, e não se
  # depende do padrão): o gl n − p do nlme é liberal com poucos grupos.
  if (modelo$classe == "gls") args$mode <- "satterthwaite"
  if (modelo$classe %in% c("glm", "glmer") && escala == "resposta") args$type <- "response"
  r <- .tr_models_ajustar(.tr_models_capturar(do.call(emmeans::emmeans, args)), no)
  grade <- r$valor
  s <- summary(grade, level = 1 - alfa)
  tab <- .tr_models_emm_tabela(s, c(esp, cond))
  # Letras DENTRO de cada combinação dos fatores de condição.
  tab$grupo <- NA_character_
  chave <- if (length(cond)) interaction(tab[, cond, drop = FALSE], drop = TRUE) else factor(rep(1L, nrow(tab)))
  for (g in levels(chave)) {
    i <- which(chave == g)
    dif <- .tr_models_ajustar(.tr_models_difere(grade[i], ajuste, alfa), no)
    tab$grupo[i] <- .tr_models_letras(tab$media[i], dif)
  }
  for (v in c(esp, cond)) tab[[v]] <- factor(tab[[v]], levels = levels(modelo$dados[[v]]))
  interacao <- any(grepl("interaction", r$avisos))
  nota <- .tr_models_nota(
    sprintf("letras: %s a %s%%", ajuste, formatC(100 * alfa, format = "fg", decimal.mark = ",")),
    if (interacao) "o fator participa de interação: veja as médias com 'por'" else "",
    if (modelo$classe == "split") "gl de Satterthwaite pelo misto equivalente" else "",
    if (modelo$classe == "gls") "GLS: gl de Satterthwaite (o quadro e os coeficientes usam n − p, do nlme)" else "",
    if (modelo$classe %in% c("glm", "glmer") && escala == "resposta") "médias na escala da resposta" else "",
    if (modelo$classe == "glmer") "GLM misto: médias no efeito aleatório zero (sujeito típico), não médias populacionais" else "")
  .tr_models_emm_obj(grade, tibble::as_tibble(tab), esp, cond, ajuste, alfa, modelo$resposta, nota)
}

#' Comparações de médias: todos os pares, ou contra um controle (Dunnett).
#' @param medias objeto `tr_models_emm`.
#' @param metodo `"todos os pares"` ou `"contra controle"`.
#' @param controle nível do controle (com `"contra controle"`).
#' @param ajuste correção; `"dunnett"` é o padrão natural contra controle.
#'   É o Dunnett exato: `adjust = "mvt"` do emmeans, integração da t
#'   multivariada (Genz-Bretz), com erro numérico de ~1e-3 a partir de 3
#'   contrastes.
#' @param .seed semente do nó: torna o Dunnett (Monte Carlo quase-aleatório)
#'   reprodutível, sem mexer no estado aleatório da sessão.
#' @return objeto `tr_models_effects`.
#' @export
tr_models_pairwise <- function(medias, metodo = "todos os pares", controle = "", ajuste = "tukey", .seed = NULL) {
  .tr_models_emm_conferir(medias)
  no <- "models/pairwise"
  if (is.null(medias$grade)) {
    .tr_models_abort("tr_models_error_not_applicable",
                     paste0("'%s': as letras de %s não vêm de contrastes com p-valor. Para as comparações ",
                            "par a par, ligue um 'models/emmeans' ao modelo."), no, medias$ajuste)
  }
  metodo <- .tr_models_enum(metodo, c("todos os pares", "contra controle"), "metodo")
  ajuste <- .tr_models_enum(ajuste, c(.TR_MODELS_AJUSTES, "dunnett"), "ajuste")
  aj_r <- if (ajuste == "dunnett") "mvt" else .tr_models_ajuste_r(ajuste)
  grade <- medias$grade
  if (metodo == "todos os pares") {
    if (ajuste == "dunnett") {
      .tr_models_abort("tr_models_error_bad_option",
                       "'%s': o ajuste de Dunnett é para comparações contra um controle. Escolha 'contra controle', ou use 'tukey'.", no)
    }
    ct <- .tr_models_ajustar(emmeans::contrast(grade, "pairwise", adjust = aj_r), no)
    titulo <- "Comparações entre pares"
  } else {
    if (length(medias$especs) != 1L) {
      .tr_models_abort("tr_models_error_not_applicable",
                       "'%s': contra controle pede médias de UM fator em 'especs' (vieram %s). Use 'por' para os outros.",
                       no, paste(medias$especs, collapse = ", "))
    }
    ctl <- .tr_models_obrigatorio(controle, "controle")
    niveis <- levels(medias$tabela[[medias$especs]])
    ref <- match(ctl, niveis)
    if (is.na(ref)) {
      .tr_models_abort("tr_models_error_unknown_level",
                       "Param 'controle': '%s' não é nível de '%s'. Níveis: %s.",
                       ctl, medias$especs, paste(niveis, collapse = ", "))
    }
    ct <- .tr_models_ajustar(emmeans::contrast(grade, "trt.vs.ctrl", ref = ref, adjust = aj_r), no)
    titulo <- sprintf("Contra o controle (%s)", ctl)
  }
  semente <- if (is.null(.seed) || !length(.seed) || is.na(.seed[[1]])) 1955L else as.integer(.seed[[1]])
  s <- .tr_models_com_semente(semente, as.data.frame(summary(ct, infer = c(TRUE, TRUE), level = 0.95)))
  est <- setdiff(names(s), c("contrast", medias$por, "SE", "df", "null"))[[1]]
  estat <- grep("ratio$", names(s), value = TRUE)
  estat <- estat[estat != est][[1]]
  li <- grep("LCL|lower", names(s), value = TRUE)[[1]]
  ls <- grep("UCL|upper", names(s), value = TRUE)[[1]]
  tab <- data.frame(s[, medias$por, drop = FALSE], termo = as.character(s$contrast),
                    estimativa = s[[est]], erro_padrao = s$SE, gl = s$df,
                    estat = s[[estat]], p_valor = s$p.value, li_95 = s[[li]], ls_95 = s[[ls]],
                    check.names = FALSE)
  col <- if (startsWith(estat, "z")) "z" else "t"
  names(tab)[names(tab) == "estat"] <- col
  if (length(medias$por)) {
    # O card lê só `termo`: com condição, ela entra no rótulo para que "L - M"
    # de lã A e de lã B não pareçam a mesma linha repetida.
    rot <- do.call(paste, c(lapply(medias$por, function(v) paste0(v, " ", s[[v]])), sep = ", "))
    tab$termo <- paste0(tab$termo, " | ", rot)
  }
  .tr_models_efeitos(tibble::as_tibble(tab), titulo, coluna_estat = col,
                     rodape = list(ajuste = ajuste),
                     nota = if (est %in% c("ratio", "odds.ratio")) "estimativa como razão (escala da resposta)" else "",
                     fonte = switch(ajuste, tukey = "Tukey (1949)", dunnett = "Dunnett (1955)",
                                    bonferroni = "Bonferroni (1936)", holm = "Holm (1979)",
                                    sidak = "Šidák (1967)", nenhum = "Lenth (2016)"))
}

#' Médias com intervalo de confiança e letras.
#' @param medias objeto `tr_models_emm`.
#' @param letras escrever as letras acima dos intervalos.
#' @export
tr_models_plot_means <- function(medias, letras = TRUE, aspecto = "16:9", tema = "padrão", titulo = "",
                                 rotulo_x = "", rotulo_y = "", legenda = "direita") {
  .tr_models_emm_conferir(medias)
  d <- as.data.frame(medias$tabela)
  x <- medias$especs[[1]]
  cor <- if (length(medias$especs) >= 2L) medias$especs[[2]] else NULL
  d$.cor <- if (is.null(cor)) "média" else d[[cor]]
  dodge <- ggplot2::position_dodge(width = if (is.null(cor)) 0 else .5)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[[x]], y = .data[["media"]], colour = .data[[".cor"]],
                                       group = .data[[".cor"]])) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data[["li"]], ymax = .data[["ls"]]), width = .15,
                           position = dodge, linewidth = .6) +
    ggplot2::geom_point(size = 2.6, position = dodge)
  if (isTRUE(letras)) {
    p <- p + ggplot2::geom_text(ggplot2::aes(y = .data[["ls"]], label = .data[["grupo"]]),
                                vjust = -.6, size = 3.6, position = dodge, show.legend = FALSE)
  }
  p <- if (is.null(cor)) p + ggplot2::scale_colour_manual(values = c(`média` = .TR_MODELS_COR), guide = "none")
       else p + ggplot2::labs(colour = cor)
  if (length(medias$por)) {
    p <- p + ggplot2::facet_wrap(stats::as.formula(paste("~", paste(.tr_models_bt(medias$por), collapse = " + "))),
                                 labeller = ggplot2::label_both)
  }
  p <- p + ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(.05, .12))) +
    ggplot2::labs(x = x, y = sprintf("%s (média ajustada e IC %s%%)", medias$resposta,
                                     formatC(100 * (1 - medias$alfa), format = "fg", decimal.mark = ",")))
  trama.view::tr_view_finish(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}
