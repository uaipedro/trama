# Fontes: de onde uma série nasce.
#
# Só duas, e nenhuma lê arquivo: ler CSV, Excel ou Parquet é da `data`, e a
# série chega de lá como tabela, pelo `series/from_table`. Duplicar leitores
# aqui daria dois lugares para consertar o mesmo `delim`.

#' Uma das séries do pacote `datasets`.
#'
#' É a metade que o `data/example` recusa de propósito: dos conjuntos do R, 31
#' são `ts`, e lá eles pintariam o card de vermelho no `store` de
#' `data/table`. Aqui eles são o assunto.
#'
#' Série múltipla vira uma opção por coluna (`EuStockMarkets$DAX`): o tipo é
#' univariado, e um param a mais só para escolher a coluna seria um campo que
#' metade das opções ignora.
#' @export
tr_series_example <- function(dataset = "AirPassengers") {
  x <- NULL
  if (is.character(dataset) && length(dataset) == 1L && !is.na(dataset)) {
    partes <- strsplit(dataset, "$", fixed = TRUE)[[1]]
    obj <- tryCatch(get(partes[[1]], envir = asNamespace("datasets")), error = function(e) NULL)
    if (length(partes) == 1L && stats::is.ts(obj) && is.null(dim(obj))) {
      x <- obj
    } else if (length(partes) == 2L && stats::is.mts(obj) && partes[[2]] %in% colnames(obj)) {
      x <- .tr_series_uni(obj[, partes[[2]]])
    }
  }
  # A lista de aceitos só é montada no caminho do erro, pelo mesmo motivo do
  # `data/example`: derivá-la custa carregar os ~100 objetos do `datasets`.
  if (is.null(x)) .tr_series_option("dataset", dataset, .tr_series_exemplos())
  x
}

#' As séries ofertadas: derivadas do `datasets` que está rodando, nunca
#' escritas à mão — mesma escolha do `data/example`, e pelo mesmo motivo.
#' @noRd
.tr_series_exemplos <- function() {
  itens <- unique(sub(" .*", "", utils::data(package = "datasets")$results[, "Item"]))
  ofertas <- unlist(lapply(itens, function(n) {
    x <- tryCatch(get(n, envir = asNamespace("datasets")), error = function(e) NULL)
    if (stats::is.mts(x)) paste0(n, "$", colnames(x))
    else if (stats::is.ts(x) && is.null(dim(x))) n
  }))
  ofertas[order(tolower(ofertas))]
}

#' Monta a série a partir de uma tabela.
#'
#' A frequência é declarada AQUI, uma vez, e viaja com a série — é o motivo
#' de o tipo `series/ts` existir.
#'
#' Com uma coluna de tempo, o nó ORDENA por ela e confere a grade. As duas
#' recusas (tempo repetido, período faltando) são o coração do nó: um `ts` é
#' só um vetor com início e frequência, e não sabe que a linha de maio sumiu.
#' Sem a checagem, junho passaria a ser chamado de maio e todo o calendário
#' dali em diante sairia deslocado de um mês — a sazonalidade estimada, a
#' previsão e o gráfico sazonal errados, e o card verde.
#'
#' A grade só é conferida onde há calendário a conferir: data com frequência
#' 12, 4 ou 1, e ano inteiro com frequência 1. Para o resto (diária com ciclo
#' semanal, por exemplo) a coluna só ordena, e a ajuda diz isso.
#'
#' `inicio` e uma coluna de datas ao mesmo tempo é recusado, e não resolvido
#' por precedência: seriam duas fontes para o mesmo fato, e a que perdesse
#' discordaria da tela sem aviso.
#' @export
tr_series_from_table <- function(dados, valor = "", tempo = "", frequencia = 12L, inicio = "") {
  valor <- .tr_series_col(dados, .tr_series_obrigatorio(valor, "valor"), "valor")
  f <- .tr_series_int(frequencia, "frequencia", min = 1)
  v <- dados[[valor]]
  if (!is.numeric(v)) {
    .tr_series_abort("tr_series_error_not_numeric",
                     paste0("Param 'valor': a coluna '%s' é '%s', e uma série precisa de número. ",
                            "Converta-a antes num 'data/convert'."), valor, class(v)[[1]])
  }
  tempo <- trimws(as.character(tempo %||% "")[[1]])
  inicio <- trimws(as.character(inicio %||% "")[[1]])
  start <- 1

  if (nzchar(tempo)) {
    t <- dados[[.tr_series_col(dados, tempo, "tempo")]]
    if (anyNA(t)) {
      .tr_series_abort("tr_series_error_gap",
                       "Param 'tempo': a coluna '%s' tem %d instante(s) faltante(s); a série não sabe onde pô-los.",
                       tempo, sum(is.na(t)))
    }
    o <- order(t); t <- t[o]; v <- v[o]
    if (anyDuplicated(t)) {
      .tr_series_abort("tr_series_error_duplicate_time",
                       "Param 'tempo': o instante %s aparece mais de uma vez. Agregue antes (data/group_summarise).",
                       format(t[anyDuplicated(t)]))
    }
    calendario <- inherits(t, c("Date", "POSIXt")) && f %in% c(1, 4, 12)
    anos <- is.numeric(t) && f == 1 && all(t == round(t))
    if (calendario || anos) {
      if (nzchar(inicio)) {
        .tr_series_abort("tr_series_error_bad_period",
                         paste0("Param 'inicio': a coluna '%s' já diz quando a série começa. Deixe o ",
                                "início em branco, ou esvazie o tempo."), tempo)
      }
      if (calendario) {
        d <- as.Date(t)
        ano <- as.integer(format(d, "%Y")); mes <- as.integer(format(d, "%m"))
        per <- (mes - 1L) %/% (12L %/% as.integer(f))
        idx <- ano * f + per
        if (anyDuplicated(idx)) {
          .tr_series_abort("tr_series_error_duplicate_time",
                           paste0("Param 'tempo': %s e %s caem no mesmo período de uma série de frequência %d. ",
                                  "Agregue antes (data/group_summarise)."),
                           format(d[anyDuplicated(idx) - 1L]), format(d[anyDuplicated(idx)]), f)
        }
        start <- c(ano[[1]], per[[1]] + 1L)
      } else {
        idx <- t
        start <- t[[1]]
      }
      salto <- which(diff(idx) != 1)
      if (length(salto)) {
        i <- salto[[1]]
        .tr_series_abort("tr_series_error_gap",
                         paste0("Param 'tempo': entre %s e %s faltam %d período(s). Uma série não pula ",
                                "períodos; complete a tabela (com NA no valor, se não houver dado) ",
                                "ou recorte-a antes."),
                         format(t[[i]]), format(t[[i + 1L]]), as.integer(idx[[i + 1L]] - idx[[i]] - 1))
      }
    } else if (nzchar(inicio)) {
      start <- .tr_series_periodo(inicio, "inicio", f)
    }
  } else if (nzchar(inicio)) {
    start <- .tr_series_periodo(inicio, "inicio", f)
  }
  stats::ts(as.numeric(v), start = start, frequency = f)
}

`%||%` <- function(x, y) if (is.null(x)) y else x
