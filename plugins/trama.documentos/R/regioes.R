# Regiões gerenciadas: o formato do documento, sem projeto nem Quarto.
#
# Um documento é um `.qmd` da pessoa. O trama só é dono dos blocos cercados
# com a classe `.trama` e o id `#trama-<no>`:
#
#   ::: {#trama-lm .trama trama-hash="3f9a…"}
#   <conteúdo gerado pelo trama>
#   :::
#
# Tudo o mais (texto, títulos, chunks, YAML) é da pessoa, e as funções daqui
# são as únicas que escrevem no arquivo. Elas trabalham sobre as linhas CRUAS
# (cada uma com o `\r` que trouxe, se o arquivo é CRLF), para que as linhas de
# fora das regiões voltem ao disco byte a byte como estavam; a regra de dono
# é conferida antes de gravar (`.td_conferir_fora()`), não só confiada à
# construção.

#' Lê o documento: linhas cruas, BOM e quebra final, para regravar igual.
#' @noRd
.td_ler <- function(caminho) {
  if (!file.exists(caminho)) {
    .td_erro(sprintf("Documento n\u{E3}o encontrado: '%s'.", caminho), "tr_documento_erro_caminho")
  }
  bruto <- readBin(caminho, "raw", file.info(caminho)$size)
  txt <- rawToChar(bruto)
  Encoding(txt) <- "UTF-8"
  bom <- startsWith(txt, "\u{FEFF}")
  if (bom) txt <- sub("^\u{FEFF}", "", txt)
  list(linhas = if (nzchar(txt)) strsplit(txt, "\n", fixed = TRUE)[[1]] else character(),
       final = endsWith(txt, "\n"), bom = bom, bruto = bruto)
}

#' Texto do documento de volta, com o mesmo BOM e a mesma quebra final.
#' @noRd
.td_texto <- function(doc) {
  txt <- paste(doc$linhas, collapse = "\n")
  if (doc$final && length(doc$linhas)) txt <- paste0(txt, "\n")
  if (doc$bom) txt <- paste0("\u{FEFF}", txt)
  txt
}

#' O arquivo ainda é o que foi lido? Conferido também antes de materializar,
#' para a pasta `_trama/` não andar na frente de um documento que não será
#' gravado.
#' @noRd
.td_intacto <- function(caminho, doc) {
  agora <- readBin(caminho, "raw", file.info(caminho)$size)
  if (!identical(agora, doc$bruto)) {
    .td_erro(paste0("O documento mudou no disco durante a opera\u{E7}\u{E3}o (salvo no editor de texto?). ",
                    "Nada foi gravado; tente de novo."), "tr_documento_erro_concorrente")
  }
  invisible(TRUE)
}

#' Grava de forma atômica, e só se o arquivo não mudou desde a leitura: a
#' pessoa pode ter salvo no editor de texto enquanto o trama calculava.
#' @noRd
.td_gravar <- function(caminho, doc) {
  .td_intacto(caminho, doc)
  tmp <- tempfile(tmpdir = dirname(caminho), fileext = ".tmp")
  con <- file(tmp, "wb")
  writeBin(charToRaw(enc2utf8(.td_texto(doc))), con)
  close(con)
  if (!file.rename(tmp, caminho)) {
    unlink(tmp)
    .td_erro(sprintf("N\u{E3}o foi poss\u{ED}vel gravar '%s'.", caminho), "tr_documento_erro_caminho")
  }
  invisible(caminho)
}

.td_sem_cr <- function(x) sub("\r$", "", x)

# Cerca de div do Pandoc: `:::` (3 ou mais) e atributos em chaves ou uma
# palavra (`::: callout-note`); fecha com só os dois-pontos. Cerca de código
# com qualquer recuo: dentro de item de lista ela vem com 4 ou mais espaços.
.TD_DIV_ABRE <- "^(\\s*)(:{3,})\\s*(\\{.*\\}|[^\\s:{]\\S*)\\s*:*\\s*$"
.TD_DIV_FECHA <- "^\\s*:{3,}\\s*$"
.TD_CODIGO <- "^\\s*(`{3,}|~{3,})"

#' Atributos de uma cerca `{#id .classe chave="valor"}`, na ordem do texto.
#' @noRd
.td_atributos <- function(txt) {
  dentro <- sub("^\\{(.*)\\}$", "\\1", trimws(txt))
  toks <- regmatches(dentro, gregexpr(
    "#[^\\s}]+|\\.[^\\s}=]+|[A-Za-z_][A-Za-z0-9_:.-]*=(\"[^\"]*\"|'[^']*'|[^\\s}]+)", dentro, perl = TRUE))[[1]]
  id <- NULL; classes <- character(); pares <- list()
  for (t in toks) {
    if (startsWith(t, "#")) id <- substring(t, 2L)
    else if (startsWith(t, ".")) classes <- c(classes, substring(t, 2L))
    else {
      k <- sub("=.*$", "", t)
      v <- sub("^[^=]*=", "", t)
      pares[[k]] <- if (grepl("^([\"']).*\\1$", v)) substr(v, 2L, nchar(v) - 1L) else v
    }
  }
  list(id = id, classes = classes, pares = pares)
}

#' Última linha do YAML do topo (0 sem YAML). O YAML abre com `---` na
#' primeira linha e fecha com `---` ou `...` na coluna 0: um `...` recuado
#' dentro de um `abstract: |` não fecha.
#' @noRd
.td_fim_yaml <- function(linhas) {
  l <- .td_sem_cr(linhas)
  if (!length(l) || !grepl("^---\\s*$", l[[1]])) return(0L)
  f <- which(grepl("^(---|\\.\\.\\.)\\s*$", l[-1]))
  if (length(f)) f[[1]] + 1L else 0L
}

#' Uma passada pelo documento: o YAML, as linhas de código, as regiões e o que
#' ficou aberto no fim. Tudo que precisa saber onde se está no texto usa isto.
#' @noRd
.td_varrer <- function(linhas) {
  l <- .td_sem_cr(linhas)
  n <- length(l)
  yaml <- .td_fim_yaml(linhas)
  em_codigo <- rep(FALSE, n)      # a linha é de bloco de código (cercas incluídas)
  codigo_depois <- rep(FALSE, n)  # depois da linha, ainda se está num bloco de código
  pilha <- list()   # cada item: NULL (div da pessoa) ou índice da região aberta
  regioes <- list()
  codigo <- NULL
  for (i in seq_len(n)[seq_len(n) > yaml]) {
    x <- l[[i]]
    if (!is.null(codigo)) {
      em_codigo[[i]] <- TRUE
      if (grepl(sprintf("^\\s*%s{%d,}\\s*$", if (codigo$char == "`") "`" else "~", codigo$tam), x)) codigo <- NULL
    } else if (grepl(.TD_CODIGO, x)) {
      em_codigo[[i]] <- TRUE
      cerca <- sub(paste0(.TD_CODIGO, ".*$"), "\\1", x)
      codigo <- list(char = substr(cerca, 1L, 1L), tam = nchar(cerca))
    } else if (grepl(.TD_DIV_FECHA, x)) {
      if (length(pilha)) {
        topo <- pilha[[length(pilha)]]
        pilha[[length(pilha)]] <- NULL
        if (!is.null(topo)) regioes[[topo]]$fim <- i
      }
    } else if (grepl(.TD_DIV_ABRE, x, perl = TRUE)) {
      at <- sub(.TD_DIV_ABRE, "\\3", x, perl = TRUE)
      a <- if (startsWith(at, "{")) .td_atributos(at) else list(id = NULL, classes = at, pares = list())
      eh_regiao <- "trama" %in% a$classes && !is.null(a$id) && startsWith(a$id, "trama-")
      if (eh_regiao) {
        aberta <- Filter(Negate(is.null), pilha)
        if (length(aberta)) {
          .td_erro(sprintf("Regi\u{E3}o '%s' (linha %d) est\u{E1} dentro da regi\u{E3}o '%s'. Separe as duas.",
                           a$id, i, regioes[[aberta[[1]]]]$id), "tr_documento_erro_formato")
        }
        chave <- sub("^trama-", "", a$id)
        porta <- a$pares[["trama-porta"]]
        regioes[[length(regioes) + 1L]] <- list(
          inicio = i, fim = NA_integer_, id = a$id, chave = chave,
          no = if (is.null(porta)) chave else sub(paste0("--", porta, "$"), "", chave),
          porta = porta, hash = a$pares[["trama-hash"]] %||% "",
          pares = a$pares, classes = setdiff(a$classes, "trama"),
          recuo = sub(.TD_DIV_ABRE, "\\1", x, perl = TRUE),
          cerca = sub(.TD_DIV_ABRE, "\\2", x, perl = TRUE))
        pilha[[length(pilha) + 1L]] <- length(regioes)
      } else {
        pilha[length(pilha) + 1L] <- list(NULL)
      }
    }
    codigo_depois[[i]] <- !is.null(codigo)
  }
  list(yaml = yaml, em_codigo = em_codigo, codigo_depois = codigo_depois, regioes = regioes, divs_abertas = length(pilha),
       codigo_aberto = !is.null(codigo))
}

#' Regiões do trama nas linhas do documento, na ordem do texto.
#'
#' Ignora o YAML do topo e o que está dentro de bloco de código; acompanha a
#' profundidade das divs da pessoa, para que uma região possa morar dentro de
#' um callout e um callout gerado pelo trama possa morar dentro da região.
#' Região sem fechamento ou dentro de outra é erro: com a cerca quebrada não
#' há como saber onde termina o que é do trama, e nada é escrito.
#' @return Lista de regiões: `inicio`, `fim` (linhas das cercas), `id`,
#'   `chave`, `no`, `porta`, `hash`, `pares` (atributos `chave=valor`),
#'   `classes` (as da pessoa, além de `.trama`), `recuo` e `cerca` (o que
#'   vem antes e os dois-pontos da abertura) e `conteudo`.
#' @noRd
.td_regioes <- function(linhas) {
  regioes <- .td_varrer(linhas)$regioes
  for (r in regioes) {
    if (is.na(r$fim)) {
      .td_erro(sprintf("A regi\u{E3}o '%s' (linha %d) n\u{E3}o tem a linha ':::' que a fecha. Nada foi escrito.",
                       r$id, r$inicio), "tr_documento_erro_formato")
    }
  }
  lapply(regioes, function(r) {
    r$conteudo <- if (r$fim > r$inicio + 1L) linhas[(r$inicio + 1L):(r$fim - 1L)] else character()
    r
  })
}

#' Hash do conteúdo de uma região, insensível ao que editores mexem sozinhos:
#' CRLF, espaço no fim da linha e linhas em branco nas pontas. Qualquer outra
#' mudança é edição da pessoa.
#' @noRd
.td_hash_conteudo <- function(conteudo) {
  l <- sub("\\s+$", "", .td_sem_cr(conteudo))
  cheias <- which(nzchar(l))
  l <- if (length(cheias)) l[cheias[[1]]:cheias[[length(cheias)]]] else character()
  substr(digest::digest(paste(l, collapse = "\n"), algo = "sha256", serialize = FALSE), 1L, 16L)
}

#' A região foi editada à mão? (o conteúdo não bate com o hash que o trama
#' gravou). O hash é do conteúdo sem o recuo da abertura: levar a região para
#' dentro de um item de lista é mover, não editar.
#' @noRd
.td_editada <- function(r) !identical(.td_hash_conteudo(.td_desrecuar(r$conteudo, r$recuo)), r$hash)

.td_desrecuar <- function(l, recuo) {
  if (!nzchar(recuo %||% "")) return(l)
  ifelse(startsWith(l, recuo), substring(l, nchar(recuo) + 1L), l)
}

#' Valor de atributo entre aspas. Aspas duplas no valor vão entre simples;
#' quebra de linha ou os dois tipos de aspas não cabem num cabeçalho.
#' @noRd
.td_valor_attr <- function(k, v) {
  v <- as.character(v)
  if (length(v) != 1L || is.na(v) || grepl("[\r\n]", v) || (grepl('"', v) && grepl("'", v))) {
    .td_erro(sprintf("O valor do atributo '%s' n\u{E3}o cabe no cabe\u{E7}alho da regi\u{E3}o.", k),
             "tr_documento_erro_opcoes")
  }
  if (grepl('"', v)) sprintf("%s='%s'", k, v) else sprintf('%s="%s"', k, v)
}

#' Linhas de uma região: cabeçalho com id, classe, hash, as classes e os
#' atributos que a pessoa pôs (na ordem em que vieram), corpo e fecho, todos
#' com o recuo da abertura (a região pode morar num item de lista).
#' @noRd
.td_regiao_linhas <- function(chave, corpo, pares = list(), cerca = ":::", cr = FALSE,
                              classes = character(), recuo = "") {
  pares[["trama-hash"]] <- NULL
  hash <- .td_hash_conteudo(corpo)
  corpo <- ifelse(nzchar(corpo), paste0(recuo, corpo), corpo)
  outros <- vapply(names(pares), function(k) .td_valor_attr(k, pares[[k]]), "")
  cab <- paste(c(sprintf("#trama-%s", chave), ".trama", if (length(classes)) paste0(".", classes),
                 sprintf('trama-hash="%s"', hash), outros), collapse = " ")
  out <- c(sprintf("%s%s {%s}", recuo, cerca, cab), corpo, paste0(recuo, cerca))
  if (cr) paste0(out, "\r") else out
}

#' A região de novo, a partir da que está no texto (mesmo cabeçalho da pessoa).
#' @noRd
.td_regiao_de <- function(r, corpo, pares, cr) {
  .td_regiao_linhas(r$chave, corpo, pares, cerca = r$cerca, cr = cr, classes = r$classes, recuo = r$recuo)
}

#' Um bloco gerado é exatamente uma região bem formada, da chave certa: um
#' corpo com `:::` ou cerca de código aberta engoliria o texto que vem depois.
#' @noRd
.td_conferir_bloco <- function(bloco, chave) {
  v <- tryCatch(.td_varrer(bloco), tr_documento_erro = function(e) NULL)
  ok <- !is.null(v) && length(v$regioes) == 1L && identical(v$regioes[[1]]$chave, chave) &&
    identical(v$regioes[[1]]$inicio, 1L) && identical(v$regioes[[1]]$fim, length(bloco)) &&
    v$divs_abertas == 0L && !v$codigo_aberto
  if (!ok) {
    .td_erro(sprintf(paste0("O conte\u{FA}do gerado para '%s' n\u{E3}o forma uma regi\u{E3}o fechada ",
                            "(cerca ':::' ou de c\u{F3}digo no corpo?). Nada foi gravado."), chave),
             "tr_documento_erro_parte")
  }
  invisible(TRUE)
}

#' O documento usa CRLF? (as linhas novas seguem o arquivo)
#' @noRd
.td_crlf <- function(linhas) length(linhas) > 0L && mean(endsWith(linhas, "\r")) >= 0.5

#' As linhas de fora das regiões, com cada região trocada por um marcador:
#' é o que a regra de dono exige que não mude numa atualização.
#' @noRd
.td_fora <- function(linhas, regioes) {
  if (!length(regioes)) return(linhas)
  marca <- rep(FALSE, length(linhas))
  inicio <- vapply(regioes, `[[`, 0L, "inicio")
  for (r in regioes) marca[r$inicio:r$fim] <- TRUE
  out <- linhas
  out[inicio] <- "\u{1}regi\u{E3}o\u{1}"
  out[marca & !seq_along(out) %in% inicio] <- NA
  out[!is.na(out)]
}

#' A regra de dono, conferida: fora das regiões, nada mudou.
#' @noRd
.td_conferir_fora <- function(antes, depois) {
  if (!identical(.td_fora(antes, .td_regioes(antes)), .td_fora(depois, .td_regioes(depois)))) {
    .td_erro("Erro interno: a opera\u{E7}\u{E3}o mudaria texto fora das regi\u{F5}es do trama. Nada foi gravado.",
             "tr_documento_erro_interno")
  }
  invisible(TRUE)
}

#' Troca as linhas de regiões (de baixo para cima, para os índices valerem).
#' `trocas`: lista de `list(regiao, linhas)`; `linhas = NULL` apaga a região.
#' @noRd
.td_trocar <- function(linhas, trocas) {
  ordem <- order(vapply(trocas, function(t) t$regiao$inicio, 0L), decreasing = TRUE)
  for (t in trocas[ordem]) {
    r <- t$regiao
    antes <- if (r$inicio > 1L) linhas[seq_len(r$inicio - 1L)] else character()
    depois <- if (r$fim < length(linhas)) linhas[(r$fim + 1L):length(linhas)] else character()
    linhas <- c(antes, t$linhas, depois)
  }
  linhas
}

#' Onde uma região nova pode entrar: depois da linha `apos` (0 = topo do
#' corpo, depois do YAML), nunca dentro do YAML, de região ou de código.
#' @noRd
.td_pode_inserir <- function(linhas, apos) {
  n <- length(linhas)
  if (!is.numeric(apos) || length(apos) != 1L || is.na(apos) || apos < 0 || apos > n) return(FALSE)
  v <- .td_varrer(linhas)
  # O YAML do topo é da criação; ninguém insere lá dentro.
  if (v$yaml > 0L && apos < v$yaml) return(FALSE)
  for (r in v$regioes) if (!is.na(r$fim) && apos >= r$inicio && apos < r$fim) return(FALSE)
  apos == 0L || !v$codigo_depois[[apos]]
}

#' Insere `bloco` depois da linha `apos`, com linha em branco em volta só
#' onde falta (Pandoc pede a div separada do parágrafo).
#' @noRd
.td_inserir_linhas <- function(linhas, bloco, apos) {
  cr <- .td_crlf(linhas)
  branco <- if (cr) "\r" else ""
  vazia <- function(x) !nzchar(trimws(.td_sem_cr(x)))
  antes <- if (apos > 0) linhas[seq_len(apos)] else character()
  depois <- if (apos < length(linhas)) linhas[(apos + 1L):length(linhas)] else character()
  c(antes,
    if (length(antes) && !vazia(antes[[length(antes)]])) branco,
    bloco,
    if (length(depois) && !vazia(depois[[1]])) branco,
    depois)
}

.td_erro <- function(msg, classe, ...) {
  rlang::abort(msg, class = c(classe, "tr_documento_erro"), ...)
}

`%||%` <- function(x, y) if (is.null(x)) y else x
