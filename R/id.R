#' Id de instância de nó: aleatório, ordenável por criação, imutável.
#'
#' **Nunca derivado de label, posição, tipo ou ordem.** No insumo o id era o
#' nome que o usuário via, e o seed de cada nó saía de `hash(seed_global,
#' node_id)` — então renomear um nó, ou agrupá-lo num subgrafo (que prefixava
#' o id), mudava o ruído de tudo que era estocástico a jusante. Refatoração
#' pura alterando resultado é contrato ruim: aqui identidade e rótulo são
#' coisas separadas desde o começo.
#'
#' Formato tipo ULID (tempo em base32 + aleatório): ordenável por criação, o
#' que deixa listagens e diffs de documento estáveis sem campo extra. Ids
#' criados no MESMO milissegundo não ordenam entre si — limitação conhecida de
#' ULID não-monotônico, aceitável porque nada no sistema depende dessa ordem
#' (só de unicidade); um contador dentro do ms resolveria se um dia depender.

#' Entropia que NÃO consome o RNG do usuário.
#'
#' `sample()`/`sample.int()` avançam `.Random.seed`. Num pacote cuja proposta é
#' reprodutibilidade — e cujos testes vão querer `set.seed()` — isso significa
#' que dois documentos criados depois do mesmo `set.seed()` receberiam ids
#' IDÊNTICOS. Como id é chave estrangeira entre documento e store, seria
#' colisão silenciosa entre projetos. `tempfile()` usa a fonte interna do R,
#' independente de `.Random.seed`, e não tem dependência nova.
#'
#' O nome do `tempfile()` é prefixo + pid em hex + aleatório em hex, e o pid
#' vem PRIMEIRO. Pegar os primeiros caracteres era pegar o pid: ids do mesmo
#' milissegundo colidiam, e seeds novos saíam de uns 16 valores por processo.
#' Por isso o prefixo e o pid são descartados antes de aproveitar o resto.
#' O primeiro caractere do aleatório também: o `%x` não preenche com zeros,
#' então o dígito de abertura de cada pedaço de `rand()` nunca é 0 e sai de
#' 1-7 em ~93% das vezes. Os seguintes são uniformes.
#'
#' Processos filhos de fork (`parallel`, `mclapply`) herdam o estado do
#' `rand()` do C e repetiriam a sequência; hoje não importa porque ids só são
#' cunhados no processo principal.
#' @noRd
.tr_entropy_hex <- function(n = 12L) {
  skip <- nchar(sprintf("x%x", Sys.getpid()))
  out <- ""
  while (nchar(out) < n) {
    nm <- substring(basename(tempfile(pattern = "x")), skip + 2L)
    out <- paste0(out, gsub("[^0-9a-f]", "", nm))
  }
  substr(out, 1L, n)
}

.tr_b32 <- c(0:9, LETTERS[c(1:8, 10:11, 13:14, 16:20, 22:26)])

.tr_new_id <- function() {
  ms <- as.numeric(Sys.time()) * 1000
  t_chars <- character(10)
  for (i in 10:1) { t_chars[i] <- .tr_b32[(ms %% 32) + 1]; ms <- ms %/% 32 }
  hex <- .tr_entropy_hex(6L)
  rnd <- .tr_b32[strtoi(strsplit(hex, "")[[1]], base = 16L) + 1L]
  paste0(paste(t_chars, collapse = ""), paste(rnd, collapse = ""))
}

#' Id de instância vindo do cliente precisa ser validado: `.tr_edge_key()`
#' junta id e porta com ":", então um id contendo ":" tornaria duas arestas
#' distintas indistinguíveis.
#' @noRd
.tr_check_node_id <- function(id) {
  if (!is.character(id) || length(id) != 1L || is.na(id) || !nzchar(id) ||
      grepl("[^A-Za-z0-9_.-]", id)) {
    rlang::abort(sprintf("Id de nó inválido: '%s'. Use apenas [A-Za-z0-9_.-].", id),
                 class = "tr_error_bad_id")
  }
  invisible(id)
}

#' Seed materializada: sorteada na criação do nó e gravada no documento.
#' Editável pelo usuário; estável sob renome, cópia e agrupamento. Mesma
#' razão de `.tr_entropy_hex()` pra não usar o RNG do usuário.
#' @noRd
.tr_new_seed <- function() {
  as.integer(strtoi(substr(.tr_entropy_hex(7L), 1L, 7L), base = 16L))
}
