#' A seção de ajuda que explica o card de teste de hipótese.
#'
#' O card `trama/test` é do núcleo e desenhado igual em toda coleção: régua do
#' p-valor com estrelas quando há p-valor, três pontinhos de nível quando o
#' teste só tem tabela de valores críticos. A explicação mora aqui, escrita UMA
#' vez, pelo mesmo motivo do componente: quinze páginas com quinze cópias
#' divergiriam na primeira mudança do renderer (`inst/www/runtime.js`,
#' `VereditoTeste`; regras em `inst/www/teste.js`).
#'
#' Os cortes vão por extenso ("1 em mil"), e não "0,001": a coleção `series`
#' confere que todo decimal escrito na ajuda de um teste existe como literal no
#' código do bloco, e o corte do CARD não é do bloco.
#'
#' Uma coleção que emite um tipo de teste registra o id dele apontando para o
#' renderer do núcleo (`registerRenderer("x/test", getRenderer("trama/test"))`)
#' e cola esta seção na ajuda de cada bloco de teste.
#' @return string markdown, começando por um título de terceiro nível.
#' @export
tr_help_test_card <- function() {
  "
### Como ler o card do teste

O topo diz o teste, as estrelas e a hipótese nula (**H0**). No meio, o número
grande e o **selo** da decisão ao nível de 5%: preenchido quando rejeita H0,
vazado quando não rejeita. Embaixo, a conclusão em uma linha.

**Quando o teste tem p-valor**, o número grande é o p-valor (abaixo de 1 em mil,
escrito em potência de dez) e embaixo dele vem a **régua**:

- a régua é o p-valor em escala logarítmica: quanto MAIS COMPRIDA a barra,
  MENOR o p-valor e mais forte a evidência contra H0. A ponta marca onde o
  p-valor está;
- as marcas são os cortes de 10%, 5%, 1% e 1 em mil; a barra enche de vez
  abaixo de 1 em dez mil;
- as **estrelas** são as do `summary()` do R: `***` abaixo de 1 em mil, `**`
  abaixo de 1%, `*` abaixo de 5%, `.` abaixo de 10% e `ns` acima. A cor da barra
  fica mais forte a cada estrela; sem estrela, cinza.

**Quando o teste só tem tabela de valores críticos** (sem p-valor), o número
grande é a estatística, e no lugar da régua vêm **três pontinhos**, dos cortes
de 10%, 5% e 1%:

- **preenchido** — a estatística passa do valor crítico daquele nível, na cauda
  que o teste usa;
- **na cor de destaque** — a decisão a 5% é rejeitar H0; **cinza** — não é: um
  ponto cinza preenchido é um teste que vence só o corte de 10%;
- **vazio** — não passa daquele corte.

Quantos pontos acendem diz a FOLGA da decisão, não uma decisão diferente.

A vista **detalhe** traz o registro inteiro: estatística, graus de liberdade,
p-valor ou valores críticos, o tamanho do efeito com o intervalo de 95%
desenhado contra a referência (quando o teste tem um), as partes de um teste
conjunto, a nota e a fonte.

Não rejeitar H0 não é provar H0: com poucas observações o teste deixa de
rejeitar por falta de poder. A conclusão diz \"não há evidência\", e não \"é
igual\", por isso.
"
}

# ---- O registro de um teste ---------------------------------------------------
#
# Mecanismo, não domínio: o núcleo sabe o que o CARD lê (campos) e a regra de
# decisão que o card já aplica no `teste.js::venceu` — p-valor contra 5%, ou a
# estatística contra o crítico de 5% na cauda que `sentido` diz. Nome de teste,
# H0, qual cauda cada teste usa e se um p-valor é obrigatório são da coleção
# que chama. A regra mora aqui, e não em cada coleção, porque é a MESMA do
# desenho: se as duas divergissem, o selo diria "rejeita" e a conclusão "não".

.TR_TEST_CAMPOS <- c("teste", "h0", "estatistica", "rotulo_estat", "gl", "p_valor", "criticos",
                     "sentido", "decisao_5", "conclusao", "efeito", "nota", "fonte", "extra")

#' O registro de um teste de hipótese, na forma que o card `trama/test` desenha.
#'
#' Precisa de uma fonte de decisão: `p_valor` ou `criticos` com o nível "5%".
#' Sem nenhuma das duas o teste viraria um "não rejeita H0" confiante, e isso
#' é erro aqui, não card.
#'
#' @param teste,h0 nome do teste e a hipótese nula, em texto.
#' @param estatistica o valor da estatística; `rotulo_estat` como ela se escreve.
#' @param p_valor o p-valor, ou `NA` quando o teste só tem tabela de críticos.
#' @param gl graus de liberdade em texto ("2; 27"), ou `NA`.
#' @param criticos vetor nomeado de valores críticos ("10%", "5%", "1%").
#' @param sentido "menor" ou "maior": a cauda em que a estatística rejeita
#'   contra os `criticos`. Ignorado quando há p-valor.
#' @param conclusao_sim,conclusao_nao a conclusão em uma linha, conforme rejeite
#'   ou não H0 a 5%.
#' @param efeito `list(rotulo, valor, li, ls)` — o tamanho do efeito com o
#'   intervalo de 95% — ou `NULL`.
#' @param nota,fonte texto livre; `extra` lista nomeada de valores por teste.
#' @param classe classe S3 a mais, na frente de `tr_test` (a coleção de origem).
#' @return objeto `tr_test`.
#' @export
tr_test <- function(teste, h0, estatistica, rotulo_estat = "estatística", p_valor = NA_real_,
                    gl = NA_character_, criticos = NULL, sentido = "menor",
                    conclusao_sim, conclusao_nao, efeito = NULL, nota = "", fonte = "",
                    extra = NULL, classe = NULL) {
  txt <- function(v) is.character(v) && length(v) == 1L && !is.na(v)
  if (!txt(teste) || !txt(h0) || !txt(conclusao_sim) || !txt(conclusao_nao)) {
    rlang::abort("tr_test(): 'teste', 'h0' e as conclusões são texto de um elemento.",
                 class = "tr_error_bad_test")
  }
  if (!sentido %in% c("menor", "maior")) {
    rlang::abort(sprintf("tr_test('%s'): 'sentido' é \"menor\" ou \"maior\".", teste),
                 class = "tr_error_bad_test")
  }
  p <- suppressWarnings(as.numeric(p_valor))
  est <- suppressWarnings(as.numeric(estatistica))
  if (length(p) != 1L || length(est) != 1L) {
    rlang::abort(sprintf("tr_test('%s'): estatística e p-valor são um número cada.", teste),
                 class = "tr_error_bad_test")
  }
  cv <- if (is.null(criticos)) NULL else criticos[["5%"]]
  if (is.na(p) && (is.null(cv) || is.na(cv))) {
    rlang::abort(sprintf("tr_test('%s'): sem p-valor e sem crítico de 5%%, não há como decidir.", teste),
                 class = "tr_error_bad_test")
  }
  rejeita <- if (!is.na(p)) p < 0.05 else if (sentido == "menor") est < cv else est > cv
  rejeita <- isTRUE(rejeita)
  structure(list(
    teste = teste, h0 = h0, estatistica = est, rotulo_estat = rotulo_estat,
    gl = as.character(gl), p_valor = p, criticos = criticos, sentido = sentido,
    decisao_5 = if (rejeita) "rejeita H0" else "não rejeita H0",
    conclusao = if (rejeita) conclusao_sim else conclusao_nao,
    efeito = efeito, nota = nota, fonte = fonte, extra = extra
  ), class = c(classe, "tr_test"))
}

#' Confere que `x` é um `tr_test` inteiro; erro classificado se não.
#' @noRd
.tr_test_conferir <- function(x) {
  falta <- if (is.list(x)) setdiff(.TR_TEST_CAMPOS, names(x)) else .TR_TEST_CAMPOS
  if (!inherits(x, "tr_test") || length(falta)) {
    rlang::abort(sprintf("O nó produziu um objeto '%s', não o resultado de um teste%s.",
                         class(x)[[1]],
                         if (inherits(x, "tr_test")) sprintf(" (faltam: %s)", paste(falta, collapse = ", ")) else ""),
                 class = "tr_error_not_a_test")
  }
  invisible(x)
}

#' As estrelas do `summary()` do R — as mesmas do `teste.js::estrelas`.
#' @noRd
.tr_test_estrelas <- function(p) {
  if (is.na(p)) return(NA_character_)
  if (p < 0.001) "***" else if (p < 0.01) "**" else if (p < 0.05) "*" else if (p < 0.1) "." else "ns"
}

#' O tipo do teste, com store, restore e preview no card `trama/test`.
#'
#' O núcleo não registra tipos: quem registra é uma coleção (a `data`), com o
#' id no namespace dela. Por isso o id é argumento.
#' @param id id qualificado do tipo.
#' @return um `tr_type`.
#' @export
tr_test_type <- function(id) {
  tr_type(
    id, version = 1L, label = "Teste", color = "#ef4444", ext = "rds",
    store = function(x, path) {
      .tr_test_conferir(x)
      saveRDS(x, path, compress = FALSE)
    },
    restore = function(path) readRDS(path),
    # Sem `summary`: o runtime acrescenta uma aba `resumo` a todo tipo que
    # declare um, e ela repetiria a vista `detalhe` do card.
    preview = function(x, ctx) {
      nulo <- function(v) if (is.numeric(v) && length(v) == 1L && is.na(v)) NULL else v
      tr_preview("trama/test", data = list(
        teste = x$teste, h0 = x$h0, estatistica = x$estatistica, rotulo_estat = x$rotulo_estat,
        gl = if (is.na(x$gl)) NULL else x$gl, p_valor = nulo(x$p_valor),
        # Lista NOMEADA: vira objeto no JSON, e o front indexa por "5%".
        criticos = if (is.null(x$criticos)) NULL else as.list(x$criticos),
        sentido = x$sentido, decisao_5 = x$decisao_5, conclusao = x$conclusao,
        efeito = if (is.null(x$efeito)) NULL else lapply(x$efeito, nulo),
        nota = x$nota, fonte = x$fonte,
        extra = if (is.null(x$extra)) NULL else as.list(x$extra)))
    }
  )
}

#' Teste -> UMA linha de data.frame, com colunas FIXAS.
#'
#' Sempre as mesmas colunas, com NA onde não se aplica (teste de tabela não tem
#' p-valor; quase nenhum tem efeito): o pesquisador empilha testes de coleções
#' diferentes num quadro só, e esquema estável é o que deixa o empilhamento
#' ser um relatório. Só `extra` vira coluna quando existe — é, por definição,
#' o que cada teste tem de próprio.
#' @param x um `tr_test`.
#' @return data.frame de uma linha.
#' @export
tr_test_table <- function(x) {
  .tr_test_conferir(x)
  ef <- x$efeito
  num <- function(v) if (is.null(v)) NA_real_ else as.numeric(v)
  cv <- if (is.null(x$criticos)) NULL else x$criticos[["5%"]]
  base <- data.frame(
    teste = x$teste, h0 = x$h0, rotulo_estat = x$rotulo_estat, estatistica = x$estatistica,
    gl = x$gl, p_valor = x$p_valor, significancia = .tr_test_estrelas(x$p_valor),
    valor_critico_5 = num(cv), decisao_5 = x$decisao_5, conclusao = x$conclusao,
    efeito = if (is.null(ef)) NA_character_ else ef$rotulo,
    efeito_valor = num(ef$valor), efeito_li_95 = num(ef$li), efeito_ls_95 = num(ef$ls),
    nota = x$nota, fonte = x$fonte, stringsAsFactors = FALSE)
  if (length(x$extra)) base <- cbind(base, as.data.frame(x$extra, stringsAsFactors = FALSE))
  base
}
