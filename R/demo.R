#' A coleção `demo`, que o próprio núcleo traz.
#'
#' O núcleo do trama não tem domínio: bloco nenhum mora aqui, e é isso que
#' permite uma coleção nova chegar sem tocar no pacote. O preço é que o pacote
#' instalado sozinho não faz NADA — sem coleção não há bloco, não há fluxo, e
#' não há um único exemplo executável na ajuda. Quem instala fica olhando um
#' editor vazio, e quem revisa uma submissão vê 85 páginas de ajuda sem
#' exemplo.
#'
#' A `demo` fecha esse buraco sem furar a regra: ela é uma coleção como
#' qualquer outra — registrada por `tr_use()`, com ids qualificados por
#' `demo/` — e o que ela traz é aritmética e uma tabela de brinquedo, não
#' domínio. O núcleo continua cego ao que ela significa.
#'
#' Ela é encontrada pelo mesmo caminho de toda coleção: `tr_use()` recebe o
#' NOME DE UM PACOTE e chama a função `trama_collection()` do namespace dele.
#' Como quem exporta aqui é o próprio `trama`, o nome do pacote é `"trama"` e o
#' id da coleção é `demo` — os dois só coincidem por acidente nas outras
#' coleções (`trama.data` traz a coleção `data`), então nenhum caso especial
#' entra em `tr_use()`.
#'
#' Tudo em R base, de propósito: uma coleção de demonstração que exigisse
#' dplyr ou ggplot2 transformaria o primeiro contato com o pacote numa
#' instalação de dependências.
#' @return Um `tr_collection` com os tipos `demo/num` e `demo/tabela` e os
#'   blocos `demo/const`, `demo/soma`, `demo/tabela`, `demo/filtrar` e
#'   `demo/resumo`.
#' @examples
#' # A coleção que acompanha o pacote, num registro próprio.
#' reg <- tr_registry()
#' tr_use("trama", registry = reg)
#' names(reg$nodes)
#'
#' # Um fluxo se escreve em R e vira o mesmo documento que a interface produz.
#' f <- tr_flow(reg) |>
#'   tr_add("dois", "demo/const", value = 2) |>
#'   tr_add("tres", "demo/const", value = 3) |>
#'   tr_add("soma", "demo/soma", from = c("dois", "tres"), k = 10)
#' cat(tr_flow_code(f, reg), sep = "\n")
#'
#' # E roda, com um store numa pasta temporária.
#' s <- tr_store(file.path(tempdir(), "demo-store"))
#' tr_value(tr_flow_doc(f), "soma", reg, s)
#' @export
trama_collection <- function() {
  tr_collection(
    id = "demo", version = "1.0.0", label = .tr_msg("demo.collection_label"),
    categories = list(
      # Aritmética primeiro: é o grupo que explica o motor (cache, tipos,
      # portas) com o menor dado possível. Tabela depois, porque é onde o
      # preview do card começa a valer a pena.
      tr_category("numeros", .tr_msg("demo.cat_numeros"), "#6366f1"),
      tr_category("tabelas", .tr_msg("demo.cat_tabelas"), "#0ea5e9")
    ),
    types = list(.tr_demo_tipo_num(), .tr_demo_tipo_tabela()),
    nodes = list(
      tr_node("demo/const", fn = function(value) value,
              label = .tr_msg("demo.const_label"), category = "numeros",
              description = .tr_msg("demo.const_desc"),
              icon = tr_icon("hash"),
              outputs = list(out = "demo/num"),
              params = list(value = tr_param_num(1, label = .tr_msg("demo.const_param")))),
      tr_node("demo/soma", fn = function(a, b, k) a + b + k,
              label = .tr_msg("demo.soma_label"), category = "numeros",
              description = .tr_msg("demo.soma_desc"),
              icon = tr_icon("plus"),
              inputs = list(a = "demo/num", b = "demo/num"),
              outputs = list(out = "demo/num"),
              params = list(k = tr_param_num(0, label = .tr_msg("demo.soma_param_k")))),
      tr_node("demo/tabela", fn = .tr_demo_figuras,
              label = .tr_msg("demo.tabela_label"), category = "tabelas",
              description = .tr_msg("demo.tabela_desc"),
              icon = tr_icon("table"),
              outputs = list(out = "demo/tabela")),
      tr_node("demo/filtrar", fn = .tr_demo_filtrar,
              label = .tr_msg("demo.filtrar_label"), category = "tabelas",
              description = .tr_msg("demo.filtrar_desc"),
              icon = tr_icon("list-filter"),
              inputs = list(data = "demo/tabela"),
              outputs = list(out = "demo/tabela"),
              params = list(
                # Enum, e não texto: com a coluna livre, nome inexistente só
                # apareceria como erro dentro do `fn`, e a alternativa de
                # aceitar uma CONDIÇÃO em texto significaria avaliar expressão
                # arbitrária vinda de um cliente. As duas colunas numéricas da
                # tabela demo estão aqui, e o núcleo recusa qualquer outra.
                column = tr_param_enum("area", c("lados", "area"),
                                       label = .tr_msg("demo.filtrar_param_coluna")),
                limit  = tr_param_num(0, label = .tr_msg("demo.filtrar_param_limite"))
              )),
      tr_node("demo/resumo", fn = .tr_demo_resumo,
              label = .tr_msg("demo.resumo_label"), category = "tabelas",
              description = .tr_msg("demo.resumo_desc"),
              icon = tr_icon("clipboard-list"),
              inputs = list(data = "demo/tabela"),
              outputs = list(out = "demo/tabela"))
    )
  )
}

#' O tipo `demo/num`: um número, e nada mais.
#'
#' Sem `store`/`restore`: o núcleo cai em RDS, que é exatamente o certo para um
#' escalar. Declarar persistência aqui seria copiar o padrão sem necessidade.
#'
#' O `preview` aponta para `trama/keyvalue`, um dos renderers EMBUTIDOS do
#' núcleo (ver `inst/www/runtime.js`). É o que permite a esta coleção mostrar
#' resultado no card sem trazer módulo JS nenhum.
#' @noRd
.tr_demo_tipo_num <- function() {
  tr_type("demo/num", label = .tr_msg("demo.tipo_num"), color = "#818cf8",
          preview = function(x, ctx) tr_preview("trama/keyvalue", data = list(valor = x)),
          summary = function(x) list(valor = x))
}

#' O tipo `demo/tabela`: um `data.frame` pequeno.
#'
#' O `preview` manda DADO, não imagem: tabela cabe no payload, e mandada como
#' dado ela ganha o cabeçalho e a formatação do renderer embutido. O corte em
#' 25 linhas é o mesmo que uma coleção de verdade faria — aqui a tabela é menor
#' que isso, mas o corte é o hábito que se quer demonstrar.
#' @noRd
.tr_demo_tipo_tabela <- function() {
  tr_type("demo/tabela", label = .tr_msg("demo.tipo_tabela"), color = "#38bdf8",
          preview = function(x, ctx) {
            visivel <- utils::head(as.data.frame(x), 25L)
            tr_preview("trama/table", data = list(
              columns = as.list(names(visivel)),
              rows = lapply(seq_len(nrow(visivel)), function(i) {
                as.list(lapply(visivel[i, , drop = FALSE], function(v) v[[1]]))
              }),
              nrow = nrow(x), ncol = ncol(x)
            ))
          },
          summary = function(x) list(linhas = nrow(x), colunas = ncol(x)))
}

#' A tabela de brinquedo: medidas de figuras geométricas.
#'
#' Escolhida por não ter domínio nenhum — ninguém vai confundir isto com um
#' dado real e tirar conclusão dele, o que um `data.frame` de vendas ou de
#' pacientes convidaria a fazer.
#'
#' Os nomes das figuras vêm do catálogo de mensagens porque têm acento, e
#' string acentuada em código R é aviso no `R CMD check` (ver `R/mensagens.R`).
#' Os nomes das COLUNAS ficam em ASCII aqui de propósito: eles são
#' identificadores, não texto de interface — o param `column` do
#' `demo/filtrar` carrega um deles como valor, e um documento salvo guardaria
#' esse valor.
#' @noRd
.tr_demo_figuras <- function() {
  figuras <- strsplit(.tr_msg("demo.figuras"), ",", fixed = TRUE)[[1]]
  data.frame(
    figura = figuras,
    lados  = c(4L, 4L, 3L, 6L, 4L, 5L),
    area   = c(4.0, 6.0, 3.5, 10.4, 5.0, 8.6),
    stringsAsFactors = FALSE
  )
}

#' Mantém as linhas em que `column` é maior ou igual a `limit`.
#'
#' Comparação direta, sem `eval()`: o valor de um param chega de um cliente, e
#' avaliar expressão vinda de fora seria execução de código arbitrário num
#' bloco cuja razão de existir é ser o primeiro que alguém abre.
#' @noRd
.tr_demo_filtrar <- function(data, column, limit) {
  data[data[[column]] >= limit, , drop = FALSE]
}

#' `summary()` das colunas numéricas, devolvido como tabela.
#'
#' Sai como `demo/tabela`, e não como `demo/num`, porque `summary()` de uma
#' coluna são seis números: reduzir a um só escolheria uma estatística no lugar
#' de quem está olhando. Como tabela, o resumo continua no grafo — dá para
#' ligá-lo de volta num `demo/filtrar`, já que as colunas do resumo são as
#' colunas numéricas da entrada.
#'
#' Os nomes das estatísticas vêm do próprio `summary()` ("Min.", "Median",
#' ...): são de R, não de interface, e traduzi-los aqui inventaria uma segunda
#' verdade sobre o que a função devolveu.
#' @noRd
.tr_demo_resumo <- function(data) {
  numericas <- names(data)[vapply(data, is.numeric, logical(1))]
  if (!length(numericas)) return(data.frame(estatistica = character(0)))
  resumos <- lapply(numericas, function(nm) summary(data[[nm]]))
  out <- data.frame(estatistica = names(resumos[[1]]), stringsAsFactors = FALSE)
  for (i in seq_along(numericas)) out[[numericas[[i]]]] <- as.numeric(resumos[[i]])
  out
}
