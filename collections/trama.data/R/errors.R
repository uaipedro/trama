#' Classes de erro da coleção `data`.
#'
#' Mesmo princípio de `trama::tr_errors()`, e mesmo teste: uma tabela que um
#' teste confere contra o código, nas duas direções, pra não envelhecer em
#' silêncio. Prefixo próprio (`tr_data_error_*`) porque o catálogo do núcleo
#' só varre o `R/` dele — as duas listas não se misturam.
#' @return data.frame com `class` e `when`.
#' @export
tr_data_errors <- function() {
  e <- c(
    tr_data_error_bad_expr = "texto do param não é expressão R válida (falha de parse)",
    tr_data_error_eval = "a expressão fez parse mas falhou ao avaliar sobre a tabela",
    tr_data_error_unknown_column = "param nomeia coluna que não existe na tabela de entrada",
    tr_data_error_missing_package = "nó depende de pacote em Suggests que não está instalado",
    tr_data_error_bad_conversion = "conversão de tipo produziria NA onde a entrada não era NA",
    tr_data_error_bad_option = "param de escolha recebeu valor fora do conjunto aceito",
    tr_data_error_mismatched_names = "listas de nomes com tamanhos diferentes (renomear)",
    tr_data_error_name_collision =
      "nome de destino já em uso na tabela, ou repetido na própria lista (renomear)",
    tr_data_error_blank_param = "param de texto obrigatório deixado em branco no card",
    tr_data_error_duplicate_key =
      "a chave do espalhar se repete: uma célula receberia mais de um valor",
    tr_data_error_not_a_table =
      paste("o nó recebeu ou produziu um objeto que não é tabela; o tipo data/table",
            "recusa guardá-lo"),
    tr_data_error_missing_file =
      "caminho de .rds aponta pra arquivo (leitura) ou pasta (gravação) que não existe",
    tr_data_error_stream_columns =
      paste("passo do fluxo que não é tabela, ou com colunas diferentes do primeiro:",
            "empilhar preencheria faltante em silêncio")
  )
  data.frame(class = names(e), when = unname(e), stringsAsFactors = FALSE)
}

#' Confere que todas as colunas citadas existem, e devolve a lista.
#'
#' Só serve pra param que é LISTA DE NOMES (`cols`, `by`) — nunca pra
#' expressão. `dplyr::any_of()` e `intersect()`, que era o que estava aqui
#' antes, descartam nome inexistente EM SILÊNCIO: escrever `regaio` no lugar
#' de `regiao` fazia o fluxo rodar, ficar verde e devolver resultado errado.
#' @noRd
.tr_data_cols <- function(data, cols, param) {
  faltando <- setdiff(cols, names(data))
  if (length(faltando)) {
    rlang::abort(
      sprintf("Param '%s': coluna(s) inexistente(s): %s. Disponíveis: %s.",
              param, paste(faltando, collapse = ", "), paste(names(data), collapse = ", ")),
      class = "tr_data_error_unknown_column")
  }
  cols
}

#' Recusa param de texto obrigatório deixado em branco.
#'
#' A DOUTRINA do param vazio, uma vez só e para a coleção inteira: vazio é
#' "desligado" quando o nó tem um comportamento honesto para o vazio
#' (`tr_filter` sem condição não filtra, `tr_select` sem colunas passa a tabela
#' adiante, `tr_replace_na` sem valor não preenche); vazio é ERRO quando não
#' tem — o nome da coluna criada por `tr_mutate`, a planilha do `tr_read_excel`,
#' os nomes de saída do `tr_pivot_longer`. Na prática o segundo grupo é o dos
#' params de texto livre com default NÃO VAZIO no spec, e é isso que a
#' varredura de `test-param-vazio.R` trava, nó por nó: nó novo nasce reprovado
#' até se conformar, e a exceção tem de ser declarada com o motivo.
#'
#' Voltar ao default do spec seria pior que abortar: o card ficaria em branco e
#' a tabela sairia com uma coluna chamada "nome", tela e resultado discordando.
#'
#' Sem esta checagem o vazio descia até o verbo e voltava como
#' "coluna(s) inexistente(s): ." (o nome vazio não renderiza) ou como
#' "tentativa de usar um nome de variável com comprimento zero" — sem classe e
#' sem dizer qual dos campos do card estava em branco.
#' @noRd
.tr_data_obrigatorio <- function(valor, param) {
  if (!length(valor) || !all(nzchar(trimws(as.character(valor))))) {
    rlang::abort(
      sprintf("Param '%s': campo obrigatório em branco. Preencha-o no card.", param),
      class = "tr_data_error_blank_param")
  }
  valor
}

#' A EXCEÇÃO ao critério "deixa passar o que o terceiro recusa com mensagem
#' boa" — porque para o `readRDS` a premissa é falsa, e isso foi medido.
#'
#' `readr` diz `'.../x.csv' does not exist.` e o `arrow` diz
#' `IOError: Failed to open local file '...'`: os dois põem o caminho, e por
#' isso a coleção não os intercepta. Já `readRDS` num arquivo inexistente dá
#' `simpleError: não é possível abrir a conexão` — sem classe e sem caminho,
#' que vai num `warning()` e não sobe ao card. É o erro de quem renomeou um
#' arquivo ou moveu o projeto, e ele chega cego. Seguir o critério aqui seria
#' inércia: onde ele perde a razão de ser, ele não se aplica.
#'
#' `pasta = TRUE` é o lado da gravação: `saveRDS` num diretório inexistente dá
#' exatamente a mesma mensagem cega.
#' @noRd
.tr_data_arquivo <- function(path, param, pasta = FALSE) {
  if (!file.exists(path)) {
    rlang::abort(
      sprintf("Param '%s': %s não existe: %s.", param,
              if (pasta) "a pasta de destino" else "o arquivo", path),
      class = "tr_data_error_missing_file")
  }
  path
}

#' Faz parse do texto de um param, dizendo QUAL param falhou.
#'
#' O erro cru do parser não diz de onde veio, e o card tem três campos de
#' texto — "unexpected end of input" sozinho não ajuda ninguém.
#' @noRd
.tr_data_parse <- function(expr, param) {
  tryCatch(rlang::parse_expr(expr), error = function(e) {
    rlang::abort(sprintf("Param '%s': expressão inválida — %s. Texto: %s",
                         param, conditionMessage(e), expr),
                 class = "tr_data_error_bad_expr")
  })
}

#' Faz parse de uma LISTA de expressões separadas por vírgula.
#'
#' Quebrar na vírgula com `strsplit()`, como `.as_cols()` faz para nomes de
#' coluna, partiria `sum(valor, na.rm = TRUE)` no meio. Quem sabe onde a
#' vírgula separa de verdade é o parser do R: embrulhar o texto em `list(...)`
#' e pegar os argumentos respeita parêntese, colchete e string de graça, sem
#' esta função ter opinião sobre sintaxe.
#'
#' O erro ecoa o texto ORIGINAL, não o embrulhado: o usuário nunca escreveu
#' `list(`, e ver esse pedaço na mensagem manda procurar um parêntese que não
#' é dele.
#'
#' Item vazio é descartado, não é erro — mesma doutrina de `.as_cols()`, que
#' apara e joga fora o vazio. A vírgula sobrando no fim de `"sum(valor), "`
#' vira um argumento faltante aqui, e ela é acidente de digitação nos dois
#' campos pelo mesmo motivo.
#' @noRd
.tr_data_parse_exprs <- function(expr, param) {
  lst <- tryCatch(rlang::parse_expr(paste0("list(\n", expr, "\n)")),
                  error = function(e) {
                    rlang::abort(sprintf("Param '%s': expressão inválida — %s. Texto: %s",
                                         param, conditionMessage(e), expr),
                                 class = "tr_data_error_bad_expr")
                  })
  es <- as.list(lst)[-1]
  es[!vapply(es, function(x) identical(x, quote(expr = )), logical(1))]
}

#' Avalia o verbo de dplyr reclassificando o erro e anexando as colunas.
#'
#' Deliberadamente NÃO faz análise estática de símbolos da expressão: varrer a
#' árvore sintática dá falso positivo em `x$y`, em argumento nomeado e em
#' função de pacote, e recusar expressão VÁLIDA é pior do que a mensagem
#' chegar um passo depois. `code` é avaliado preguiçosamente, aqui dentro.
#'
#' A mensagem NÃO interpola `conditionMessage(e)`: para erro encadeado do rlang
#' ele já traz a cadeia inteira, e o `parent = e` faz o rlang renderizar a mesma
#' cadeia logo abaixo — o usuário lia a causa duas vezes, com "Colunas
#' disponíveis" encravado no meio da citação.
#'
#' `data = NULL` é o nó que não tem tabela de entrada (`data/generate`): ali
#' "Colunas disponíveis:" seguido de nada seria pior que o silêncio, porque
#' anuncia uma lista vazia como se fosse resposta. O `parent = e` continua
#' levando a causa de verdade ("objeto 'z' não encontrado") logo abaixo.
#' @noRd
.tr_data_eval <- function(code, param, data = NULL) {
  tryCatch(force(code), error = function(e) {
    colunas <- if (is.null(data)) "" else
      sprintf(" Colunas disponíveis: %s.", paste(names(data), collapse = ", "))
    rlang::abort(sprintf("Param '%s' falhou ao avaliar.%s", param, colunas),
                 class = "tr_data_error_eval", parent = e)
  })
}

#' Valor fora do conjunto aceito de um param de escolha.
#'
#' O enum protege pelo lado da UI, mas estes `fn` são nível 1 e chamáveis no
#' console — que é o caminho pelo qual esta coleção se testa. Sem default, o
#' `switch` devolvia NULL: no `tr_convert()` isso virava "attempt to apply
#' non-function" cru, e no `tr_remove_empty()` era pior, porque o janitor
#' recebia `which = NULL` e simplesmente não removia nada, em silêncio.
#' @noRd
.tr_data_option <- function(param, valor, aceitos) {
  rlang::abort(
    sprintf("Param '%s': valor inválido '%s'. Aceitos: %s.",
            param, paste(as.character(valor), collapse = ", "),
            paste(aceitos, collapse = ", ")),
    class = "tr_data_error_bad_option")
}

#' Pacote de `Suggests` ausente vira erro classificado, não "could not find
#' function".
#' @noRd
.tr_data_need <- function(pkg, nodo) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    rlang::abort(sprintf("O nó '%s' precisa do pacote '%s'. Instale com install.packages(\"%s\").",
                         nodo, pkg, pkg),
                 class = "tr_data_error_missing_package")
  }
}

#' Param inteiro conferido no NÍVEL 1, onde não existe widget.
#'
#' O `min` do `tr_param_int` protege pelo card, mas estes `fn` são função R
#' comum e é por elas que a coleção se testa. Sem isto, `lote = 0` chegava ao
#' `seq.int(by = 0)` e voltava como erro cru do R — sem classe e sem dizer qual
#' campo do card estava errado. Reaproveita `tr_data_error_bad_option` porque a
#' pergunta é a mesma de um enum: o valor está fora do conjunto aceito.
#' @noRd
.tr_data_inteiro <- function(valor, param, min = 0L) {
  v <- suppressWarnings(as.integer(valor))
  if (length(v) != 1L || is.na(v) || v < min) {
    .tr_data_option(param, valor, sprintf("inteiro >= %d", min))
  }
  v
}
