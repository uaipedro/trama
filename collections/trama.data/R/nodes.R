# Nós da coleção `data`. Cada `fn` é uma função R comum, exportada e chamável
# direto no console — o "nível 1" que o trama preserva de propósito: qualquer
# caixa da tela é inspecionável sem passar pela UI.

#' Fingerprint compartilhado dos leitores de arquivo.
#'
#' O hash de conteúdo do trama cobre código e params, mas não o mundo. Sem
#' isso, editar o arquivo no disco não muda a chave e o grafo inteiro serve
#' dado velho — em silêncio, que é o pior modo de falha possível numa
#' ferramenta de análise. Um helper só pra não haver cinco versões disso.
#'
#' Roda ANTES do `fn`, inclusive com `path = ""` (card recém-arrastado da
#' paleta) e com arquivo inexistente. Nos dois casos `file.info()` devolve NA
#' em tudo e a chave sai `"|NA|NA"`: estável (não invalida o grafo a cada
#' passada) e diferente da chave do arquivo presente (o nó reexecuta quando o
#' arquivo aparece). Quem RECUSA o branco é o verbo, classificado.
#' @noRd
.tr_data_file_print <- function(params, ctx) {
  p <- ctx$path(params$path); i <- file.info(p)
  paste(p, i$size, i$mtime, sep = "|")
}

#' `na` é LISTA separada por vírgula (o idioma da coleção para lista de texto,
#' daí o `.as_cols()`): uma planilha real traz `NA`, `-` e `sem dado` na mesma
#' coluna, e um campo de um valor só obrigava a escolher qual deles contaria.
#'
#' Célula VAZIA conta SEMPRE como faltante, e por isso o `""` entra na lista à
#' força. Era o bug: o default do `readr` é `c("", "NA")`, e passar `na = "NA"`
#' cru TIRAVA a string vazia do conjunto. `a,,b` chegava como texto vazio
#' presente, e a partir dali o fluxo inteiro ficava verde mentindo —
#' `data/drop_na` não descartava a linha, `data/remove_empty` não a via e
#' `data/summary` reportava zero faltantes numa coluna cheia de buracos. É
#' exatamente o silêncio que produz resultado errado que esta coleção existe
#' para não ter, e nenhuma das quatro páginas de ajuda avisava.
#'
#' Esvaziar o campo no card é honesto e não é erro: sobra só a célula vazia,
#' que é o piso. Marcador com espaço nas pontas não é expressável, porque o
#' `.as_cols()` apara — o mesmo contrato de todo campo de lista daqui.
#' @export
tr_read_csv <- function(path, delim = ",", na = "NA", .ctx = NULL) {
  .tr_data_obrigatorio(path, "path")
  if (!is.null(.ctx)) path <- .ctx$path(path)
  readr::read_delim(path, delim = delim, na = unique(c("", .as_cols(na))),
                    show_col_types = FALSE, progress = FALSE)
}

#' Lê um JSON tabular.
#'
#' O formato natural é um array de objetos, em que cada objeto vira uma linha.
#' `jsonlite` também aceita um objeto de vetores com o mesmo comprimento. O
#' resultado é normalizado como tibble para cumprir o contrato `data/table`.
#' @export
tr_read_json <- function(path, .ctx = NULL) {
  .tr_data_obrigatorio(path, "path")
  if (!is.null(.ctx)) path <- .ctx$path(path)
  x <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  tabular <- is.data.frame(x) ||
    (is.list(x) && length(x) > 0L && !is.null(names(x)) && all(nzchar(names(x))))
  if (!tabular) {
    rlang::abort("O JSON não representa uma tabela: use um array de objetos ou um objeto de vetores.",
                 class = "tr_data_error_not_a_table")
  }
  tryCatch(tibble::as_tibble(x), error = function(e) {
    rlang::abort("O JSON não representa uma tabela: as colunas têm tamanhos incompatíveis.",
                 class = "tr_data_error_not_a_table", parent = e)
  })
}

#' Leitores de formato binário. A regra é classificar o que a coleção mesma
#' valida — o param em branco, que só ela sabe que veio de um card — e deixar
#' passar o que o terceiro já recusa alto e COM O CAMINHO na mensagem, que é o
#' caso do `readr` e do `arrow`.
#'
#' `readRDS` é a exceção, medida: arquivo inexistente dá "não é possível abrir
#' a conexão", sem caminho (ele vai num `warning()` que não sobe ao card) e
#' sem classe. Por isso, e só por isso, este leitor confere o arquivo ele
#' mesmo — ver `.tr_data_arquivo()`.
#' @export
tr_read_rds <- function(path, .ctx = NULL) {
  .tr_data_obrigatorio(path, "path")
  if (!is.null(.ctx)) path <- .ctx$path(path)
  .tr_data_arquivo(path, "path")
  readRDS(path)
}

#' @export
tr_read_parquet <- function(path, .ctx = NULL) {
  .tr_data_need("arrow", "data/read_parquet")
  .tr_data_obrigatorio(path, "path")
  if (!is.null(.ctx)) path <- .ctx$path(path)
  tibble::as_tibble(arrow::read_parquet(path))
}

#' `sheet` é TEXTO, não número, pra aceitar nome de planilha ou posição no
#' mesmo campo do card. "1" vira o índice 1; "Plan1" continua nome.
#'
#' Planilha em branco é o mesmo tipo de branco que o `path`: sem ela o nó não
#' tem como produzir tabela nenhuma. Cru, o readxl dava `Sheet '' not found`,
#' sem classe e sem dizer qual campo do card estava vazio.
#' @export
tr_read_excel <- function(path, sheet = 1, .ctx = NULL) {
  .tr_data_need("readxl", "data/read_excel")
  .tr_data_obrigatorio(path, "path")
  .tr_data_obrigatorio(sheet, "sheet")
  if (!is.null(.ctx)) path <- .ctx$path(path)
  s <- suppressWarnings(as.integer(sheet))
  readxl::read_excel(path, sheet = if (is.na(s)) sheet else s)
}

#' Fonte sem arquivo: é o que dá a um projeto em branco algo pra arrastar. Sem
#' isso, o custo de abrir o editor pela primeira vez é "arrume um CSV" — que é
#' fricção demais pra demonstrar uma ferramenta cujo argumento é a iteração
#' rápida. Puro: não toca o mundo, logo não precisa de `fingerprint`.
#'
#' Antes eram cinco conjuntos num `switch`, e a lista estava escrita DUAS vezes
#' — aqui e no `tr_param_enum` do spec. Com cinco isso passava; com quarenta e
#' seis as duas listas divergiriam na primeira edição, e divergir aqui é o pior
#' caso possível: o card ofereceria um nome que a função recusa, ou recusaria
#' um que a função aceita. A lista agora é DERIVADA (`.tr_data_exemplos()`) e
#' os dois lados leem a mesma.
#'
#' Nada de `switch`: buscar pelo nome no namespace do `datasets` é o que
#' dispensa uma linha por conjunto. A validação não é por lista (seria carregar
#' os 108 objetos a cada execução do nó), é pela PERGUNTA que interessa — veio
#' uma tabela? Isso cobre de uma vez o nome inexistente (`titanic`) e o nome
#' que existe mas não é tabela (`AirPassengers`, que é `ts`); os dois são
#' igualmente inválidos neste nó, e a lista de aceitos só é montada no caminho
#' do erro.
#' @export
tr_example <- function(dataset = "mtcars") {
  x <- if (is.character(dataset) && length(dataset) == 1L && !is.na(dataset)) {
    tryCatch(get(dataset, envir = asNamespace("datasets")), error = function(e) NULL)
  }
  # Dois terços do `datasets` não são tabela (31 são `ts`, 15 são matriz ou
  # tabela de contingência). Deixar passar seria oferecer na paleta uma lista
  # em que a maioria das opções pinta o card de vermelho lá no `store` do
  # tipo `data/table` — erro certo, longe da causa, e depois de já ter rodado.
  if (!is.data.frame(x)) .tr_data_option("dataset", dataset, .tr_data_exemplos())

  # O caso do `mtcars` virou REGRA. O nome do carro mora em rowname, que o
  # tibble descarta por padrão, e sem isto a coluna mais interessante do
  # conjunto some; o mesmo vale para o estado em `USArrests`, a província em
  # `swiss`, o ano em `longley` — nove dos quarenta e seis. Uma exceção
  # nomeada por conjunto seria a lista escrita à mão de novo, agora disfarçada.
  #
  # O teste é por CONTEÚDO, e não por `.row_names_info(x) > 0`: onze conjuntos
  # (`faithful`, `ChickWeight`, `CO2`…) guardam rowname explícito que é só
  # "1","2","3" — informação nenhuma —, e pelo `.row_names_info` todos eles
  # ganhariam uma coluna inútil.
  #
  # `nome` e não `modelo`: com quarenta e seis conjuntos, cada um teria o seu
  # nome natural (modelo, estado, província, ano) e escolher um por conjunto é
  # a lista à mão outra vez. Nenhum dos nove tem coluna `nome` — são todos em
  # inglês —, então não há colisão a tratar.
  rn <- rownames(x)
  tibble::as_tibble(
    x, rownames = if (identical(rn, as.character(seq_len(nrow(x))))) NULL else "nome")
}

#' Os conjuntos ofertados: todo `data.frame` do pacote `datasets`, derivado na
#' hora e nunca escrito à mão.
#'
#' Derivar tem uma consequência que é feature, não descuido: a lista é a do R
#' que está rodando. `penguins` só existe a partir do R 4.5, e num R mais velho
#' ele simplesmente não aparece na paleta — enquanto um fluxo salvo que o
#' mencione é recusado pelo enum com erro classificado, alto e perto da causa.
#' Uma lista fixa daria o contrário: a opção apareceria no card e falharia na
#' execução.
#'
#' `sub(" .*", "")` porque o índice do `data()` escreve alias entre parênteses
#' ("beaver1 (beavers)"), e é o primeiro nome que se busca.
#'
#' Ordem por `tolower`: o dropdown é alfabético para quem lê, e não em ordem
#' ASCII, que jogaria `BOD`, `CO2` e `DNase` para antes de `airquality`.
#' @noRd
.tr_data_exemplos <- function() {
  itens <- unique(sub(" .*", "", utils::data(package = "datasets")$results[, "Item"]))
  tabelas <- Filter(function(n) {
    is.data.frame(tryCatch(get(n, envir = asNamespace("datasets")), error = function(e) NULL))
  }, itens)
  tabelas[order(tolower(tabelas))]
}

#' Fonte que FABRICA o dado, irmã do `tr_example()` que apenas carrega um que
#' já existe. É o que permite demonstrar simulação — amostra, regressão com
#' ruído, processo gerador conhecido — sem arquivo e sem conjunto embutido que
#' por acaso tenha a forma certa.
#'
#' Dois nós e não um: os campos de um gerador ("quanto" e "como") e os de um
#' carregador ("qual") não se sobrepõem em nada, e um nó só teria metade do
#' card sempre apagada.
#'
#' `.seed` é o argumento reservado do NÚCLEO, não um param desta coleção — e é
#' a escolha que faz este nó ser cacheável sem deixar de ser aleatório. A seed
#' vive no documento por INSTÂNCIA (`doc$nodes[[id]]$seed`), é salva com o
#' fluxo, é estável sob renome, entra na chave de cache só para nó declarado
#' `stochastic` (`.tr_unit_key()`, R/hash.R) e volta escrita no código que
#' `tr_flow_code()` gera. Dois geradores no mesmo fluxo têm seeds
#' independentes; re-sortear é trocar a seed de UM deles, e nada mais no grafo
#' recomputa.
#'
#' Um param `semente` faria o mesmo de fora, pior: ficaria de fora do
#' tratamento de `stochastic` e o mesmo conceito existiria duas vezes.
#' @export
tr_generate <- function(n = 100L, expr = "x = rnorm(n)", .seed = 1L) {
  .tr_data_obrigatorio(expr, "expr")
  n <- .tr_data_tamanho(n)
  es <- .tr_data_parse_exprs(expr, "expr")
  if (!length(es)) .tr_data_obrigatorio("", "expr")

  # O RNG global é do PROCESSO, e com o executor sequencial esse processo é o
  # do Shiny. Um `set.seed()` solto aqui mudaria o sorteio de qualquer outro
  # código rodando na mesma sessão — inclusive o do gerador vizinho, que é
  # exatamente a independência que este nó promete. Devolver o estado é o que
  # mantém a promessa dos dois lados.
  anterior <- if (exists(".Random.seed", globalenv(), inherits = FALSE)) {
    get(".Random.seed", globalenv(), inherits = FALSE)
  }
  on.exit(if (is.null(anterior)) {
    suppressWarnings(rm(".Random.seed", envir = globalenv()))
  } else {
    assign(".Random.seed", anterior, envir = globalenv())
  }, add = TRUE)
  set.seed(.seed)

  # `n` é uma VARIÁVEL à disposição da expressão, não uma promessa sobre o
  # número de linhas: `1:5` com n = 100 devolve cinco linhas, e forçar o
  # contrário impediria de escrever `expand.grid()` ou `data.frame()` à mão. A
  # ajuda diz isso com todas as letras.
  #
  # Pai `globalenv()`: a expressão enxerga exatamente o que enxergaria digitada
  # no console, e nada do ambiente desta função (`expr` e `.seed` viram nomes
  # ao alcance de quem escrever `expr` no campo, o que é confuso e inútil).
  env <- rlang::new_environment(list(n = n), parent = globalenv())

  # Um único termo SEM nome é o caso "me dê um vetor": `runif(n, 0, 1)` tem que
  # funcionar como se digita, e `tibble()` o nomearia `runif(n, 0, 1)` — nome de
  # coluna que nenhum nó a jusante quer digitar. Com nome, ou com mais de um
  # termo, quem monta é o `tibble()`: ele avalia na ORDEM escrita, então
  # `y = 2 * x + rnorm(n)` enxerga o `x` da coluna anterior, que é o que uma
  # simulação escreve naturalmente.
  simples <- length(es) == 1L && (is.null(names(es)) || !nzchar(names(es)[[1]]))
  chamada <- if (simples) es[[1]] else rlang::call2(tibble::tibble, !!!es)
  val <- .tr_data_eval(rlang::eval_bare(chamada, env), "expr")

  if (simples) {
    # Matriz vira tabela de colunas `V1...Vn`; `data.frame` passa direto; vetor
    # vira a coluna `valor`. O que não é nada disso (uma lista, um modelo) não
    # é remendado aqui: cai no `store` do tipo `data/table`, que é o funil onde
    # essa recusa mora para a coleção inteira.
    # `as.data.frame()` antes do tibble por causa do NOME: `as_tibble()` numa
    # matriz sem dimnames repara para `...1`, `...2` — nomes que ninguém quer
    # digitar num nó a jusante — e ainda emite mensagem de reparo no meio do
    # log. O caminho pelo data.frame dá `V1`, `V2`, calado.
    if (is.matrix(val)) return(tibble::as_tibble(as.data.frame(val)))
    if (is.data.frame(val)) return(tibble::as_tibble(val))
    if (is.atomic(val)) return(tibble::tibble(valor = val))
  }
  val
}

#' O tamanho tem que ser um inteiro positivo ANTES de virar a variável `n`.
#'
#' O `tr_param_int(min = 1)` protege pelo card, mas o `fn` é nível 1 e chamável
#' no console — e sem isto `tr_generate(0)` devolvia uma tabela de zero linhas,
#' verde, e `tr_generate(-1)` estourava dentro do `rnorm` com "invalid arguments",
#' longe da causa e sem dizer qual campo do card estava errado.
#' @noRd
.tr_data_tamanho <- function(n) {
  if (length(n) != 1L || !is.numeric(n) || is.na(n) || n < 1) {
    rlang::abort(sprintf("Param 'n': o tamanho tem que ser um inteiro >= 1 (veio '%s').",
                         paste(as.character(n), collapse = ", ")),
                 class = "tr_data_error_bad_option")
  }
  as.integer(n)
}

#' Colunas de agrupamento, ou NULL. `.by` de dplyr é tidyselect e aceita NULL
#' pra "sem grupo" — é o que deixa UM nó cobrir o caso agrupado e o não
#' agrupado sem partir a paleta em dois. Valida ANTES do verbo, pelo mesmo
#' motivo de `tr_select()`: aninhado, a classe útil sobraria só no `parent`.
#' @noRd
.tr_data_by <- function(data, by) {
  by <- .as_cols(by)
  if (!length(by)) return(NULL)
  .tr_data_cols(data, by, "by")
  by
}

#' @export
tr_filter <- function(dados, expr, by = "") {
  if (!nzchar(trimws(expr))) return(dados)
  g <- .tr_data_by(dados, by)
  e <- .tr_data_parse(expr, "expr")
  .tr_data_eval(dplyr::filter(dados, !!e, .by = dplyr::all_of(g)), "expr", dados)
}

#' Sem expressão, o nó está DESLIGADO: `expr` tem default vazio no spec e não
#' calcular nada é comportamento honesto para um card recém-arrastado. Sem
#' NOME, não está — `name` tem default não vazio no spec e não existe "criar
#' coluna sem nome". Antes os dois vazios eram a mesma coisa e devolviam a
#' tabela intacta: `tr_mutate(d, "", "valor*2")` ficava verde sem criar coluna
#' nenhuma, enquanto o irmão `tr_group_summarise()` abortava no mesmo caso.
#' A doutrina está travada por varredura em `test-param-vazio.R`.
#'
#' A ordem importa: a checagem do nome vem DEPOIS do desligado, senão um card
#' novo com o nome apagado pintaria de vermelho por um campo que ele ainda nem
#' vai usar.
#'
#' Vírgula separa colunas, com nomes casados pela POSIÇÃO — mesmo contrato de
#' `tr_group_summarise()`. `name` passa por `.as_cols()`; `expr` NÃO pode,
#' porque a vírgula de `sum(valor, na.rm = TRUE)` não separa coluna nenhuma —
#' quem sabe disso é `.tr_data_parse_exprs()`. Nome repetido, ou igual a uma
#' coluna de `by`, sobrescreveria uma coluna nova (ou o próprio grupo) em
#' silêncio — daí a mesma checagem de colisão do resumir.
#' @export
tr_mutate <- function(dados, name, expr, by = "") {
  if (!nzchar(trimws(expr))) return(dados)
  .tr_data_obrigatorio(name, "name")
  g <- .tr_data_by(dados, by)
  nomes <- .as_cols(name)
  es <- .tr_data_parse_exprs(expr, "expr")
  .tr_data_obrigatorio(nomes, "name")
  if (!length(es)) .tr_data_obrigatorio(character(), "expr")
  if (length(nomes) != length(es)) {
    rlang::abort(sprintf("Criar coluna: %d nome(s) para %d expressão(ões).",
                         length(nomes), length(es)),
                 class = "tr_data_error_mismatched_names")
  }
  colide <- unique(c(nomes[duplicated(nomes)], intersect(nomes, g)))
  if (length(colide)) {
    rlang::abort(
      sprintf(paste0("Criar coluna: o nome de coluna se repete: %s. ",
                     "Cada coluna nova precisa do seu."),
              paste(colide, collapse = ", ")),
      class = "tr_data_error_name_collision")
  }
  .tr_data_eval(
    dplyr::mutate(dados, !!!stats::setNames(es, nomes), .by = dplyr::all_of(g)),
    "expr", dados)
}

#' Manter as colunas escolhidas, ou jogá-las fora.
#'
#' Um nó só, com chave, em vez de dois: a escolha entre manter e remover é a
#' mesma lista de colunas vista pelos dois lados, e quem monta o fluxo troca de
#' ideia sem trocar de caixa. `remove` entrou na versão 2 do nó; documento
#' salvo na versão 1 não traz o param, e o default do spec o preenche.
#' @export
tr_select <- function(dados, cols, remove = FALSE) {
  cols <- .as_cols(cols)
  if (!length(cols)) return(dados)
  # Valida ANTES do verbo, nunca aninhado dentro dele: o dplyr embrulha a
  # condição num `rlang_error` novo e a classe útil sobra só no `parent` — o
  # motor grava `class(e)[1]` e o card receberia "rlang_error".
  .tr_data_cols(dados, cols, "cols")
  if (isTRUE(remove)) dplyr::select(dados, -dplyr::all_of(cols))
  else dplyr::select(dados, dplyr::all_of(cols))
}

#' Uma linha por combinação das colunas escolhidas.
#'
#' Guarda a PRIMEIRA linha de cada grupo (`.keep_all = TRUE` do dplyr), com o
#' resto das colunas como estava nela — não resume nem escolhe: para escolher
#' qual repetida fica, ordene antes.
#'
#' Sem colunas, compara a linha INTEIRA, que é o pedido mais comum de quem
#' acabou de empilhar duas tabelas.
#' @export
tr_distinct <- function(dados, cols) {
  cols <- .as_cols(cols)
  if (!length(cols)) return(dplyr::distinct(dados))
  .tr_data_cols(dados, cols, "cols")
  dplyr::distinct(dados, dplyr::across(dplyr::all_of(cols)), .keep_all = TRUE)
}

#' Renomeia colunas, casando as duas listas pela POSIÇÃO.
#'
#' Casar por posição é o que deixa o card ter dois campos de texto em vez de um
#' editor de pares — e é também o jeito de errar calado: com listas de tamanhos
#' diferentes, `setNames()` recicla e a coluna errada muda de nome. Daí a
#' checagem explícita, antes de tudo.
#'
#' `stats::setNames(from, to)` e não o contrário: `dplyr::rename()` recebe
#' `novo = velho`, ou seja, o nome de destino é o NOME do par e o de origem é
#' o VALOR.
#' @export
tr_rename <- function(dados, from, to) {
  from <- .as_cols(from); to <- .as_cols(to)
  if (!length(from)) return(dados)
  if (length(from) != length(to)) {
    rlang::abort(sprintf("Renomear: %d nome(s) de origem para %d de destino.",
                         length(from), length(to)),
                 class = "tr_data_error_mismatched_names")
  }
  .tr_data_cols(dados, from, "from")
  # Destino já ocupado é validação de PARAM, e param inválido é o que esta
  # coleção classifica em todo lugar. Sem isto, o vctrs abortava com
  # `vctrs_error_names_must_be_unique`, em inglês e sem dizer qual nome bateu.
  # `setdiff(names(data), from)` porque a coluna que está saindo de cena na
  # mesma chamada não conta como ocupada: trocar dois nomes é simultâneo.
  colide <- unique(c(intersect(to, setdiff(names(dados), from)), to[duplicated(to)]))
  if (length(colide)) {
    rlang::abort(
      sprintf(paste0("Renomear: o nome de destino já está em uso: %s. Escolha outro, ",
                     "ou remova a coluna antes."),
              paste(colide, collapse = ", ")),
      class = "tr_data_error_name_collision")
  }
  dplyr::rename(dados, !!!stats::setNames(from, to))
}

#' Um resumo por vírgula, com os nomes casados pela POSIÇÃO.
#'
#' Dois campos de texto em vez de um editor de pares, exatamente como
#' `tr_rename()` — e com o mesmo jeito de errar calado, porque listas de
#' tamanhos diferentes casariam o nome de um resumo com a conta de outro. Daí
#' a checagem de tamanho, explícita e antes do verbo.
#'
#' `name` passa por `.as_cols()`, que apara e descarta vazio; `expr` NÃO pode
#' passar por ele, porque a vírgula de `sum(valor, na.rm = TRUE)` não separa
#' resumo nenhum — quem sabe disso é `.tr_data_parse_exprs()`.
#' @export
tr_group_summarise <- function(dados, by, name, expr) {
  by <- .as_cols(by)
  if (length(by)) .tr_data_cols(dados, by, "by")
  # Mesmo caso de `names_to`/`values_to`: default não vazio no spec, e em
  # branco o `quos()` abortava culpando o param 'expr', que estava certo.
  .tr_data_obrigatorio(name, "name")
  # `expr` também tem default não vazio no spec (`dplyr::n()`) e resumir sem
  # expressão não é comportamento nenhum — logo, vazio é erro, não desligado.
  # Em branco, o parser do rlang abortava com "`x` must contain exactly 1
  # expression, not 0": era o único erro em INGLÊS por caminho próprio da
  # coleção, e culpava a sintaxe quando o problema é campo em branco.
  .tr_data_obrigatorio(expr, "expr")
  nomes <- .as_cols(name)
  es <- .tr_data_parse_exprs(expr, "expr")
  # Um campo só de vírgulas e espaço passa pelo `.tr_data_obrigatorio()`, que
  # olha o texto cru, e chegaria aqui com lista vazia — `summarise()` sem
  # nenhum argumento devolve os grupos e mais nada, verde e sem resumo.
  .tr_data_obrigatorio(nomes, "name")
  if (!length(es)) .tr_data_obrigatorio(character(), "expr")
  if (length(nomes) != length(es)) {
    rlang::abort(sprintf("Agrupar e resumir: %d nome(s) para %d resumo(s).",
                         length(nomes), length(es)),
                 class = "tr_data_error_mismatched_names")
  }
  # Nome repetido na lista faria o segundo resumo sobrescrever o primeiro em
  # silêncio: a tabela sai com o número de colunas certo e a conta errada.
  # Contra coluna de agrupamento vale o mesmo — o `by` é que ganharia.
  colide <- unique(c(nomes[duplicated(nomes)], intersect(nomes, by)))
  if (length(colide)) {
    rlang::abort(
      sprintf(paste0("Agrupar e resumir: o nome de coluna se repete: %s. ",
                     "Cada resumo precisa do seu."),
              paste(colide, collapse = ", ")),
      class = "tr_data_error_name_collision")
  }
  out <- if (length(by)) dplyr::group_by(dados, dplyr::across(dplyr::all_of(by))) else dados
  .tr_data_eval(
    dplyr::ungroup(dplyr::summarise(out, !!!stats::setNames(es, nomes), .groups = "drop")),
    "expr", dados)
}

#' @export
tr_join <- function(left, right, by, type = "inner") {
  by <- .as_cols(by)
  if (length(by)) { .tr_data_cols(left, by, "by"); .tr_data_cols(right, by, "by") }
  f <- switch(type, inner = dplyr::inner_join, left = dplyr::left_join,
              right = dplyr::right_join, full = dplyr::full_join, anti = dplyr::anti_join,
              .tr_data_option("type", type, c("inner", "left", "right", "full", "anti")))
  if (length(by)) f(left, right, by = by) else f(left, right)
}

#' @export
tr_arrange <- function(dados, cols, desc = FALSE) {
  cols <- .as_cols(cols)
  if (!length(cols)) return(dados)
  .tr_data_cols(dados, cols, "cols")
  # Símbolos construídos e injetados com `!!!`. `across()` só existe dentro de
  # um verbo de data-masking — montá-lo numa variável antes falha com
  # "must only be used inside data-masking verbs".
  exprs <- lapply(cols, function(cl) {
    s <- rlang::sym(cl)
    if (isTRUE(desc)) rlang::expr(dplyr::desc(!!s)) else s
  })
  dplyr::arrange(dados, !!!exprs)
}

#' @export
tr_slice_head <- function(dados, n = 10L, by = "") {
  g <- .tr_data_by(dados, by)
  # Aqui é `by`, não `.by`: os `slice_*` recebem a expressão em `...` e por
  # isso batizaram o argumento sem ponto — passar `.by` aborta na hora.
  dplyr::slice_head(dados, n = as.integer(n), by = dplyr::all_of(g))
}

#' Gravador desligado: o caminho, COMO O USUÁRIO DIGITOU, está em branco.
#'
#' A decisão tem que vir ANTES do `.ctx$path()`. Depois dele, `"   "` já virou
#' `"<raiz do projeto>/   "` — não-vazio mesmo depois do `trimws()` —, e o
#' guard antigo, que estava depois, mandava gravar: o motor criava um arquivo
#' CHAMADO `   ` na raiz do projeto e o card ficava verde. Pior, no nível 1
#' (sem `.ctx`) o mesmo nó não gravava nada, então o teste de nível 1 não
#' pegava a divergência.
#' @noRd
.tr_data_sink_desligado <- function(path) {
  !length(path) || is.na(path[[1]]) || !nzchar(trimws(as.character(path[[1]])))
}

#' Gravadores. Três coisas em comum, e as três são deliberadas.
#'
#' REPASSAM a tabela adiante em vez de cortar o fluxo: um sink terminal
#' obrigaria a duplicar o nó anterior só pra continuar a análise depois de
#' gravar.
#'
#' Caminho em BRANCO é "desligado", não erro — ao contrário dos leitores, onde
#' `.tr_data_obrigatorio()` recusa. Um card de gravação recém-arrastado da
#' paleta não tem por que pintar de vermelho: ele ainda não grava nada, mas o
#' que passa por ele continua passando. Quem decide isso é
#' `.tr_data_sink_desligado()`, e decide antes de resolver o caminho.
#'
#' `fingerprint` é SÓ o caminho, e não `path|size|mtime` como o dos leitores:
#' gravar não envenena resultado nenhum. Se o arquivo de destino mudar no
#' disco, o valor que o nó produz (a própria tabela) não muda.
#'
#' E os três são `volatile = TRUE`. Sem isso, apagar o arquivo de saída e
#' re-rodar NÃO o recriava (o nó saía `cached`), e dois gravadores no mesmo
#' caminho tinham conteúdo final decidido por qual dos dois caches caiu.
#' Trocar o fingerprint não resolve, e isso foi medido: a chave é calculada
#' ANTES do `fn` (`R/plan.R:84-88`), então "arquivo ausente" é bit a bit o
#' mesmo estado da primeira passada — cuja chave o store já guarda. Nem
#' `path|mtime` nem `path|file.exists` mudam isso; nenhum fingerprint
#' pré-execução expressa uma PÓS-condição do `fn`. Só `volatile`.
#'
#' O custo é real e está medido: como os gravadores repassam a tabela adiante,
#' `volatile` invalida tudo a jusante deles a cada passada
#' (`example -> write_rds -> filter` dá `w:running f:running`). Sink FOLHA — o
#' caso normal — só paga a regravação; sink no meio da cadeia envenena o cache
#' do resto dela. Ainda assim é a escolha: sink que não grava o arquivo que
#' promete é silêncio produzindo resultado errado, e isso é pior.
#' @export
tr_write_csv <- function(dados, path, .ctx = NULL) {
  if (.tr_data_sink_desligado(path)) return(dados)
  if (!is.null(.ctx)) path <- .ctx$path(path)
  readr::write_csv(dados, path)
  dados
}

#' @export
tr_write_rds <- function(dados, path, .ctx = NULL) {
  if (.tr_data_sink_desligado(path)) return(dados)
  if (!is.null(.ctx)) path <- .ctx$path(path)
  # Mesma razão do leitor: `saveRDS` erra cego, sem o caminho na mensagem.
  .tr_data_arquivo(dirname(path), "path", pasta = TRUE)
  saveRDS(dados, path)
  dados
}

#' Caminho em branco é "desligado", e por isso vem ANTES do `.tr_data_need()`:
#' um card recém-arrastado da paleta, que ainda nem escolheu arquivo, não pode
#' pintar de vermelho por falta do `arrow` — ele não vai gravar nada mesmo, e
#' a tabela tem que continuar passando adiante.
#' @export
tr_write_parquet <- function(dados, path, .ctx = NULL) {
  if (.tr_data_sink_desligado(path)) return(dados)
  if (!is.null(.ctx)) path <- .ctx$path(path)
  .tr_data_need("arrow", "data/write_parquet")
  arrow::write_parquet(dados, path)
  dados
}

#' Colunas chegam do front como texto separado por vírgula, ou como vetor
#' quando a chamada vem do console. Aceitar os dois é o que mantém o nível 1
#' confortável sem obrigar a UI a ter um editor de vetor.
#'
#' Os DOIS ramos aparam e descartam o vazio. Antes, só o ramo do texto o fazia,
#' e o do vetor — que é justamente o caminho do console, o nível 1 —
#' devolvia `as.character(x)` cru: `tr_select(d, c(" valor ", "qtd"))` errava
#' com "coluna inexistente:  valor ", onde o culpado é invisível porque o
#' espaço não se vê na mensagem. Divergir entre os ramos é ter dois contratos
#' para o mesmo param.
.as_cols <- function(x) {
  if (!length(x)) return(character())
  v <- if (length(x) > 1L) as.character(x) else strsplit(as.character(x), ",", fixed = TRUE)[[1]]
  v <- trimws(v)
  v[nzchar(v)]
}

#' Uma linha por coluna: o nó que se pluga logo depois do load pra saber o que
#' veio. A saída é TABELA de propósito — usa o renderizador que já existe, e o
#' resumo fica ordenável e filtrável como qualquer outra tabela do fluxo.
#' Tudo vira texto porque `minimo` de uma coluna de data e de uma numérica
#' moram na mesma coluna do resultado.
#' @export
tr_summary <- function(dados) {
  # `min()` sobre texto usa a colação da locale — aceitável aqui, porque isto
  # é um resumo para o olho, não uma chave de ordenação do fluxo.
  extremo <- function(v, qual) {
    v <- v[!is.na(v)]
    if (!length(v)) return(NA_character_)
    # `is.logical(v)` explícito: `is.numeric(TRUE)` é FALSE em R, e sem isto a
    # coluna lógica caía no NA final. Coluna lógica nasce de qualquer `mutate`
    # com comparação, e min/max vazios ali leem como "o resumo falhou".
    # `format(min(c(TRUE, FALSE)))` devolve "0": o min de lógico já vem
    # coagido a inteiro. Recoagir preserva o vocabulário da coluna — quem lê o
    # resumo de uma coluna lógica espera FALSE/TRUE, não 0/1.
    if (is.logical(v)) return(format(as.logical(qual(v))))
    if (is.numeric(v) || inherits(v, "Date") || inherits(v, "POSIXt")) return(format(qual(v)))
    if (is.character(v) || is.factor(v)) return(as.character(qual(as.character(v))))
    NA_character_
  }
  # Numa coluna-lista, `v[[1]]` tem comprimento qualquer (e `format(NULL)` é
  # vazio); `vapply()` exige escalar. Colapsar é o que impede o resumo de
  # abortar justo na coluna que a pessoa não entendeu e quis inspecionar.
  primeiro <- function(v) {
    v <- v[!is.na(v)]
    if (!length(v)) return(NA_character_)
    x <- as.character(format(v[[1]]))
    if (!length(x)) NA_character_ else paste(x, collapse = ", ")
  }
  # `unname()` em cada coluna: `vapply()` herda os nomes de `dados`, e uma coluna
  # de tibble com nomes por linha é lixo que vaza para o renderizador.
  tibble::tibble(
    coluna    = names(dados),
    tipo      = unname(vapply(dados, function(v) class(v)[[1]], "")),
    faltantes = unname(vapply(dados, function(v) sum(is.na(v)), 0L)),
    distintos = unname(vapply(dados, function(v) length(unique(v[!is.na(v)])), 0L)),
    minimo    = unname(vapply(dados, extremo, "", qual = min)),
    maximo    = unname(vapply(dados, extremo, "", qual = max)),
    exemplo   = unname(vapply(dados, primeiro, ""))
  )
}

#' Converter tipo de coluna.
#'
#' Existe como nó próprio, e não como `mutate` com expressão, por causa do CSV
#' brasileiro: `1.234,56` só vira número com locale, e a alternativa era pedir
#' `as.numeric(gsub(...))` dentro de um campo de texto.
#'
#' Falha ALTO quando a conversão produziria NA onde a entrada não era NA. As
#' funções `parse_*` do readr devolvem NA com aviso, e aviso não sobe até o
#' card — o dado sumiria em silêncio, que é o modo de falha que esta coleção
#' está inteira tentando não ter.
#'
#' Cada tipo começa por um curto-circuito quando a coluna JÁ é do tipo alvo.
#' Não é otimização: o round-trip por `as.character()` corrompia dado. Com
#' `decimal = ","`, o separador de milhar é ".", e `as.character(1234.56)` dá
#' "1234.56" — cujo ponto era comido como separador de milhar, devolvendo
#' 123456. Nada virava NA, então o guard de NA não via nada: o valor só ficava
#' mil vezes maior, calado.
#' @export
tr_convert <- function(dados, cols, type = "numero", format = "", decimal = ".") {
  cols <- .as_cols(cols)
  if (!length(cols)) return(dados)
  .tr_data_cols(dados, cols, "cols")

  numerico <- function(v) if (is.numeric(v)) as.double(v) else .tr_data_double(v, decimal)
  conv <- switch(type,
    numero  = numerico,
    inteiro = function(v) as.integer(round(numerico(v))),
    texto   = function(v) as.character(v),
    data    = function(v) {
      if (inherits(v, "Date")) return(v)
      if (inherits(v, "POSIXt")) return(as.Date(v))
      suppressWarnings(readr::parse_date(
        as.character(v), format = if (nzchar(format)) format else NULL,
        locale = readr::locale(decimal_mark = decimal)))
    },
    fator   = function(v) as.factor(v),
    logico  = function(v) if (is.logical(v)) v else
                suppressWarnings(readr::parse_logical(as.character(v))),
    .tr_data_option("type", type,
                    c("numero", "inteiro", "texto", "data", "fator", "logico"))
  )

  # A dica é por tipo porque o conserto é por tipo: número sujo se limpa antes,
  # data errada quase sempre é o campo 'formato' em branco.
  dica <- switch(type,
    numero = , inteiro = sprintf(
      " Só entra número puro (separador decimal '%s'): tire moeda, unidade, %% e parênteses antes, num nó Criar coluna.",
      decimal),
    data = " Preencha o param 'formato' (ex.: %d/%m/%Y).",
    "")

  for (cl in cols) {
    antes <- dados[[cl]]; depois <- conv(antes)
    perdidos <- unique(as.character(antes[!is.na(antes) & is.na(depois)]))
    if (length(perdidos)) {
      rlang::abort(
        sprintf("Coluna '%s' não vira %s: %s%s.%s", cl, type,
                paste(utils::head(perdidos, 3L), collapse = ", "),
                if (length(perdidos) > 3L) sprintf(" (e mais %d)", length(perdidos) - 3L) else "",
                dica),
        class = "tr_data_error_bad_conversion")
    }
    dados[[cl]] <- depois
  }
  dados
}

#' Texto para double, ESTRITO — de propósito.
#'
#' `parse_number()` era tolerante demais para um nó cuja razão de existir é
#' falhar alto: "10abc", "abc10", "R$ 10", "1/2", "(50)" e "10%" viravam todos
#' 10, "1.2.3" virava 1.2 e "1.234,56" sob decimal "." virava 1.23456. Nenhum
#' virava NA e `readr::problems()` não trazia linha nenhuma, então o guard de
#' NA do `tr_convert()` era cego a tudo isso. É mudança de contrato
#' deliberada: "R$ 10" agora ABORTA em vez de virar 10 — número errado em
#' silêncio é pior que um erro dizendo qual valor atrapalhou.
#'
#' `parse_double()` recusa tudo isso, mas ignora `grouping_mark` por completo:
#' "1.234,56" com `decimal_mark = ","` também é NA nele. Daí os dois passos —
#' tirar o separador de milhar só onde ele está BEM formado (grupos de exatos
#' três dígitos) e só então entregar ao parser. É o passo que separa
#' "1,234.56" (mil duzentos e trinta e quatro) de "1,2,3" (lixo).
#' @noRd
.tr_data_double <- function(v, decimal) {
  x <- trimws(as.character(v))
  milhar <- if (identical(decimal, ",")) "." else ","
  agrupado <- grepl(sprintf("^[+-]?[0-9]{1,3}([%s][0-9]{3})+([%s][0-9]+)?$",
                            milhar, decimal), x)
  agrupado[is.na(agrupado)] <- FALSE   # NA de entrada não é "agrupado"
  x[agrupado] <- gsub(milhar, "", x[agrupado], fixed = TRUE)
  suppressWarnings(readr::parse_double(x, locale = readr::locale(decimal_mark = decimal)))
}

#' Padroniza os nomes das colunas. Uma linha de fachada sobre o janitor —
#' existe como função exportada só para o nó ter um `fn` de nível 1 como
#' todos os outros. Colisão depois da limpeza (duas colunas que viram
#' `valor_r`) o janitor desambigua sozinho, com sufixo (`valor_r_2`).
#' @export
tr_clean_names <- function(dados) janitor::clean_names(dados)

#' @export
tr_remove_empty <- function(dados, which = "ambos") {
  # "ambos" aplica linhas e depois colunas; a ordem não muda o resultado,
  # porque só remove o que está inteiramente vazio nos dois sentidos.
  # Sem default, `which` inválido devolvia NULL e o janitor não removia NADA,
  # sem reclamar — o pior desfecho possível num nó de limpeza.
  w <- switch(which, linhas = "rows", colunas = "cols", ambos = c("rows", "cols"),
              .tr_data_option("which", which, c("linhas", "colunas", "ambos")))
  janitor::remove_empty(dados, which = w)
}

#' Ao contrário do `distinct`, que REMOVE repetidas, este MOSTRA quais são —
#' é nó da fase "conhecer", não da fase "limpar". Ver antes de apagar.
#' @export
tr_get_dupes <- function(dados, cols) {
  cols <- .as_cols(cols)
  # Sem colunas, o janitor usa todas — e avisa com `message()`. Aviso não sobe
  # até o card, só sujaria o log de execução do nó; a informação já está no
  # campo vazio da tela.
  if (!length(cols)) return(suppressMessages(janitor::get_dupes(dados)))
  .tr_data_cols(dados, cols, "cols")
  # `get_dupes()` é tidyselect: `all_of()` funciona dentro dele.
  janitor::get_dupes(dados, dplyr::all_of(cols))
}

#' Empilha várias colunas em duas: uma com o nome, outra com o valor.
#'
#' Valida ANTES do `pivot_longer`, pelo mesmo motivo de `tr_select()`: erro
#' levantado de dentro do verbo tidyr é embrulhado e a classe útil sobra só no
#' `parent`, onde o card não a enxerga.
#' @export
tr_pivot_longer <- function(dados, cols, names_to = "nome", values_to = "valor") {
  cols <- .as_cols(cols)
  if (!length(cols)) return(dados)
  .tr_data_cols(dados, cols, "cols")
  # `names_to`/`values_to` são obrigatórios: têm default não vazio no spec e o
  # usuário pode limpá-los no card. Vazio aqui não é "desligado" — sem eles as
  # duas colunas de saída não têm nome, e o R aborta com "tentativa de usar um
  # nome de variável com comprimento zero", sem classe e sem dizer qual campo.
  .tr_data_obrigatorio(names_to, "names_to")
  .tr_data_obrigatorio(values_to, "values_to")
  tidyr::pivot_longer(dados, cols = dplyr::all_of(cols),
                      names_to = names_to, values_to = values_to)
}

#' Espalha os valores de uma coluna em colunas novas.
#'
#' `names_from` e `values_from` são validados SEPARADAMENTE, cada um com o nome
#' do seu próprio param: são dois campos distintos no card, e uma mensagem que
#' dissesse "names_from/values_from" deixaria o usuário adivinhando qual dos
#' dois ele digitou errado.
#'
#' `values_fill` chega como texto (é um campo do card). Vazio = sem
#' preenchimento; número escrito vira número, pra não transformar coluna
#' numérica em texto só porque o preenchimento veio de um input.
#' @export
tr_pivot_wider <- function(dados, names_from, values_from, values_fill = "") {
  # Antes do `.tr_data_cols()`: o default dos dois no spec é "", então todo nó
  # recém-arrastado da paleta passava por aqui e a mensagem saía "coluna(s)
  # inexistente(s): ." — o nome vazio não renderiza e não sobra nada pra ler.
  .tr_data_obrigatorio(names_from, "names_from")
  .tr_data_obrigatorio(values_from, "values_from")
  # Os dois são campo de UMA coluna, não lista — daí continuarem `text` no
  # spec, e daí o `trimws()` no lugar do `.as_cols()`, que é o idioma da
  # LISTA. Sem ele, um espaço à direita vindo do card errava com
  # "coluna(s) inexistente(s): k " — o culpado invisível de sempre.
  names_from <- trimws(names_from); values_from <- trimws(values_from)
  .tr_data_cols(dados, names_from, "names_from")
  .tr_data_cols(dados, values_from, "values_from")
  .tr_data_chave_unica(dados, names_from, values_from)
  fill <- NULL
  if (nzchar(trimws(values_fill))) {
    n <- suppressWarnings(as.numeric(values_fill))
    fill <- if (is.na(n)) values_fill else n
  }
  tidyr::pivot_wider(dados, names_from = dplyr::all_of(names_from),
                     values_from = dplyr::all_of(values_from), values_fill = fill)
}

#' Recusa espalhar quando a chave não identifica uma linha só.
#'
#' O `pivot_wider()` só EMITE AVISO nesse caso e devolve colunas de listas — e
#' aviso não sobe até o card. Pior: o `fmt()` de `inst/www/runtime.js` faz
#' `String(v)` na coluna de listas, então a célula mostra "1,2" como se fosse
#' UM valor. Um número plausível, na tela, que não existe no dado — exatamente
#' o modo de falha que esta coleção existe para não ter.
#'
#' A identidade da linha é tudo que não é `names_from` nem `values_from`, que é
#' a mesma regra que o tidyr usa para montar a saída.
#' @noRd
.tr_data_chave_unica <- function(data, names_from, values_from) {
  chave <- setdiff(names(data), c(names_from, values_from))
  chave <- c(chave, names_from)
  repetida <- duplicated(data[chave])
  if (!any(repetida)) return(invisible(NULL))
  # Rótulo `coluna=valor` por linha: sozinho, o valor repetido não diz de qual
  # coluna veio, e a chave costuma ter mais de uma.
  rotulo <- do.call(paste, c(
    lapply(chave, function(cl) paste0(cl, "=", as.character(data[[cl]]))),
    list(sep = ", ")))
  combos <- unique(rotulo[repetida])
  rlang::abort(
    sprintf(paste0("Espalhar: a chave (%s) se repete, e cada célula receberia ",
                   "mais de um valor: %s%s. Agregue antes num nó 'Agrupar e ",
                   "resumir' (data/group_summarise)."),
            paste(chave, collapse = ", "),
            paste(utils::head(combos, 3L), collapse = "; "),
            if (length(combos) > 3L) sprintf(" (e mais %d)", length(combos) - 3L) else ""),
    class = "tr_data_error_duplicate_key")
}

#' Empilha várias tabelas uma embaixo da outra, casando as colunas pelo nome.
#'
#' `tabelas` é porta VARIÁDICA: `R/plan.R:67` entrega uma LISTA quando a porta
#' declara `multiple = TRUE`, já ordenada pelo `index` da aresta. Por isso o
#' argumento é uma lista também no nível 1 — a assinatura do console é a mesma
#' que o motor chama.
#'
#' Coluna que só existe em parte das tabelas vira NA nas outras; coluna com
#' tipos incompatíveis entre tabelas faz o vctrs recusar o cast, e a falha sobe
#' crua (sem classe da coleção).
#' @export
tr_bind_rows <- function(tabelas) dplyr::bind_rows(tabelas)

#' Descarta as linhas com faltante nas colunas escolhidas.
#'
#' Sem colunas, usa TODAS — que é o que a description promete, e é o pedido
#' mais comum ("some com o que está furado"). Valida FORA do `drop_na()` pelo
#' mesmo motivo do `tr_select()`: erro levantado de dentro do verbo tidyr vira
#' embrulho, e a classe útil sobra só no `parent`, onde o card não a enxerga.
#' @export
tr_drop_na <- function(dados, cols) {
  cols <- .as_cols(cols)
  if (!length(cols)) return(tidyr::drop_na(dados))
  .tr_data_cols(dados, cols, "cols")
  tidyr::drop_na(dados, dplyr::all_of(cols))
}

#' Preenche os faltantes das colunas escolhidas com um valor fixo.
#'
#' O valor de substituição chega como TEXTO (é um campo do card) e precisa
#' virar o tipo da coluna antes de entrar — senão uma coluna numérica vira
#' texto por causa de um campo de input, e todo verbo numérico a jusante
#' quebra longe daqui.
#'
#' A conversão é por tipo porque a atribuição crua `v[is.na(v)] <- valor`
#' erra de um jeito diferente em cada um, e todos calados: em coluna LÓGICA
#' promove o vetor inteiro a texto; em FATOR de nível novo emite aviso
#' (que não sobe até o card) e deixa o NA como estava — o nó não faz nada e
#' diz que fez; em INTEIRA promove a double. Só `Date` e `character` sobrevivem
#' à atribuição crua, e `Date` com texto que não é data levanta erro do R sem
#' classe nenhuma.
#'
#' Param vazio é no-op: campo em branco no card não é ordem de mexer.
#' @export
tr_replace_na <- function(dados, cols, value = "0") {
  cols <- .as_cols(cols)
  if (!length(cols)) return(dados)
  # Valor em branco também é no-op, como o resto da coleção: campo limpo no
  # card não é ordem de trocar NA por string vazia. Antes, isto abortava em
  # coluna numérica e virava "" em coluna de texto.
  if (!nzchar(trimws(value))) return(dados)
  .tr_data_cols(dados, cols, "cols")
  for (cl in cols) {
    v <- dados[[cl]]
    if (is.factor(v) && !value %in% levels(v)) levels(v) <- c(levels(v), value)
    v[is.na(v)] <- .tr_data_na_valor(v, value, cl)
    dados[[cl]] <- v
  }
  dados
}

#' Texto do card no tipo da coluna, ou erro classificado.
#'
#' Mesma regra do `tr_convert()`: falha ALTO quando a conversão produziria NA.
#' Aqui o valor é um escalar digitado, então `as.numeric()` basta para número —
#' não há CSV brasileiro para desembaraçar, e o campo tem um valor só.
#' @noRd
.tr_data_na_valor <- function(v, value, cl) {
  ruim <- function(tipo) rlang::abort(
    sprintf("Coluna '%s' é %s e '%s' não vira valor desse tipo.", cl, tipo, value),
    class = "tr_data_error_bad_conversion")
  num <- function() suppressWarnings(as.numeric(value))

  # `is.logical` antes de `is.numeric` não é firula: em R, `is.numeric(TRUE)`
  # é FALSE, mas `is.integer` de um lógico também é — a ordem aqui é a que
  # deixa cada tipo cair no seu próprio ramo.
  if (is.logical(v)) {
    r <- suppressWarnings(readr::parse_logical(value))
    if (is.na(r)) ruim("lógica (aceita TRUE/FALSE, T/F, 1/0)")
    return(r)
  }
  if (is.integer(v)) {
    r <- num()
    # `as.integer(0.5)` é 0 em silêncio; num nó de preenchimento isso é o
    # usuário pedindo 0.5 e recebendo 0 sem saber.
    if (is.na(r) || r != round(r)) ruim("de números inteiros")
    return(as.integer(r))
  }
  if (is.numeric(v)) {
    r <- num()
    if (is.na(r)) ruim("numérica")
    return(r)
  }
  if (inherits(v, "Date")) {
    r <- suppressWarnings(readr::parse_date(value))
    if (is.na(r)) ruim("de datas (use AAAA-MM-DD)")
    return(r)
  }
  if (inherits(v, "POSIXt")) {
    r <- suppressWarnings(readr::parse_datetime(value))
    if (is.na(r)) ruim("de data e hora (use AAAA-MM-DD HH:MM:SS)")
    return(r)
  }
  # Fator já teve o nível acrescentado pelo chamador; texto entra como está.
  if (is.factor(v) || is.character(v)) return(value)
  # Fora dos ramos acima, devolver `value` cru ASSUMIA texto — e a atribuição
  # promovia a coluna inteira (uma coluna `complex` virava `character`), em
  # silêncio. As fontes desta coleção não produzem esses tipos, mas assumir é
  # justamente o modo de falha que ela não quer ter.
  ruim(sprintf("do tipo '%s'", class(v)[[1]]))
}

# ---- A fronteira da região de fluxo ------------------------------------
#
# Um par de nós, e não uma aresta especial (Decisão 5): os dois têm params, e
# param nenhum tem onde morar numa aresta. Quem fatia é a FONTE, porque o
# driver do núcleo não sabe o que é uma linha de uma tabela e não vai saber
# (Decisão 4) — é aqui que `data/table` conhece a si mesmo.

#' Reparte a tabela na sequência finita de pontos que a região vai percorrer.
#'
#' O contrato do driver em duas frases: este `fn` roda UMA vez, antes do laço,
#' e devolve uma LISTA NUA — um elemento por passo, na ordem em que o driver
#' vai andar. Devolver a tabela inteira é o esquecimento mais provável de quem
#' escreve uma fonte, e ele não erra alto por conta própria: um `data.frame` É
#' uma lista, então o laço andaria nas COLUNAS e o histórico sairia plausível e
#' errado. O driver tem guarda contra isso (`tr_error_stream_bad_source`); aqui
#' o que garante a forma é o `lapply`, que nunca devolve retângulo.
#'
#' O ponto é uma TABELA de `lote` linhas (a última, parcial), e é essa a
#' decisão que dispensa tipo novo: o tipo do ponto é `data/table`, o mesmo de
#' sempre, e por isso TODO nó que já aceita uma tabela funciona dentro da
#' região sem mudar uma linha. Um ponto de uma linha continua sendo uma tabela
#' de uma linha — não um vetor, não uma lista —, porque é `data[i, ]` com
#' `drop = FALSE`; sem o `drop = FALSE` uma tabela de uma coluna viraria vetor
#' e o `data/filter` elevado morreria com erro cru de dplyr no primeiro ponto.
#'
#' `ordenar_por` ordena ANTES de lotear, e a ordem é a única coisa que o param
#' pode significar: ordenar depois do corte ordenaria dentro de cada lote e a
#' sequência de PONTOS continuaria a da tabela — o param ficaria sem efeito
#' observável, que é pior que não existir.
#'
#' Zero pontos NÃO é erro. É o que uma tabela vazia produz, e tabela vazia é o
#' que um filtro sem resultado a montante produz — recusar aqui pintaria de
#' vermelho um fluxo saudável, e a região de zero passos tem semântica
#' definida (o colapso roda uma vez, com o histórico vazio).
#' @export
tr_to_stream <- function(dados = NULL, lote = 1L, ordenar_por = "", max_passos = 0L) {
  # Params ANTES do retorno antecipado: com a validação depois, um card com
  # `lote = 0` e nada ligado lia como sadio e só ficava vermelho quando o
  # usuário conectasse a tabela — o erro chegando um passo depois do engano.
  lote <- .tr_data_inteiro(lote, "lote", min = 1L)
  max_passos <- .tr_data_inteiro(max_passos, "max_passos", min = 0L)

  # Porta opcional solta: card recém-arrastado da paleta, ou nível 1 sem
  # tabela. Zero pontos é a leitura honesta — a região não tem o que percorrer.
  if (is.null(dados)) return(list())

  # Nível 1 com um vetor no lugar da tabela: sem isto, `nrow()` devolve NULL e
  # o `if (n == 0L)` morre com "argumento tem comprimento zero", que não nomeia
  # nem o argumento nem o problema. A porta e o `store` do tipo é que impõem
  # tipo no grafo; aqui é só a chamada de função R comum que merece a frase.
  if (is.null(nrow(dados))) {
    rlang::abort(sprintf("'dados' não é uma tabela, e sim '%s'.", class(dados)[[1]]),
                 class = "tr_data_error_not_a_table")
  }

  ordem <- .as_cols(ordenar_por)
  if (length(ordem)) {
    .tr_data_cols(dados, ordem, "ordenar_por")
    # `order()` e não `dplyr::arrange()` porque a ordenação aqui não é do
    # domínio do usuário (não há expressão a avaliar): são colunas nomeadas, e
    # `method = "radix"` é o que torna a sequência de pontos — logo o
    # histórico — independente da locale de quem roda o fluxo.
    dados <- dados[do.call(order, c(unname(as.list(dados[ordem])),
                                  list(method = "radix"))), , drop = FALSE]
  }

  n <- nrow(dados)
  if (n == 0L) return(list())
  inicios <- seq.int(1L, n, by = lote)
  # `0 = todos` é a forma inteira do "vazio é desligado" da coleção, e vem
  # declarado no label do param — sem isso o número zero num campo chamado
  # "máximo de passos" leria como "nenhum passo".
  if (max_passos > 0L) inicios <- utils::head(inicios, max_passos)

  lapply(inicios, function(i) dados[seq.int(i, min(i + lote - 1L, n)), , drop = FALSE])
}

#' Empilha o histórico da região num valor comum: é aqui que a região colapsa
#' de volta no ecossistema (Decisão 6).
#'
#' O contrato do colapso: este `fn` roda UMA vez, DEPOIS do laço, e `dados` é a
#' lista com o valor de todos os passos. O driver entrega a lista inteira de
#' uma vez justamente porque juntar N pedaços num valor do tipo declarado é
#' conhecimento de domínio — `rbind` de tabela, `c()` de vetor, mosaico de
#' raster —, e o núcleo não o tem.
#'
#' Por isso também o nó tem UMA porta só. Uma segunda porta comum, ligada a um
#' nó de dentro da região, também acumularia: um `resumo = "x"` constante
#' chegaria como `list("x", "x", "x")`, uma cópia por passo, e quem lesse
#' `resumo[[1]]` esconderia o mal-entendido em vez de ver o erro. Quem precisar
#' de uma constante junto do histórico a consome DEPOIS do colapso, onde ela é
#' uma constante de verdade.
#'
#' As colunas do resultado são as do PRIMEIRO ponto, e passo com conjunto
#' diferente de colunas para ALTO nomeando o passo. `rbind`/`bind_rows` cru
#' preencheria `NA` na coluna que falta — histórico verde, com um buraco cuja
#' causa está em um passo que a mensagem não diria qual.
#' @export
tr_from_stream <- function(dados = NULL, passo = TRUE) {
  pontos <- if (is.null(dados)) {
    list()
  } else if (!is.null(dim(dados))) {
    # Retângulo, não lista de pontos: `data/from_stream` ligado direto numa
    # tabela é grafo aceito (sem fonte não há região), e aí o `fn` recebe a
    # tabela inteira. Percorrê-la como lista andaria nas COLUNAS — o mesmo erro
    # calado que a guarda do driver evita do outro lado da fronteira. Uma
    # tabela é o histórico de UM ponto, que é a única leitura sensata.
    list(dados)
  } else {
    as.list(dados)
  }

  # Zero pontos: não há ponto de onde aprender as colunas da tabela, e este nó
  # não vê a entrada da fonte — dentro da região só o que passa pelas arestas
  # chega aqui. O que ele sabe declarar é a coluna que é DELE, o índice do
  # passo. Devolver `NULL` seria pior: o tipo `data/table` recusaria guardar, e
  # um fluxo que só ficou sem resultado apareceria como fluxo quebrado.
  if (!length(pontos)) {
    return(if (isTRUE(passo)) tibble::tibble(passo = integer()) else tibble::tibble())
  }

  cols <- names(pontos[[1]])
  for (i in seq_along(pontos)) {
    p <- pontos[[i]]
    if (!is.data.frame(p)) {
      rlang::abort(sprintf(
        "Passo %d do fluxo não é uma tabela, e sim '%s'. Só tabela empilha aqui.",
        i, class(p)[[1]]), class = "tr_data_error_stream_columns")
    }
    if (!setequal(names(p), cols)) {
      faltando <- setdiff(cols, names(p)); sobrando <- setdiff(names(p), cols)
      rlang::abort(sprintf(
        paste0("Passo %d do fluxo tem colunas diferentes do passo 1.%s%s ",
               "Empilhar assim preencheria faltante em silêncio."),
        i,
        if (length(faltando)) sprintf(" Falta(m): %s.", paste(faltando, collapse = ", ")) else "",
        if (length(sobrando)) sprintf(" Sobra(m): %s.", paste(sobrando, collapse = ", ")) else ""),
        class = "tr_data_error_stream_columns")
    }
    # Na ordem do primeiro ponto: com as mesmas colunas em outra ordem,
    # `bind_rows` casa por nome e o resultado sai certo, mas a ordem das
    # colunas passaria a depender de qual ponto veio primeiro.
    pontos[[i]] <- p[cols]
  }

  if (isTRUE(passo)) {
    # `passo` PRIMEIRO, e é ele que vence uma coluna de mesmo nome vinda do
    # ponto: um nó de dentro da região que já emite o próprio `passo` (o
    # caso previsto) daria duas colunas de mesmo nome, que quebra todo verbo de
    # dplyr a jusante. O índice vem de `seq_along`, nunca de contar linhas —
    # um ponto VAZIO (o `data/filter` elevado que não achou nada) é um passo
    # que contribui zero linhas, e contar linhas deslocaria o índice de todos
    # os pontos seguintes.
    pontos <- lapply(seq_along(pontos), function(i) {
      p <- pontos[[i]][setdiff(cols, "passo")]
      dplyr::bind_cols(tibble::tibble(passo = rep(i, nrow(p))), p)
    })
  }
  dplyr::bind_rows(pontos)
}

# ---- Separar, juntar, recodificar, amostrar ------------------------------------
#
# Os quatro que faltavam para a preparação de planilha de campo: `parcela_A_1`
# que precisa virar três colunas, a data que chegou em três, o tratamento em
# códigos que precisam de nome, a idade que precisa de faixa, e a amostra para
# conferir à mão. Todos em base R + tidyr, que a coleção já importa.

#' Uma coluna só, existente, com o campo nomeado no erro.
#' @noRd
.tr_data_uma_col <- function(dados, col, param) {
  col <- .as_cols(.tr_data_obrigatorio(col, param))
  if (length(col) != 1L) {
    rlang::abort(sprintf("Param '%s': informe UMA coluna (vieram %d: %s).", param, length(col),
                         paste(col, collapse = ", ")), class = "tr_data_error_bad_option")
  }
  .tr_data_cols(dados, col, param)
}

#' Separa uma coluna de texto em várias pelo separador.
#'
#' O separador é LITERAL (`fixed`), e não expressão regular: quem digita `.`
#' ou `|` quer o caractere, e na regex os dois casariam com tudo.
#' @param dados tabela.
#' @param variavel a coluna a separar.
#' @param nomes os nomes das colunas novas, separados por vírgula.
#' @param separador o texto que separa as partes.
#' @param remover tirar a coluna original.
#' @export
tr_separate <- function(dados, variavel = "", nomes = "", separador = "_", remover = TRUE) {
  col <- .tr_data_uma_col(dados, variavel, "variavel")
  novos <- .as_cols(.tr_data_obrigatorio(nomes, "nomes"))
  .tr_data_obrigatorio(separador, "separador")
  colide <- setdiff(intersect(novos, names(dados)), if (isTRUE(remover)) col)
  if (length(colide) || anyDuplicated(novos)) {
    rlang::abort(sprintf("Param 'nomes': nome já em uso ou repetido: %s.",
                         paste(unique(c(colide, novos[duplicated(novos)])), collapse = ", ")),
                 class = "tr_data_error_name_collision")
  }
  partes <- strsplit(as.character(dados[[col]]), separador, fixed = TRUE)
  k <- length(novos)
  # Parte a mais ou a menos é ERRO, contando as linhas: o `separate()` do
  # tidyr só avisa e descarta ou preenche com NA, e o aviso não chega ao card.
  tam <- lengths(partes)
  ruins <- which(!is.na(dados[[col]]) & tam != k)
  if (length(ruins)) {
    rlang::abort(sprintf(paste0("Param 'nomes': %d nome(s), mas %d linha(s) têm outro número de partes ",
                                "(ex.: linha %d, '%s', em %d). Ajuste os nomes ou o separador."),
                         k, length(ruins), ruins[[1]], dados[[col]][ruins[[1]]], tam[ruins[[1]]]),
                 class = "tr_data_error_bad_option")
  }
  m <- do.call(rbind, lapply(partes, function(p) if (length(p) == k) p else rep(NA_character_, k)))
  pos <- match(col, names(dados))
  novas <- stats::setNames(as.data.frame(m, stringsAsFactors = FALSE), novos)
  antes <- c(names(dados)[seq_len(pos - 1L)], if (!isTRUE(remover)) col)
  depois <- names(dados)[-seq_len(pos)]
  # As colunas novas no lugar da original, e não no fim: é onde se olha.
  tibble::as_tibble(cbind(dados[antes], novas, dados[depois]))
}

#' Junta colunas numa só, com um separador.
#' @param dados tabela.
#' @param cols as colunas a juntar, na ordem.
#' @param nome o nome da coluna nova.
#' @param separador o texto posto entre os valores.
#' @param remover tirar as colunas originais.
#' @export
tr_unite <- function(dados, cols = "", nome = "junto", separador = "_", remover = TRUE) {
  cs <- .tr_data_cols(dados, .as_cols(.tr_data_obrigatorio(cols, "cols")), "cols")
  nome <- trimws(.tr_data_obrigatorio(nome, "nome"))
  if (!nzchar(separador)) .tr_data_obrigatorio("", "separador")
  if (nome %in% setdiff(names(dados), if (isTRUE(remover)) cs)) {
    rlang::abort(sprintf("Param 'nome': a coluna '%s' já existe.", nome), class = "tr_data_error_name_collision")
  }
  # Faltante vira "NA" no texto, como no `paste()`: sumir com ele juntaria
  # "A__1" e "A_1" no mesmo lugar.
  v <- do.call(paste, c(unname(as.list(as.data.frame(dados)[cs])), sep = separador))
  pos <- match(cs[[1]], names(dados))
  fora <- if (isTRUE(remover)) cs else character()
  antes <- setdiff(names(dados)[seq_len(pos - 1L)], fora)
  depois <- setdiff(names(dados)[pos:ncol(dados)], fora)
  tibble::as_tibble(cbind(dados[antes], stats::setNames(data.frame(v, stringsAsFactors = FALSE), nome),
                          dados[depois]))
}

#' Lê "a=x; b=y" em pares de/para.
#' @noRd
.tr_data_pares <- function(txt) {
  itens <- trimws(strsplit(txt, ";", fixed = TRUE)[[1]])
  itens <- itens[nzchar(itens)]
  sem_igual <- itens[!grepl("=", itens, fixed = TRUE)]
  if (length(sem_igual)) {
    rlang::abort(sprintf("Param 'niveis': cada item é 'de=para', separados por ';' (sem '=': %s).",
                         paste(sem_igual, collapse = "; ")), class = "tr_data_error_bad_option")
  }
  de <- trimws(sub("=.*$", "", itens)); para <- trimws(sub("^[^=]*=", "", itens))
  if (anyDuplicated(de)) {
    rlang::abort(sprintf("Param 'niveis': o nível '%s' aparece duas vezes.", de[duplicated(de)][[1]]),
                 class = "tr_data_error_bad_option")
  }
  stats::setNames(para, de)
}

#' Números separados por ";", com vírgula decimal aceita.
#' @noRd
.tr_data_numeros <- function(txt, param) {
  v <- trimws(strsplit(txt, ";", fixed = TRUE)[[1]]); v <- v[nzchar(v)]
  x <- suppressWarnings(as.numeric(sub(",", ".", v, fixed = TRUE)))
  if (anyNA(x)) {
    rlang::abort(sprintf("Param '%s': '%s' não é número. Separe os cortes com ';' (ex.: 0; 10; 20).",
                         param, v[is.na(x)][[1]]), class = "tr_data_error_bad_option")
  }
  x
}

#' Recodifica níveis ou cria faixas de uma coluna numérica.
#'
#' Um nó para as duas coisas porque é a mesma pergunta ("que nome dar a estes
#' valores"), e o modo se decide pelo campo preenchido: **Níveis** troca
#' valores um a um, **Cortes** fatia um número em faixas.
#' @param dados tabela.
#' @param variavel a coluna.
#' @param niveis pares `de=para` separados por `;`. Nível não citado fica igual.
#' @param cortes limites das faixas separados por `;` (use `-Inf`/`Inf` nas pontas).
#' @param rotulos nomes das faixas separados por `;`; em branco, `(a,b]`.
#' @param fechado `"direita"` (`(a,b]`) ou `"esquerda"` (`[a,b)`).
#' @param nome a coluna de saída; em branco, substitui a original.
#' @export
tr_recode <- function(dados, variavel = "", niveis = "", cortes = "", rotulos = "",
                      fechado = "direita", nome = "") {
  col <- .tr_data_uma_col(dados, variavel, "variavel")
  if (!fechado %in% c("direita", "esquerda")) {
    rlang::abort(sprintf("Param 'fechado': '%s' não é 'direita' nem 'esquerda'.", fechado),
                 class = "tr_data_error_bad_option")
  }
  tem_niv <- nzchar(trimws(niveis)); tem_cor <- nzchar(trimws(cortes))
  if (tem_niv == tem_cor) {
    rlang::abort("Preencha 'niveis' (trocar valores) OU 'cortes' (faixas de um número), um dos dois.",
                 class = "tr_data_error_bad_option")
  }
  v <- dados[[col]]
  if (tem_niv) {
    mapa <- .tr_data_pares(niveis)
    txt <- as.character(v)
    ausentes <- setdiff(names(mapa), unique(txt))
    if (length(ausentes)) {
      # Nível citado que não existe é quase sempre erro de digitação ("Sul"
      # contra "sul"), e passar calado deixaria o valor sem recodificar.
      rlang::abort(sprintf("Param 'niveis': valor(es) que não existem na coluna '%s': %s.",
                           col, paste(ausentes, collapse = ", ")), class = "tr_data_error_bad_option")
    }
    novo <- ifelse(txt %in% names(mapa), unname(mapa[txt]), txt)
    # Fator continua fator, com os níveis na ordem antiga (já traduzidos): a
    # ordem é a do eixo dos gráficos, e reordenar em silêncio a mudaria.
    if (is.factor(v)) novo <- factor(novo, levels = unique(ifelse(levels(v) %in% names(mapa),
                                                                   mapa[levels(v)], levels(v))))
  } else {
    if (!is.numeric(v)) {
      rlang::abort(sprintf("Param 'cortes': a coluna '%s' não é numérica. Converta antes num 'data/convert'.", col),
                   class = "tr_data_error_bad_option")
    }
    br <- .tr_data_numeros(cortes, "cortes")
    if (length(br) < 2L || is.unsorted(br, strictly = TRUE)) {
      rlang::abort("Param 'cortes': informe ao menos dois cortes, em ordem crescente e sem repetir.",
                   class = "tr_data_error_bad_option")
    }
    rot <- trimws(strsplit(rotulos, ";", fixed = TRUE)[[1]]); rot <- rot[nzchar(rot)]
    if (length(rot) && length(rot) != length(br) - 1L) {
      rlang::abort(sprintf("Param 'rotulos': %d corte(s) formam %d faixa(s), e vieram %d rótulo(s).",
                           length(br), length(br) - 1L, length(rot)), class = "tr_data_error_bad_option")
    }
    # `include.lowest`: o primeiro corte entra na primeira faixa (fechado à
    # direita) — sem ele, o 0 de "0; 10; 20" cairia fora de tudo, como NA.
    novo <- cut(v, br, labels = if (length(rot)) rot else NULL, right = fechado == "direita",
                include.lowest = TRUE)
    fora <- sum(is.na(novo) & !is.na(v))
    if (fora) {
      rlang::abort(sprintf(paste0("Param 'cortes': %d valor(es) de '%s' caem fora das faixas ",
                                  "(de %g a %g). Estenda os cortes, ou use -Inf/Inf nas pontas."),
                           fora, col, min(br), max(br)), class = "tr_data_error_bad_option")
    }
  }
  destino <- if (nzchar(trimws(nome))) trimws(nome) else col
  dados[[destino]] <- novo
  dados
}

#' Amostra linhas, com semente.
#' @param dados tabela.
#' @param n quantas linhas; 0 usa a fração.
#' @param fracao a fração das linhas (0 a 1), quando `n` é 0.
#' @param reposicao sortear com reposição.
#' @param grupo colunas: amostra dentro de cada grupo.
#' @param .seed semente (a do nó).
#' @export
tr_sample <- function(dados, n = 10L, fracao = 0.1, reposicao = FALSE, grupo = "", .seed = 1L) {
  if (length(n) != 1L || !is.numeric(n) || is.na(n) || n < 0 || n != round(n)) {
    rlang::abort("Param 'n': um inteiro >= 0 (0 usa a fração).", class = "tr_data_error_bad_option")
  }
  por_fracao <- n == 0
  if (por_fracao && (length(fracao) != 1L || !is.numeric(fracao) || is.na(fracao) || fracao <= 0 ||
                     (fracao > 1 && !isTRUE(reposicao)))) {
    rlang::abort("Param 'fracao': entre 0 e 1 (acima de 1 só com reposição).", class = "tr_data_error_bad_option")
  }
  g <- .as_cols(grupo)
  if (length(g)) .tr_data_cols(dados, g, "grupo")
  chave <- if (length(g)) interaction(as.data.frame(dados)[g], drop = TRUE, lex.order = TRUE) else
    factor(rep(1L, nrow(dados)))
  idx <- split(seq_len(nrow(dados)), chave)
  tamanhos <- vapply(idx, function(i) if (por_fracao) round(fracao * length(i)) else n, 0)
  pequenos <- names(idx)[tamanhos > lengths(idx)]
  if (!isTRUE(reposicao) && length(pequenos)) {
    rlang::abort(sprintf(paste0("Param 'n': sem reposição não se tiram %d linhas de %s com %d. ",
                                "Diminua n ou ligue a reposição."), as.integer(n),
                         if (length(g)) sprintf("o grupo '%s'", pequenos[[1]]) else "uma tabela",
                         length(idx[[pequenos[[1]]]])), class = "tr_data_error_bad_option")
  }
  # Mesma higiene do `tr_generate()`: a semente é do nó, e o RNG global volta
  # como estava — o sorteio daqui não mexe no de ninguém.
  anterior <- if (exists(".Random.seed", globalenv(), inherits = FALSE)) get(".Random.seed", globalenv())
  on.exit(if (is.null(anterior)) suppressWarnings(rm(".Random.seed", envir = globalenv())) else
    assign(".Random.seed", anterior, envir = globalenv()), add = TRUE)
  set.seed(.seed)
  # `i[sample.int(length(i), ...)]` e não `sample(i)`: com um índice só,
  # `sample(5)` sortearia de 1:5.
  escolhidos <- unlist(Map(function(i, k) i[sample.int(length(i), k, replace = isTRUE(reposicao))],
                           idx, tamanhos), use.names = FALSE)
  dados[sort(escolhidos), , drop = FALSE]
}
