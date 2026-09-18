# Fonte que não depende de arquivo no disco. O ramo do `mtcars` é o frágil:
# os nomes dos carros são rownames, e rowname que se perde some sem erro.

test_that("dados de exemplo saem como tibble, com rownames virando coluna", {
  m <- tr_example("mtcars")
  expect_s3_class(m, "tbl_df")
  expect_equal(nrow(m), 32L)
  expect_equal(names(m)[[1]], "nome")
  expect_equal(m$nome[[1]], "Mazda RX4")
  expect_equal(nrow(tr_example("iris")), 150L)
  # E o outro lado da regra: rowname que é só "1","2","3" NÃO vira coluna.
  # `faithful` guarda rowname explícito (`.row_names_info` > 0) e ganharia uma
  # coluna de informação nenhuma se o teste fosse pela forma e não pelo conteúdo.
  expect_false("nome" %in% names(tr_example("faithful")))
  expect_false("nome" %in% names(tr_example("iris")))
})

# A varredura é a guarda que substitui a lista escrita à mão: se `datasets`
# ganhar um conjunto novo (o `penguins` entrou no R 4.5), ele entra na paleta
# sozinho — e este teste é o que garante que entrar sozinho não significa
# entrar quebrado.
test_that("todos os conjuntos ofertados existem e saem utilizáveis", {
  ofertados <- trama.data:::.tr_data_exemplos()
  expect_gt(length(ofertados), 40L)

  for (d in ofertados) {
    x <- tr_example(d)
    expect_s3_class(x, "tbl_df")
    expect_gt(nrow(x), 0L)
    expect_gt(ncol(x), 0L)
  }
})

# O enum do card e o `fn` leem a MESMA lista, e é isso que esta asserção trava:
# enquanto foram duas listas escritas à mão, o card podia oferecer um nome que
# a função recusava sem que nada acusasse.
test_that("o que o card oferece é exatamente o que a função aceita", {
  n <- trama::tr_get_node("data/example", data_registry())
  expect_equal(n$params$dataset$choices, trama.data:::.tr_data_exemplos())
  expect_true(n$params$dataset$default %in% n$params$dataset$choices)
})

# Sem validação o `get()` devolveria o objeto que fosse, e um `ts` só falharia
# lá no `store` do tipo — depois de rodar, longe da causa. A classe é conferida
# em PRIMEIRA ORDEM porque é essa que o motor grava.
test_that("conjunto desconhecido aborta classificado", {
  expect_error(tr_example("titanic"), class = "tr_data_error_bad_option")
  expect_equal(class(tryCatch(tr_example("titanic"), error = identity))[[1]],
               "tr_data_error_bad_option")
})

# O mesmo erro, pelo outro motivo: existe no `datasets`, mas não é tabela. São
# 62 objetos assim — séries temporais, matrizes, tabelas de contingência — e
# nenhum deles pode chegar ao card como opção válida.
test_that("objeto do datasets que não é tabela é recusado igual", {
  for (d in c("AirPassengers", "Titanic", "volcano", "euro", "precip")) {
    expect_error(tr_example(d), class = "tr_data_error_bad_option", info = d)
  }
})

# ---- Leitores de arquivo -----------------------------------------------
# Todo leitor é impuro, e o `fingerprint` é o único freio contra servir dado
# velho em silêncio. É ele que estes testes travam.

test_that("leitores voltam o que foi gravado", {
  d <- df_exemplo()
  p <- tempfile(fileext = ".rds"); saveRDS(d, p)
  expect_equal(tr_read_rds(p), d)

  skip_if_not_installed("arrow")
  q <- tempfile(fileext = ".parquet"); arrow::write_parquet(d, q)
  expect_equal(nrow(tr_read_parquet(q)), nrow(d))
  expect_s3_class(tr_read_parquet(q), "tbl_df")
  expect_equal(names(tr_read_parquet(q)), names(d))
})

test_that("read_json lê array de objetos como tibble", {
  p <- tempfile(fileext = ".json"); on.exit(unlink(p), add = TRUE)
  writeLines('[{"nome":"Ana","valor":10},{"nome":"Bia","valor":null}]', p)

  out <- tr_read_json(p)
  expect_s3_class(out, "tbl_df")
  expect_equal(out$nome, c("Ana", "Bia"))
  expect_equal(out$valor, c(10L, NA_integer_))
})

test_that("read_json aceita objeto de vetores", {
  p <- tempfile(fileext = ".json"); on.exit(unlink(p), add = TRUE)
  writeLines('{"nome":["Ana","Bia"],"valor":[10,20]}', p)

  expect_equal(tr_read_json(p), tibble::tibble(
    nome = c("Ana", "Bia"), valor = c(10L, 20L)))
})

test_that("read_json recusa raiz que não representa tabela", {
  for (json in c("1", '"texto"', "[1,2]", "null")) {
    p <- tempfile(fileext = ".json"); on.exit(unlink(p), add = TRUE)
    writeLines(json, p)
    err <- tryCatch(tr_read_json(p), error = identity)
    expect_equal(class(err)[[1]], "tr_data_error_not_a_table", info = json)
  }
})

test_that("pacote de Suggests ausente vira erro classificado, em primeira ordem", {
  expect_error(.tr_data_need("pacote_que_nao_existe_xyz", "data/read_parquet"),
               class = "tr_data_error_missing_package")
  e <- tryCatch(.tr_data_need("pacote_que_nao_existe_xyz", "data/read_parquet"),
                error = identity)
  expect_equal(class(e)[[1]], "tr_data_error_missing_package")
})

test_that("cada leitor declara fingerprint sensível ao arquivo", {
  reg <- data_registry()
  ctx <- list(path = function(p) p)
  p <- tempfile(fileext = ".rds"); saveRDS(df_exemplo(), p)
  for (id in c("data/read_csv", "data/read_json", "data/read_rds",
               "data/read_parquet", "data/read_excel")) {
    fp <- reg$nodes[[id]]$fingerprint
    expect_true(is.function(fp), info = id)
    antes <- fp(list(path = p), ctx)
    Sys.setFileTime(p, Sys.time() + 10)
    expect_false(identical(antes, fp(list(path = p), ctx)), info = id)
  }
})

# A prova de que o teste acima não é vacuoso: SEM mexer no arquivo a chave tem
# que ser a mesma. Se `.tr_data_file_print` devolvesse algo aleatório (ou se o
# `expect_false` passasse por qualquer motivo que não a mtime), este teste
# quebraria.
test_that("fingerprint é estável quando o arquivo não muda, e igual nos cinco", {
  ctx <- list(path = function(p) p)
  p <- tempfile(fileext = ".rds"); saveRDS(df_exemplo(), p)
  expect_identical(.tr_data_file_print(list(path = p), ctx),
                   .tr_data_file_print(list(path = p), ctx))

  reg <- data_registry()
  ids <- c("data/read_csv", "data/read_json", "data/read_rds",
           "data/read_parquet", "data/read_excel")
  chaves <- vapply(ids, function(id) reg$nodes[[id]]$fingerprint(list(path = p), ctx), "")
  expect_equal(length(unique(chaves)), 1L)
})

# Arquivo ausente não pode dar chave INSTÁVEL (senão nada é reaproveitado) nem
# chave IGUAL à do arquivo presente (senão o nó nunca reexecuta quando o
# arquivo aparece).
test_that("arquivo ausente dá chave estável e diferente da do arquivo presente", {
  ctx <- list(path = function(p) p)
  p <- tempfile(fileext = ".rds")
  ausente <- .tr_data_file_print(list(path = p), ctx)
  expect_identical(ausente, .tr_data_file_print(list(path = p), ctx))
  saveRDS(df_exemplo(), p)
  expect_false(identical(ausente, .tr_data_file_print(list(path = p), ctx)))
})

# O card recém-arrastado da paleta tem `path = ""`. O fingerprint roda ANTES do
# `fn`, então ele não pode explodir — quem recusa é o verbo, classificado.
test_that("path em branco não quebra o fingerprint e aborta classificado no verbo", {
  ctx <- list(path = function(p) p)
  expect_silent(branco <- .tr_data_file_print(list(path = ""), ctx))
  expect_identical(branco, .tr_data_file_print(list(path = ""), ctx))

  for (f in list(tr_read_csv, tr_read_json, tr_read_rds, tr_read_parquet,
                 tr_read_excel)) {
    expect_error(f(""), class = "tr_data_error_blank_param")
    expect_equal(class(tryCatch(f(""), error = identity))[[1]],
                 "tr_data_error_blank_param")
  }
})

# O bug mais caro que esta coleção teve: `na = "NA"` TIRAVA a string vazia do
# conjunto do readr (cujo default é `c("", "NA")`), e a célula vazia do CSV
# chegava como TEXTO VAZIO PRESENTE. Dali em diante o fluxo inteiro ficava
# verde mentindo — `drop_na` não descartava a linha, `remove_empty` não a via,
# `summary` reportava zero faltantes numa coluna cheia de buracos.
test_that("célula vazia, NA e marcador custom viram todos faltante", {
  p <- tempfile(fileext = ".csv"); on.exit(unlink(p), add = TRUE)
  writeLines(c("a,b", "1,x", "2,", "3,NA", "4,sem dado", "5,-"), p)

  b <- tr_read_csv(p, na = "NA, sem dado, -")$b
  expect_equal(b, c("x", NA, NA, NA, NA))
  expect_equal(sum(is.na(b)), 4L)

  # E o efeito a jusante, que é o que de fato importa: os três somem juntos.
  d <- tr_read_csv(p, na = "NA, sem dado, -")
  expect_equal(tr_drop_na(d, "b")$a, 1)
  expect_equal(tr_summary(d)$faltantes[[2]], 4L)

  # A célula vazia não depende de estar declarada, e nem de haver declaração.
  expect_true(is.na(tr_read_csv(p, na = "NA")$b[[2]]))
  expect_true(is.na(tr_read_csv(p, na = "")$b[[2]]))
  # Espaço em volta da marca é aparado, como em todo campo de lista daqui.
  expect_true(is.na(tr_read_csv(p, na = "  sem dado  ")$b[[4]]))
})

test_that("tr_read_excel aceita planilha por número e por nome", {
  skip_if_not_installed("readxl")
  x <- readxl::readxl_example("datasets.xlsx")
  expect_equal(readxl::excel_sheets(x)[[1]], "mtcars")
  expect_equal(ncol(tr_read_excel(x, sheet = "1")), ncol(tr_read_excel(x, sheet = "mtcars")))
  expect_equal(names(tr_read_excel(x, sheet = "chickwts")), c("weight", "feed"))
  # "1" tem que ser o ÍNDICE 1, não uma planilha chamada "1".
  expect_equal(names(tr_read_excel(x, sheet = "1")), names(tr_read_excel(x, sheet = 1)))
})

# ---- Gravadores --------------------------------------------------------
# Gravar é impuro mas ASSIMÉTRICO em relação a ler: se o arquivo de destino
# mudar no disco, o valor produzido (a própria tabela, repassada adiante) não
# muda. Por isso o fingerprint destes é só o caminho.

test_that("grava e lê de volta", {
  d <- df_exemplo()
  p <- tempfile(fileext = ".rds")
  out <- tr_write_rds(d, p)
  expect_equal(out, d)                      # repassa adiante
  expect_equal(tr_read_rds(p), d)

  skip_if_not_installed("arrow")
  q <- tempfile(fileext = ".parquet")
  expect_equal(tr_write_parquet(d, q), d)   # repassa adiante
  expect_equal(nrow(tr_read_parquet(q)), nrow(d))
})

# Contar linhas não prova quase nada: o que interessa num round-trip é se o
# TIPO voltou. Esta tabela existe para medir isso — e a medida é a evidência
# de por que o artefato interno do trama continua em .rds.
df_tipos <- function() {
  tibble::tibble(
    txt  = c("a", "b"),
    int  = c(1L, 2L),
    dbl  = c(1.5, 2.5),
    lgl  = c(TRUE, FALSE),
    fct  = factor(c("medio", "baixo"), levels = c("baixo", "medio", "alto")),
    data = as.Date(c("2020-01-01", "2020-06-15")),
    hora = as.POSIXct(c("2020-01-01 10:30:00", "2020-06-15 23:59:59"), tz = "UTC")
  )
}

test_that("rds preserva tipo, atributo e ordem de níveis", {
  d <- df_tipos()
  p <- tempfile(fileext = ".rds")
  tr_write_rds(d, p)
  expect_identical(tr_read_rds(p), d)
  expect_identical(levels(tr_read_rds(p)$fct), levels(d$fct))
})

test_that("parquet preserva os tipos comuns; o fator volta como fator com níveis", {
  skip_if_not_installed("arrow")
  d <- df_tipos()
  q <- tempfile(fileext = ".parquet")
  tr_write_parquet(d, q)
  v <- tr_read_parquet(q)
  expect_equal(nrow(v), nrow(d))
  expect_equal(names(v), names(d))
  expect_type(v$txt, "character")
  expect_type(v$int, "integer")
  expect_type(v$dbl, "double")
  expect_type(v$lgl, "logical")
  expect_s3_class(v$data, "Date")
  expect_s3_class(v$hora, "POSIXct")
  # O arrow guarda fator como `dictionary`, e a ordem dos níveis sobrevive.
  expect_s3_class(v$fct, "factor")
  expect_identical(levels(v$fct), levels(d$fct))
})

# O que o parquet NÃO devolve igual. Não é bug do nó — é o formato, e está
# aqui medido porque é a evidência de que o artefato interno do trama
# continua em .rds: lá o round-trip é `identical()`, aqui não.
test_that("parquet perde a unidade do difftime e recusa complexo e lista heterogênea", {
  skip_if_not_installed("arrow")
  q <- tempfile(fileext = ".parquet")
  d <- tibble::tibble(dif = as.difftime(c(1, 2), units = "hours"))
  tr_write_parquet(d, q)
  expect_equal(units(tr_read_parquet(q)$dif), "secs")   # entrou em horas

  expect_error(tr_write_parquet(tibble::tibble(x = c(1 + 2i, 3 - 1i)), q))
  expect_error(tr_write_parquet(tibble::tibble(l = list(1:3, letters[1:2])), q))
  # Coluna-lista HOMOGÊNEA o arrow aceita, e ela volta como `arrow_list`.
  expect_no_error(tr_write_parquet(tibble::tibble(l = list(1:3, 4:5)), q))
})

# Branco nos gravadores é "desligado", não erro — ao contrário dos leitores.
# Um card de gravação recém-arrastado não pode pintar de vermelho, e o nó
# ainda repassa a tabela adiante para quem estiver ligado depois dele.
test_that("caminho em branco desliga a gravação e repassa a tabela", {
  d <- df_exemplo()
  expect_equal(tr_write_csv(d, ""), d)
  expect_equal(tr_write_rds(d, "   "), d)
  skip_if_not_installed("arrow")
  expect_equal(tr_write_parquet(d, ""), d)
})

test_that("gravar sobrescreve o arquivo existente, sem reclamar", {
  p <- tempfile(fileext = ".rds")
  tr_write_rds(df_exemplo(), p)
  d2 <- df_exemplo()[1:2, ]
  expect_silent(tr_write_rds(d2, p))
  expect_equal(nrow(tr_read_rds(p)), 2L)
})

# O fingerprint dos gravadores é SÓ o caminho, de propósito: o valor que o nó
# produz não depende do estado do arquivo de destino.
test_that("gravadores declaram fingerprint só-caminho", {
  reg <- data_registry()
  ctx <- list(path = function(p) file.path("/proj", p))
  for (id in c("data/write_csv", "data/write_rds", "data/write_parquet")) {
    fp <- reg$nodes[[id]]$fingerprint
    expect_true(is.function(fp), info = id)
    expect_identical(fp(list(path = "x.out"), ctx), "/proj/x.out", info = id)
    expect_false(reg$nodes[[id]]$pure, info = id)
  }
})

test_that("gravadores repassam a tabela: a porta de saída existe", {
  reg <- data_registry()
  for (id in c("data/write_csv", "data/write_rds", "data/write_parquet")) {
    expect_true("out" %in% names(reg$nodes[[id]]$outputs), info = id)
    expect_true("data" %in% names(reg$nodes[[id]]$inputs), info = id)
  }
})

# Gravar é o único nó cuja PÓS-CONDIÇÃO é um efeito no disco, e nenhum
# fingerprint pré-execução expressa isso: apagar o arquivo de saída deixa o
# mundo bit a bit igual ao da primeira passada, cuja chave o store já guarda.
# O sink saía `cached` e o arquivo não voltava — silêncio produzindo resultado
# errado. `volatile = TRUE` é o que fecha isso.
test_that("apagar o arquivo de saída e re-rodar regrava", {
  reg <- data_registry()
  s <- trama::tr_store(tempfile("store"))
  alvo <- tempfile(fileext = ".rds")
  doc <- trama::tr_doc()
  for (op in list(
    list(op = "add_node", type = "data/example", id = "fonte",
         params = list(dataset = "mtcars")),
    list(op = "add_node", type = "data/write_rds", id = "grava",
         params = list(path = alvo)),
    list(op = "connect", from_node = "fonte", from_port = "out",
         to_node = "grava", to_port = "data"))) {
    doc <- trama::tr_doc_apply(doc, op, reg)
  }

  trama::tr_run(doc, registry = reg, store = s)
  expect_true(file.exists(alvo))

  unlink(alvo)
  expect_false(file.exists(alvo))
  trama::tr_run(doc, registry = reg, store = s)
  expect_true(file.exists(alvo))
  expect_equal(nrow(readRDS(alvo)), 32L)
})

test_that("os três sinks são volatile — nenhum se declara cacheável", {
  reg <- data_registry()
  for (id in c("data/write_csv", "data/write_rds", "data/write_parquet")) {
    expect_true(isTRUE(reg$nodes[[id]]$volatile), info = id)
  }
})

# A exceção ao critério "deixa passar o que o terceiro recusa com mensagem
# boa": readr e arrow põem o caminho na mensagem, `readRDS` NÃO põe. Ele dá
# "não é possível abrir a conexão" e manda o caminho num `warning()`, que não
# sobe ao card. Renomear um arquivo, ou mover o projeto, virava erro cego.
test_that("read_rds com arquivo inexistente aborta com o caminho e classificado", {
  p <- file.path(tempdir(), "nao_existe_xyz.rds")
  expect_error(tr_read_rds(p), class = "tr_data_error_missing_file")
  e <- tryCatch(tr_read_rds(p), error = identity)
  expect_equal(class(e)[[1]], "tr_data_error_missing_file")
  expect_match(conditionMessage(e), basename(p), fixed = TRUE)
})

test_that("write_rds em diretório inexistente aborta com o caminho e classificado", {
  p <- file.path(tempdir(), "pasta_que_nao_existe_xyz", "saida.rds")
  expect_error(tr_write_rds(df_exemplo(), p), class = "tr_data_error_missing_file")
  e <- tryCatch(tr_write_rds(df_exemplo(), p), error = identity)
  expect_equal(class(e)[[1]], "tr_data_error_missing_file")
  expect_match(conditionMessage(e), "pasta_que_nao_existe_xyz", fixed = TRUE)
})

# `path = ""` é "desligado" por decisão explícita, e vem ANTES de qualquer
# outra checagem: um card recém-arrastado não pinta de vermelho por falta do
# arrow numa máquina que ainda nem escolheu arquivo.
test_that("write_parquet com caminho em branco não exige o arrow", {
  expect_equal(tr_write_parquet(df_exemplo(), ""), df_exemplo())
})

# Planilha em branco é o mesmo tipo de branco que o `path` já classifica: sem
# ela o nó não tem como produzir tabela. Cru dava `Sheet '' not found`, sem
# classe e sem dizer que campo do card estava vazio.
test_that("read_excel com planilha em branco aborta classificado", {
  skip_if_not_installed("readxl")
  x <- readxl::readxl_example("datasets.xlsx")
  expect_error(tr_read_excel(x, sheet = ""), class = "tr_data_error_blank_param")
  e <- tryCatch(tr_read_excel(x, sheet = ""), error = identity)
  expect_equal(class(e)[[1]], "tr_data_error_blank_param")
  expect_match(conditionMessage(e), "sheet", fixed = TRUE)
})

# O branco tem que ser decidido sobre o caminho COMO O USUÁRIO DIGITOU. Pelo
# motor o `path` chega já resolvido contra a raiz do projeto, e `"   "` vira
# `"<raiz>/   "` — não-vazio mesmo depois do `trimws()`. O guard antigo, posto
# depois do `.ctx$path()`, deixava passar: o motor criava um arquivo CHAMADO
# `   ` na raiz do projeto e o card ficava verde, enquanto o nível 1 (sem
# `.ctx`) não gravava nada. Nível 1 e motor divergindo é o que escondeu o bug.
test_that("caminho só de espaços não grava arquivo nenhum, no motor", {
  reg <- data_registry()
  raiz <- tempfile("proj"); dir.create(raiz)
  s <- trama::tr_store(tempfile("store"), project_root = raiz)

  doc <- trama::tr_flow(reg) |>
    trama::tr_add("fonte", "data/example", dataset = "mtcars") |>
    trama::tr_add("g", "data/write_csv", path = "   ", from = "fonte") |>
    trama::tr_flow_doc()
  ev <- list()
  trama::tr_run(doc, registry = reg, store = s,
                on_event = function(e) ev[[length(ev) + 1]] <<- e)
  expect_length(Filter(function(e) identical(e$type, "failed"), ev), 0L)
  # A tabela continua passando adiante...
  expect_equal(nrow(trama::tr_value(doc, "g", reg, s)), 32L)
  # ...e nada foi criado na raiz do projeto.
  expect_equal(list.files(raiz, all.files = TRUE, no.. = TRUE), character())
})

test_that("caminho só de espaços: nível 1 e motor concordam nos três gravadores", {
  raiz <- tempfile("proj"); dir.create(raiz)
  ctx <- list(path = function(p) trama::tr_store_path(list(project_root = raiz), p))
  d <- df_exemplo()
  expect_equal(tr_write_csv(d, "   ", .ctx = ctx), d)
  expect_equal(tr_write_rds(d, "   ", .ctx = ctx), d)
  if (requireNamespace("arrow", quietly = TRUE)) {
    expect_equal(tr_write_parquet(d, "   ", .ctx = ctx), d)
  }
  expect_equal(list.files(raiz, all.files = TRUE, no.. = TRUE), character())
})

# ---- Round-trip do par CSV ---------------------------------------------
# O par `data/write_csv` -> `data/read_csv` era o buraco maior da suíte:
# `tr_write_csv()` podia virar um no-op COMPLETO sem quebrar nada (nada olhava
# o arquivo gravado, só que a tabela era repassada adiante), e `delim` e `na`
# do leitor não tinham uma única asserção — um leitor que ignorasse os dois
# params passava verde. Um gravador que não grava e um leitor que ignora o
# separador são as duas formas mais diretas de "verde mentindo" que este par
# pode ter.

test_that("write_csv grava o arquivo de verdade, com o conteúdo da tabela", {
  d <- tibble::tibble(a = c(1, 2), b = c("x", "y"))
  p <- tempfile(fileext = ".csv"); on.exit(unlink(p), add = TRUE)

  expect_equal(tr_write_csv(d, p), d)        # repassa adiante
  expect_true(file.exists(p))
  # CONTEÚDO, não existência: o cabeçalho, as linhas e o separador. Sem isto,
  # gravar um arquivo vazio (ou não gravar) passaria igual.
  expect_equal(readLines(p), c("a,b", "1,x", "2,y"))
})

test_that("o par write_csv -> read_csv devolve a tabela, faltantes inclusive", {
  d <- tibble::tibble(a = c(1, NA, 3), b = c("x", "y", NA))
  p <- tempfile(fileext = ".csv"); on.exit(unlink(p), add = TRUE)

  tr_write_csv(d, p)
  # O gravador escreve o faltante como `NA`, que é a marca padrão do leitor:
  # o round-trip fecha sem ajuste nenhum nos dois cards.
  volta <- tr_read_csv(p)
  expect_equal(volta$a, d$a)
  expect_equal(volta$b, d$b)
  expect_equal(sum(is.na(volta$b)), 1L)

  # E a prova de que o `NA` do arquivo é lido pelo PARAM, e não por mágica:
  # sem a marca declarada, o mesmo arquivo devolve o texto "NA" presente.
  expect_equal(tr_read_csv(p, na = "")$b, c("x", "y", "NA"))
})

# `delim` não tinha teste nenhum. Um leitor que ignorasse o param devolveria
# UMA coluna com a linha inteira dentro — plausível na tela, e errado.
test_that("read_csv usa o delim que recebe, e só ele", {
  p <- tempfile(fileext = ".csv"); on.exit(unlink(p), add = TRUE)
  writeLines(c("a;b;c", "1;x;2", "3;y;4"), p)

  ponto_virgula <- tr_read_csv(p, delim = ";")
  expect_equal(names(ponto_virgula), c("a", "b", "c"))
  expect_equal(ponto_virgula$a, c(1, 3))
  expect_equal(ponto_virgula$b, c("x", "y"))

  # Com o default (vírgula), o mesmo arquivo não tem separador nenhum: uma
  # coluna só, com a linha inteira como texto. É o que denuncia o param
  # ignorado.
  virgula <- tr_read_csv(p)
  expect_equal(ncol(virgula), 1L)
  expect_equal(names(virgula), "a;b;c")

  # E o tabulado, que é o outro caso real do enum do card.
  q <- tempfile(fileext = ".tsv"); on.exit(unlink(q), add = TRUE)
  writeLines(c("a\tb", "1\tx"), q)
  expect_equal(names(tr_read_csv(q, delim = "\t")), c("a", "b"))
  expect_equal(ncol(tr_read_csv(q)), 1L)
})

# `na` é LISTA separada por vírgula, e a célula vazia entra à força — é o
# comportamento novo, e o que fecha o bug de `na = "NA"` TIRAR a string vazia
# do conjunto do readr. Aqui ele é medido junto com o `delim`, porque é assim
# que a planilha brasileira chega: `;` e `-` na mesma leitura.
test_that("read_csv trata na como lista, com a célula vazia sempre por baixo", {
  p <- tempfile(fileext = ".csv"); on.exit(unlink(p), add = TRUE)
  writeLines(c("a;b", "1;x", "2;", "3;-", "4;sem dado", "5;NA"), p)

  # Lista de marcas: as três somem juntas, e a célula vazia junto.
  todos <- tr_read_csv(p, delim = ";", na = "-, sem dado, NA")$b
  expect_equal(todos, c("x", NA, NA, NA, NA))

  # Só uma marca declarada: as outras continuam texto PRESENTE, e a célula
  # vazia continua faltante mesmo sem estar na lista.
  so_traco <- tr_read_csv(p, delim = ";", na = "-")$b
  expect_equal(so_traco, c("x", NA, NA, "sem dado", "NA"))

  # Campo em branco no card: sobra só o piso, a célula vazia.
  nenhuma <- tr_read_csv(p, delim = ";", na = "")$b
  expect_equal(nenhuma, c("x", NA, "-", "sem dado", "NA"))

  # Espaço em volta de cada marca é aparado, como em todo campo de lista daqui.
  expect_true(is.na(tr_read_csv(p, delim = ";", na = "  sem dado  ")$b[[4]]))
})

# Gravar e ler de volta com marca custom fecha o ciclo pelos dois lados: o
# arquivo que o gravador produz é lido pelo leitor com o param certo, e o
# param errado devolve outra coisa.
test_that("o round-trip do par sobrevive a marca de faltante custom", {
  d <- tibble::tibble(v = c("a", NA, "c"))
  p <- tempfile(fileext = ".csv"); on.exit(unlink(p), add = TRUE)

  tr_write_csv(d, p)
  expect_equal(readLines(p), c("v", "a", "NA", "c"))
  # Declarando outra marca, o `NA` gravado deixa de contar como faltante — o
  # que prova que quem decide é o param, e não o formato do arquivo.
  expect_equal(tr_read_csv(p, na = "sem dado")$v, c("a", "NA", "c"))
  expect_equal(tr_read_csv(p, na = "NA, sem dado")$v, c("a", NA, "c"))
})

# Os conjuntos de exemplo eram INTERCAMBIÁVEIS: trocar `airquality` por
# `ToothGrowth` no `switch` passava pela suíte inteira, porque o teste acima só
# conta linhas e colunas. O card diria "airquality" e a tabela seria outra —
# e é justamente `airquality` que a ajuda promete como o conjunto COM
# faltantes, que é o que faz dele o exemplo dos nós de limpar.
test_that("cada conjunto de exemplo é o conjunto que o card diz ser", {
  identidade <- function(nome) {
    x <- tr_example(nome)
    list(n = nrow(x), cols = names(x))
  }
  esperado <- list(
    mtcars      = list(n = 32L,  cols = c("nome", "mpg", "cyl", "disp", "hp",
                                          "drat", "wt", "qsec", "vs", "am",
                                          "gear", "carb")),
    USArrests   = list(n = 50L,  cols = c("nome", "Murder", "Assault",
                                          "UrbanPop", "Rape")),
    iris        = list(n = 150L, cols = c("Sepal.Length", "Sepal.Width",
                                          "Petal.Length", "Petal.Width",
                                          "Species")),
    airquality  = list(n = 153L, cols = c("Ozone", "Solar.R", "Wind", "Temp",
                                          "Month", "Day")),
    ToothGrowth = list(n = 60L,  cols = c("len", "supp", "dose")),
    ChickWeight = list(n = 578L, cols = c("weight", "Time", "Chick", "Diet")))

  for (nome in names(esperado)) {
    expect_equal(identidade(nome), esperado[[nome]], info = nome)
  }

  # E a promessa que a página faz de propósito: `airquality` é o único com
  # faltante de verdade, e é por isso que ele é o exemplo dos nós de limpar.
  ar <- tr_example("airquality")
  expect_gt(sum(is.na(ar$Ozone)), 0L)
  expect_equal(sum(is.na(tr_example("ToothGrowth"))), 0L)
})
