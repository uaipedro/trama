# Abrir projeto REUSANDO o registry da página é o que permite trocar de projeto
# sem recarregar: as coleções (e portanto o JS do navegador) não mudam, só o
# store, o flows_dir e a raiz.

# Mesmo truque de `fake_registry()` em test-app.R: a entrada vai direto em
# `registry$collections` porque é essa a forma que a validação lê, e o campo que
# importa é `package` — o manifesto guarda nome de PACOTE, não id de coleção.
reg_com <- function(...) {
  reg <- tr_registry()
  for (p in c(...)) reg$collections[[p]] <- list(id = p, label = p, version = "1",
                                                 js = NULL, css = NULL, package = p)
  reg
}

escrever_manifesto <- function(root, cols) {
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  writeLines(jsonlite::toJSON(list(collections = cols), auto_unbox = FALSE),
             file.path(root, "trama.json"))
  root
}

test_that("abre projeto cujo manifesto cabe nas coleções carregadas", {
  raiz <- escrever_manifesto(file.path(tempfile(), "p"), c("trama.data"))
  p <- tr_project_at(raiz, reg_com("trama.data", "trama.view"))

  expect_s3_class(p, "tr_project")
  expect_equal(p$collections, "trama.data")
  expect_true(dir.exists(file.path(raiz, "flows")))
  expect_true(dir.exists(file.path(raiz, ".trama", "store")))
})

test_that("o registry é o MESMO objeto, não uma cópia recarregada", {
  raiz <- escrever_manifesto(file.path(tempfile(), "p"), character())
  reg <- reg_com("trama.data")
  p <- tr_project_at(raiz, reg)
  # Identidade de environment: se um dia isto virar cópia, as coleções da página
  # e as do projeto podem divergir sem ninguém notar.
  expect_true(identical(p$registry, reg))
})

test_that("manifesto pedindo coleção ausente é recusado, nomeando-a", {
  raiz <- escrever_manifesto(file.path(tempfile(), "p"), c("trama.data", "trama.terrain"))
  err <- tryCatch(tr_project_at(raiz, reg_com("trama.data")), error = identity)

  expect_equal(class(err)[[1]], "tr_error_missing_collection")
  expect_match(conditionMessage(err), "trama.terrain", fixed = TRUE)
  # E NÃO nomeia a que está presente: a mensagem tem que apontar o que falta.
  expect_false(grepl("trama.data", conditionMessage(err), fixed = TRUE))
})

test_that("pasta inexistente é recusada antes de qualquer leitura", {
  err <- tryCatch(tr_project_at(file.path(tempfile(), "nao-existe"), reg_com()),
                  error = identity)
  expect_equal(class(err)[[1]], "tr_error_bad_root")
})

test_that("caminho que existe mas é arquivo é recusado como raiz", {
  arq <- tempfile(); writeLines("{}", arq)
  err <- tryCatch(tr_project_at(arq, reg_com()), error = identity)
  expect_equal(class(err)[[1]], "tr_error_bad_root")
})

test_that("registry que não é um tr_registry é recusado", {
  raiz <- escrever_manifesto(file.path(tempfile(), "p"), character())
  expect_error(tr_project_at(raiz, NULL), class = "tr_error_bad_root")
})

# A tolerância é deliberada: `tr_project()` também abre pasta sem manifesto, e
# os dois construtores discordarem sobre o que conta como projeto seria pior que
# aceitar a pasta vazia. Quem precisa de manifesto filtra antes — o botão
# "Abrir" do diálogo só habilita quando a pasta tem um.
test_that("pasta sem trama.json abre como projeto sem coleção nenhuma", {
  raiz <- file.path(tempfile(), "solta"); dir.create(raiz, recursive = TRUE)
  p <- tr_project_at(raiz, reg_com("trama.data"))
  expect_equal(p$collections, character())
})

# `tr_project()` LÊ o trama.json e nunca o escreve — por isso criar projeto é
# função à parte, e não um argumento a mais. Ver o comentário dela.
test_that("cria as pastas e grava o manifesto", {
  raiz <- file.path(tempfile(), "novo")
  tr_project_new(raiz, c("trama.data", "trama.view"))

  expect_true(dir.exists(file.path(raiz, "flows")))
  expect_true(dir.exists(file.path(raiz, ".trama", "store")))
  cfg <- jsonlite::fromJSON(file.path(raiz, "trama.json"), simplifyVector = FALSE)
  expect_equal(unlist(cfg$collections), c("trama.data", "trama.view"))
})

test_that("o projeto criado abre no registry que o criou", {
  raiz <- file.path(tempfile(), "novo")
  tr_project_new(raiz, "trama.data")
  expect_s3_class(tr_project_at(raiz, reg_com("trama.data")), "tr_project")
})

# Criar por cima do projeto de alguém é o tipo de coisa que não se faz calado: o
# manifesto seria reescrito com outras coleções e o fluxo do dono ficaria órfão.
test_that("recusa onde já existe um trama.json", {
  raiz <- file.path(tempfile(), "ocupado")
  tr_project_new(raiz, "trama.data")
  err <- tryCatch(tr_project_new(raiz, "trama.view"), error = identity)

  expect_equal(class(err)[[1]], "tr_error_project_exists")
  # E o manifesto de quem já estava lá continua intacto.
  cfg <- jsonlite::fromJSON(file.path(raiz, "trama.json"), simplifyVector = FALSE)
  expect_equal(unlist(cfg$collections), "trama.data")
})

# `tr_project_flow()` já devolve documento vazio quando o arquivo não existe, e
# o autosave grava na primeira op: um main.json nascido junto seria só um flow
# vazio versionado antes de alguém desenhar qualquer coisa.
test_that("não cria flows/main.json", {
  raiz <- file.path(tempfile(), "novo")
  tr_project_new(raiz, character())
  expect_false(file.exists(file.path(raiz, "flows", "main.json")))
})

# Pasta pai só-leitura é o caso do usuário que digita um caminho no diálogo: sem
# classificação isto voltava como "não é possível abrir a conexão", sem caminho.
test_that("pasta sem permissão de escrita aborta classificado, citando o caminho", {
  skip_on_os("windows")
  skip_if(identical(Sys.info()[["user"]], "root"), "root escreve mesmo sem permissão")
  pai <- file.path(tempfile(), "travado"); dir.create(pai, recursive = TRUE)
  on.exit(Sys.chmod(pai, "0755"), add = TRUE)
  Sys.chmod(pai, "0555")

  alvo <- file.path(pai, "novo")
  err <- tryCatch(tr_project_new(alvo, character()), error = identity)

  expect_equal(class(err)[[1]], "tr_error_project_write")
  expect_match(conditionMessage(err), alvo, fixed = TRUE)
})

test_that("sem coleção nenhuma ainda grava um manifesto válido", {
  raiz <- file.path(tempfile(), "vazio")
  tr_project_new(raiz, character())
  cfg <- jsonlite::fromJSON(file.path(raiz, "trama.json"), simplifyVector = FALSE)
  expect_equal(length(cfg$collections), 0L)
})

# ---- Nome do projeto novo, digitado no diálogo --------------------------

test_that("nome válido vira caminho dentro da base, sem espaço nas bordas", {
  base <- tempfile(); dir.create(base)
  expect_equal(.tr_project_path(base, "meu-projeto"), file.path(base, "meu-projeto"))
  expect_equal(.tr_project_path(base, "  meu projeto  "), file.path(base, "meu projeto"))
})

# O caso `""` é o mais provável e o pior: clicar "Criar" sem digitar fazia a
# pasta que o usuário estava navegando (Documentos, home) virar projeto.
test_that("nome que não nomeia, ou que sai da base, é recusado", {
  base <- tempfile(); dir.create(base)
  for (nome in list("", "   ", ".", "..", "a/b", "../escapou", "a\\b",
                    NULL, NA_character_, c("a", "b"), 1)) {
    expect_error(.tr_project_path(base, nome), class = "tr_error_bad_name")
  }
  # E recusar é não escrever: a base continua vazia.
  expect_equal(list.files(base, all.files = TRUE, no.. = TRUE), character())
})

test_that("base que não é pasta é a mesma recusa de tr_project_at()", {
  err <- tryCatch(.tr_project_path(file.path(tempfile(), "nada"), "p"), error = identity)
  expect_equal(class(err)[[1]], "tr_error_bad_root")
})

# A tolerância de `tr_project_at()` (acima) continua valendo: o que exige
# manifesto é o gesto de ABRIR, não o construtor.
test_that("abrir exige manifesto, e recusar não deixa andaime na pasta", {
  solta <- file.path(tempfile(), "fotos"); dir.create(solta, recursive = TRUE)
  err <- tryCatch(.tr_check_project(solta), error = identity)

  expect_equal(class(err)[[1]], "tr_error_not_project")
  expect_match(conditionMessage(err), "trama.json", fixed = TRUE)
  expect_false(dir.exists(file.path(solta, ".trama")))
  expect_false(dir.exists(file.path(solta, "flows")))

  raiz <- file.path(tempfile(), "p"); tr_project_new(raiz, character())
  expect_equal(.tr_check_project(raiz), raiz)
})

# ---- Listagem para o navegador de pastas --------------------------------

test_that("lista só diretórios, marcando os que são projeto", {
  base <- tempfile(); dir.create(base)
  dir.create(file.path(base, "proj")); tr_project_new(file.path(base, "proj"), "trama.data")
  dir.create(file.path(base, "comum"))
  writeLines("x", file.path(base, "arquivo.txt"))

  l <- .tr_dir_listing(base)
  nomes <- vapply(l$entries, function(e) e$nome, "")

  expect_setequal(nomes, c("comum", "proj"))
  ehproj <- vapply(l$entries, function(e) e$projeto, logical(1))
  expect_true(ehproj[nomes == "proj"])
  expect_false(ehproj[nomes == "comum"])
})

# `.trama` é o store, `.git` é o repositório: nenhum dos dois é lugar de projeto,
# e listá-los só põe ruído entre o usuário e a pasta que ele procura.
test_that("pasta oculta não aparece", {
  base <- tempfile(); dir.create(base)
  dir.create(file.path(base, ".oculta")); dir.create(file.path(base, "visivel"))

  nomes <- vapply(.tr_dir_listing(base)$entries, function(e) e$nome, "")
  expect_equal(nomes, "visivel")
})

test_that("a pasta atual diz se ela mesma é projeto", {
  raiz <- file.path(tempfile(), "p"); tr_project_new(raiz, character())
  expect_true(.tr_dir_listing(raiz)$project)
  expect_false(.tr_dir_listing(dirname(raiz))$project)
})

# `dirname("/")` devolve "/" — sem esta checagem o diálogo desenharia um ".."
# que não vai a lugar nenhum, e clicar nele recarregaria a mesma pasta.
test_that("na raiz do sistema de arquivos não há para onde subir", {
  expect_null(.tr_dir_listing("/")$parent)
  expect_equal(.tr_dir_listing(tempdir())$parent, dirname(normalizePath(tempdir())))
})

test_that("pasta inexistente é a mesma recusa de tr_project_at()", {
  err <- tryCatch(.tr_dir_listing(file.path(tempfile(), "nada")), error = identity)
  expect_equal(class(err)[[1]], "tr_error_bad_root")
})
