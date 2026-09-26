test_that("sem temas no manifesto valem os embutidos, padrão escuro", {
  s <- .tr_settings(list())
  expect_setequal(names(s$temas), c("escuro", "claro", "clássico"))
  expect_equal(s$tema_padrao, "escuro")
})

test_that("tema do projeto é completado com os campos padrão", {
  s <- .tr_settings(list(temas = list(rel = list(fundo = "#ffffff")), tema_padrao = "rel"))
  expect_equal(s$temas$rel$fundo, "#ffffff")
  expect_equal(s$temas$rel$base, "minimal")
  expect_equal(s$tema_padrao, "rel")
})

test_that("campo inválido é erro classificado que nomeia tema e campo", {
  err <- expect_error(.tr_settings(list(temas = list(rel = list(fundo = "azul")))),
                      class = "tr_error_bad_theme")
  expect_match(conditionMessage(err), "rel")
  expect_match(conditionMessage(err), "fundo")
  expect_error(.tr_settings(list(temas = list(rel = list(base = "xkcd")))), class = "tr_error_bad_theme")
  expect_error(.tr_settings(list(temas = list(rel = list(paleta = list())))), class = "tr_error_bad_theme")
  expect_error(.tr_settings(list(temas = list(rel = list(paleta = character())))), class = "tr_error_bad_theme")
  expect_error(.tr_settings(list(temas = list(rel = list(tamanho = -1)))), class = "tr_error_bad_theme")
  expect_error(.tr_settings(list(temas = list(rel = list(fonte = "comic")))), class = "tr_error_bad_theme")
  expect_error(.tr_settings(list(temas = list(rel = list(continua = "arco-iris")))), class = "tr_error_bad_theme")
  expect_error(.tr_settings(list(temas = list(rel = "escuro"))), class = "tr_error_bad_theme")
})

test_that("campo desconhecido é erro que nomeia o campo (typo não passa calado)", {
  err <- expect_error(.tr_settings(list(temas = list(rel = list(fudno = "#ffffff")))),
                      class = "tr_error_bad_theme")
  expect_match(conditionMessage(err), "fudno")
  expect_match(conditionMessage(err), "rel")
})

test_that("paleta vinda do JSON como lista vira vetor de caracteres", {
  s <- .tr_settings(list(temas = list(rel = list(paleta = list("#111111", "#222222")))))
  expect_identical(s$temas$rel$paleta, c("#111111", "#222222"))
  s <- .tr_settings(jsonlite::fromJSON('{"temas": {"rel": {"paleta": ["#111111"], "tamanho": 12}}}',
                                       simplifyVector = FALSE))
  expect_identical(s$temas$rel$paleta, "#111111")
  expect_equal(s$temas$rel$tamanho, 12)
})

test_that("tamanho inteiro é aceito", {
  s <- .tr_settings(list(temas = list(rel = list(tamanho = 11L))))
  expect_equal(s$temas$rel$tamanho, 11)
})

test_that("'padrão' é nome reservado", {
  expect_error(.tr_settings(list(temas = list(`padrão` = list()))), class = "tr_error_bad_theme")
})

test_that("padrão que não existe é erro", {
  expect_error(.tr_settings(list(temas = list(rel = list()), tema_padrao = "outro")),
               class = "tr_error_bad_theme")
})

test_that("marca vale TRUE quando o manifesto não fala dela", {
  expect_true(.tr_settings(list())$marca)
})

test_that("marca aceita FALSE e recusa o que não é booleano", {
  expect_false(.tr_settings(list(marca = FALSE))$marca)
  expect_error(.tr_settings(list(marca = "sim")), class = "tr_error_bad_theme")
  expect_error(.tr_settings(list(marca = list(TRUE, FALSE))), class = "tr_error_bad_theme")
})

test_that("resolver: padrão, nome, inexistente", {
  s <- .tr_settings(list())
  expect_equal(.tr_theme_resolve("padrão", s)$nome, "escuro")
  expect_equal(.tr_theme_resolve("claro", s)$nome, "claro")
  r <- .tr_theme_resolve("sumiu", s)
  expect_equal(r$nome, "escuro")
  expect_true(isTRUE(r$ausente))
  expect_equal(.tr_theme_resolve("claro", NULL)$nome, "claro")
})

test_that("resolver é determinístico e só marca ausente quando falta", {
  s <- .tr_settings(list(temas = list(rel = list(fundo = "#ffffff"))))
  a <- .tr_theme_resolve("rel", s)
  expect_identical(a, .tr_theme_resolve("rel", s))
  expect_identical(a, .tr_theme_resolve("rel", .tr_settings(list(temas = list(rel = list(fundo = "#ffffff"))))))
  expect_false("ausente" %in% names(a))
  expect_false("ausente" %in% names(.tr_theme_resolve("padrão", s)))
})

# --- tr_param_theme(): o plano resolve o tema antes do hash ---------------

theme_collection <- function() tr_collection(id = "t", version = "1.0.0",
    types = list(tr_type("t/box", version = 1L, store = function(x, path) saveRDS(x, path),
                         restore = function(path) readRDS(path), ext = "rds")),
    nodes = list(tr_node("t/tema", fn = function(tema) list(v = tema$fundo),
                         description = "Devolve o fundo do tema resolvido.",
                         outputs = list(out = "t/box"), params = list(tema = tr_param_theme()))))
theme_registry <- function() { r <- tr_registry(); tr_use(theme_collection(), registry = r); r }
theme_doc <- function(reg, valor = NULL) {
  op <- list(op = "add_node", type = "t/tema", id = "g")
  if (!is.null(valor)) op$params <- list(tema = valor)
  build(reg, list(op))
}

test_that("tr_param_theme() declara kind theme, default padrão e rótulo Tema", {
  p <- tr_param_theme()
  expect_equal(p$kind, "theme")
  expect_equal(p$default, "padrão")
  expect_equal(p$label, "Tema")
})

test_that("valor de tema é um nome não vazio; existência não é checada", {
  p <- tr_param_theme()
  expect_equal(.tr_check_param_value(p, "claro", "tema"), "claro")
  expect_equal(.tr_check_param_value(p, "nao-existe", "tema"), "nao-existe")
  expect_error(.tr_check_param_value(p, 1, "tema"), class = "tr_error_bad_param_value")
  expect_error(.tr_check_param_value(p, "", "tema"), class = "tr_error_bad_param_value")
})

test_that("editar o tema USADO muda a chave; editar outro tema não", {
  reg <- theme_registry(); doc <- theme_doc(reg, "rel")
  base <- list(rel = list(fundo = "#ffffff"), outro = list(fundo = "#000000"))
  k <- function(temas) tr_plan(doc, registry = reg, settings = .tr_settings(list(temas = temas)))$keys$g
  k0 <- k(base)
  b1 <- base; b1$rel$fundo <- "#eeeeee"
  b2 <- base; b2$outro$fundo <- "#111111"
  expect_false(identical(k0, k(b1)))
  expect_identical(k0, k(b2))
})

test_that("nó em padrão segue tema_padrao; nó fixo em claro não", {
  reg <- theme_registry()
  s_esc <- .tr_settings(list(tema_padrao = "escuro"))
  s_cla <- .tr_settings(list(tema_padrao = "clássico"))
  dp <- theme_doc(reg); dc <- theme_doc(reg, "claro")
  expect_false(identical(tr_plan(dp, registry = reg, settings = s_esc)$keys$g,
                         tr_plan(dp, registry = reg, settings = s_cla)$keys$g))
  expect_identical(tr_plan(dc, registry = reg, settings = s_esc)$keys$g,
                   tr_plan(dc, registry = reg, settings = s_cla)$keys$g)
})

test_that("settings NULL equivale aos embutidos", {
  reg <- theme_registry(); doc <- theme_doc(reg)
  expect_identical(tr_plan(doc, registry = reg, settings = NULL)$keys$g,
                   tr_plan(doc, registry = reg, settings = .tr_settings(list()))$keys$g)
})

test_that("a unidade carrega a definição do tema, não o nome", {
  reg <- theme_registry(); doc <- theme_doc(reg, "claro")
  u <- tr_plan(doc, registry = reg)$units$g
  expect_true(is.list(u$params$tema))
  expect_equal(u$params$tema$fundo, "#ffffff")
  expect_equal(u$params$tema$nome, "claro")
})

manifesto_temas <- function(root, cfg) {
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  writeLines(jsonlite::toJSON(cfg, auto_unbox = TRUE, pretty = TRUE), file.path(root, "trama.json"))
  root
}

test_that("tr_project() e tr_project_at() expõem os temas do manifesto", {
  root <- manifesto_temas(tempfile("proj"), list(temas = list(rel = list(fundo = "#ffffff")),
                                                 tema_padrao = "rel"))
  p <- tr_project(root)
  expect_equal(names(p$settings$temas), "rel")
  expect_equal(p$settings$tema_padrao, "rel")
  expect_equal(tr_project_at(root, tr_registry())$settings$temas$rel$fundo, "#ffffff")

  sem <- tr_project(manifesto_temas(tempfile("proj"), list(collections = list())))
  expect_setequal(names(sem$settings$temas), c("escuro", "claro", "clássico"))
})

test_that("tr_project_at() recusa tema inválido antes de criar pastas", {
  root <- manifesto_temas(tempfile("proj"), list(temas = list(rel = list(fundo = "azul"))))
  expect_error(tr_project_at(root, tr_registry()), class = "tr_error_bad_theme")
  expect_false(dir.exists(file.path(root, "flows")))
})

test_that("tr_project_gc preserva o cache de um flow que usa tema do projeto", {
  root <- manifesto_temas(tempfile("proj"), list(temas = list(rel = list(fundo = "#ffffff")),
                                                 tema_padrao = "rel"))
  project <- tr_project(root)
  tr_use(theme_collection(), registry = project$registry)
  reg <- project$registry
  doc <- theme_doc(reg)
  tr_doc_write(doc, file.path(root, "flows", "main.json"))
  tr_run(doc, registry = reg, store = project$store, settings = project$settings)
  key <- tr_plan(doc, registry = reg, store = project$store, settings = project$settings)$units$g$outputs$out
  expect_true(tr_store_has(project$store, key))
  h <- tr_store_handle(project$store, key); h$created <- 0
  jsonlite::write_json(h, file.path(project$store$root, "handles", paste0(key, ".json")),
                       auto_unbox = TRUE, null = "null", digits = NA)
  tr_project_gc(project, max_age_days = 0)
  expect_true(tr_store_has(project$store, key))
})

test_that("tr_project_set_themes grava temas e preserva o resto do manifesto", {
  root <- withr::local_tempdir("proj")
  writeLines('{"collections": ["trama.data"], "executor": {"workers": 2}}', file.path(root, "trama.json"))
  tr_project_set_themes(root, list(rel = list(fundo = "#ffffff", paleta = list("#123456"), tamanho = 11L)), "rel")
  txt <- readLines(file.path(root, "trama.json"), encoding = "UTF-8")
  expect_true(any(grepl('"collections": \\[', txt)))
  expect_true(any(grepl('"paleta": \\[', txt)))
  cfg <- jsonlite::fromJSON(file.path(root, "trama.json"), simplifyVector = FALSE)
  expect_equal(cfg$executor$workers, 2)
  expect_equal(cfg$tema_padrao, "rel")
})

test_that("tr_project_set_themes round-trip via tr_project_at, com acento e paleta de uma cor", {
  root <- withr::local_tempdir("proj")
  tr_project_new(root)
  tr_project_set_themes(root, list(`clássico` = list(paleta = "#abcdef", tamanho = 15.5),
                                   escuro = list()), "clássico")
  expect_true(any(grepl('"collections": \\[\\]', readLines(file.path(root, "trama.json")))))
  p <- tr_project_at(root, tr_registry())
  expect_equal(p$settings$tema_padrao, "clássico")
  expect_identical(p$settings$temas$`clássico`$paleta, "#abcdef")
  expect_identical(p$settings$temas$`clássico`$tamanho, 15.5)
  expect_setequal(names(p$settings$temas), c("clássico", "escuro"))
  expect_length(list.files(root, pattern = "\\.part$", all.files = TRUE), 0L)
})

test_that("tr_project_set_themes recusa tema inválido sem tocar no arquivo", {
  root <- withr::local_tempdir("proj")
  tr_project_new(root)
  f <- file.path(root, "trama.json")
  antes <- readBin(f, "raw", file.size(f)); mt <- file.mtime(f)
  expect_error(tr_project_set_themes(root, list(rel = list(fundo = "azul")), "rel"),
               class = "tr_error_bad_theme")
  expect_error(tr_project_set_themes(root, list(rel = list()), "sumiu"), class = "tr_error_bad_theme")
  expect_identical(readBin(f, "raw", file.size(f)), antes)
  expect_equal(file.mtime(f), mt)
})

test_that("tr_project_set_themes exige padrao e não escolhe um calado", {
  root <- withr::local_tempdir("proj")
  tr_project_new(root)
  f <- file.path(root, "trama.json")
  antes <- readBin(f, "raw", file.size(f))
  err <- expect_error(tr_project_set_themes(root, list(rel = list()), NULL), class = "tr_error_bad_theme")
  expect_match(conditionMessage(err), "tema_padrao")
  expect_error(tr_project_set_themes(root, list(rel = list()), ""), class = "tr_error_bad_theme")
  expect_error(tr_project_set_themes(root, list(rel = list()), NA_character_), class = "tr_error_bad_theme")
  expect_identical(readBin(f, "raw", file.size(f)), antes)
})

test_that("tr_project_set_marca grava sem perder o resto do manifesto", {
  root <- withr::local_tempdir()
  tr_project_new(root, collections = "trama.data")
  tr_project_set_themes(root, list(), "claro")
  devolvido <- tr_project_set_marca(root, mostrar = FALSE)
  cfg <- jsonlite::fromJSON(file.path(root, "trama.json"), simplifyVector = FALSE)
  expect_false(cfg$marca)
  expect_equal(unlist(cfg$collections), "trama.data")
  expect_equal(cfg$tema_padrao, "claro")
  # Pelo caminho da leitura (o mesmo que `tr_project_at()` usa), e não só pelo
  # JSON cru: o que importa é o projeto reaberto trazer a marca desligada.
  # `tr_project_at()` não serve aqui porque pediria a coleção instalada.
  expect_false(.tr_settings(cfg)$marca)
  expect_false(devolvido$marca)
  expect_equal(devolvido$tema_padrao, "claro")
})

test_that("tr_project_set_marca recusa valor que não é booleano", {
  root <- withr::local_tempdir(); tr_project_new(root)
  err <- expect_error(tr_project_set_marca(root, mostrar = "sim"), class = "tr_error_bad_theme")
  # A recusa nomeia o argumento: do console, "marca precisa ser true ou false."
  # solto não diz o que foi recusado nem onde mexer.
  expect_match(conditionMessage(err), "'mostrar'", fixed = TRUE)
})

# O simétrico do teste dos temas: são dois verbos sobre o MESMO arquivo, e cada
# um só pode mexer na chave que é dele. Gravar temas depois de desligar a marca
# é o gesto comum (o painel salva os dois), e religar a marca calado seria o
# jeito mais fácil de desfazer a escolha de quem já a tinha desligado.
test_that("tr_project_set_themes preserva a marca já gravada", {
  root <- withr::local_tempdir()
  tr_project_new(root)
  tr_project_set_marca(root, mostrar = FALSE)
  tr_project_set_themes(root, list(), "claro")
  cfg <- jsonlite::fromJSON(file.path(root, "trama.json"), simplifyVector = FALSE)
  expect_false(cfg$marca)
  # E pelo caminho da leitura, que é o que o projeto reaberto enxerga.
  expect_false(.tr_settings(cfg)$marca)
  expect_equal(cfg$tema_padrao, "claro")
  # O retorno do verbo também fala do disco: montado só a partir dos temas
  # recebidos, ele diria "marca ligada" num projeto que a tem desligada.
  expect_false(tr_project_set_themes(root, list(), "claro")$marca)
})

test_that("mensagens de tema apontam pro trama.json", {
  err <- expect_error(.tr_settings(list(temas = list(rel = list(fundo = "azul")))), class = "tr_error_bad_theme")
  expect_match(conditionMessage(err), "Tema 'rel' em trama.json: campo 'fundo'", fixed = TRUE)
  err <- expect_error(.tr_settings(list(temas = list(rel = list()), tema_padrao = "foo")), class = "tr_error_bad_theme")
  expect_match(conditionMessage(err), "tema_padrao 'foo' não está entre os temas de trama.json.", fixed = TRUE)
})

test_that("tr_project_set_themes em pasta sem manifesto é erro classificado", {
  root <- withr::local_tempdir("proj")
  expect_error(tr_project_set_themes(root, list(rel = list()), "rel"), class = "tr_error_not_project")
  expect_false(file.exists(file.path(root, "trama.json")))
})

test_that("settings no formato do front: paleta de uma cor sai como array", {
  s <- .tr_settings(list(temas = list(rel = list(paleta = "#123456"))))
  j <- jsonlite::toJSON(.tr_settings_json(s), auto_unbox = TRUE)
  expect_match(j, '"paleta":\\["#123456"\\]')
  expect_match(j, '"temas":\\{"rel"')
  expect_match(j, '"tema_padrao":"rel"')
})

test_that("tr_value() passa settings adiante: mesma chave que a sessão", {
  reg <- tr_registry()
  tr_use(theme_collection(), registry = reg)
  store <- tr_store(withr::local_tempdir("st"))
  doc <- theme_doc(reg)
  s <- .tr_settings(list(temas = list(rel = list(fundo = "#ffffff"))))
  tr_value(doc, "g", registry = reg, store = store, settings = s)
  key <- tr_plan(doc, registry = reg, store = store, settings = s)$units$g$outputs$out
  expect_true(tr_store_has(store, key))
})

test_that("tr_theme resolve nome e padrão com os embutidos", {
  expect_equal(tr_theme()$nome, "escuro")
  cl <- tr_theme("claro")
  expect_equal(cl$fundo, "#ffffff")
  expect_null(cl$ausente)
  r <- tr_theme("sumiu")
  expect_equal(r$nome, "escuro")
  expect_true(isTRUE(r$ausente))
  expect_setequal(tr_theme_names(), c("escuro", "claro", "clássico"))
})

test_that("tr_theme aceita a lista resolvida, valida e mantém nome e ausente", {
  r <- .tr_theme_resolve("sumiu", NULL)
  expect_identical(tr_theme(r), r)
  cl <- tr_theme("claro")
  expect_identical(tr_theme(cl), cl)
  expect_equal(tr_theme(list(fundo = "#123456"))$fundo, "#123456")
  expect_error(tr_theme(list(nome = "x", fudno = "#123456")), class = "tr_error_bad_theme")
})

test_that("tr_theme usa os settings do projeto quando dados", {
  s <- .tr_settings(list(temas = list(rel = list(fundo = "#ffffff")), tema_padrao = "rel"))
  expect_equal(tr_theme("padrão", s)$nome, "rel")
  expect_equal(tr_theme_names(s), "rel")
})

test_that("sugestoes vale TRUE ausente e recusa o que não é booleano", {
  expect_true(.tr_settings(list())$sugestoes)
  expect_false(.tr_settings(list(sugestoes = FALSE))$sugestoes)
  err <- expect_error(.tr_settings(list(sugestoes = "sim")), class = "tr_error_bad_theme")
  expect_match(conditionMessage(err), "sugestoes", fixed = TRUE)
  expect_error(.tr_settings(list(sugestoes = NA)), class = "tr_error_bad_theme")
  expect_true(.tr_settings_json(.tr_settings(list()))$sugestoes)
})

test_that("tr_project_set_sugestoes grava, relê e preserva o resto", {
  root <- withr::local_tempdir()
  tr_project_new(root, collections = "trama.data")
  tr_project_set_marca(root, mostrar = FALSE)
  devolvido <- tr_project_set_sugestoes(root, ligar = FALSE)
  expect_false(devolvido$sugestoes); expect_false(devolvido$marca)
  cfg <- jsonlite::fromJSON(file.path(root, "trama.json"), simplifyVector = FALSE)
  expect_false(cfg$sugestoes)
  expect_equal(unlist(cfg$collections), "trama.data")
  expect_true(tr_project_set_sugestoes(root, ligar = TRUE)$sugestoes)
  err <- expect_error(tr_project_set_sugestoes(root, ligar = "sim"), class = "tr_error_bad_theme")
  expect_match(conditionMessage(err), "'ligar'", fixed = TRUE)
})

test_that("gravar temas e marca preserva as sugestões", {
  root <- withr::local_tempdir(); tr_project_new(root)
  tr_project_set_sugestoes(root, ligar = FALSE)
  tr_project_set_themes(root, list(), "claro")
  expect_false(tr_project_set_marca(root, mostrar = TRUE)$sugestoes)
})
