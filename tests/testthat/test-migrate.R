# Migração de params por versão do nó. A coleção daqui é de brinquedo e só
# existe pra renomear params: `m/ic` nasceu com `nivel` (v1), virou `conf`
# (v2) e `confianca` (v3). O núcleo não sabe o que é um intervalo.

migra_collection <- function(migracoes = list(
  `2` = function(params) { params$conf <- params$nivel; params$nivel <- NULL; params },
  `3` = function(params) { params$confianca <- params$conf; params$conf <- NULL; params }
)) {
  tr_collection(
    id = "m", version = "1.0.0", label = "Migra",
    types = list(tr_type("m/num", label = "Número")),
    nodes = list(
      tr_node("m/ic", fn = function(confianca) confianca * 100, version = 3L,
              description = "Devolve o nível em porcentagem.",
              outputs = list(out = "m/num"),
              params = list(confianca = tr_param_num(0.95)),
              migracoes = migracoes),
      tr_node("m/fixo", fn = function(valor) valor, version = 2L,
              description = "Nó que mudou de versão sem migração.",
              outputs = list(out = "m/num"),
              params = list(valor = tr_param_num(1)))
    )
  )
}

migra_registry <- function(...) {
  reg <- tr_registry(); tr_use(migra_collection(...), registry = reg); reg
}

doc_velho <- function(versao = 1L, params = '{"nivel":0.9}', tipo = "m/ic") {
  tr_doc_parse(sprintf(
    '{"format":1,"rev":1,"nodes":{"a":{"type":"%s","type_version":%d,"params":%s,"seed":1}},"edges":[]}',
    tipo, versao, params))
}

test_that("migração em cadeia v1 -> v3 renomeia o param e sobe a versão", {
  reg <- migra_registry()
  doc <- tr_doc_migrate(doc_velho(1L), reg)
  expect_equal(doc$nodes$a$params, list(confianca = 0.9))
  expect_identical(doc$nodes$a$type_version, 3L)
  log <- attr(doc, "migracoes")
  expect_length(log, 1)
  expect_equal(log[[1]][c("node", "type", "from", "to")],
               list(node = "a", type = "m/ic", from = 1L, to = 3L))
  expect_match(.tr_migracoes_texto(log), "migrado de v1 para v3")
})

test_that("migração começa da versão do documento, não da primeira", {
  reg <- migra_registry()
  doc <- tr_doc_migrate(doc_velho(2L, '{"conf":0.8}'), reg)
  expect_equal(doc$nodes$a$params, list(confianca = 0.8))
})

test_that("migrar é idempotente: documento atual passa intocado e sem log", {
  reg <- migra_registry()
  uma <- tr_doc_migrate(doc_velho(1L), reg)
  duas <- tr_doc_migrate(uma, reg)
  expect_equal(duas$nodes, uma$nodes)
  expect_length(attr(duas, "migracoes"), 0)
})

test_that("documento velho valida limpo e roda com o param novo", {
  reg <- migra_registry(); s <- tmp_store()
  doc <- doc_velho(1L)
  expect_length(tr_doc_validate(doc, reg), 0)
  expect_equal(tr_value(doc, "a", reg, s), 90)
  # A chave é a mesma de um documento criado já na versão nova: migrar não
  # parte o cache em dois.
  novo <- tr_doc_apply(tr_doc(), list(op = "add_node", type = "m/ic", id = "a", seed = 1L,
                                      params = list(confianca = 0.9)), reg)
  expect_identical(tr_plan(novo, registry = reg)$keys$a, tr_plan(doc, registry = reg)$keys$a)
})

test_that("sem caminho de migração, a deriva continua reportada como antes", {
  reg <- migra_registry()
  doc <- doc_velho(1L, '{"valor":2}', tipo = "m/fixo")
  kinds <- vapply(tr_doc_validate(doc, reg), function(p) p$kind, "")
  expect_true("version_drift" %in% kinds)
  expect_identical(tr_doc_migrate(doc, reg)$nodes$a$type_version, 1L)
})

test_that("migração que não devolve lista nomeada vira erro com o id do nó", {
  reg <- migra_registry(migracoes = list(`2` = function(params) 42))
  err <- expect_error(tr_doc_migrate(doc_velho(1L), reg), class = "tr_error_migration")
  expect_match(conditionMessage(err), "'a'")
})

test_that("migração que falha vira erro classificado com o id do nó", {
  reg <- migra_registry(migracoes = list(`3` = function(params) stop("quebrou")))
  err <- expect_error(tr_doc_migrate(doc_velho(1L), reg), class = "tr_error_migration")
  expect_match(conditionMessage(err), "'a'")
  expect_match(conditionMessage(err), "quebrou")
})

test_that("tr_node recusa migração mal declarada", {
  f <- function(x) x
  expect_error(tr_node("m/x", fn = function() 1, version = 2L, description = "x",
                       migracoes = list(f)), class = "tr_error_bad_migration")
  expect_error(tr_node("m/x", fn = function() 1, version = 2L, description = "x",
                       migracoes = list(`5` = f)), class = "tr_error_bad_migration")
  expect_error(tr_node("m/x", fn = function() 1, version = 2L, description = "x",
                       migracoes = list(`2` = 1)), class = "tr_error_bad_migration")
})

test_that("add_node com type_version velho migra os params antes de checar", {
  reg <- migra_registry()
  doc <- tr_doc_apply(tr_doc(), list(op = "add_node", type = "m/ic", id = "a",
                                     type_version = 1L, params = list(nivel = 0.5)), reg)
  expect_equal(doc$nodes$a$params, list(confianca = 0.5))
  expect_identical(doc$nodes$a$type_version, 3L)
  # Sem type_version, o param velho continua recusado.
  expect_error(tr_doc_apply(tr_doc(), list(op = "add_node", type = "m/ic",
                                           params = list(nivel = 0.5)), reg),
               class = "tr_error_unknown_param")
})

test_that("template com params velhos entra migrado", {
  reg <- migra_registry()
  tpl <- tr_template_parse(paste0(
    '{"trama":"template","versao":1,"nome":"x","doc":{"format":1,',
    '"nodes":{"a":{"type":"m/ic","type_version":1,"params":{"nivel":0.7}}},"edges":[]}}'))
  doc <- tr_doc_apply(tr_doc(), tr_template_op(tpl), reg)
  expect_equal(doc$nodes[[1]]$params, list(confianca = 0.7))
})

test_that("abrir o flow do projeto migra e avisa", {
  reg <- migra_registry()
  proj <- list(flows_dir = withr::local_tempdir(), registry = reg)
  tr_doc_write(doc_velho(1L), file.path(proj$flows_dir, "main.json"))
  expect_message(doc <- tr_project_flow(proj), "migrado de v1 para v3")
  expect_equal(doc$nodes$a$params, list(confianca = 0.9))
})

# Os dois mecanismos juntos (integração da coesão com a main): primeiro os
# renomes declarados pela coleção, depois as funções por versão do nó.
migra_registry_colecao <- function(migrations) {
  col <- migra_collection()
  col$migrations <- trama:::.tr_check_migrations(migrations, "m")
  reg <- tr_registry(); tr_use(col, registry = reg); reg
}

test_that("renome de param da coleção roda antes da migração por versão", {
  reg <- migra_registry_colecao(list(params = list("m/ic" = list(alfa = list(
    to = "nivel", value = function(v) 1 - v)))))
  doc <- tr_doc_migrate(doc_velho(1L, '{"alfa":0.1}'), reg)
  expect_equal(doc$nodes$a$params, list(confianca = 0.9))
  expect_identical(doc$nodes$a$type_version, 3L)
  expect_true(isTRUE(attr(doc, "migrated")))
  expect_length(attr(doc, "migracoes"), 1)
  expect_length(tr_doc_validate(doc, reg), 0)
})

test_that("nó renomeado pela coleção não roda as funções de versão do destino", {
  reg <- migra_registry_colecao(list(nodes = list("m/velho" = "m/ic"),
                                     params = list("m/ic" = list(nivel = list(to = "confianca")))))
  # type_version 1 é a do nó antigo: aplicar as migrações v2/v3 de m/ic seria errado
  doc <- tr_doc_migrate(doc_velho(1L, '{"nivel":0.8}', tipo = "m/velho"), reg)
  expect_identical(doc$nodes$a$type, "m/ic")
  expect_equal(doc$nodes$a$params, list(confianca = 0.8))
  expect_identical(doc$nodes$a$type_version, 3L)
  expect_true(isTRUE(attr(doc, "migrated")))
  expect_identical(tr_doc_migrate(doc, reg)$nodes, doc$nodes)
  expect_length(tr_doc_validate(doc_velho(1L, '{"nivel":0.8}', tipo = "m/velho"), reg), 0)
})

test_that("param renomeado na migração não deixa marca de sugerido velha", {
  reg <- migra_registry_colecao(list(params = list("m/ic" = list(alfa = list(
    to = "nivel", value = function(v) 1 - v)))))
  d <- doc_velho(1L, '{"alfa":0.1}')
  d$nodes$a$sugeridos <- "alfa"
  doc <- tr_doc_migrate(d, reg)
  expect_null(doc$nodes$a$sugeridos)
})
