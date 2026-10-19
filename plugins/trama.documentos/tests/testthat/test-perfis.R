# Perfis de autor no nível do usuário (uma pasta temporária nos testes).

test_that("salvar, ler, padrão e remover", {
  local_usuario()
  expect_identical(tr_documento_perfis(), list(padrao = NULL, perfis = list()))
  expect_null(tr_documento_perfil())
  tr_documento_perfil_salvar("ufla", "Ana Lima", email = "ana@ufla.br", orcid = "0000-0002-1825-0097",
                             instituicao = "Universidade Federal de Lavras", curso = "Estatística",
                             orientador = "João Souza")
  tr_documento_perfil_salvar("yucca", "Ana Lima", instituicao = "Yucca Consultoria")
  p <- tr_documento_perfis()
  expect_identical(p$padrao, "ufla")    # o primeiro vira padrão
  expect_setequal(names(p$perfis), c("ufla", "yucca"))
  expect_identical(tr_documento_perfil()$orientador, "João Souza")
  expect_true(file.exists(file.path(tools::R_user_dir("trama", "data"), "perfis.yml")))

  tr_documento_perfil_salvar("yucca", "Ana Lima", instituicao = "Yucca", padrao = TRUE)
  expect_identical(tr_documento_perfil()$perfil, "yucca")
  expect_identical(tr_documento_perfil()$instituicao, "Yucca")
  tr_documento_perfil_remover("yucca")
  expect_identical(tr_documento_perfis()$padrao, "ufla")
  expect_error(tr_documento_perfil("yucca"), class = "tr_documento_erro_perfil")
  expect_error(tr_documento_perfil_remover("yucca"), class = "tr_documento_erro_perfil")
})

test_that("ORCID com dígito verificador, e-mail e nome conferidos", {
  local_usuario()
  # 0000-0002-1825-0097 é o exemplo do próprio ORCID; 0000-0001-5109-3700 também.
  expect_true(trama.documentos:::.td_orcid_valido("0000-0002-1825-0097"))
  expect_true(trama.documentos:::.td_orcid_valido("0000-0001-5109-3700"))
  expect_true(trama.documentos:::.td_orcid_valido("0000-0002-1694-233X"))
  expect_false(trama.documentos:::.td_orcid_valido("0000-0002-1825-0098"))
  expect_false(trama.documentos:::.td_orcid_valido("0000-0002-1825-009"))
  expect_error(tr_documento_perfil_salvar("x", "Ana", orcid = "0000-0002-1825-0098"), class = "tr_documento_erro_perfil")
  expect_error(tr_documento_perfil_salvar("x", "Ana", email = "ana"), class = "tr_documento_erro_perfil")
  expect_error(tr_documento_perfil_salvar("x y", "Ana"), class = "tr_documento_erro_perfil")
  expect_error(tr_documento_perfil_salvar("x", ""), class = "tr_documento_erro_perfil")
  expect_identical(tr_documento_perfis()$perfis, list())
})

test_that("o perfil preenche o autor do documento novo", {
  proj <- local_projeto()
  salvar_fluxo(proj, no("tab"))
  tr_documento_perfil_salvar("ufla", "Ana Lima", email = "ana@ufla.br", orcid = "0000-0002-1825-0097",
                             instituicao = "UFLA", curso = "Estatística")
  qmd <- tr_documento_criar("rel", proj, titulo = "Consumo e peso")
  y <- trama.documentos:::.td_yaml(linhas(qmd))
  expect_identical(y$title, "Consumo e peso")
  expect_identical(y$author[[1]]$name, "Ana Lima")
  expect_identical(y$author[[1]]$orcid, "0000-0002-1825-0097")
  expect_identical(y$author[[1]]$affiliations[[1]], list(name = "UFLA", department = "Estatística"))
  expect_identical(y$trama$perfil, "ufla")
  sem <- tr_documento_criar("sem", proj, perfil = FALSE)
  expect_null(trama.documentos:::.td_yaml(linhas(sem))$author)
  expect_error(tr_documento_criar("outro", proj, perfil = "nao-existe"), class = "tr_documento_erro_perfil")
})
