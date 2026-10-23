# A galeria de modelos: embutidos, criar, editar, duplicar, remover,
# importar e exportar, e modelo inválido que não quebra nada.

dir_usuario <- function(...) file.path(tools::R_user_dir("trama", "data"), "modelos", ...)

modelo_minimo <- function(pasta, css = TRUE) {
  dir.create(pasta, recursive = TRUE)
  writeLines(c("title: Mínimo", "contributes:", "  formats:", "    html:", if (css) "      css: x.css"),
             file.path(pasta, "_extension.yml"))
  if (css) writeLines("/* x */", file.path(pasta, "x.css"))
  writeLines(c("---", "title: Mínimo", "---", "", "Texto."), file.path(pasta, "template.qmd"))
  pasta
}

test_that("os embutidos estão na galeria, válidos, com os formatos de cada um", {
  local_usuario()
  m <- tr_documento_modelos()
  esperado <- c(basico = "html, typst, docx", ufla = "typst, docx, html", consultoria = "typst, html, docx",
                artigo = "typst, html, docx", slides = "revealjs", painel = "dashboard", pagina = "html")
  expect_setequal(m$nome, names(esperado))
  expect_identical(stats::setNames(m$formatos, m$nome)[names(esperado)], esperado)
  expect_true(all(m$valido))
  expect_true(all(is.na(m$problema)))
  expect_true(all(m$origem == "embutido"))
  expect_false(any(m$sobre_embutido))
  expect_true(all(nzchar(m$descricao)))
  expect_identical(stats::setNames(m$perfil, m$nome)[c("ufla", "consultoria", "basico")],
                   c(ufla = "aluno-ufla", consultoria = "yucca", basico = NA))
  expect_identical(m$titulo[m$nome == "ufla"], "UFLA (ABNT)")
})

test_that("o detalhe de um modelo traz cores, fontes, logo, capa e CSL", {
  local_usuario()
  c1 <- tr_documento_modelo("consultoria")
  expect_identical(c1$cores$primaria, "#003366")
  expect_identical(c1$cores$secundaria, "#e6eff6")
  expect_identical(c1$fontes$texto, "Inter")
  expect_true(file.exists(c1$logo))
  expect_true(c1$capa)
  expect_true(c1$valido)
  u <- tr_documento_modelo("ufla")
  expect_identical(u$csl, "abnt.csl")
  expect_null(u$logo)
  expect_identical(u$perfil, "aluno-ufla")
  expect_null(tr_documento_modelo("basico")$capa)
  expect_error(tr_documento_modelo("nao-ha"), class = "tr_documento_erro_modelo")
})

test_that("duplicar copia para o usuário; nomes ruins e repetidos são recusados", {
  local_usuario()
  d <- tr_documento_modelo_duplicar("ufla", "meu-tcc")
  expect_identical(d$titulo, "UFLA (ABNT) (cópia)")
  expect_identical(d$origem, "usuario")
  expect_false(d$sobre_embutido)
  expect_true(file.exists(dir_usuario("meu-tcc", "abnt.csl")))
  expect_identical(tr_documento_modelo_duplicar("ufla", "outro", titulo = "Outro")$titulo, "Outro")
  expect_error(tr_documento_modelo_duplicar("ufla", "meu-tcc"), class = "tr_documento_erro_modelo")
  for (ruim in c("Meu", "a b", "-x", "dashboard", "html")) {
    expect_error(tr_documento_modelo_duplicar("basico", ruim), class = "tr_documento_erro_modelo")
  }
  expect_true("meu-tcc" %in% tr_documento_modelos()$nome)
})

test_that("editar um embutido cria a cópia do usuário; remover volta ao embutido", {
  local_usuario()
  e <- tr_documento_modelo_editar("basico", titulo = "Básico da casa")
  expect_identical(e$origem, "usuario")
  expect_true(e$sobre_embutido)
  m <- tr_documento_modelos()
  expect_identical(m$titulo[m$nome == "basico"], "Básico da casa")
  expect_identical(sum(m$nome == "basico"), 1L)
  expect_identical(tr_documento_modelo_remover("basico"), "basico")
  expect_identical(tr_documento_modelo("basico")$titulo, "Básico")
  expect_error(tr_documento_modelo_remover("basico"), "embutido", class = "tr_documento_erro_modelo")
  expect_error(tr_documento_modelo_remover("nao-ha"), class = "tr_documento_erro_modelo")
})

test_that("editar: título, descrição, cores, fontes, logo, capa, CSL e perfil", {
  local_usuario()
  tr_documento_modelo_duplicar("basico", "meu")
  logo <- withr::local_tempfile(fileext = ".svg")
  writeLines('<svg xmlns="http://www.w3.org/2000/svg" width="4" height="4"/>', logo)
  csl <- system.file("modelos/ufla/abnt.csl", package = "trama.documentos")
  e <- tr_documento_modelo_editar("meu", titulo = "Meu", descricao = "Para a disciplina.",
                                  cores = c(primaria = "#7B2D26", fundo = "#FFFFFF"), fontes = list(texto = "Lora"),
                                  logo = logo, capa = TRUE, csl = csl, perfil = "aluno")
  expect_identical(e$titulo, "Meu")
  expect_identical(e$descricao, "Para a disciplina.")
  expect_identical(e$cores$primaria, "#7b2d26")
  expect_identical(e$fontes$texto, "Lora")
  expect_identical(basename(e$logo), "logo.svg")
  expect_true(file.exists(e$logo))
  expect_true(e$capa)
  expect_identical(e$csl, "abnt.csl")
  expect_true(file.exists(dir_usuario("meu", "abnt.csl")))
  expect_identical(e$perfil, "aluno")
  expect_true(e$valido)
  # YAML que o Quarto lê: `true`, não o `yes` do pacote yaml.
  ext <- linhas(dir_usuario("meu", "_extension.yml"))
  expect_true(any(grepl("^\\s+capa: true$", ext)))
  expect_true(yaml::read_yaml(dir_usuario("meu", "_extension.yml"))$contributes$formats$typst$citeproc)
  expect_false(any(grepl("\\byes\\b", ext)))
  # Tirar: logo, CSL e perfil.
  e2 <- tr_documento_modelo_editar("meu", logo = FALSE, csl = FALSE, perfil = FALSE, descricao = "")
  expect_null(e2$logo)
  expect_false(file.exists(dir_usuario("meu", "logo.svg")))
  expect_null(e2$csl)
  expect_null(e2$perfil)
  expect_true(is.na(e2$descricao))
  expect_identical(e2$cores$primaria, "#7b2d26")
})

test_that("desligar um formato guarda a configuração; religar a traz de volta", {
  local_usuario()
  e <- tr_documento_modelo_editar("ufla", formatos = c("typst", "html"))
  expect_identical(e$formatos, c("typst", "html"))
  expect_identical(e$formatos_desligados, "docx")
  e <- tr_documento_modelo_editar("ufla", formatos = c("docx", "typst", "html", "revealjs"))
  expect_identical(e$formatos, c("docx", "typst", "html", "revealjs"))
  expect_length(e$formatos_desligados, 0L)
  ext <- yaml::read_yaml(dir_usuario("ufla", "_extension.yml"))
  expect_identical(ext$contributes$formats$docx$`reference-doc`, "referencia.docx")
  expect_true(ext$contributes$formats$revealjs$`slide-number`)
  expect_identical(names(ext$contributes$formats)[[1]], "common")
})

test_that("edição recusada não escreve nada (nem cria a cópia do embutido)", {
  local_usuario()
  expect_error(tr_documento_modelo_editar("basico", cores = c(primaria = "azul")), "#rrggbb", class = "tr_documento_erro_modelo")
  expect_error(tr_documento_modelo_editar("basico", cores = c(roxo = "#000000")), class = "tr_documento_erro_modelo")
  expect_error(tr_documento_modelo_editar("basico", formatos = c("html", "pdf")), "pdf", class = "tr_documento_erro_modelo")
  expect_error(tr_documento_modelo_editar("basico", formatos = character()), class = "tr_documento_erro_modelo")
  expect_error(tr_documento_modelo_editar("basico", logo = "nao-existe.png"), class = "tr_documento_erro_modelo")
  ruim <- withr::local_tempfile(fileext = ".csl")
  writeLines("não é CSL", ruim)
  expect_error(tr_documento_modelo_editar("basico", csl = ruim), "CSL", class = "tr_documento_erro_modelo")
  expect_error(tr_documento_modelo_editar("basico", capa = "sim"), class = "tr_documento_erro_modelo")
  expect_false(dir.exists(dir_usuario("basico")))
})

test_that("criar parte de um modelo e aplica as edições", {
  local_usuario()
  m <- tr_documento_modelo_criar("relatorio-lab", titulo = "Relatório do laboratório", base = "consultoria",
                                 formatos = c("typst", "html"), cores = c(primaria = "#2f6b3a"))
  expect_identical(m$titulo, "Relatório do laboratório")
  expect_identical(m$formatos, c("typst", "html"))
  expect_identical(m$cores$primaria, "#2f6b3a")
  expect_true(file.exists(dir_usuario("relatorio-lab", "typst-template.typ")))
  expect_identical(tr_documento_modelo_criar("simples")$formatos, c("html", "typst", "docx"))
})

test_that("exportar e importar: ida e volta, com nome novo e substituindo", {
  local_usuario()
  dir <- withr::local_tempdir()
  z <- tr_documento_modelo_exportar("ufla", file.path(dir, "ufla"))
  expect_identical(basename(z), "ufla.zip")
  expect_true(all(c("ufla/_extension.yml", "ufla/abnt.csl", "ufla/template.qmd") %in% utils::unzip(z, list = TRUE)$Name))
  m <- tr_documento_modelo_importar(z, nome = "ufla-2")
  expect_identical(m$origem, "usuario")
  expect_identical(m$titulo, "UFLA (ABNT)")
  expect_true(m$valido)
  expect_identical(tools::md5sum(file.path(m$caminho, "referencia.docx"))[[1]],
                   tools::md5sum(system.file("modelos/ufla/referencia.docx", package = "trama.documentos"))[[1]])
  expect_error(tr_documento_modelo_importar(z, nome = "ufla-2"), "substituir", class = "tr_documento_erro_modelo")
  tr_documento_modelo_editar("ufla-2", titulo = "Mudado")
  expect_identical(tr_documento_modelo_importar(z, nome = "ufla-2", substituir = TRUE)$titulo, "UFLA (ABNT)")
  # Sem nome: o da pasta do zip (aqui, a cópia do usuário por cima do embutido).
  expect_true(tr_documento_modelo_importar(z)$sobre_embutido)
})

test_that("importar aceita a extensão Quarto publicada (_extensions/<nome>/ e template.qmd na raiz)", {
  local_usuario()
  base <- withr::local_tempdir()
  modelo_minimo(file.path(base, "_extensions", "Revista X"))
  file.remove(file.path(base, "_extensions", "Revista X", "template.qmd"))
  writeLines(c("---", "title: Artigo da revista", "---", "", "# Introdução"), file.path(base, "template.qmd"))
  writeLines("leia", file.path(base, "README.md"))
  z <- file.path(withr::local_tempdir(), "revista.zip")
  zip::zip(z, c("_extensions", "template.qmd", "README.md"), root = base)
  m <- tr_documento_modelo_importar(z)
  expect_identical(m$nome, "revista-x")
  expect_true(m$valido)
  expect_identical(linhas(file.path(m$caminho, "template.qmd"))[[2]], "title: Artigo da revista")
  # O zip que o GitHub baixa: tudo dentro de `<repo>-main/`, com organização.
  gh <- withr::local_tempdir()
  modelo_minimo(file.path(gh, "revista-main", "_extensions", "org", "jbes"))
  file.remove(file.path(gh, "revista-main", "_extensions", "org", "jbes", "template.qmd"))
  writeLines(c("---", "title: JBES", "---"), file.path(gh, "revista-main", "template.qmd"))
  zg <- file.path(withr::local_tempdir(), "revista-main.zip")
  zip::zip(zg, "revista-main", root = gh)
  mg <- tr_documento_modelo_importar(zg)
  expect_identical(mg$nome, "jbes")
  expect_identical(linhas(file.path(mg$caminho, "template.qmd"))[[2]], "title: JBES")
  # Zip sem esqueleto ganha um mínimo.
  base2 <- withr::local_tempdir()
  modelo_minimo(file.path(base2, "seco"))
  file.remove(file.path(base2, "seco", "template.qmd"))
  z2 <- file.path(withr::local_tempdir(), "seco.zip")
  zip::zip(z2, "seco", root = base2)
  expect_true(file.exists(file.path(tr_documento_modelo_importar(z2)$caminho, "template.qmd")))
})

test_that("importar recusa zip que não é modelo, que sai da pasta ou que é inválido", {
  local_usuario()
  tmp <- withr::local_tempdir()
  nao_zip <- file.path(tmp, "x.zip")
  writeLines("texto", nao_zip)
  expect_error(tr_documento_modelo_importar(nao_zip), "zip", class = "tr_documento_erro_modelo")
  writeLines("oi", file.path(tmp, "leia.txt"))
  sem <- file.path(tmp, "sem.zip")
  zip::zip(sem, "leia.txt", root = tmp)
  expect_error(tr_documento_modelo_importar(sem), "_extension.yml", class = "tr_documento_erro_modelo")
  # Arquivo referenciado que falta: não entra, e diz qual.
  ruim <- modelo_minimo(file.path(tmp, "ruim"))
  file.remove(file.path(ruim, "x.css"))
  zr <- file.path(tmp, "ruim.zip")
  zip::zip(zr, "ruim", root = tmp)
  expect_error(tr_documento_modelo_importar(zr), "x.css", class = "tr_documento_erro_modelo")
  expect_false(dir.exists(dir_usuario("ruim")))
  # Caminho que sai da pasta (zip slip).
  fora <- file.path(tmp, "fora")
  modelo_minimo(file.path(fora, "m"))
  zf <- file.path(tmp, "fora.zip")
  zip::zip(zf, "m", root = fora)
  # Troca "m/_ex" por "../ex" nos nomes guardados no zip (mesmo tamanho; o CRC
  # é do conteúdo, não do nome).
  b <- readBin(zf, "raw", file.info(zf)$size)
  alvo <- charToRaw("m/_ex")
  n <- length(alvo)
  achados <- Filter(function(i) identical(b[i:(i + n - 1L)], alvo), seq_len(length(b) - n + 1L))
  expect_length(achados, 2L)  # cabeçalho local e diretório central
  for (i in achados) b[i:(i + n - 1L)] <- charToRaw("../ex")
  zs <- file.path(tmp, "slip.zip")
  writeBin(b, zs)
  expect_error(tr_documento_modelo_importar(zs), "saem da pasta", class = "tr_documento_erro_modelo")
})

test_that("modelo inválido aparece na galeria com o problema, e criar documento com ele dá erro claro", {
  proj <- local_projeto()
  ruim <- modelo_minimo(dir_usuario("quebrado"))
  writeLines(c("title: [sem fechar", "contributes: {"), file.path(ruim, "_extension.yml"))
  semfmt <- modelo_minimo(dir_usuario("sem-formato"), css = FALSE)
  writeLines("title: Sem formato", file.path(semfmt, "_extension.yml"))
  semcss <- modelo_minimo(dir_usuario("sem-css"))
  file.remove(file.path(semcss, "x.css"))
  m <- tr_documento_modelos()
  expect_true(all(c("basico", "ufla", "quebrado", "sem-formato", "sem-css") %in% m$nome))
  p <- stats::setNames(m$problema, m$nome)
  v <- stats::setNames(m$valido, m$nome)
  expect_false(any(v[c("quebrado", "sem-formato", "sem-css")]))
  expect_match(p[["quebrado"]], "_extension.yml não é YAML válido")
  expect_match(p[["sem-formato"]], "não declara formatos")
  expect_match(p[["sem-css"]], "'x.css'")
  expect_true(v[["ufla"]])
  expect_error(tr_documento_criar("doc", proj, modelo = "sem-css", perfil = FALSE),
               "x.css", class = "tr_documento_erro_modelo")
  expect_false(dir.exists(file.path(proj$root, "documentos", "doc")))
  expect_false(tr_documento_modelo("quebrado")$valido)
})

test_that("o documento novo usa o perfil que o modelo sugere e preenche os campos do perfil", {
  proj <- local_projeto()
  tr_documento_perfil_salvar("outro", "Outra Pessoa")
  tr_documento_perfil_salvar("aluno-ufla", "Ana Souza", instituicao = "UFLA", curso = "Estatística",
                             orientador = "Prof. Beltrano")
  qmd <- tr_documento_criar("tcc", proj, modelo = "ufla")
  y <- trama.documentos:::.td_yaml(linhas(qmd))
  expect_identical(y$trama$perfil, "aluno-ufla")
  expect_identical(y$author[[1]]$name, "Ana Souza")
  expect_identical(y$orientador, "Prof. Beltrano")
  expect_identical(y$instituicao, c("Universidade Federal de Lavras", "Estatística"))
  expect_identical(y$ano, format(Sys.Date(), "%Y"))
  expect_identical(names(y$format), c("ufla-typst", "ufla-docx", "ufla-html"))
  # O que é do trama e não do Quarto não vai para a extensão do documento.
  pasta <- dirname(qmd)
  expect_true(file.exists(file.path(pasta, "_extensions/ufla/typst-template.typ")))
  expect_false(file.exists(file.path(pasta, "_extensions/ufla/trama-modelo.yml")))
  expect_false(file.exists(file.path(pasta, "_extensions/ufla/template.qmd")))
  # Perfil sem os campos: os itens somem, sem deixar "{{...}}".
  qmd2 <- tr_documento_criar("tcc2", proj, modelo = "ufla", perfil = "outro")
  y2 <- trama.documentos:::.td_yaml(linhas(qmd2))
  expect_null(y2$orientador)
  expect_identical(y2$instituicao, "Universidade Federal de Lavras")
  expect_false(any(grepl("{{", linhas(qmd2), fixed = TRUE)))
  # Perfil sugerido que não existe: vale o padrão.
  tr_documento_perfil_remover("aluno-ufla")
  y3 <- trama.documentos:::.td_yaml(linhas(tr_documento_criar("tcc3", proj, modelo = "ufla")))
  expect_identical(y3$trama$perfil, "outro")
})

test_that("o _brand.yml do documento aponta o logo para a cópia da extensão", {
  proj <- local_projeto()
  qmd <- tr_documento_criar("rel", proj, modelo = "consultoria", perfil = FALSE)
  pasta <- dirname(qmd)
  b <- yaml::read_yaml(file.path(pasta, "_brand.yml"))
  expect_identical(b$logo$medium, "_extensions/consultoria/logo.svg")
  expect_true(file.exists(file.path(pasta, b$logo$medium)))
  expect_identical(b$color$primary, "azul-yucca")
})

test_that("os formatos do documento, nos nomes do render", {
  proj <- local_projeto()
  expect_identical(tr_documento_formatos(tr_documento_criar("a", proj, modelo = "ufla", perfil = FALSE)),
                   c("html", "pdf", "docx"))
  expect_identical(tr_documento_formatos(tr_documento_criar("b", proj, modelo = "slides", perfil = FALSE)), "revealjs")
  pa <- tr_documento_criar("c", proj, modelo = "painel", perfil = FALSE)
  expect_identical(tr_documento_formatos(pa), "dashboard")
  expect_identical(trama.documentos:::.td_formato_quarto(linhas(pa), "dashboard"), "painel-dashboard")
})

# ---- achados da revisão independente ---------------------------------------------------

test_that("editar não transforma lista de um item em escalar (filters: [abnt.lua])", {
  local_usuario()
  tr_documento_modelo_editar("ufla", titulo = "UFLA editado")
  ext <- yaml::read_yaml(dir_usuario("ufla", "_extension.yml"), handlers = list(seq = function(x) as.list(x)))
  expect_true(is.list(ext$contributes$formats$common$filters))
  expect_true(is.list(ext$contributes$formats$typst$`template-partials`))
  expect_true(any(grepl("^\\s+- abnt.lua$", linhas(dir_usuario("ufla", "_extension.yml")))))
  tr_documento_modelo_editar("painel", titulo = "Painel editado")
  expect_true(any(grepl("^\\s+- cartoes.lua$", linhas(dir_usuario("painel", "_extension.yml")))))
  # E o esqueleto: lista do template continua lista no documento.
  proj <- local_projeto()
  m <- modelo_minimo(dir_usuario("lista"), css = FALSE)
  writeLines(c("---", "title: X", "filters:", "  - x.lua", "---"), file.path(m, "template.qmd"))
  qmd <- tr_documento_criar("doc", proj, modelo = "lista", perfil = FALSE)
  expect_true("  - x.lua" %in% linhas(qmd))
})

test_that("zip com link simbólico não traz o arquivo de fora", {
  local_usuario()
  skip_on_os("windows")
  skip_if(!nzchar(Sys.which("zip")), "sem o programa zip")
  base <- withr::local_tempdir()
  segredo <- file.path(withr::local_tempdir(), "segredo.txt")
  writeLines("CHAVE-SECRETA", segredo)
  m <- modelo_minimo(file.path(base, "mal"))
  file.symlink(segredo, file.path(m, "logo.png"))
  z <- file.path(withr::local_tempdir(), "mal.zip")
  withr::with_dir(base, system2("zip", c("-q", "-r", "--symlinks", z, "mal")))
  r <- tryCatch(tr_documento_modelo_importar(z), error = function(e) e)
  dentro <- dir_usuario("mal")
  if (!inherits(r, "error")) {
    expect_identical(Sys.readlink(file.path(dentro, "logo.png")), "")
    expect_false(any(grepl("CHAVE-SECRETA", readLines(file.path(dentro, "logo.png"), warn = FALSE))))
  } else {
    expect_false(dir.exists(dentro))
  }
})

test_that("_brand.yml fora do formato: o documento segue, a galeria mostra o problema", {
  proj <- local_projeto()
  salvar_fluxo(proj, no("padrao", "tst/tema"))
  qmd <- tr_documento_criar("rel", proj, modelo = "consultoria", perfil = FALSE)
  writeLines(c("color: \"#003366\"", "typography: Inter"), file.path(dirname(qmd), "_brand.yml"))
  expect_silent(tr_documento_inserir(qmd, "padrao", proj))
  expect_identical(unname(estado_de(qmd, proj)), "atualizada")
  m <- modelo_minimo(dir_usuario("marca-torta"), css = FALSE)
  writeLines(c("color: \"#003366\"", "logo: [1, 2]"), file.path(m, "_brand.yml"))
  g <- tr_documento_modelos()
  expect_false(g$valido[g$nome == "marca-torta"])
  expect_match(g$problema[g$nome == "marca-torta"], "color")
  expect_true(all(g$valido[g$nome != "marca-torta"]))
})

test_that("logo em images com {path, alt} e extensões publicadas comuns são válidos", {
  local_usuario()
  m <- modelo_minimo(dir_usuario("publicada"), css = FALSE)
  writeLines("x", file.path(m, "x.lua"))
  writeLines("<svg/>", file.path(m, "marca.svg"))
  writeLines(c("title: Publicada", "contributes:", "  formats:", "    html: default",
               "    pdf:", "      filters:", "        - quarto", "        - path: x.lua", "          at: pre",
               "      include-in-header:", "        text: '\\usepackage{x}'"), file.path(m, "_extension.yml"))
  writeLines(c("logo:", "  images:", "    principal:", "      path: marca.svg", "      alt: Marca", "  medium: principal"),
             file.path(m, "_brand.yml"))
  d <- tr_documento_modelo("publicada")
  expect_true(d$valido)
  expect_identical(basename(d$logo), "marca.svg")
  # Formato que a galeria não configura (pdf) fica e pode seguir ligado.
  e <- tr_documento_modelo_editar("publicada", formatos = c("pdf", "typst"))
  expect_identical(e$formatos, c("pdf", "typst"))
  expect_error(tr_documento_modelo_editar("publicada", formatos = "beamer"), "beamer", class = "tr_documento_erro_modelo")
})

test_that("CSL: citeproc ao ligar o PDF depois; tirar o CSL único; só .csl", {
  local_usuario()
  tr_documento_modelo_criar("so-html", formatos = "html")
  csl <- system.file("modelos/ufla/abnt.csl", package = "trama.documentos")
  tr_documento_modelo_editar("so-html", csl = csl)
  tr_documento_modelo_editar("so-html", formatos = c("html", "typst"))
  ext <- yaml::read_yaml(dir_usuario("so-html", "_extension.yml"))
  expect_true(ext$contributes$formats$typst$citeproc)
  m <- modelo_minimo(dir_usuario("so-csl"), css = FALSE)
  file.copy(csl, m)
  writeLines(c("title: X", "contributes:", "  formats:", "    common:", "      csl: abnt.csl", "    html: {}"),
             file.path(m, "_extension.yml"))
  expect_null(tr_documento_modelo_editar("so-csl", csl = FALSE)$csl)
  outro <- withr::local_tempfile(fileext = ".xml")
  file.copy(csl, outro, overwrite = TRUE)
  expect_error(tr_documento_modelo_editar("so-csl", csl = outro), ".csl", class = "tr_documento_erro_modelo")
  # Desligar formato sem configuração não grava `[]`.
  tr_documento_modelo_editar("so-csl", formatos = "typst")
  tr_documento_modelo_editar("so-csl", formatos = c("typst", "html"))
  expect_false(any(grepl("\\[\\]", linhas(dir_usuario("so-csl", "_extension.yml")))))
})

test_that("cor da paleta muda na paleta; logo copiado sobre ele mesmo; nome que já existe tem classe própria", {
  local_usuario()
  e <- tr_documento_modelo_editar("consultoria", cores = c(primaria = "#2f6b3a"))
  b <- yaml::read_yaml(dir_usuario("consultoria", "_brand.yml"))
  expect_identical(b$color$primary, "azul-yucca")
  expect_identical(b$color$palette$`azul-yucca`, "#2f6b3a")
  expect_identical(e$cores$primaria, "#2f6b3a")
  expect_silent(tr_documento_modelo_editar("consultoria", logo = e$logo))
  expect_true(file.size(e$logo) > 0)
  z <- tr_documento_modelo_exportar("consultoria", file.path(withr::local_tempdir(), "c.zip"))
  expect_error(tr_documento_modelo_importar(z), class = "tr_documento_erro_modelo_existe")
  expect_error(tr_documento_modelo_duplicar("basico", "consultoria"), class = "tr_documento_erro_modelo_existe")
})
