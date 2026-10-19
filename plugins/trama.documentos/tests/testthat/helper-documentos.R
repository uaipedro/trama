# Projeto de teste com uma coleção falsa (só o núcleo), perfis e modelos do
# usuário numa pasta temporária, e atalhos para ler o documento.

colecao_falsa <- function() {
  tab <- trama::tr_type("tst/tabela", label = "Tabela")
  txt <- trama::tr_type("tst/texto", label = "Texto",
                        report = function(x) paste0("Resultado: **", x$valor, "**."))
  mod <- trama::tr_type("tst/modelo", label = "Modelo")
  trama::tr_collection("tst", types = list(tab, txt, mod), nodes = list(
    trama::tr_node("tst/tabela", fn = function(n) data.frame(x = seq_len(n), y = seq_len(n) * 1.5),
                   label = "Tabela de teste", description = "Tabela com n linhas.",
                   outputs = list(out = "tst/tabela"), params = list(n = trama::tr_param_int(3L))),
    trama::tr_node("tst/texto", fn = function(valor) structure(list(valor = valor), class = "tst_texto"),
                   label = "Texto", description = "Um valor com report.",
                   outputs = list(out = "tst/texto"), params = list(valor = trama::tr_param_text("a"))),
    trama::tr_node("tst/duas", fn = function(n) list(a = data.frame(a = seq_len(n)), b = data.frame(b = seq_len(n) * 2)),
                   label = "Duas", description = "Duas saídas.",
                   outputs = list(a = "tst/tabela", b = "tst/tabela"), params = list(n = trama::tr_param_int(2L))),
    trama::tr_node("tst/falha", fn = function(n) stop("quebrou de propósito"),
                   label = "Falha", description = "Sempre falha.",
                   outputs = list(out = "tst/tabela"), params = list(n = trama::tr_param_int(1L))),
    trama::tr_node("tst/quebrado", fn = function(n) structure(list(n = n), class = "tst_quebrado"),
                   label = "Quebrado", description = "Corpo com cerca solta.",
                   outputs = list(out = "tst/modelo"), params = list(n = trama::tr_param_int(1L))),
    trama::tr_node("tst/modelo", fn = function(n) structure(list(n = n), class = "tst_modelo"),
                   label = "Modelo com treino", description = "Carrega dados de treino.",
                   outputs = list(out = "tst/modelo"), params = list(n = trama::tr_param_int(1L))),
    # O tema que o plano resolveu para o nó, como tabela (testa o tema do documento sem ggplot).
    trama::tr_node("tst/tema", fn = function(tema) data.frame(nome = tema$nome, fundo = tema$fundo, texto = tema$texto,
                                                              cor1 = tema$paleta[[1]], cor2 = tema$paleta[[2]]),
                   label = "Tema", description = "O tema resolvido.",
                   outputs = list(out = "tst/tabela"), params = list(tema = trama::tr_param_theme()))))
}

# Um "modelo" que carrega linhas do treino, como uma SVM.
registerS3method("tr_documento_componente", "tst_modelo", function(x, ctx) {
  tr_documento_parte("modelo", ctx$titulo, arquivos = list(m.md = sprintf("n = %d", x$n)),
                     corpo = sprintf("{{< include %s >}}", ctx$arquivo("m.md")),
                     dados_treino = "Leva as 10 linhas de treino (vetores de suporte).")
}, envir = asNamespace("trama.documentos"))

# Um método de coleção descuidado: o corpo fecha a região antes da hora.
registerS3method("tr_documento_componente", "tst_quebrado", function(x, ctx) {
  tr_documento_parte("texto", ctx$titulo, corpo = c("antes", ":::", "depois"))
}, envir = asNamespace("trama.documentos"))

local_usuario <- function(env = parent.frame()) {
  withr::local_envvar(R_USER_DATA_DIR = withr::local_tempdir(.local_envir = env), .local_envir = env)
}

# Projeto novo com a coleção falsa e o fluxo `nos` (lista de list(id, tipo, params)).
local_projeto <- function(env = parent.frame()) {
  local_usuario(env)
  raiz <- withr::local_tempdir(.local_envir = env)
  proj <- suppressMessages(trama::tr_project(raiz))
  trama::tr_use(colecao_falsa(), registry = proj$registry)
  proj
}

salvar_fluxo <- function(proj, ...) {
  f <- trama::tr_flow(proj$registry)
  for (n in list(...)) f <- do.call(trama::tr_add, c(list(f, n$id, n$tipo), n$params %||% list()))
  trama::tr_project_save(proj, trama::tr_flow_doc(f))
  invisible(proj)
}

no <- function(id, tipo = "tst/tabela", ...) list(id = id, tipo = tipo, params = list(...))

linhas <- function(caminho) readLines(caminho, warn = FALSE, encoding = "UTF-8")
bytes <- function(caminho) readBin(caminho, "raw", file.info(caminho)$size)

# O texto da pessoa: o documento sem as regiões (cada uma vira um marcador).
fora <- function(caminho) {
  l <- trama.documentos:::.td_ler(caminho)$linhas
  trama.documentos:::.td_fora(l, trama.documentos:::.td_regioes(l))
}

regiao <- function(caminho, chave) {
  regs <- trama.documentos:::.td_regioes(trama.documentos:::.td_ler(caminho)$linhas)
  Filter(function(r) r$chave == chave, regs)[[1]]
}

estado_de <- function(caminho, proj) {
  e <- tr_documento_estado(caminho, proj)
  stats::setNames(e$estado, e$regiao)
}

# Troca linhas do documento como a pessoa faria no editor de texto.
editar <- function(caminho, de, para) {
  l <- linhas(caminho)
  i <- which(l == de)
  stopifnot(length(i) == 1L)
  l[i] <- para
  writeLines(l, caminho, useBytes = TRUE)
}

`%||%` <- function(x, y) if (is.null(x)) y else x
