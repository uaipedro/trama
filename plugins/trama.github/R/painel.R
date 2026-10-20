# Painel GitHub (#15): as seções Projeto (repositório privado do projeto) e
# Publicações (repositórios públicos de cada documento publicado pelo
# `trama.documentos`). As ações chegam
# por `input$trama_github` como as da conta (`R/plugin.R`); aqui ficam as que
# precisam do projeto aberto.

# Ações das seções Projeto e Publicações. `ctx` é o do plugin:
# `ctx$project()$root` e `ctx$recarregar()`.
.gh_painel <- function(enviar, ctx) {
  raiz <- function() ctx$project()$root
  erro <- function(contexto) function(e) {
    enviar(list(tipo = "erro", contexto = contexto, mensagem = conditionMessage(e)))
  }
  projeto <- function() {
    tryCatch(enviar(c(list(tipo = "projeto"), .gh_projeto(raiz()))), error = erro("projeto"))
  }
  list(
    projeto = projeto,
    criar_repo = function(m) tryCatch({
      tr_github_versionar_criar(raiz(), nome = .gh_texto(m$nome))
      r <- tr_github_salvar_versao(raiz(), "Primeira versão")
      enviar(list(tipo = "salvo", mudou = TRUE, url = r$url, sha = r$sha, primeira = TRUE))
      projeto()
    }, error = erro("projeto")),
    salvar = function(m) tryCatch({
      r <- tr_github_salvar_versao(raiz(), .gh_texto(m$mensagem) %||% "")
      enviar(list(tipo = "salvo", mudou = isTRUE(r$mudou), url = r$url, sha = r$sha))
      projeto()
    }, error = erro("projeto")),
    marcar = function(m) tryCatch({
      tr_github_versionar_marcar(raiz(), .gh_texto(m$caminho), incluir = isTRUE(m$incluir))
      enviar(list(tipo = "arquivos", arquivos = .gh_resumo_arquivos(raiz())))
    }, error = erro("projeto")),
    restaurar = function(m) tryCatch({
      r <- tr_github_restaurar(raiz(), .gh_texto(m$sha))
      # O disco mudou por fora: o editor relê o projeto, ou o próximo
      # autosave gravaria por cima da versão restaurada.
      if (is.function(ctx$recarregar)) ctx$recarregar()
      enviar(list(tipo = "restaurado", escritos = length(r$escritos), removidos = length(r$removidos),
                  copia = if (is.na(r$copia)) NULL else .gh_relativo(r$copia, raiz())))
      projeto()
    }, error = erro("projeto")),
    publicacoes = function() {
      tryCatch(enviar(c(list(tipo = "publicacoes"), .gh_publicacoes(raiz()))), error = erro("publicacoes"))
    }
  )
}

.gh_texto <- function(x) if (is.null(x)) NULL else as.character(x)[1]

# `caminho` relativo a `raiz`, com "/". Comparação literal: a raiz como regex
# quebrava no Windows, onde ela vem com barra invertida ("C:\\Users\\...") e
# `r$copia` mistura as duas barras; no macOS a pasta temporária ainda tem dois
# nomes (/var e /private/var), por isso os dois passam por `normalizePath()`.
# Fora da raiz, volta o caminho inteiro.
.gh_relativo <- function(caminho, raiz) {
  barra <- function(x) gsub("\\\\", "/", normalizePath(x, winslash = "/", mustWork = FALSE))
  c <- barra(caminho); r <- sub("/+$", "", barra(raiz))
  if (startsWith(c, paste0(r, "/"))) substring(c, nchar(r) + 2L) else c
}

# Estado da seção Projeto. Arquivos são locais; as versões vêm da API.
.gh_projeto <- function(root) {
  cfg <- tr_github_versionar_config(root)
  base <- list(ligado = !is.null(cfg$repo), nome_sugerido = tr_github_versionar_nome(root),
               arquivos = .gh_resumo_arquivos(root))
  if (is.null(cfg$repo)) return(base)
  v <- tr_github_versoes(root, n = 15)
  c(base, list(repo = cfg$repo, url = cfg$url %||% paste0(tr_github_url(), "/", cfg$repo),
               versoes = lapply(seq_len(nrow(v)), function(i) list(
                 sha = v$sha[i], mensagem = v$mensagem[i], autor = v$autor[i], url = v$url[i],
                 data = if (is.na(v$data[i])) NULL else format(v$data[i], "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")))))
}

.gh_resumo_arquivos <- function(root) {
  a <- tr_github_versionar_arquivos(root)
  p <- a[a$tipo == "projeto", , drop = FALSE]
  d <- a[a$tipo == "dados", , drop = FALSE]
  list(projeto_n = nrow(p), projeto_bytes = sum(p$tamanho),
       dados = lapply(seq_len(nrow(d)), function(i) list(
         caminho = d$caminho[i], tamanho = d$tamanho[i], incluido = d$incluido[i],
         bloqueado = d$bloqueado[i], aviso = if (is.na(d$aviso[i])) NULL else d$aviso[i])))
}

# Seção Publicações: o registro é do trama.documentos, por documento
# (opcional; o trama.github nunca depende dele, é ele que depende deste).
# Versões e a última execução do Actions são lidas da API na hora; falha num
# repositório não esconde os outros.
.gh_publicacoes <- function(root) {
  ler <- .gh_publicacoes_leitor()
  if (is.null(ler)) return(list(disponivel = FALSE, itens = list()))
  df <- ler(root)
  itens <- lapply(seq_len(nrow(df)), function(i) {
    p <- lapply(as.list(df[i, , drop = FALSE]), function(x) if (is.na(x)) NULL else as.character(x))
    extra <- tryCatch({
      tags <- grep("^v[0-9]", tr_github_tags(p$repo), value = TRUE)
      runs <- tr_github_execucoes(p$repo)
      list(versoes = as.list(tags),
           execucao = if (length(runs)) runs[[1]][c("nome", "estado", "conclusao", "url")])
    }, tr_github_erro = function(e) list(erro = conditionMessage(e)))
    c(p, list(nome = basename(dirname(p$documento %||% "")),
              release = if (!is.null(p$tag)) sprintf("%s/releases/tag/%s", p$url, p$tag)), extra)
  })
  list(disponivel = TRUE, itens = itens)
}

.gh_publicacoes_leitor <- function() {
  if (!requireNamespace("trama.documentos", quietly = TRUE)) return(NULL)
  tryCatch(getExportedValue("trama.documentos", "tr_documento_publicacoes"), error = function(e) NULL)
}
