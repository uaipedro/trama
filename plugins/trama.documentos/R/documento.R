# A API do documento vivo: inserir, ver o estado, atualizar, resolver.
#
# Regras de dono (ver `R/regioes.R`): fora das regiões `.trama` nada é
# escrito, a não ser o YAML que o próprio trama cria em `tr_documento_criar()`
# e as linhas de uma região nova que a pessoa pediu para inserir. Região
# editada à mão é conflito e não é sobrescrita; região de nó que saiu do
# fluxo é órfã e fica até a pessoa decidir. A ordem é a do texto.

#' Insere no documento a região de um nó
#'
#' Calcula (ou lê do cache) o valor do nó, materializa em `_trama/<no>/` ao
#' lado do documento e insere a região `::: {#trama-<no> .trama}` que o
#' referencia. Só acrescenta linhas: o texto existente não muda.
#'
#' @param documento Caminho do `.qmd`.
#' @param no Id do nó no fluxo do documento (YAML `trama: fluxo:`, padrão
#'   `"main"`).
#' @param projeto Um `trama::tr_project()` ou a pasta dele; `NULL` acha a
#'   primeira pasta com `trama.json` acima do documento.
#' @param porta Porta de saída; `NULL` é a primeira.
#' @param apos Onde inserir: `NULL` (fim do documento), o número da linha
#'   depois da qual entra (`0`, logo depois do YAML) ou a chave de uma região
#'   existente (entra depois dela). Linha dentro do YAML, de região ou de
#'   bloco de código é recusada.
#' @param opcoes Lista de opções da região, gravadas no cabeçalho como
#'   `trama-<opção>` (a pessoa pode mudá-las ali): `linhas` (tabela),
#'   `tabela = "completa"` (o `dados.csv` leva a tabela inteira, e não só as
#'   linhas mostradas),
#'   `formato` (`"png"` ou `"svg"`), `largura` e `altura` (figura, polegadas).
#' @param aceitar_dados_treino `TRUE` aceita pôr no documento um modelo que
#'   carrega linhas do treino (SVM, floresta de regressão…). O aceite fica
#'   no cabeçalho da região (`trama-dados-treino="aceito"`).
#' @return Invisível: lista com `regiao` (a chave) e `avisos`.
#' @export
tr_documento_inserir <- function(documento, no, projeto = NULL, porta = NULL, apos = NULL,
                                 opcoes = list(), aceitar_dados_treino = FALSE) {
  projeto <- .td_projeto(projeto, documento)
  d <- .td_ler(documento)
  regs <- .td_regioes(d$linhas)
  fluxo <- .td_fluxo_do(d$linhas)
  doc <- trama::tr_project_flow(projeto, fluxo)
  s <- .td_saida(projeto, doc, no, porta)
  if (is.null(s)) {
    .td_erro(sprintf("O n\u{F3} '%s' n\u{E3}o est\u{E1} no fluxo '%s'%s.", no, fluxo,
                     if (is.null(porta)) " ou n\u{E3}o tem sa\u{ED}da" else sprintf(" ou n\u{E3}o tem a sa\u{ED}da '%s'", porta)),
             "tr_documento_erro_no")
  }
  chave <- if (s$padrao) no else paste0(no, "--", s$porta)
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9_.-]*$", chave)) {
    .td_erro(sprintf("O id '%s' n\u{E3}o serve de id de regi\u{E3}o (use letras, d\u{ED}gitos, '-', '_').", chave),
             "tr_documento_erro_no")
  }
  if (chave %in% vapply(regs, `[[`, "", "chave")) {
    .td_erro(sprintf("O documento j\u{E1} tem a regi\u{E3}o '%s'. Use tr_documento_atualizar().", chave),
             "tr_documento_erro_regiao_existe")
  }
  if (length(opcoes) && (is.null(names(opcoes)) || !all(grepl("^[a-z][a-z0-9-]*$", names(opcoes))))) {
    .td_erro("'opcoes' tem de ser lista nomeada (linhas, formato, largura, altura).", "tr_documento_erro_opcoes")
  }
  pares <- c(if (!s$padrao) list("trama-porta" = s$porta),
             if (length(opcoes)) stats::setNames(lapply(opcoes, as.character), paste0("trama-", names(opcoes))))
  if (is.null(pares)) pares <- list()
  pasta_doc <- dirname(normalizePath(documento))
  g <- .td_gerar(projeto, doc, chave, no, s, pares, aceitar_dados_treino)
  if (!is.null(g$erro)) .td_erro(g$erro, g$classe)

  apos <- .td_posicao(d$linhas, regs, apos)
  bloco <- .td_regiao_linhas(chave, g$parte$corpo, g$pares, cr = .td_crlf(d$linhas))
  .td_conferir_bloco(bloco, chave)
  novas <- .td_inserir_linhas(d$linhas, bloco, apos)
  # Dono: tirando as linhas novas, o documento é o mesmo de antes.
  n_novas <- length(novas) - length(d$linhas)
  if (!identical(novas[-(apos + seq_len(n_novas))], d$linhas)) {
    .td_erro("Erro interno: inserir mudaria texto existente. Nada foi gravado.", "tr_documento_erro_interno")
  }
  .td_intacto(documento, d)
  .td_materializar(pasta_doc, chave, g$parte, .td_manifesto_novo(no, s, g, .td_chaves(projeto, doc)))
  d$linhas <- novas
  .td_gravar(documento, d)
  invisible(list(regiao = chave, avisos = g$parte$avisos))
}

#' Estado das regiões do documento
#'
#' Não calcula nada: compara o conteúdo de cada região com o hash que o trama
#' gravou e a chave do store de agora com a da última materialização.
#'
#' - `atualizada`: a região mostra o resultado de agora;
#' - `desatualizada`: o nó, o que ele lê ou as opções mudaram desde então
#'   (atualizar confirma pelo resultado e só reescreve se ele mudou);
#' - `conflito`: o conteúdo foi editado à mão; atualizar não toca nela;
#' - `orfa`: o nó (ou a porta) saiu do fluxo; a região fica até a pessoa decidir;
#' - `indisponivel`: o nó está no fluxo, mas a coleção dele não carregou;
#' - `duplicada`: outra região com a mesma chave veio antes no texto (cópia
#'   à mão); só a primeira é do trama, e esta não é tocada.
#'
#' @inheritParams tr_documento_inserir
#' @return data.frame com `regiao`, `no`, `porta`, `estado` e `linha`, na
#'   ordem do texto.
#' @export
tr_documento_estado <- function(documento, projeto = NULL) {
  projeto <- .td_projeto(projeto, documento)
  d <- .td_ler(documento)
  regs <- .td_regioes(d$linhas)
  doc <- trama::tr_project_flow(projeto, .td_fluxo_do(d$linhas))
  chaves <- .td_chaves(projeto, doc)
  pasta_doc <- dirname(normalizePath(documento))
  dup <- duplicated(vapply(regs, `[[`, "", "chave"))
  .td_quadro(lapply(seq_along(regs), function(i) {
    r <- regs[[i]]
    list(regiao = r$chave, no = r$no, porta = r$porta %||% NA_character_,
         estado = if (dup[[i]]) "duplicada" else .td_estado_regiao(r, projeto, doc, chaves, pasta_doc),
         linha = r$inicio)
  }), c("regiao", "no", "porta", "estado", "linha"))
}

#' Atualiza as regiões cujo resultado mudou
#'
#' Para cada região desatualizada (ou todas, com `forcar`), recalcula o
#' valor, materializa e compara pelo hash do resultado: igual, nada é
#' reescrito; diferente, reescreve a pasta `_trama/<chave>/` e, se o corpo da
#' região mudou, só as linhas dela. Regiões em conflito e órfãs não são
#' tocadas. Antes de gravar, confere que o texto de fora das regiões ficou
#' idêntico, byte a byte.
#'
#' @inheritParams tr_documento_inserir
#' @param regioes Chaves das regiões a atualizar; `NULL`, todas.
#' @param forcar `TRUE` recalcula também as que parecem atualizadas.
#' @return data.frame com `regiao`, `estado` (`atualizada`, `igual`,
#'   `conflito`, `orfa`, `indisponivel`, `erro`) e `mensagem`.
#' @export
tr_documento_atualizar <- function(documento, projeto = NULL, regioes = NULL, forcar = FALSE) {
  projeto <- .td_projeto(projeto, documento)
  d <- .td_ler(documento)
  regs <- .td_regioes(d$linhas)
  doc <- trama::tr_project_flow(projeto, .td_fluxo_do(d$linhas))
  chaves <- .td_chaves(projeto, doc)
  pasta_doc <- dirname(normalizePath(documento))
  cr <- .td_crlf(d$linhas)
  linhas_res <- list()
  trocas <- list()
  res <- function(r, estado, msg = "") {
    linhas_res[[length(linhas_res) + 1L]] <<- list(regiao = r$chave, estado = estado, mensagem = msg)
  }
  pastas <- list()
  vistas <- character()
  for (r in regs) {
    if (!is.null(regioes) && !r$chave %in% regioes) next
    if (r$chave %in% vistas) { res(r, "duplicada", "C\u{F3}pia de uma regi\u{E3}o que j\u{E1} apareceu antes no texto."); next }
    vistas <- c(vistas, r$chave)
    est <- .td_estado_regiao(r, projeto, doc, chaves, pasta_doc)
    if (est == "conflito") { res(r, est, "Editada \u{E0} m\u{E3}o: use tr_documento_resolver()."); next }
    if (est == "orfa") { res(r, est, sprintf("O n\u{F3} '%s' n\u{E3}o est\u{E1} mais no fluxo.", r$no)); next }
    if (est == "indisponivel") {
      res(r, est, sprintf("O bloco de '%s' \u{E9} de uma cole\u{E7}\u{E3}o que n\u{E3}o carregou.", r$no))
      next
    }
    if (est == "atualizada" && !forcar) { res(r, "igual"); next }
    s <- .td_saida(projeto, doc, r$no, r$porta)
    g <- .td_gerar(projeto, doc, r$chave, r$no, s, .td_sem_hash(r$pares), FALSE)
    if (!is.null(g$erro)) { res(r, "erro", g$erro); next }
    nova <- .td_regiao_de(r, g$parte$corpo, g$pares, cr)
    ok <- tryCatch(.td_conferir_bloco(nova, r$chave), tr_documento_erro = function(e) conditionMessage(e))
    if (!isTRUE(ok)) { res(r, "erro", ok); next }
    m <- .td_manifesto(pasta_doc, r$chave)
    mesmo <- identical(m$hash, g$hash)
    pastas[[length(pastas) + 1L]] <- list(chave = r$chave, mesmo = mesmo, parte = g$parte,
                                          manifesto = .td_manifesto_novo(r$no, s, g, chaves))
    corpo_mudou <- !identical(.td_sem_cr(nova), .td_sem_cr(d$linhas[r$inicio:r$fim]))
    if (corpo_mudou) trocas[[length(trocas) + 1L]] <- list(regiao = r, linhas = nova)
    res(r, if (mesmo && !corpo_mudou) "igual" else "atualizada", paste(g$parte$avisos, collapse = " "))
  }
  # Só escreve depois de calcular tudo, e com o arquivo ainda como foi lido.
  if (length(pastas)) .td_intacto(documento, d)
  for (p in pastas) {
    if (p$mesmo) .td_manifesto_atualizar(pasta_doc, p$chave, p$manifesto)
    else .td_materializar(pasta_doc, p$chave, p$parte, p$manifesto)
  }
  if (length(trocas)) {
    novas <- .td_trocar(d$linhas, trocas)
    .td_conferir_fora(d$linhas, novas)
    d$linhas <- novas
    .td_gravar(documento, d)
  }
  .td_quadro(linhas_res, c("regiao", "estado", "mensagem"))
}

#' Resolve uma região em conflito ou órfã
#'
#' - `manter`: a versão que está no texto passa a ser da pessoa: as cercas
#'   da região saem e o conteúdo fica como texto comum, que o trama não toca
#'   mais. A pasta `_trama/<chave>/` é congelada como
#'   `_trama/<chave>--mantida/` (e o texto passa a citá-la), para que um
#'   novo "inserir" do mesmo nó não mude o que a pessoa ficou;
#' - `trazer`: descarta a edição e põe a versão do trama de agora (não vale
#'   para órfã);
#' - `remover`: apaga a região do texto.
#'
#' Região em conflito com cercas `:::` dentro (a pessoa pôs um callout, por
#' exemplo) pode ter o fim diferente do que a pessoa imagina: `trazer` e
#' `remover` apagariam essas linhas, e só seguem com `confirmar = TRUE`.
#'
#' @inheritParams tr_documento_inserir
#' @param regiao Chave da região (`"lm"` para `#trama-lm`).
#' @param acao `"manter"`, `"trazer"` ou `"remover"`.
#' @param confirmar Ver acima.
#' @return Invisível: lista com `regiao` e `removidas`, as linhas que saíram
#'   do texto.
#' @export
tr_documento_resolver <- function(documento, regiao, acao = c("manter", "trazer", "remover"), projeto = NULL,
                                  confirmar = FALSE) {
  acao <- match.arg(acao)
  d <- .td_ler(documento)
  regs <- .td_regioes(d$linhas)
  i <- which(vapply(regs, `[[`, "", "chave") == regiao)
  if (!length(i)) {
    .td_erro(sprintf("O documento n\u{E3}o tem a regi\u{E3}o '%s'.", regiao), "tr_documento_erro_regiao")
  }
  r <- regs[[i[[1]]]]
  pasta_doc <- dirname(normalizePath(documento))
  cercas <- grepl(.TD_DIV_ABRE, .td_sem_cr(r$conteudo), perl = TRUE)
  if (acao != "manter" && .td_editada(r) && any(cercas) && !isTRUE(confirmar)) {
    .td_erro(paste0(sprintf("A regi\u{E3}o '%s' foi editada e tem cercas ':::' dentro; '%s' apagaria as linhas %d a %d:\n",
                            regiao, acao, r$inicio, r$fim),
                    paste(.td_sem_cr(d$linhas[r$inicio:r$fim]), collapse = "\n"),
                    "\nConfira e repita com confirmar = TRUE."), "tr_documento_erro_confirmar")
  }
  congelar <- NULL
  linhas <- switch(acao,
    manter = {
      origem <- .td_pasta_regiao(pasta_doc, r$chave)
      if (dir.exists(origem)) {
        congelar <- .td_nome_livre(pasta_doc, paste0(r$chave, "--mantida"))
        gsub(paste0(.TD_PASTA, "/", r$chave, "/"), paste0(.TD_PASTA, "/", congelar, "/"), r$conteudo, fixed = TRUE)
      } else r$conteudo
    },
    remover = NULL,
    trazer = {
      projeto <- .td_projeto(projeto, documento)
      doc <- trama::tr_project_flow(projeto, .td_fluxo_do(d$linhas))
      s <- .td_saida(projeto, doc, r$no, r$porta)
      if (is.null(s)) {
        .td_erro(sprintf("A regi\u{E3}o '%s' \u{E9} \u{F3}rf\u{E3}: n\u{E3}o h\u{E1} vers\u{E3}o do trama para trazer.", regiao),
                 "tr_documento_erro_regiao")
      }
      g <- .td_gerar(projeto, doc, r$chave, r$no, s, .td_sem_hash(r$pares), FALSE)
      if (!is.null(g$erro)) .td_erro(g$erro, g$classe)
      nova <- .td_regiao_de(r, g$parte$corpo, g$pares, .td_crlf(d$linhas))
      .td_conferir_bloco(nova, r$chave)
      .td_intacto(documento, d)
      .td_materializar(pasta_doc, r$chave, g$parte, .td_manifesto_novo(r$no, s, g, .td_chaves(projeto, doc)))
      nova
    })
  removidas <- d$linhas[r$inicio:r$fim]
  .td_intacto(documento, d)
  if (!is.null(congelar)) file.rename(.td_pasta_regiao(pasta_doc, r$chave), .td_pasta_regiao(pasta_doc, congelar))
  d$linhas <- .td_trocar(d$linhas, list(list(regiao = r, linhas = linhas)))
  .td_gravar(documento, d)
  invisible(list(regiao = regiao, removidas = .td_sem_cr(removidas)))
}

#' Nome de pasta em `_trama/` que ainda não existe (`x`, `x-2`, `x-3`…).
#' @noRd
.td_nome_livre <- function(pasta_doc, base) {
  nome <- base
  k <- 1L
  while (dir.exists(.td_pasta_regiao(pasta_doc, nome))) {
    k <- k + 1L
    nome <- paste0(base, "-", k)
  }
  nome
}

#' Apaga de `_trama/` o que nenhuma região usa
#'
#' Uma pasta fica se há região com a chave dela ou se o texto ainda cita o
#' caminho (região mantida como texto da pessoa), em qualquer `.qmd` da
#' pasta do documento.
#'
#' @inheritParams tr_documento_inserir
#' @return Invisível: as pastas apagadas.
#' @export
tr_documento_limpar <- function(documento) {
  # `_trama/` é da pasta: todo .qmd dela conta, não só este.
  qmds <- unique(c(normalizePath(documento), Sys.glob(file.path(dirname(normalizePath(documento)), "*.qmd"))))
  ls <- lapply(qmds, function(q) .td_ler(q)$linhas)
  usadas <- unlist(lapply(ls, function(l) vapply(.td_regioes(l), `[[`, "", "chave")))
  texto <- paste(unlist(ls), collapse = "\n")
  base <- file.path(dirname(normalizePath(documento)), .TD_PASTA)
  pastas <- if (dir.exists(base)) list.dirs(base, full.names = FALSE, recursive = FALSE) else character()
  fora <- pastas[!pastas %in% usadas & !vapply(pastas, function(p) {
    grepl(paste0(.TD_PASTA, "/", p, "/"), texto, fixed = TRUE)
  }, logical(1))]
  unlink(file.path(base, fora), recursive = TRUE)
  invisible(fora)
}

#' Documentos do projeto
#'
#' @param projeto Pasta do projeto (ou um `trama::tr_project()`).
#' @return data.frame com `nome`, `caminho`, `fluxo`, `modelo` e `regioes`.
#' @export
tr_documento_listar <- function(projeto = ".") {
  raiz <- if (inherits(projeto, "tr_project")) projeto$root else normalizePath(projeto, mustWork = TRUE)
  arqs <- Sys.glob(file.path(raiz, "documentos", "*", "*.qmd"))
  .td_quadro(lapply(arqs, function(a) {
    d <- .td_ler(a)
    y <- .td_yaml(d$linhas)
    regs <- tryCatch(length(.td_regioes(d$linhas)), tr_documento_erro = function(e) NA_integer_)
    list(nome = basename(dirname(a)), caminho = a, fluxo = y$trama$fluxo %||% "main",
         modelo = y$trama$modelo %||% NA_character_, regioes = regs)
  }), c("nome", "caminho", "fluxo", "modelo", "regioes"))
}

# ---- internos ------------------------------------------------------------------------

#' Valor -> parte, com as regras que valem para todo documento: parte com
#' erro não entra; modelo com dados de treino só com aceite.
#' @noRd
.td_gerar <- function(projeto, doc, chave, no, s, pares, aceitar_dados_treino) {
  falha <- function(msg, classe = "tr_documento_erro_parte") list(erro = msg, classe = classe)
  valor <- tryCatch(.td_valor(projeto, doc, no, s$porta), error = function(e) e)
  if (inherits(valor, "error")) {
    return(falha(sprintf("O n\u{F3} '%s' n\u{E3}o produziu valor: %s", no, conditionMessage(valor)),
                 "tr_documento_erro_valor"))
  }
  ctx <- list(no = no, porta = s$porta, chave = chave, titulo = s$titulo, tipo = s$tipo,
              opcoes = .td_opcoes(pares),
              arquivo = function(nome) paste(.TD_PASTA, chave, nome, sep = "/"))
  parte <- tryCatch(tr_documento_componente(valor, ctx),
                    error = function(e) tr_documento_parte("erro", s$titulo, erro = conditionMessage(e)))
  if (!inherits(parte, "tr_documento_parte")) {
    return(falha(sprintf("O m\u{E9}todo de tr_documento_componente() para '%s' n\u{E3}o devolveu tr_documento_parte().",
                         class(valor)[[1]])))
  }
  if (!is.null(parte$erro)) return(falha(sprintf("'%s': %s", no, parte$erro)))
  if (!is.null(parte$dados_treino)) {
    if (!isTRUE(aceitar_dados_treino) && !identical(pares[["trama-dados-treino"]], "aceito")) {
      return(falha(sprintf(paste0(
        "'%s' leva dados de treino: %s Para p\u{F4}r no documento, aceite explicitamente ",
        "(aceitar_dados_treino = TRUE, ou trama-dados-treino=\"aceito\" no cabe\u{E7}alho da regi\u{E3}o)."),
        no, parte$dados_treino), "tr_documento_erro_dados_treino"))
    }
    pares[["trama-dados-treino"]] <- "aceito"
  }
  list(parte = parte, hash = .td_hash_parte(parte), pares = pares)
}

.td_estado_regiao <- function(r, projeto, doc, chaves, pasta_doc) {
  if (.td_editada(r)) return("conflito")
  n <- doc$nodes[[r$no]]
  if (is.null(n)) return("orfa")
  # Nó no fluxo, mas a coleção dele não carregou: não é órfã, só não dá
  # para calcular aqui.
  if (is.null(projeto$registry$nodes[[n$type]])) return("indisponivel")
  s <- .td_saida(projeto, doc, r$no, r$porta)
  if (is.null(s)) return("orfa")
  m <- .td_manifesto(pasta_doc, r$chave)
  agora <- chaves[[r$no]][[s$porta]]
  # O rótulo do card não entra na chave do store, mas entra na legenda.
  if (is.null(m) || is.null(agora) || !identical(m$chave_store, agora) ||
      !identical(m$opcoes, .td_opcoes_txt(.td_sem_hash(r$pares))) || !identical(m$titulo, s$titulo)) {
    return("desatualizada")
  }
  "atualizada"
}

.td_manifesto_novo <- function(no, s, g, chaves) {
  list(no = no, porta = s$porta, tipo = g$parte$tipo, titulo = g$parte$titulo, hash = g$hash,
       chave_store = chaves[[no]][[s$porta]], opcoes = .td_opcoes_txt(g$pares),
       dados_treino = g$parte$dados_treino,
       versao = as.character(utils::packageVersion("trama.documentos")),
       gerado_em = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"))
}

.td_sem_hash <- function(pares) {
  pares[["trama-hash"]] <- NULL
  pares
}

#' Opções da região: os atributos `trama-*` que não são do motor.
#' @noRd
.td_opcoes <- function(pares) {
  k <- setdiff(as.character(names(pares)), c("trama-hash", "trama-porta", "trama-dados-treino"))
  k <- k[startsWith(k, "trama-")]
  stats::setNames(pares[k], sub("^trama-", "", k))
}

#' As opções e o aceite numa linha, para comparar com o manifesto.
#' @noRd
.td_opcoes_txt <- function(pares) {
  pares[["trama-hash"]] <- NULL
  if (!length(pares)) return("")
  paste(sort(sprintf("%s=%s", names(pares), unlist(pares))), collapse = ";")
}

#' Onde entra a região nova (linha depois da qual), já conferido.
#' @noRd
.td_posicao <- function(linhas, regs, apos) {
  if (is.null(apos)) return(length(linhas))
  if (is.character(apos)) {
    i <- which(vapply(regs, `[[`, "", "chave") == apos)
    if (!length(i)) .td_erro(sprintf("N\u{E3}o h\u{E1} regi\u{E3}o '%s' para inserir depois.", apos), "tr_documento_erro_posicao")
    return(regs[[i[[1]]]]$fim)
  }
  apos <- as.integer(apos)
  if (identical(apos, 0L)) apos <- .td_fim_yaml(linhas)
  if (!.td_pode_inserir(linhas, apos)) {
    .td_erro(sprintf("A linha %s fica dentro do YAML, de uma regi\u{E3}o ou de um bloco de c\u{F3}digo.", apos),
             "tr_documento_erro_posicao")
  }
  apos
}

#' YAML do topo do documento (lista vazia se não há ou não lê).
#' @noRd
.td_yaml <- function(linhas) {
  f <- .td_fim_yaml(linhas)
  if (f < 2L) return(list())
  tryCatch(yaml::yaml.load(paste(.td_sem_cr(linhas[2:(f - 1L)]), collapse = "\n")) %||% list(),
           error = function(e) list())
}

.td_fluxo_do <- function(linhas) {
  f <- .td_yaml(linhas)$trama$fluxo
  if (is.character(f) && length(f) == 1L && nzchar(f)) f else "main"
}

.td_quadro <- function(linhas, colunas) {
  if (!length(linhas)) {
    return(as.data.frame(stats::setNames(rep(list(character()), length(colunas)), colunas), stringsAsFactors = FALSE))
  }
  do.call(rbind, lapply(linhas, function(l) as.data.frame(l[colunas], stringsAsFactors = FALSE)))
}
