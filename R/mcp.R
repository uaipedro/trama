#' Servidor MCP do canal de controle.
#'
#' Expõe os verbos do `trama-agente` como tools do Model Context Protocol,
#' por stdio (JSON-RPC 2.0, uma mensagem por linha). Serve qualquer cliente
#' MCP local: Claude Code, Claude Desktop, Codex, Cursor. Registro de uma
#' linha, por exemplo:
#'
#' ```
#' claude mcp add trama -- Rscript -e "trama::tr_mcp()"
#' ```
#'
#' Cada tool é um comando do CLI (`R/cli.R`) montado a partir dos argumentos
#' da tool: não há segundo caminho de edição. As tools que mexem no fluxo
#' precisam do editor aberto; se não houver um para o projeto, o servidor
#' sobe `tr_app()` num processo R em segundo plano, abre o navegador e
#' espera a aba conectar. O editor que o servidor subiu é encerrado junto com
#' ele; o que a pessoa abriu fica.
#'
#' stdout é do protocolo. Tudo o que um bloco ou pacote imprimir durante uma
#' tool é desviado para stderr, que o cliente trata como log.
#' @param projeto Pasta do projeto trama. Sem `trama.json`, o projeto é
#'   criado com todas as coleções `trama.*` instaladas.
#' @param entrada,saida Conexões do protocolo (o padrão é stdio).
#' @return Invisível, quando a entrada fecha.
#' @export
tr_mcp <- function(projeto = ".", entrada = file("stdin"), saida = stdout()) {
  ctx <- .tr_mcp_contexto(projeto)
  on.exit(.tr_mcp_encerrar(ctx), add = TRUE)
  open(entrada, "r")
  on.exit(close(entrada), add = TRUE)
  repeat {
    linha <- readLines(entrada, n = 1L, warn = FALSE, encoding = "UTF-8")
    if (!length(linha)) break
    if (!nzchar(trimws(linha))) next
    # Erro fora do envelope (serializar, escrever) não derruba o servidor.
    tryCatch({
      resp <- .tr_mcp_responder(linha, ctx)
      if (!is.null(resp)) {
        writeLines(.tr_mcp_json(resp), saida, useBytes = TRUE)
        flush(saida)
      }
    }, error = function(e) message("trama mcp: ", conditionMessage(e)))
  }
  invisible()
}

.tr_mcp_versoes <- c("2025-06-18", "2025-03-26", "2024-11-05")

.tr_mcp_contexto <- function(projeto = ".", registro = NULL) {
  ctx <- new.env(parent = emptyenv())
  ctx$projeto <- normalizePath(projeto, mustWork = FALSE)
  ctx$registro <- registro
  ctx$filho <- NULL
  ctx
}

.tr_mcp_json <- function(x) {
  as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null", digits = NA))
}

# Protocolo ---------------------------------------------------------------

#' Uma linha de entrada, uma resposta (lista) ou `NULL` para notificação.
#' Nenhum erro de R escapa: vira erro JSON-RPC ou `isError` da tool.
#' @noRd
.tr_mcp_responder <- function(linha, ctx) {
  msg <- tryCatch(jsonlite::fromJSON(linha, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.list(msg) || is.null(names(msg))) {
    return(.tr_mcp_erro(NULL, -32700L, "Mensagem JSON-RPC inv\u00e1lida."))
  }
  if (is.null(msg$method)) {
    # Resposta do cliente a algo nosso: não se responde.
    if (!is.null(msg$result) || !is.null(msg$error)) return(NULL)
    return(.tr_mcp_erro(msg$id, -32600L, "Requisi\u00e7\u00e3o sem 'method'."))
  }
  if (is.null(msg$id)) return(NULL)
  res <- tryCatch(.tr_mcp_metodo(msg$method, msg$params %||% list(), ctx),
                  tr_mcp_desconhecido = function(e) e,
                  error = function(e) structure(class = "tr_mcp_falha", list(message = conditionMessage(e))))
  if (inherits(res, "tr_mcp_desconhecido")) {
    return(.tr_mcp_erro(msg$id, -32601L, conditionMessage(res)))
  }
  if (inherits(res, "tr_mcp_falha")) {
    return(.tr_mcp_erro(msg$id, -32603L, res$message))
  }
  list(jsonrpc = "2.0", id = msg$id, result = res)
}

.tr_mcp_erro <- function(id, code, message) {
  list(jsonrpc = "2.0", id = id, error = list(code = code, message = message))
}

.tr_mcp_metodo <- function(metodo, params, ctx) {
  switch(metodo,
    initialize = {
      pedida <- params$protocolVersion %||% ""
      list(protocolVersion = if (pedida %in% .tr_mcp_versoes) pedida else .tr_mcp_versoes[[1]],
           capabilities = list(tools = list(listChanged = FALSE)),
           serverInfo = list(name = "trama", title = "trama",
                             version = as.character(utils::packageVersion("trama"))),
           instructions = .tr_mcp_instrucoes)
    },
    ping = structure(list(), names = character()),
    `tools/list` = list(tools = unname(lapply(.tr_mcp_tools, function(t) t[c("name", "title", "description", "inputSchema", "annotations")]))),
    `tools/call` = .tr_mcp_chamar(params$name, params$arguments %||% list(), ctx),
    rlang::abort(sprintf("M\u00e9todo n\u00e3o suportado: '%s'.", metodo), class = "tr_mcp_desconhecido"))
}

.tr_mcp_instrucoes <- paste(
  "trama \u00e9 um editor visual de fluxos de an\u00e1lise em R. Voc\u00ea edita o fluxo que est\u00e1 aberto na tela da pessoa:",
  "cada bloco aparece e roda na hora, e ela pode desfazer com Ctrl+Z.",
  "Roteiro: 'catalogo' com busca para achar o bloco (ex.: 'regressao', 'exemplo');",
  "'explicar' para ver params e portas; 'adicionar' com 'de' para ligar ao bloco anterior;",
  "'ajustar' para mudar params; 'resultado' para ler o que o bloco produziu (gr\u00e1ficos v\u00eam como imagem).",
  "Toda edi\u00e7\u00e3o devolve 'efeito': o status do que mudou e de tudo abaixo; n\u00f3 'blocked' traz 'causa'.",
  "Comece por um bloco de dados (busque 'exemplo' ou 'ler'). Os n\u00fameros v\u00eam do R: n\u00e3o calcule nem invente resultado,",
  "leia com 'resultado'. Se faltar uma cole\u00e7\u00e3o, diga \u00e0 pessoa o comando de instala\u00e7\u00e3o; n\u00e3o instale sozinho.",
  "Conte a an\u00e1lise como narrativa (leia 'guia' antes de montar): um 'frame' por etapa, com t\u00edtulo numerado;",
  "'adicionar' com 'frame' p\u00f5e o bloco na etapa; uma 'nota' Markdown abre cada frame (o que se testa e por qu\u00ea)",
  "e outra fica presa aos resultados-chave (o n\u00famero vem de 'resultado'); r\u00f3tulos claros com 'renomear';",
  "um frame de conclus\u00e3o no fim; depois 'organizar' (etapas da esquerda para a direita, diagn\u00f3sticos abaixo) e ajustes com 'posicionar'.")

# Tools -------------------------------------------------------------------

.tr_mcp_obj <- function(props = list(), obrigatorios = character()) {
  s <- list(type = "object",
            properties = if (length(props)) props else structure(list(), names = character()),
            additionalProperties = FALSE)
  if (length(obrigatorios)) s$required <- as.list(obrigatorios)
  s
}

.tr_mcp_str <- function(d) list(type = "string", description = d)

.tr_mcp_ler <- list(readOnlyHint = TRUE, destructiveHint = FALSE, idempotentHint = TRUE, openWorldHint = FALSE)
.tr_mcp_editar <- list(readOnlyHint = FALSE, destructiveHint = FALSE, idempotentHint = FALSE, openWorldHint = FALSE)
.tr_mcp_params_schema <- list(type = "object", additionalProperties = TRUE,
  description = "Params do bloco, por nome (veja 'explicar'). Valor JSON: n\u00famero, texto, booleano ou lista de colunas.")
.tr_mcp_espera_schema <- list(type = "number", minimum = 0, maximum = 300,
  description = "Segundos para esperar o fluxo rodar antes de responder (padr\u00e3o 30).")

#' Cada tool: metadados do protocolo, se precisa do editor e como vira um
#' comando do CLI (`list(cmd, pos, opt)`, o mesmo de `.tr_cli_parse`).
#' @noRd
.tr_mcp_tools <- list(
  list(name = "abrir", title = "Abrir o editor",
       description = paste("Abre o editor do trama no navegador para o projeto desta pasta (ou se conecta ao que j\u00e1 est\u00e1 aberto).",
                           "As tools de edi\u00e7\u00e3o abrem sozinhas; use esta para mostrar o editor \u00e0 pessoa antes de come\u00e7ar."),
       inputSchema = .tr_mcp_obj(), annotations = .tr_mcp_ler, editor = TRUE,
       cmd = function(x) list(cmd = "state", pos = character(), opt = list())),
  list(name = "estado", title = "Estado do fluxo",
       description = "O fluxo aberto na tela: n\u00f3s com tipo, params e status (ok, failed, blocked, running), e as liga\u00e7\u00f5es entre portas.",
       inputSchema = .tr_mcp_obj(), annotations = .tr_mcp_ler, editor = TRUE,
       cmd = function(x) list(cmd = "state", pos = character(), opt = list())),
  list(name = "catalogo", title = "Buscar blocos",
       description = paste("Busca blocos dispon\u00edveis (sem acento nem caixa; toda palavra tem de casar; at\u00e9 12, o melhor primeiro).",
                           "Procura no texto dos blocos, n\u00e3o em sin\u00f4nimos: se 'normalidade' n\u00e3o achar, tente 'normal'.",
                           "Devolve tipo, t\u00edtulo e portas. Para os params de um bloco, use 'explicar'."),
       inputSchema = .tr_mcp_obj(list(busca = .tr_mcp_str("Palavras do que voc\u00ea procura, ex.: 'regressao linear', 'exemplo', 'histograma'.")),
                                 "busca"),
       annotations = .tr_mcp_ler, editor = FALSE,
       cmd = function(x) list(cmd = "catalog", pos = character(), opt = list(busca = x$busca))),
  list(name = "explicar", title = "Explicar um bloco",
       description = "Um bloco inteiro: params (tipo, padr\u00e3o, escolhas), portas de entrada e sa\u00edda, ajuda e refer\u00eancias.",
       inputSchema = .tr_mcp_obj(list(tipo = .tr_mcp_str("Tipo do bloco, ex.: 'models/lm'.")), "tipo"),
       annotations = .tr_mcp_ler, editor = FALSE,
       cmd = function(x) list(cmd = "explain", pos = x$tipo, opt = list())),
  list(name = "adicionar", title = "Adicionar bloco",
       description = paste("P\u00f5e um bloco no fluxo da tela e o roda. 'de' liga a entrada dele \u00e0 sa\u00edda de blocos j\u00e1 existentes",
                           "(porta omitida = primeira compat\u00edvel). Devolve o id e o 'efeito' da execu\u00e7\u00e3o."),
       inputSchema = .tr_mcp_obj(list(
         tipo = .tr_mcp_str("Tipo do bloco, do 'catalogo'."),
         id = .tr_mcp_str("Id curto e leg\u00edvel para o n\u00f3 (opcional), ex.: 'dados', 'ajuste'."),
         de = list(type = "array", items = list(type = "string"),
                   description = "N\u00f3s de origem, 'no' ou 'no:porta', na ordem das entradas."),
         rotulo = .tr_mcp_str("R\u00f3tulo mostrado no card (opcional)."),
         frame = .tr_mcp_str("Frame (etapa) onde o bloco entra (opcional): o frame se rearruma para caber."),
         params = .tr_mcp_params_schema, espera = .tr_mcp_espera_schema), "tipo"),
       annotations = .tr_mcp_editar, editor = TRUE,
       cmd = function(x) list(cmd = "add", pos = c(x$tipo, .tr_mcp_kv(x$params)),
                              opt = Filter(Negate(is.null), list(id = x$id, label = x$rotulo, frame = x$frame,
                                                                 from = as.character(unlist(x$de)),
                                                                 wait = x$espera %||% 30)))),
  list(name = "ajustar", title = "Ajustar params",
       description = "Muda params de um n\u00f3 e roda de novo o que depende dele. Devolve o 'efeito'.",
       inputSchema = .tr_mcp_obj(list(no = .tr_mcp_str("Id do n\u00f3."), params = .tr_mcp_params_schema,
                                      espera = .tr_mcp_espera_schema), c("no", "params")),
       annotations = .tr_mcp_editar, editor = TRUE,
       cmd = function(x) list(cmd = "set", pos = c(x$no, .tr_mcp_kv(x$params)), opt = list(wait = x$espera %||% 30))),
  list(name = "ligar", title = "Ligar blocos",
       description = "Liga a sa\u00edda de um n\u00f3 \u00e0 entrada de outro. Porta omitida = primeira compat\u00edvel.",
       inputSchema = .tr_mcp_obj(list(de = .tr_mcp_str("Origem: 'no' ou 'no:porta'."),
                                      para = .tr_mcp_str("Destino: 'no' ou 'no:porta'."),
                                      espera = .tr_mcp_espera_schema), c("de", "para")),
       annotations = .tr_mcp_editar, editor = TRUE,
       cmd = function(x) list(cmd = "link", pos = c(x$de, x$para), opt = list(wait = x$espera %||% 30))),
  list(name = "remover", title = "Remover bloco",
       description = "Tira um n\u00f3 do fluxo, com suas liga\u00e7\u00f5es. A pessoa (ou 'desfazer') pode trazer de volta.",
       inputSchema = .tr_mcp_obj(list(no = .tr_mcp_str("Id do n\u00f3.")), "no"),
       annotations = utils::modifyList(.tr_mcp_editar, list(destructiveHint = TRUE)), editor = TRUE,
       cmd = function(x) list(cmd = "rm", pos = x$no, opt = list(wait = 30))),
  list(name = "resultado", title = "Ler resultado",
       description = paste("O que um n\u00f3 produziu: status, resumo, pr\u00e9via da tabela; gr\u00e1ficos v\u00eam como imagem.",
                           "N\u00f3 'blocked' traz 'causa'. Espera o n\u00f3 terminar de rodar."),
       inputSchema = .tr_mcp_obj(list(no = .tr_mcp_str("Id do n\u00f3."), espera = .tr_mcp_espera_schema), "no"),
       annotations = .tr_mcp_ler, editor = TRUE,
       cmd = function(x) list(cmd = "result", pos = x$no, opt = list(wait = x$espera %||% 30))),
  list(name = "desfazer", title = "Desfazer",
       description = "Desfaz a \u00faltima edi\u00e7\u00e3o do fluxo, sua ou da pessoa.",
       inputSchema = .tr_mcp_obj(), annotations = utils::modifyList(.tr_mcp_editar, list(destructiveHint = TRUE)),
       editor = TRUE,
       cmd = function(x) list(cmd = "undo", pos = character(), opt = list(wait = 30))),
  list(name = "aceitar_versao", title = "Aceitar vers\u00e3o nova",
       description = paste("Bloco que subiu de vers\u00e3o desde que o fluxo foi salvo aparece em 'estado' como problema 'version_drift':",
                           "o resultado pode ter mudado. Depois de conferir, aceitar grava a vers\u00e3o nova e o aviso some.",
                           "N\u00e3o muda params nem resultado; 'desfazer' volta o aviso."),
       inputSchema = .tr_mcp_obj(list(nos = list(type = "array", items = list(type = "string"),
                                                 description = "Ids dos n\u00f3s; vazio aceita todos em version_drift."))),
       annotations = .tr_mcp_editar, editor = TRUE,
       cmd = function(x) list(cmd = "accept", pos = as.character(unlist(x$nos)), opt = list(wait = 0))),
  list(name = "renomear", title = "Renomear bloco",
       description = "Muda o r\u00f3tulo mostrado no card (e usado como legenda no documento). N\u00e3o muda o id nem o resultado.",
       inputSchema = .tr_mcp_obj(list(no = .tr_mcp_str("Id do n\u00f3."), rotulo = .tr_mcp_str("R\u00f3tulo novo.")),
                                 c("no", "rotulo")),
       annotations = .tr_mcp_editar, editor = TRUE,
       cmd = function(x) list(cmd = "rename", pos = c(x$no, x$rotulo), opt = list())),
  list(name = "posicionar", title = "Posicionar",
       description = paste("Move blocos, frames ou notas como o arrasto do editor: frame leva o que tem dentro,",
                           "quem est\u00e1 num grupo leva o grupo (nota presa vai junto). Posi\u00e7\u00e3o absoluta com 'itens',",
                           "ou desloca 'ids' por 'dx'/'dy'. Coordenadas do canvas: x para a direita, y para baixo; 'estado' traz as atuais."),
       inputSchema = .tr_mcp_obj(list(
         itens = list(type = "array", description = "Posi\u00e7\u00f5es absolutas, canto de cima \u00e0 esquerda.",
                      items = list(type = "object", additionalProperties = FALSE, required = list("id", "x", "y"),
                                   properties = list(id = .tr_mcp_str("Id do bloco, frame ou nota."),
                                                     x = list(type = "number"), y = list(type = "number")))),
         ids = list(type = "array", items = list(type = "string"), description = "Itens que andam juntos por dx/dy."),
         dx = list(type = "number", description = "Deslocamento horizontal."),
         dy = list(type = "number", description = "Deslocamento vertical."))),
       annotations = .tr_mcp_editar, editor = TRUE,
       cmd = function(x) {
         pos <- c(vapply(x$itens %||% list(), function(i) sprintf("%s=%s,%s", i$id, i$x, i$y), ""),
                  as.character(unlist(x$ids)))
         list(cmd = "move", pos = pos, opt = Filter(Negate(is.null), list(dx = x$dx, dy = x$dy)))
       }),
  list(name = "frame", title = "Frame (etapa)",
       description = paste("Frames s\u00e3o as etapas da narrativa. 'criar': frame novo \u00e0 direita de tudo, com t\u00edtulo e,",
                           "se vierem 'blocos', j\u00e1 com eles dentro, arrumados; devolve o id. 'editar': t\u00edtulo, cor, propor\u00e7\u00e3o,",
                           "ordem (posi\u00e7\u00e3o na narrativa) ou tamanho. 'por'/'tirar': blocos entram ou saem (o frame se rearruma).",
                           "'remover': tira o frame; os blocos ficam. Pertencer a um frame \u00e9 estar inteiro dentro dele."),
       inputSchema = .tr_mcp_obj(list(
         acao = list(type = "string", enum = list("criar", "editar", "remover", "por", "tirar")),
         frame = .tr_mcp_str("Id do frame (menos em 'criar')."),
         id = .tr_mcp_str("Id curto para o frame novo (opcional, s\u00f3 em 'criar'), ex.: 'etapa1'."),
         titulo = .tr_mcp_str("T\u00edtulo, numerado: '1. Dados e descritivas'."),
         cor = list(type = "string", enum = list("azul", "verde", "amarelo", "laranja", "rosa", "roxo", "cinza")),
         proporcao = list(type = "string", enum = list("livre", "16:9", "4:3", "1:1", "A4"),
                          description = "Padr\u00e3o 'livre': o frame tem o tamanho do conte\u00fado."),
         ordem = list(type = "integer", minimum = 1, description = "Posi\u00e7\u00e3o do frame na sequ\u00eancia (1 = primeiro)."),
         largura = list(type = "number"), altura = list(type = "number"),
         blocos = list(type = "array", items = list(type = "string"), description = "Blocos para 'criar', 'por' ou 'tirar'.")),
         "acao"),
       annotations = .tr_mcp_editar, editor = TRUE,
       cmd = function(x) {
         acao <- c(criar = "add", editar = "set", remover = "rm", por = "put", tirar = "take")[[x$acao]]
         alvo <- if (acao == "add") x$titulo %||% rlang::abort("Falta 'titulo' para criar o frame.") else
           x$frame %||% rlang::abort("Falta 'frame'.")
         list(cmd = "frame", pos = c(acao, alvo, as.character(unlist(x$blocos))),
              opt = Filter(Negate(is.null), list(id = x$id, color = x$cor, aspect = x$proporcao,
                                                 title = if (acao != "add") x$titulo, order = x$ordem,
                                                 w = x$largura, h = x$altura)))
       }),
  list(name = "nota", title = "Nota Markdown",
       description = paste("Nota de texto em Markdown no canvas, sem execu\u00e7\u00e3o. 'criar' com 'frame': abre a etapa",
                           "(o frame se rearruma para ela caber); com 'bloco': fica presa logo abaixo do bloco e anda com ele;",
                           "sem nenhum: solta, \u00e0 direita de tudo (ou em x, y). A altura sai do texto. 'editar' muda texto,",
                           "cor ou tamanho; 'remover' apaga. Devolve o id da nota criada."),
       inputSchema = .tr_mcp_obj(list(
         acao = list(type = "string", enum = list("criar", "editar", "remover")),
         nota = .tr_mcp_str("Id da nota (menos em 'criar')."),
         id = .tr_mcp_str("Id curto para a nota nova (opcional)."),
         texto = .tr_mcp_str("Markdown: t\u00edtulos (#), listas, **negrito**, `c\u00f3digo`, tabelas simples."),
         frame = .tr_mcp_str("Frame onde a nota abre a etapa."),
         bloco = .tr_mcp_str("Bloco ao qual a nota fica presa."),
         cor = list(type = "string", enum = list("nenhuma", "azul", "verde", "amarelo", "laranja", "rosa", "roxo", "cinza")),
         largura = list(type = "number", description = "Padr\u00e3o 320."), altura = list(type = "number"),
         x = list(type = "number"), y = list(type = "number")), "acao"),
       annotations = .tr_mcp_editar, editor = TRUE,
       cmd = function(x) {
         acao <- c(criar = "add", editar = "set", remover = "rm")[[x$acao]]
         alvo <- if (acao == "add") x$texto %||% rlang::abort("Falta 'texto' para criar a nota.") else
           x$nota %||% rlang::abort("Falta 'nota'.")
         list(cmd = "note", pos = c(acao, alvo),
              opt = Filter(Negate(is.null), list(id = x$id, text = if (acao != "add") x$texto,
                                                 frame = x$frame, node = x$bloco, color = x$cor,
                                                 w = x$largura, h = x$altura, x = x$x, y = x$y)))
       }),
  list(name = "organizar", title = "Organizar por etapas",
       description = paste("Auto-layout por etapas: cada frame vira uma coluna, na ordem dos frames, da esquerda para a direita;",
                           "dentro dele o ramo principal fica em cima e diagn\u00f3sticos, gr\u00e1ficos e tabelas embaixo, irm\u00e3os empilhados;",
                           "a nota de abertura fica \u00e0 esquerda e notas presas ficam sob o bloco. Blocos fora de frame v\u00e3o para uma faixa abaixo.",
                           "Chame depois de montar; ajuste com 'posicionar'. Um 'desfazer' volta tudo."),
       inputSchema = .tr_mcp_obj(), annotations = .tr_mcp_editar, editor = TRUE,
       cmd = function(x) list(cmd = "layout", pos = character(), opt = list())),
  list(name = "guia", title = "Guia de narrativa",
       description = "Como contar a an\u00e1lise no canvas: perguntas, etapas em frames, notas, conclus\u00e3o, layout. Leia antes de montar um fluxo.",
       inputSchema = .tr_mcp_obj(), annotations = .tr_mcp_ler, editor = FALSE,
       cmd = function(x) list(cmd = "guide", pos = character(), opt = list())),
  list(name = "validar", title = "Validar arquivo de fluxo",
       description = "L\u00ea, migra e valida um fluxo ou template .json do disco, sem editor. Lista os problemas.",
       inputSchema = .tr_mcp_obj(list(arquivo = .tr_mcp_str("Caminho do .json, relativo \u00e0 pasta do projeto ou absoluto.")), "arquivo"),
       annotations = .tr_mcp_ler, editor = FALSE,
       cmd = function(x) list(cmd = "validate", pos = x$arquivo, opt = list()))
)
names(.tr_mcp_tools) <- vapply(.tr_mcp_tools, `[[`, "", "name")

#' Params da tool (objeto JSON) para `param=valor` do CLI, que relê como JSON.
#' @noRd
.tr_mcp_kv <- function(params) {
  if (!length(params)) return(character())
  vapply(names(params), function(k) {
    paste0(k, "=", jsonlite::toJSON(params[[k]], auto_unbox = TRUE, null = "null", digits = NA))
  }, "", USE.NAMES = FALSE)
}

#' Chama uma tool. Recusa do trama e erro de argumento voltam como
#' `isError: true` com a mensagem, para o modelo se corrigir.
#' @noRd
.tr_mcp_chamar <- function(nome, args, ctx) {
  t <- .tr_mcp_tools[[nome %||% ""]]
  if (is.null(t)) {
    return(.tr_mcp_conteudo(list(ok = FALSE, message = sprintf(
      "Tool desconhecida: '%s'. Dispon\u00edveis: %s.", nome, paste(names(.tr_mcp_tools), collapse = ", ")))))
  }
  saida <- NULL
  impresso <- utils::capture.output(saida <- tryCatch({
    faltam <- setdiff(unlist(t$inputSchema$required), names(args))
    if (length(faltam)) rlang::abort(sprintf("Falta argumento: %s.", paste(faltam, collapse = ", ")))
    a <- t$cmd(args)
    if (nome == "validar" && !grepl("^(/|\\\\|~|[A-Za-z]:)", a$pos[[1]])) a$pos <- file.path(ctx$projeto, a$pos)
    a$opt$projeto <- ctx$projeto
    if (isTRUE(t$editor)) .tr_mcp_editor(ctx)
    if (!t$editor && !is.null(ctx$registro)) {
      precisa <- function(n, uso) if (length(a$pos) < n) rlang::abort(paste("Uso:", uso))
      .tr_cli_sem_editor(a, a$pos, precisa, registro = ctx$registro)
    } else {
      .tr_cli_run(a)
    }
  }, error = function(e) list(ok = FALSE, message = conditionMessage(e))))
  if (length(impresso)) cat(impresso, sep = "\n", file = stderr())
  if (nome == "guia" && isTRUE(saida$ok)) {
    saida$mcp <- paste("Nas tools: frame -> 'frame' (criar, por...), note -> 'nota', move -> 'posicionar',",
                       "rename -> 'renomear', layout -> 'organizar', result -> 'resultado', add --frame -> 'adicionar' com 'frame'.")
  }
  if (nome == "abrir" && isTRUE(saida$ok)) {
    saida <- list(ok = TRUE, url = ctx$url, nodes = length(saida$nodes),
                  message = "Editor aberto. A pessoa v\u00ea cada edi\u00e7\u00e3o na tela.")
  }
  .tr_mcp_conteudo(saida)
}

#' Resposta de tool: o JSON do CLI como texto e cada PNG das prévias como
#' imagem. `ok: false` vira `isError`.
#' @noRd
.tr_mcp_conteudo <- function(res) {
  pngs <- unique(Filter(function(f) is.character(f) && grepl("[.]png$", f, ignore.case = TRUE) && file.exists(f),
                        unlist(lapply(res$outputs, function(o) o$preview$files))))
  partes <- c(list(list(type = "text", text = as.character(jsonlite::toJSON(
    res, auto_unbox = TRUE, null = "null", na = "null", digits = NA, pretty = TRUE)))),
    lapply(pngs, function(f) list(type = "image", mimeType = "image/png",
                                  data = jsonlite::base64_enc(readBin(f, "raw", file.size(f))))))
  list(content = partes, isError = !isTRUE(res$ok))
}

# Editor sob demanda ------------------------------------------------------

#' Garante um editor com aba conectada para o projeto. Editor de pé sem aba
#' (a pessoa fechou a aba) não ganha um gêmeo: a recusa pede para reabrir.
#' Sem editor, sobe um em segundo plano e espera a aba; filho que morre
#' volta com o fim do log e permite nova tentativa.
#' @noRd
.tr_mcp_editor <- function(ctx, espera = 40) {
  sonda <- .tr_mcp_sondar(ctx)
  if (identical(sonda, "pronto")) return(invisible(TRUE))
  if (identical(sonda, "sem_aba") && is.null(ctx$filho)) {
    rlang::abort(paste0("O editor deste projeto est\u00e1 aberto, mas sem aba no navegador. ",
                        "Pe\u00e7a \u00e0 pessoa para abrir (ou recarregar) a aba do trama e tente de novo."))
  }
  if (is.null(ctx$filho)) .tr_mcp_subir(ctx)
  fim <- Sys.time() + espera
  repeat {
    if (identical(.tr_mcp_sondar(ctx), "pronto")) return(invisible(TRUE))
    if (!.tr_mcp_filho_vivo(ctx)) {
      ctx$filho <- NULL
      log <- tryCatch(utils::tail(readLines(ctx$log, warn = FALSE), 8), error = function(e) character())
      rlang::abort(paste0("O editor n\u00e3o subiu. Fim do log (", ctx$log, "):\n", paste(log, collapse = "\n")))
    }
    if (Sys.time() >= fim) break
    Sys.sleep(0.5)
  }
  rlang::abort(paste0(
    "O editor subiu em ", ctx$url %||% "segundo plano", " mas nenhuma aba do navegador conectou. ",
    "Pe\u00e7a \u00e0 pessoa para abrir esse endere\u00e7o e tente de novo. Log do editor: ", ctx$log %||% "(sem log)", "."))
}

#' "pronto" (editor do projeto com aba), "sem_aba" (canal responde, sem
#' sessão) ou "nada" (sem arquivo, outro projeto ou canal morto).
#' @noRd
.tr_mcp_sondar <- function(ctx) {
  cx <- tryCatch(.tr_cli_conexao(ctx$projeto), error = function(e) NULL)
  if (is.null(cx)) return("nada")
  if (!identical(normalizePath(cx$root %||% "", mustWork = FALSE), ctx$projeto)) return("nada")
  st <- tryCatch(.tr_cli_http(cx, "GET", "/state"), error = function(e) NULL)
  if (is.null(st)) return("nada")
  if (isTRUE(st$ok)) "pronto" else "sem_aba"
}

#' O editor que subimos ainda vive? Sem pid ainda (carregando) conta como
#' vivo; no Windows não há teste barato, e o prazo decide.
#' @noRd
.tr_mcp_filho_vivo <- function(ctx) {
  if (.Platform$OS.type != "unix" || is.null(ctx$pidfile) || !file.exists(ctx$pidfile)) return(TRUE)
  pid <- suppressWarnings(as.integer(readLines(ctx$pidfile, n = 1L, warn = FALSE)))
  !length(pid) || is.na(pid) || isTRUE(tools::pskill(pid, 0L))
}

#' Sobe `tr_app()` num Rscript separado, com navegador. Projeto sem
#' `trama.json` nasce com todas as coleções `trama.*` instaladas.
#' @noRd
.tr_mcp_subir <- function(ctx) {
  dir.create(file.path(ctx$projeto, ".trama"), recursive = TRUE, showWarnings = FALSE)
  porta <- tr_port_free(getOption("trama.port", tr_port_default()))
  ctx$log <- file.path(ctx$projeto, ".trama", "mcp-editor.log")
  ctx$pidfile <- tempfile("trama-mcp-", fileext = ".pid")
  dev <- Sys.getenv("TRAMA_DEV")
  carregar <- if (nzchar(dev)) {
    sprintf("pkgload::load_all(%s, quiet = TRUE)", deparse(dev))
  } else "library(trama)"
  codigo <- paste(
    # pid antes de carregar o pacote: MCP que fecha durante a carga ainda
    # acha quem matar.
    sprintf("writeLines(as.character(Sys.getpid()), %s)", deparse(ctx$pidfile)),
    carregar,
    # Cliente que mata o MCP com SIGTERM não roda o on.exit: o editor vigia
    # o processo pai e encerra sozinho quando ele some.
    sprintf("pai <- %dL", Sys.getpid()),
    # Sinal 0 só testa se existe; no Windows pskill mataria o pai, então lá
    # fica só o on.exit.
    "vigiar <- function() if (tools::pskill(pai, 0L)) later::later(vigiar, 2) else shiny::stopApp()",
    "if (.Platform$OS.type == 'unix') later::later(vigiar, 2)",
    sprintf("raiz <- %s", deparse(ctx$projeto)),
    "cols <- if (file.exists(file.path(raiz, 'trama.json'))) character() else",
    "  trama:::.tr_colecoes_instaladas()",
    sprintf("shiny::runApp(tr_app(tr_project(raiz, collections = cols), port = %dL), launch.browser = TRUE)", porta),
    sep = "\n")
  script <- tempfile("trama-mcp-", fileext = ".R")
  writeLines(codigo, script)
  system2(file.path(R.home("bin"), "Rscript"), shQuote(script), stdout = ctx$log, stderr = ctx$log, wait = FALSE)
  ctx$filho <- TRUE
  ctx$url <- sprintf("http://127.0.0.1:%d", porta)
  invisible()
}

.tr_mcp_encerrar <- function(ctx) {
  if (is.null(ctx$pidfile) || !file.exists(ctx$pidfile)) return(invisible())
  pid <- suppressWarnings(as.integer(readLines(ctx$pidfile, n = 1L, warn = FALSE)))
  if (length(pid) && !is.na(pid)) tools::pskill(pid)
  unlink(ctx$pidfile)
  invisible()
}
