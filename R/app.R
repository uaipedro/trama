#' Sobe o editor.
#'
#' Os assets do front vêm de `inst/www` via `htmltools::htmlDependency()`, e
#' cada coleção carregada publica os próprios em `tr-<id>/`. A ordem de carga
#' é determinística e importa: `runtime.js` cria `window.tr` (os registros),
#' as coleções registram renderers e widgets, e só então o editor monta.
#'
#' @param port Porta em que o editor sobe. O padrão é fixo (e não sorteado pelo
#'   Shiny) para que a URL do editor seja sempre a mesma; se estiver ocupada,
#'   `tr_app()` procura a próxima livre e anuncia no console. Mude o padrão da
#'   sessão com `options(trama.port = ...)`, ou passe `port = NULL` para deixar
#'   o Shiny sortear.
#' @export
tr_app <- function(project = tr_project("."), flow = "main",
                   executor = tr_executor_sequential(),
                   port = getOption("trama.port", tr_port_default()), ...) {
  # Desligar aqui, não em `tr_server`: `onStop` roda uma vez por PROCESSO
  # (quando o app inteiro encerra), enquanto `onSessionEnded` roda por
  # SESSÃO. Um pool de daemons pertence ao processo, não à sessão — matar os
  # daemons no fim de uma sessão derrubaria outras abas ainda abertas.
  shiny::onStop(function() executor$shutdown())

  dots <- list(...)
  if (!is.null(port) && is.null(dots$options$port)) {
    livre <- tr_port_free(port)
    if (livre != port) {
      message(sprintf("trama: porta %d ocupada, subindo em %d.", port, livre))
    }
    dots$options <- utils::modifyList(dots$options %||% list(), list(port = livre))
  }

  do.call(shiny::shinyApp,
          c(list(ui = tr_ui(project), server = tr_server(project, flow, executor)), dots))
}

#' Porta padrão do editor: 8726, que é "TRAM" no teclado do telefone.
#'
#' Fora das faixas que costumam dar conflito — abaixo dos portos efêmeros do
#' Linux (32768–60999, sorteados pelo kernel) e longe dos padrões de ferramenta
#' de dev (3000, 3838, 5173, 8000, 8080, 8787).
#' @export
tr_port_default <- function() 8726L

#' Primeira porta livre a partir de `port`.
#'
#' O teste é abrir um socket servidor e fechar: é o mesmo recurso que o Shiny
#' vai pedir, então não há falso negativo por protocolo. Sobra a corrida entre
#' o fechamento aqui e o bind do Shiny — improvável em uso interativo, e o
#' preço de não manter o socket aberto enquanto o app sobe.
#' @export
tr_port_free <- function(port, tentativas = 20L) {
  for (p in seq.int(port, min(port + tentativas - 1L, 65535L))) {
    con <- tryCatch(serverSocket(p), error = function(e) NULL)
    if (!is.null(con)) {
      close(con)
      return(as.integer(p))
    }
  }
  stop(sprintf("trama: nenhuma porta livre entre %d e %d.",
               port, port + tentativas - 1L), call. = FALSE)
}

#' Monta a UI Shiny do editor: importmap, dependências e o `<div>` onde o React monta.
#' @export
tr_ui <- function(project) {
  # Previews com arquivo (imagem, etc.) são servidos direto do store — o
  # handle carrega caminho RELATIVO, e é aqui que ele vira URL.
  shiny::addResourcePath("trama-store", project$store$root)

  ver  <- as.character(utils::packageVersion("trama"))
  base <- paste0("trama-", ver)

  # ORDEM IMPORTA, e de dois jeitos que só aparecem no navegador:
  #
  # 1. O importmap precisa vir ANTES de qualquer `<script type="module">` que
  #    use especificador nu (`import ... from "react"`). O Shiny injeta os
  #    scripts das dependências antes do conteúdo de `tags$head()`, então pôr
  #    o importmap ali o deixaria tarde demais — silenciosamente, sem erro de
  #    console. Ele vira a PRIMEIRA dependência, com `head` apenas.
  # 2. `htmlDependency` serve tudo sob um prefixo VERSIONADO
  #    (`trama-0.0.0.9000/`), então o importmap e o src do editor têm que
  #    apontar pra lá, não pra "trama/".
  #
  # `runtime.js` não é carregado como script: o importmap resolve `"trama"`, e
  # tanto o JS das coleções quanto o editor o importam. Módulos executam na
  # ordem do documento, então as coleções registram antes de o editor montar.
  #
  # 3. O CSS das coleções vem DEPOIS do núcleo pelo mesmo motivo de ordem: o
  #    htmltools emite as dependências na ordem da lista, então o
  #    `trama.css` do core entra antes e uma regra de coleção com a mesma
  #    especificidade ganha — é o que deixa a coleção refinar o núcleo.
  deps <- list(tr_dependency_importmap(base), tr_dependency_core(ver))
  for (id in names(project$registry$collections)) {
    deps <- c(deps, tr_dependency_collection(project$registry, id))
  }

  htmltools::tagList(
    htmltools::tags$head(
      htmltools::tags$meta(charset = "utf-8"),
      # Resolve o tema ANTES do primeiro paint: aplicado só pelo React, o app
      # abriria escuro e piscaria para claro a cada recarga. Script clássico, não
      # módulo, então não esbarra na regra do importmap acima e roda síncrono,
      # antes de o <body> pintar. O editor repete a mesma resolução e assume
      # daí em diante (troca na toolbar, mudança do sistema).
      htmltools::tags$script(htmltools::HTML(paste0(
        "(function(){try{var p=localStorage.getItem('trama.temaApp')||'sistema';",
        "if(p!=='claro'&&p!=='escuro')p='sistema';",
        "var c=p==='sistema'?(matchMedia('(prefers-color-scheme: light)').matches?'claro':'escuro'):p;",
        "document.documentElement.dataset.tema=c;}catch(e){}})();"))),
      htmltools::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      htmltools::tags$title("trama"),
      # O ícone mora em `inst/www`, servido pela dependência do núcleo sob o
      # prefixo VERSIONADO — daí o `base`, e não "trama/".
      #
      # A ORDEM destes candidatos É o mecanismo de fallback: o navegador fica
      # com o PRIMEIRO cujo tipo ele sabe desenhar. Com o SVG na frente, quem
      # o entende ganha uma marca nítida em qualquer densidade de tela; quem
      # não entende simplesmente o ignora e cai nos PNGs abaixo. Inverter a
      # ordem entregaria o bitmap a todo mundo, inclusive a quem podia ter
      # coisa melhor. É a variante de 3 nós, e não a marca cheia: na aba o
      # ícone tem 16 px, tamanho em que a marca inteira vira borrão.
      htmltools::tags$link(rel = "icon", type = "image/svg+xml",
                           href = paste0("./", base, "/marca-min.svg")),
      htmltools::tags$link(rel = "icon", type = "image/png", sizes = "32x32",
                           href = paste0("./", base, "/favicon-32.png")),
      htmltools::tags$link(rel = "icon", type = "image/png", sizes = "512x512",
                           href = paste0("./", base, "/icon.png")),
      htmltools::tags$link(rel = "apple-touch-icon", sizes = "180x180",
                           href = paste0("./", base, "/icon-180.png"))
    ),
    # A versão vem no `data-` do nó raiz porque o front não tem como perguntar
    # a versão do pacote: não há chamada ao R antes de o editor montar, e o
    # prefixo versionado dos assets (`trama-0.1.0/`) é detalhe de como o
    # htmlDependency serve arquivo — caminho, não contrato. Extrair a versão
    # dali amarraria o editor a esse detalhe.
    htmltools::div(id = "tr-root", `data-versao` = ver),
    # Elemento do binding de op. O id é o nome do input no Shiny.
    htmltools::div(id = "tr_op", class = "tr-op-binding",
                   style = "display:none", `data-value` = ""),
    deps,
    # O editor é o ÚLTIMO: monta depois de todas as coleções registrarem.
    #
    # Import DINÂMICO, e não `<script type="module" src=...>`, por um motivo
    # prático: quando um módulo falha ao LINKAR (export que não existe,
    # especificador que o importmap não resolve), o navegador simplesmente não
    # executa nada — página em branco, sem erro visível e sem nada no console.
    # Isso é péssimo justamente para quem vai mexer aqui: o autor de uma
    # coleção com JS quebrado não teria pista nenhuma. Com `import().catch()`,
    # a falha vira mensagem na tela e no console.
    htmltools::tags$script(type = "module", htmltools::HTML(sprintf(
      "import('./%s/editor.js').catch(function (e) { window.__ndErr.push(['import', String(e && e.stack || e)]); var r = document.getElementById('tr-root'); if (r) { r.className = 'tr-boot-error'; r.textContent = 'Falha ao carregar o editor: ' + (e && e.message || e); } });",
      base)))
  )
}

#' Dependência Shiny que injeta o importmap e a captura de erro de boot, antes de qualquer módulo.
#' @export
tr_dependency_importmap <- function(base) {
  htmltools::htmlDependency(
    name = "trama-importmap", version = "1",
    src = c(href = ""),
    head = paste0(
      # Captura de erro de carga num script CLÁSSICO, antes de tudo. Erro de
      # linkagem de módulo e promessa rejeitada acontecem antes de qualquer
      # ferramenta de inspeção anexar ao console — sem isto, uma falha de
      # boot é uma página em branco sem pista nenhuma.
      '<script>window.__ndErr=[];',
      'addEventListener("error",function(e){window.__ndErr.push(["error",String(e.message||e.error),e.filename,e.lineno])},true);',
      'addEventListener("unhandledrejection",function(e){window.__ndErr.push(["rejection",String(e.reason&&e.reason.stack||e.reason)])});',
      '</script>',
      '<script type="importmap">', .tr_importmap(base), '</script>')
  )
}

#' Vendorizado: pacote instalado funciona offline, e CRAN não aceita
#' dependência de rede. A FORMA do importmap não mudou em relação à época do
#' CDN — só os endereços — e o contrato do módulo `trama` que as coleções
#' importam continua idêntico. `./` continua obrigatório (ver comentário
#' abaixo, que sobreviveu à mudança porque a armadilha não mudou).
#' @noRd
.tr_importmap <- function(base) {
  v <- function(f) paste0("./", base, "/vendor/", f)
  jsonlite::toJSON(list(imports = list(
    "react" = v("react.js"),
    "react-dom" = v("react-dom.js"),
    "react-dom/client" = v("react-dom-client.js"),
    "react/jsx-runtime" = v("react-jsx-runtime.js"),
    "@xyflow/react" = v("xyflow.js"),
    "@dagrejs/dagre" = v("dagre.js"),
    "html-to-image" = v("html-to-image.js"),
    "marked" = v("marked.js"),
    # `./` é OBRIGATÓRIO: num importmap o endereço tem que ser URL absoluta ou
    # começar com "/", "./" ou "../". Um caminho relativo nu é descartado em
    # silêncio — a entrada some, `import from "trama"` falha, e o navegador
    # não registra erro nenhum antes de o módulo tentar resolver.
    "trama" = paste0("./", base, "/runtime.js")
  )), auto_unbox = TRUE)
}

#' Dependência Shiny com os assets do núcleo (CSS do core e do xyflow, `runtime.js`, `editor.js`).
#' @export
tr_dependency_core <- function(ver = as.character(utils::packageVersion("trama"))) {
  htmltools::htmlDependency(
    name = "trama", version = ver,
    src = c(file = system.file("www", package = "trama")),
    # O React Flow traz CSS próprio (posicionamento de nós, handles, o canvas
    # inteiro). Sem ele o grafo renderiza empilhado no canto, sem arestas.
    stylesheet = c("vendor/xyflow.css", "trama.css"),
    all_files = TRUE
  )
}

#' JS e CSS de uma coleção — o mecanismo que permite visualização nova sem
#' tocar no núcleo. O JS carrega DEPOIS do runtime e ANTES do editor; o CSS,
#' depois do `trama.css` do núcleo (a ordem vem de `tr_ui()`).
#'
#' Devolve SEMPRE uma lista de dependências, possivelmente vazia. Uma
#' `htmlDependency` serve UM diretório, então js e css em pastas diferentes
#' precisam de duas — e com nomes distintos, porque o htmltools deduplica
#' dependência por nome e ficaria só com uma delas, sem aviso. No mesmo
#' diretório (o caso comum, `inst/trama/`) vira uma só, com os dois.
#'
#' Asset cujo diretório não existe no pacote é pulado, cada um por conta
#' própria — o mesmo silêncio de antes pro js: coleção de globalenv (sem
#' pacote) ou instalada sem o `inst/` não derruba o app.
tr_dependency_collection <- function(registry, id) {
  info <- registry$collections[[id]]
  if (is.null(info$package)) return(list())
  assets <- Filter(Negate(is.null), list(js = info$js, css = info$css))
  dirs <- vapply(assets, function(p) system.file(dirname(p), package = info$package), "")
  assets <- assets[nzchar(dirs)]; dirs <- dirs[nzchar(dirs)]
  if (!length(assets)) return(list())

  lapply(unique(dirs), function(dir) {
    here <- assets[dirs == dir]
    # O nome "cheio" fica com o diretório do js (ou o único): é o nome que a
    # dependência sempre teve, e o css só ganha sufixo quando precisa de pasta
    # própria.
    name <- if (!is.null(here$js) || length(unique(dirs)) == 1L) paste0("trama-", id)
            else paste0("trama-", id, "-css")
    htmltools::htmlDependency(
      name = name, version = as.character(info$version),
      src = c(file = dir),
      script = if (!is.null(here$js)) list(list(src = basename(here$js), type = "module")),
      stylesheet = if (!is.null(here$css)) basename(here$css),
      all_files = TRUE
    )
  })
}
