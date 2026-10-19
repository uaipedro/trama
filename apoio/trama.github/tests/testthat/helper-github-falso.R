# GitHub falso para os testes: o pedaço da API REST, do device flow e da API
# de dados do Git que o trama.github usa, com estado em memória. Roda num
# processo à parte (webfakes); os testes o controlam pelas rotas `/_falso/*`.
# Nada sai para a rede.

TOKEN_FALSO <- "gho_falso_do_teste"
CLIENT_ID_FALSO <- "cid-teste"

# O app vai serializado para outro processo: o ambiente dele é o `baseenv`,
# para não arrastar o ambiente do teste (e o namespace carregado por
# pkgload, que o outro processo não tem). Tudo de que ele precisa mora aqui.
app_github_falso <- function() {
  TOKEN_FALSO <- "gho_falso_do_teste"
  CLIENT_ID_FALSO <- "cid-teste"
  `%||%` <- function(a, b) if (is.null(a)) b else a
  app <- webfakes::new_app()
  app$use(webfakes::mw_json(simplifyVector = FALSE))
  app$use(webfakes::mw_urlencoded())

  app$locals$zerar <- function(L) {
    L$n <- 0L
    L$blobs <- list(); L$arvores <- list(); L$commits <- list(); L$refs <- list()
    L$tag_objs <- list(); L$repos <- list()
    L$conferencias <- 0L; L$blobs_criados <- 0L
    L$falhar_blob <- NA_integer_; L$corrida <- FALSE
    L$tokens <- TOKEN_FALSO
  }
  app$locals$zerar(app$locals)

  # sha falso: contador, 40 dígitos hex. Árvore igual reaproveita o sha, como
  # no git (endereçado por conteúdo).
  app$locals$novo_sha <- function(L) { L$n <- L$n + 1L; sprintf("%040d", L$n) }

  erro <- function(res, status, msg, errors = NULL) {
    corpo <- list(message = msg)
    if (!is.null(errors)) corpo$errors <- errors
    res$set_status(status)$send_json(corpo, auto_unbox = TRUE)
  }
  autorizado <- function(req, res) {
    tk <- sub("^Bearer ", "", req$get_header("Authorization") %||% "")
    if (tk %in% req$app$locals$tokens) return(TRUE)
    erro(res, 401, "Bad credentials"); FALSE
  }
  chave <- function(req) paste0(req$params$o, "/", req$params$r)

  # Controle do teste -----------------------------------------------------
  app$post("/_falso/zerar", function(req, res) { req$app$locals$zerar(req$app$locals); res$send_json(list(ok = TRUE), auto_unbox = TRUE) })
  app$post("/_falso/config", function(req, res) {
    L <- req$app$locals
    for (k in names(req$json)) L[[k]] <- req$json[[k]]
    res$send_json(list(ok = TRUE), auto_unbox = TRUE)
  })
  app$get("/_falso/estado", function(req, res) {
    L <- req$app$locals
    res$send_json(list(refs = L$refs, blobs_criados = L$blobs_criados, commits = names(L$commits),
                       arvores = lapply(L$arvores, function(a) a), blobs = L$blobs,
                       repos = names(L$repos), mensagens = lapply(L$commits, `[[`, "message"),
                       tag_objs = L$tag_objs),
                  auto_unbox = TRUE)
  })

  # Device flow (site) ----------------------------------------------------
  app$post("/login/device/code", function(req, res) {
    if (!identical(req$form$client_id, CLIENT_ID_FALSO)) {
      return(res$send_json(list(error = "incorrect_client_credentials"), auto_unbox = TRUE))
    }
    res$send_json(list(device_code = "dc-1", user_code = "ABCD-1234",
                       verification_uri = "https://github.com/login/device",
                       expires_in = 900, interval = 0, scope_pedido = req$form$scope), auto_unbox = TRUE)
  })
  app$post("/login/oauth/access_token", function(req, res) {
    L <- req$app$locals
    L$conferencias <- L$conferencias + 1L
    r <- switch(as.character(L$conferencias),
      "1" = list(error = "authorization_pending"),
      "2" = list(error = "slow_down", interval = 0),
      list(access_token = TOKEN_FALSO, token_type = "bearer", scope = "repo,workflow"))
    res$send_json(r, auto_unbox = TRUE)
  })

  # API REST ---------------------------------------------------------------
  app$get("/user", function(req, res) {
    if (!autorizado(req, res)) return()
    res$send_json(list(login = "pesquisadora", name = "Pesquisadora Teste",
                       avatar_url = "https://avatars.example/p.png",
                       html_url = "https://github.com/pesquisadora", plan = list(name = "free")),
                  auto_unbox = TRUE)
  })
  app$post("/user/repos", function(req, res) {
    if (!autorizado(req, res)) return()
    L <- req$app$locals
    nome <- req$json$name
    full <- paste0("pesquisadora/", nome)
    if (!is.null(L$repos[[full]])) {
      return(erro(res, 422, "Repository creation failed.",
                  list(list(resource = "Repository", code = "custom", field = "name",
                            message = "name already exists on this account"))))
    }
    # auto_init: um commit inicial com README.
    blob <- L$novo_sha(L); L$blobs[[blob]] <- jsonlite::base64_enc(charToRaw(paste("#", nome)))
    arv <- L$novo_sha(L); L$arvores[[arv]] <- list(README.md = blob)
    com <- L$novo_sha(L); L$commits[[com]] <- list(tree = arv, parents = list(), message = "Initial commit")
    L$refs[[paste0(full, "@heads/main")]] <- com
    L$repos[[full]] <- list(full_name = full, html_url = paste0("https://github.com/", full),
                            private = isTRUE(req$json$private), default_branch = "main",
                            description = req$json$description)
    res$set_status(201)$send_json(L$repos[[full]], auto_unbox = TRUE)
  })
  app$get("/repos/:o/:r", function(req, res) {
    if (!autorizado(req, res)) return()
    r <- req$app$locals$repos[[chave(req)]]
    if (is.null(r)) return(erro(res, 404, "Not Found"))
    res$send_json(r, auto_unbox = TRUE)
  })

  # API de dados do Git ------------------------------------------------------
  ler_ref <- function(tipo) function(req, res) {
    if (!autorizado(req, res)) return()
    sha <- req$app$locals$refs[[paste0(chave(req), "@", tipo, "/", req$params$x)]]
    if (is.null(sha)) return(erro(res, 404, "Not Found"))
    res$send_json(list(ref = paste0("refs/", tipo, "/", req$params$x), object = list(sha = sha, type = "commit")),
                  auto_unbox = TRUE)
  }
  app$get("/repos/:o/:r/git/ref/heads/:x", ler_ref("heads"))
  app$get("/repos/:o/:r/git/ref/tags/:x", ler_ref("tags"))
  app$get("/repos/:o/:r/git/commits/:sha", function(req, res) {
    if (!autorizado(req, res)) return()
    c <- req$app$locals$commits[[req$params$sha]]
    if (is.null(c)) return(erro(res, 404, "Not Found"))
    res$send_json(list(sha = req$params$sha, tree = list(sha = c$tree), message = c$message), auto_unbox = TRUE)
  })
  app$post("/repos/:o/:r/git/blobs", function(req, res) {
    if (!autorizado(req, res)) return()
    L <- req$app$locals
    L$blobs_criados <- L$blobs_criados + 1L
    if (identical(L$blobs_criados, as.integer(L$falhar_blob))) return(erro(res, 502, "Server Error"))
    igual <- Filter(function(s) identical(L$blobs[[s]], req$json$content), names(L$blobs))
    sha <- if (length(igual)) igual[[1]] else { s <- L$novo_sha(L); L$blobs[[s]] <- req$json$content; s }
    res$set_status(201)$send_json(list(sha = sha), auto_unbox = TRUE)
  })
  app$post("/repos/:o/:r/git/trees", function(req, res) {
    if (!autorizado(req, res)) return()
    L <- req$app$locals
    arv <- if (is.null(req$json$base_tree)) list() else L$arvores[[req$json$base_tree]]
    for (e in req$json$tree) arv[[e$path]] <- e$sha  # sha NULL remove o caminho
    arv <- arv[order(names(arv))]
    igual <- Filter(function(s) identical(L$arvores[[s]], arv), names(L$arvores))
    sha <- if (length(igual)) igual[[1]] else { s <- L$novo_sha(L); L$arvores[[s]] <- arv; s }
    res$set_status(201)$send_json(list(sha = sha), auto_unbox = TRUE)
  })
  app$post("/repos/:o/:r/git/commits", function(req, res) {
    if (!autorizado(req, res)) return()
    L <- req$app$locals
    sha <- L$novo_sha(L)
    L$commits[[sha]] <- list(tree = req$json$tree, parents = req$json$parents, message = req$json$message)
    res$set_status(201)$send_json(list(sha = sha), auto_unbox = TRUE)
  })
  app$patch("/repos/:o/:r/git/refs/heads/:x", function(req, res) {
    if (!autorizado(req, res)) return()
    L <- req$app$locals
    k <- paste0(chave(req), "@heads/", req$params$x)
    pais <- unlist(L$commits[[req$json$sha]]$parents)
    if (isTRUE(L$corrida) || !(L$refs[[k]] %in% pais)) return(erro(res, 422, "Update is not a fast forward"))
    L$refs[[k]] <- req$json$sha
    res$send_json(list(ref = paste0("refs/heads/", req$params$x), object = list(sha = req$json$sha)), auto_unbox = TRUE)
  })
  app$post("/repos/:o/:r/git/tags", function(req, res) {
    if (!autorizado(req, res)) return()
    L <- req$app$locals
    sha <- L$novo_sha(L)
    L$tag_objs[[sha]] <- list(tag = req$json$tag, message = req$json$message, object = req$json$object)
    res$set_status(201)$send_json(list(sha = sha), auto_unbox = TRUE)
  })
  app$post("/repos/:o/:r/git/refs", function(req, res) {
    if (!autorizado(req, res)) return()
    L <- req$app$locals
    k <- paste0(chave(req), "@", sub("^refs/", "", req$json$ref))
    if (!is.null(L$refs[[k]])) return(erro(res, 422, "Reference already exists"))
    L$refs[[k]] <- req$json$sha
    res$set_status(201)$send_json(list(ref = req$json$ref, object = list(sha = req$json$sha)), auto_unbox = TRUE)
  })
  app
}
environment(app_github_falso) <- baseenv()

# Um processo por arquivo de teste, zerado a cada `local_github_falso()`.
.falso <- new.env()

github_falso <- function() {
  if (is.null(.falso$proc) || !.falso$proc$get_state() %in% "live") {
    .falso$proc <- webfakes::new_app_process(app_github_falso())
  }
  .falso$proc
}

falso_url <- function(caminho = "") sub("/$", "", github_falso()$url(caminho))

falso_post <- function(caminho, corpo = list()) {
  req <- httr2::request(paste0(falso_url(), caminho))
  httr2::req_perform(httr2::req_body_json(req, corpo, auto_unbox = TRUE))
}

falso_estado <- function() {
  httr2::resp_body_json(httr2::req_perform(httr2::request(paste0(falso_url(), "/_falso/estado"))),
                        simplifyVector = FALSE)
}

# Aponta o pacote para o GitHub falso e isola as fontes de credencial: nenhum
# token da máquina de quem roda o teste vaza para dentro (nem para fora).
local_github_falso <- function(token = TOKEN_FALSO, env = parent.frame()) {
  falso_post("/_falso/zerar")
  withr::local_options(list(trama.github.api_url = falso_url(), trama.github.url = falso_url(),
                            trama.github.client_id = CLIENT_ID_FALSO, keyring_backend = "env"),
                       .local_envir = env)
  withr::local_envvar(c(GITHUB_PAT = token %||% "", GITHUB_TOKEN = ""), .local_envir = env)
  testthat::local_mocked_bindings(.gh_token_gh_cli = function() NULL,
                                  .gh_token_gitcreds = function() NULL,
                                  .package = "trama.github", .env = env)
  withr::defer(tr_github_sair(), envir = env)
  invisible(falso_url())
}

`%||%` <- function(a, b) if (is.null(a)) b else a
