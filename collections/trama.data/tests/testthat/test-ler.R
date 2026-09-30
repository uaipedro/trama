# data/read: o bloco único de leitura, local ou por link.

ctx_em <- function(raiz) list(path = function(p) if (grepl("^/", p)) p else file.path(raiz, p))

test_that("cada formato lê igual ao leitor antigo", {
  d <- withr::local_tempdir()
  df <- df_exemplo()
  readr::write_csv(df, f_csv <- file.path(d, "a.csv"))
  jsonlite::write_json(df, f_json <- file.path(d, "a.json"))
  saveRDS(df, f_rds <- file.path(d, "a.rds"))
  expect_identical(tr_read(f_csv), tr_read_csv(f_csv))
  expect_identical(tr_read(f_json), tr_read_json(f_json))
  expect_identical(tr_read(f_rds), tr_read_rds(f_rds))
  # formato forçado vence a extensão
  file.copy(f_csv, f_txt <- file.path(d, "a.dados"))
  expect_error(tr_read(f_txt), class = "tr_data_error_bad_option")
  expect_identical(tr_read(f_txt, formato = "csv"), tr_read_csv(f_txt))
  if (requireNamespace("arrow", quietly = TRUE)) {
    arrow::write_parquet(df, f_pq <- file.path(d, "a.parquet"))
    expect_identical(tr_read(f_pq), tr_read_parquet(f_pq))
  }
  expect_error(tr_read(""), class = "tr_data_error_blank_param")
})

test_that("links de navegador viram links de arquivo", {
  u <- .tr_ler_url_direta
  expect_equal(u("https://github.com/o/r/blob/main/pasta/x.csv")$url,
               "https://raw.githubusercontent.com/o/r/main/pasta/x.csv")
  expect_equal(u("https://github.com/o/r/raw/v1/x.csv?plain=1")$url,
               "https://raw.githubusercontent.com/o/r/v1/x.csv")
  s <- u("https://docs.google.com/spreadsheets/d/AbC-1_z/edit#gid=77")
  expect_equal(s$url, "https://docs.google.com/spreadsheets/d/AbC-1_z/export?format=csv&gid=77")
  expect_equal(s$formato, "csv")
  expect_equal(u("https://docs.google.com/spreadsheets/d/AbC/edit?usp=sharing")$url,
               "https://docs.google.com/spreadsheets/d/AbC/export?format=csv")
  expect_equal(u("https://drive.google.com/file/d/XyZ_9/view?usp=sharing")$url,
               "https://drive.google.com/uc?export=download&id=XyZ_9")
  expect_equal(u("https://drive.google.com/open?id=XyZ")$url,
               "https://drive.google.com/uc?export=download&id=XyZ")
  expect_equal(u("https://exemplo.org/dados.csv")$url, "https://exemplo.org/dados.csv")
})

# Servidor HTTP local num processo à parte: `download.file` bloqueia este R,
# e o httpuv precisa de laço de eventos para responder. O download é de
# verdade, sem depender de internet.
servidor_bg <- function(dir) {
  porta <- httpuv::randomPort()
  script <- tempfile(fileext = ".R")
  writeLines(sprintf('
    n <- 0L; log <- %s
    s <- httpuv::startServer("127.0.0.1", %d, list(call = function(req) {
      n <<- n + 1L; writeLines(as.character(n), log)
      f <- file.path(%s, sub("^/", "", req$PATH_INFO))
      if (!file.exists(f)) return(list(status = 404L, headers = list(), body = "nada"))
      list(status = 200L, headers = list("Content-Type" = "application/octet-stream"),
           body = readBin(f, "raw", file.size(f)))
    }))
    while (TRUE) httpuv::service(100)', deparse(file.path(dir, ".n")), porta, deparse(dir)), script)
  pid <- sys_bg(script)
  for (i in 1:50) { if (tryCatch({ close(socketConnection("127.0.0.1", porta, timeout = 1)); TRUE },
                                 error = function(e) FALSE, warning = function(w) FALSE)) break
                    Sys.sleep(0.1) }
  list(url = sprintf("http://127.0.0.1:%d/", porta),
       pedidos = function() { f <- file.path(dir, ".n"); if (file.exists(f)) as.integer(readLines(f)) else 0L },
       parar = function() tools::pskill(pid))
}
sys_bg <- function(script) {
  pidf <- tempfile()
  system2(file.path(R.home("bin"), "Rscript"), c("-e", shQuote(sprintf(
    'writeLines(as.character(Sys.getpid()), %s); source(%s)', deparse(pidf), deparse(script)))),
    wait = FALSE, stdout = FALSE, stderr = FALSE)
  for (i in 1:50) { if (file.exists(pidf) && length(readLines(pidf))) break; Sys.sleep(0.1) }
  as.integer(readLines(pidf))
}

test_that("link é baixado uma vez para data/, lido offline, e 'copia' baixa de novo", {
  skip_on_cran()
  srv_dir <- withr::local_tempdir(); proj <- withr::local_tempdir()
  readr::write_csv(df_exemplo(), file.path(srv_dir, "vendas.csv"))
  srv <- servidor_bg(srv_dir); on.exit(srv$parar(), add = TRUE)
  url <- paste0(srv$url, "vendas.csv")
  ctx <- ctx_em(proj)

  a <- tr_read(url, .ctx = ctx)
  expect_equal(a, tr_read_csv(file.path(srv_dir, "vendas.csv")))
  local <- list.files(file.path(proj, "data"), pattern = "^vendas-[0-9a-f]{8}\\.csv$")
  expect_length(local, 1L)
  origens <- jsonlite::fromJSON(file.path(proj, "data", ".origens.json"))
  expect_equal(origens[[local]]$url, url)
  expect_equal(srv$pedidos(), 1L)

  # Segunda leitura: nenhum pedido novo, mesmo com o arquivo remoto mudado.
  readr::write_csv(df_exemplo()[1:2, ], file.path(srv_dir, "vendas.csv"))
  expect_equal(nrow(tr_read(url, .ctx = ctx)), nrow(a))
  expect_equal(srv$pedidos(), 1L)

  # Fingerprint não usa rede e muda quando a cópia muda.
  fp1 <- .tr_ler_print(list(path = url, copia = 0), ctx)
  expect_identical(fp1, .tr_ler_print(list(path = url, copia = 0), ctx))

  # copia = 1: baixa de novo e passa a ver a versão nova.
  expect_equal(nrow(tr_read(url, copia = 1, .ctx = ctx)), 2L)
  expect_equal(srv$pedidos(), 2L)
  expect_false(identical(fp1, .tr_ler_print(list(path = url, copia = 1), ctx)))

  expect_error(tr_read(paste0(srv$url, "naoexiste.csv"), .ctx = ctx), class = "tr_data_error_download")
  writeLines("<!DOCTYPE html><html><body>login</body></html>", file.path(srv_dir, "pagina.csv"))
  expect_error(tr_read(paste0(srv$url, "pagina.csv"), .ctx = ctx), "página da web",
               class = "tr_data_error_download")
})

test_that("zip com um arquivo é lido direto; com vários, pede 'membro'", {
  d <- withr::local_tempdir()
  withr::with_dir(d, {
    readr::write_csv(df_exemplo(), "um.csv")
    utils::zip("so.zip", "um.csv", flags = "-q")
    readr::write_csv(df_exemplo()[1:2, ], "dois.csv")
    utils::zip("varios.zip", c("um.csv", "dois.csv"), flags = "-q")
  })
  expect_equal(tr_read(file.path(d, "so.zip")), tr_read_csv(file.path(d, "um.csv")))
  e <- tryCatch(tr_read(file.path(d, "varios.zip")), error = identity)
  expect_s3_class(e, "tr_data_error_bad_option")
  expect_match(conditionMessage(e), "dois.csv")
  expect_equal(nrow(tr_read(file.path(d, "varios.zip"), membro = "dois.csv")), 2L)
  expect_error(tr_read(file.path(d, "varios.zip"), membro = "tres.csv"), class = "tr_data_error_bad_option")
})

test_that("documento com os cinco leitores antigos abre migrado para data/read", {
  reg <- data_registry()
  doc <- trama::tr_doc()
  antigos <- c(csv = "data/read_csv", json = "data/read_json", rds = "data/read_rds",
               parquet = "data/read_parquet", excel = "data/read_excel")
  j <- jsonlite::fromJSON(trama::tr_doc_json(doc), simplifyVector = FALSE)
  j$nodes <- stats::setNames(lapply(seq_along(antigos), function(i) list(
    type = unname(antigos[i]), position = list(0, 100 * i), params = list(path = "x"))),
    paste0("n", seq_along(antigos)))
  migrado <- trama::tr_doc_migrate(j, reg)
  for (i in seq_along(antigos)) {
    n <- migrado$nodes[[paste0("n", i)]]
    expect_equal(n$type, "data/read")
    expect_equal(n$params$formato, names(antigos)[i])
    expect_equal(n$params$path, "x")
  }
})

test_that("nome local: hash sempre, sem ponto inicial, links distintos não colidem", {
  a <- .tr_ler_nome_local("https://a.com/x/dados.csv", "csv")
  b <- .tr_ler_nome_local("https://b.com/y/dados.csv", "csv")
  expect_match(a, "^dados-[0-9a-f]{8}\\.csv$")
  expect_false(identical(a, b))
  o <- .tr_ler_nome_local("https://h.com/.origens.json", "json")
  expect_false(startsWith(o, "."))
  expect_match(.tr_ler_nome_local("https://drive.google.com/uc?export=download&id=1", "excel"), "\\.xlsx$")
})

test_that("membro de zip com .. é ignorado", {
  d <- withr::local_tempdir()
  dentro <- file.path(d, "a", "b"); dir.create(dentro, recursive = TRUE)
  readr::write_csv(df_exemplo(), file.path(d, "a", "fora.csv"))
  readr::write_csv(df_exemplo()[1:2, ], file.path(dentro, "ok.csv"))
  withr::with_dir(dentro, utils::zip("z.zip", c("../fora.csv", "ok.csv"), flags = "-q"))
  expect_equal(nrow(tr_read(file.path(dentro, "z.zip"))), 2L)
  expect_error(tr_read(file.path(dentro, "z.zip"), membro = "../fora.csv"), class = "tr_data_error_bad_option")
})
