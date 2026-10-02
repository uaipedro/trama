#' Abre uma fonte local de dados consultável por SQL.
#' @param caminho Pasta, arquivo de dados ou banco local.
#' @return Lista serializável com tipo, caminho e esquema das tabelas.
#' @export
tr_sql_source <- function(caminho) {
  p <- path.expand(caminho)
  if (!file.exists(p) && !dir.exists(p)) stop(sprintf("Caminho n\u00e3o encontrado: %s", caminho), call. = FALSE)
  p <- normalizePath(p, winslash = "/", mustWork = TRUE)
  if (dir.exists(p)) {
    files <- sort(.tr_sql_supported(list.files(p, recursive = TRUE, full.names = TRUE)))
    if (!length(files)) stop("A pasta n\u00e3o cont\u00e9m arquivos CSV, Parquet ou JSON.", call. = FALSE)
    tipo <- "arquivos"
    names <- make.unique(vapply(files, .tr_sql_clean_name, ""), sep = "_")
  } else {
    ext <- tolower(tools::file_ext(p))
    tipo <- switch(ext, csv = "arquivo", parquet = "arquivo", json = "arquivo", duckdb = "duckdb", sqlite = "sqlite", db = "sqlite", stop("Formato n\u00e3o suportado. Use pasta/arquivo CSV, Parquet ou JSON, .duckdb, .sqlite ou .db.", call. = FALSE))
    files <- p
    names <- .tr_sql_clean_name(p)
  }
  out <- list(tipo = tipo, caminho = p, tabelas = list(), arquivos = stats::setNames(as.list(files), names))
  con <- .tr_sql_connect(out)
  on.exit(if (tipo %in% c("duckdb", "arquivos", "arquivo")) DBI::dbDisconnect(con, shutdown = TRUE) else DBI::dbDisconnect(con), add = TRUE)
  if (tipo %in% c("arquivo", "arquivos")) {
    for (nm in names) {
      fp <- out$arquivos[[nm]]
      scan <- switch(tolower(tools::file_ext(fp)), csv = "read_csv_auto", parquet = "read_parquet", json = "read_json_auto")
      DBI::dbExecute(con, sprintf("CREATE VIEW %s AS SELECT * FROM %s(%s)", as.character(DBI::dbQuoteIdentifier(con, nm)), scan, as.character(DBI::dbQuoteString(con, fp))))
    }
  }
  out$tabelas <- .tr_sql_inspect(con)
  out$arquivos <- NULL
  out
}

.tr_sql_clean_name <- function(path) {
  nm <- tools::file_path_sans_ext(basename(path))
  nm <- tolower(gsub("[^[:alnum:]_]+", "_", nm))
  nm <- gsub("^_+|_+$", "", nm)
  if (!nzchar(nm)) "tabela" else if (grepl("^[0-9]", nm)) paste0("t_", nm) else nm
}

.tr_sql_supported <- function(paths) {
  paths[tolower(tools::file_ext(paths)) %in% c("csv", "parquet", "json")]
}

.tr_sql_fingerprint <- function(params) {
  path <- path.expand(params$caminho)
  files <- if (dir.exists(path)) .tr_sql_supported(list.files(path, recursive = TRUE, full.names = TRUE)) else path
  files <- files[file.exists(files)]
  info <- file.info(files)
  ordenados <- order(files, method = "radix")
  list(arquivos = data.frame(caminho = normalizePath(files[ordenados], winslash = "/", mustWork = TRUE),
                             tamanho = info$size[ordenados], mtime = as.numeric(info$mtime[ordenados]),
                             stringsAsFactors = FALSE))
}

.tr_sql_connect <- function(fonte) {
  caminho <- fonte$caminho
  tipo <- fonte$tipo
  if (tipo == "duckdb") return(DBI::dbConnect(duckdb::duckdb(), dbdir = caminho, read_only = TRUE))
  if (tipo == "sqlite") {
    if (!requireNamespace("RSQLite", quietly = TRUE)) stop("Para abrir .sqlite/.db, instale o pacote opcional RSQLite: install.packages('RSQLite').", call. = FALSE)
    return(DBI::dbConnect(RSQLite::SQLite(), dbname = caminho, flags = RSQLite::SQLITE_RO))
  }
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  tryCatch({
    for (tb in fonte$tabelas) {
      matching <- fonte$arquivos[[tb$nome]]
      sql <- switch(tolower(tools::file_ext(matching)),
        csv = sprintf("read_csv_auto(%s)", as.character(DBI::dbQuoteString(con, matching))),
        parquet = sprintf("read_parquet(%s)", as.character(DBI::dbQuoteString(con, matching))),
        json = sprintf("read_json_auto(%s)", as.character(DBI::dbQuoteString(con, matching))))
      DBI::dbExecute(con, sprintf("CREATE VIEW %s AS SELECT * FROM %s", as.character(DBI::dbQuoteIdentifier(con, tb$nome)), sql))
    }
    con
  }, error = function(e) { DBI::dbDisconnect(con, shutdown = TRUE); stop(e) })
}

.tr_sql_inspect <- function(con) {
  tabs <- DBI::dbListTables(con)
  lapply(tabs, function(nm) {
    if (inherits(con, "SQLiteConnection")) {
      fields <- DBI::dbGetQuery(con, sprintf("PRAGMA table_info(%s)", as.character(DBI::dbQuoteString(con, nm))))
      cn <- fields$name; ct <- fields$type
    } else {
      fields <- DBI::dbGetQuery(con, sprintf("DESCRIBE %s", as.character(DBI::dbQuoteIdentifier(con, nm))))
      cn <- fields[["column_name"]]; ct <- fields[["column_type"]]
    }
    list(nome = nm, colunas = lapply(seq_along(cn), function(i) list(nome = cn[i], tipo = ct[i])))
  })
}

#' Executa uma consulta somente de leitura.
#' @param fonte Valor produzido por [tr_sql_source()].
#' @param consulta Instrução SQL iniciada por SELECT ou WITH.
#' @return Tabela data.frame com o resultado.
#' @export
tr_sql_query <- function(fonte, consulta) {
  sql <- trimws(consulta)
  if (!grepl("^(SELECT|WITH)\\b", sql, ignore.case = TRUE, perl = TRUE))
    stop("A consulta s\u00f3 pode come\u00e7ar com SELECT ou WITH.", call. = FALSE)
  # Block mutations embedded in WITH and multiple statements.
  if (grepl(";\\s*\\S|\\b(INSERT|UPDATE|DELETE|DROP|CREATE|ALTER|COPY|ATTACH|DETACH|INSTALL|LOAD|EXPORT|IMPORT|CALL|PRAGMA)\\b", sql, ignore.case = TRUE, perl = TRUE))
    stop("A consulta precisa conter uma \u00fanica instru\u00e7\u00e3o de leitura (SELECT/WITH).", call. = FALSE)
  # The compact fuente contract stores no connection; reconstruct file views locally.
  con <- if (fonte$tipo %in% c("duckdb", "sqlite")) .tr_sql_connect(fonte) else DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(if (fonte$tipo == "sqlite") DBI::dbDisconnect(con) else DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  if (fonte$tipo %in% c("arquivo", "arquivos")) {
    files <- if (dir.exists(fonte$caminho)) sort(.tr_sql_supported(list.files(fonte$caminho, recursive = TRUE, full.names = TRUE))) else fonte$caminho
    names <- make.unique(vapply(files, .tr_sql_clean_name, ""), sep = "_")
    for (i in seq_along(files)) {
      fp <- files[[i]]; scan <- switch(tolower(tools::file_ext(fp)), csv = "read_csv_auto", parquet = "read_parquet", json = "read_json_auto")
      DBI::dbExecute(con, sprintf("CREATE VIEW %s AS SELECT * FROM %s(%s)", as.character(DBI::dbQuoteIdentifier(con, names[[i]])), scan, as.character(DBI::dbQuoteString(con, fp))))
    }
  }
  DBI::dbGetQuery(con, sql)
}
