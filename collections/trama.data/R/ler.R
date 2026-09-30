# `data/read`: um bloco só para ler tabela, de arquivo local ou de link.
#
# Os cinco leitores por formato (`tr_read_csv`, `tr_read_json`...) continuam
# exportados — são o nível 1, chamáveis no console, e é neles que este bloco
# despacha. O que sumiu da paleta foram os CINCO BLOCOS: documentos antigos
# abrem migrados para `data/read` com o `formato` do bloco de origem (ver
# `.tr_data_migracoes_ler()`).
#
# Link é baixado UMA VEZ para `data/` e daí em diante lido de lá: o fluxo roda
# offline e dá o mesmo resultado amanhã. `copia` é o botão "baixar de novo":
# subir o número refaz o download. A origem de cada cópia fica em
# `data/.origens.json`, que é também o que diz se a cópia está atualizada.

.tr_ler_ext <- c(csv = "csv", tsv = "csv", txt = "csv", json = "json", rds = "rds",
                 parquet = "parquet", xlsx = "excel", xls = "excel", zip = "zip")
.tr_ler_formatos <- c("auto", "csv", "json", "rds", "parquet", "excel")

.tr_ler_eh_url <- function(x) grepl("^https?://", x, ignore.case = TRUE)

#' Link "de navegador" -> link que entrega o arquivo.
#'
#' GitHub: a página `blob` é HTML; o arquivo está em `raw.githubusercontent`.
#' Google Sheets: a planilha é exportada como CSV, mantendo a aba (`gid`).
#' Google Drive: a página de visualização vira o download direto.
#' Qualquer outro link passa intacto.
#' @return `list(url, formato)`; `formato` é `NULL` quando só a extensão decide.
#' @noRd
.tr_ler_url_direta <- function(url) {
  m <- regmatches(url, regexec("^https?://github\\.com/([^/]+)/([^/]+)/(?:blob|raw)/(.+?)(?:[?#].*)?$", url, perl = TRUE))[[1]]
  if (length(m)) return(list(url = sprintf("https://raw.githubusercontent.com/%s/%s/%s", m[2], m[3], m[4]),
                             formato = NULL))
  m <- regmatches(url, regexec("^https?://docs\\.google\\.com/spreadsheets/d/([A-Za-z0-9_-]+)", url))[[1]]
  if (length(m)) {
    gid <- regmatches(url, regexec("[#?&]gid=([0-9]+)", url))[[1]]
    return(list(url = paste0("https://docs.google.com/spreadsheets/d/", m[2], "/export?format=csv",
                             if (length(gid)) paste0("&gid=", gid[2]) else ""),
                formato = "csv"))
  }
  m <- regmatches(url, regexec("^https?://drive\\.google\\.com/(?:file/d/([A-Za-z0-9_-]+)|open\\?id=([A-Za-z0-9_-]+))", url, perl = TRUE))[[1]]
  if (length(m)) {
    id <- if (nzchar(m[2])) m[2] else m[3]
    return(list(url = paste0("https://drive.google.com/uc?export=download&id=", id), formato = NULL))
  }
  list(url = url, formato = NULL)
}

#' Formato pela extensão (sem query string); `NULL` se não reconhece.
#' @noRd
.tr_ler_formato_de <- function(caminho) {
  ext <- tolower(tools::file_ext(sub("[?#].*$", "", caminho)))
  unname(.tr_ler_ext[ext])[1] %|NA|% NULL
}
`%|NA|%` <- function(a, b) if (is.null(a) || is.na(a)) b else a

#' Nome do arquivo local de uma URL: o último trecho do caminho, limpo, SEMPRE
#' com um hash curto da URL. Sem o hash, `a.com/dados.csv` e `b.com/dados.csv`
#' disputariam `data/dados.csv` e se rebaixariam alternadamente a cada
#' execução. O ponto inicial sai (nada de `.origens.json` sobrescrito).
#' @noRd
.tr_ler_nome_local <- function(url, formato) {
  base <- basename(sub("[?#].*$", "", url))
  base <- gsub("[^A-Za-z0-9._-]", "_", utils::URLdecode(base))
  base <- sub("^[._]+", "", base)
  ext_reconhecida <- !is.null(.tr_ler_formato_de(base))
  ext <- if (ext_reconhecida) tools::file_ext(base) else
    c(csv = "csv", json = "json", rds = "rds", parquet = "parquet", excel = "xlsx", zip = "zip")[[formato %||% "csv"]]
  raiz <- if (ext_reconhecida) tools::file_path_sans_ext(base) else base
  if (!nzchar(raiz)) raiz <- "link"
  paste0(substr(raiz, 1, 60), "-", substr(rlang::hash(url), 1, 8), ".", ext)
}

.tr_ler_origens_path <- function(pasta) file.path(pasta, ".origens.json")

.tr_ler_origens <- function(pasta) {
  p <- .tr_ler_origens_path(pasta)
  if (!file.exists(p)) return(list())
  tryCatch(jsonlite::fromJSON(p, simplifyVector = FALSE), error = function(e) list())
}

#' Baixa `url` para `pasta/nome` se não há cópia, ou se a cópia é de uma
#' `copia` menor que a pedida. Devolve o caminho local.
#' @noRd
.tr_ler_baixar <- function(url, pasta, nome, copia) {
  destino <- file.path(pasta, nome)
  origens <- .tr_ler_origens(pasta)
  reg <- origens[[nome]]
  atual <- file.exists(destino) && !is.null(reg) && identical(reg$url, url) &&
    as.numeric(reg$copia %||% 0) >= copia
  if (atual) return(destino)
  dir.create(pasta, recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(fileext = paste0(".", tools::file_ext(nome)))
  ok <- tryCatch({
    suppressWarnings(utils::download.file(url, tmp, mode = "wb", quiet = TRUE))
    file.exists(tmp) && file.size(tmp) > 0
  }, error = function(e) FALSE)
  if (!isTRUE(ok)) {
    rlang::abort(sprintf("Não foi possível baixar %s. Confira o link e a conexão.", url),
                 class = "tr_data_error_download")
  }
  # Página HTML no lugar do arquivo: link privado, login, ou o aviso de
  # "arquivo grande" do Drive. Lida como CSV viraria lixo verde.
  inicio <- tolower(rawToChar(readBin(tmp, "raw", 200L)[readBin(tmp, "raw", 200L) != as.raw(0)]))
  if (grepl("^\\s*(<!doctype html|<html)", inicio)) {
    rlang::abort(sprintf(paste("O link %s devolveu uma página da web, não um arquivo de dados.",
                               "Confira se ele é público e aponta para o arquivo."), url),
                 class = "tr_data_error_download")
  }
  file.copy(tmp, destino, overwrite = TRUE)
  origens[[nome]] <- list(url = url, copia = copia, baixado_em = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"))
  jsonlite::write_json(origens, .tr_ler_origens_path(pasta), auto_unbox = TRUE, pretty = TRUE)
  destino
}

#' Abre um .zip ao lado dele (pasta com o mesmo nome) e escolhe o arquivo de
#' dados: o único, ou o que `membro` nomeia. Vários sem escolha é erro que
#' lista as opções — o card mostra e a pessoa copia o nome.
#' @noRd
.tr_ler_zip <- function(zip, membro) {
  lista <- utils::unzip(zip, list = TRUE)$Name
  # Membro com `..` ou absoluto apontaria para FORA da pasta extraída (o
  # `unzip` descarta o `..` ao gravar, mas o caminho lido seria o original).
  lista <- lista[!grepl("(^|/)\\.\\.(/|$)", lista) & !grepl("^(/|[A-Za-z]:)", lista)]
  dados <- lista[!grepl("/$", lista) & !grepl("(^|/)(__MACOSX|\\.)", lista) &
                   vapply(lista, function(f) !is.null(.tr_ler_formato_de(f)) &&
                            !identical(.tr_ler_formato_de(f), "zip"), logical(1))]
  if (!length(dados)) {
    rlang::abort("O .zip não tem nenhum arquivo de dados reconhecido (csv, json, rds, parquet, xlsx).",
                 class = "tr_data_error_bad_option")
  }
  escolhido <- if (nzchar(membro %||% "")) {
    hit <- dados[dados == membro | basename(dados) == membro]
    if (!length(hit)) .tr_data_option("membro", membro, dados)
    hit[1]
  } else if (length(dados) == 1L) dados else {
    rlang::abort(sprintf("O .zip tem %d arquivos de dados; escolha um em 'Arquivo no zip': %s.",
                         length(dados), paste(dados, collapse = ", ")),
                 class = "tr_data_error_bad_option")
  }
  pasta <- file.path(dirname(zip), tools::file_path_sans_ext(basename(zip)))
  alvo <- file.path(pasta, escolhido)
  if (!file.exists(alvo) || file.mtime(alvo) < file.mtime(zip)) {
    utils::unzip(zip, files = escolhido, exdir = pasta, overwrite = TRUE)
  }
  alvo
}

#' Resolve o param `path` no arquivo local a ler e no formato: baixa link,
#' abre zip. Separado do `tr_read()` pra o fingerprint e os testes usarem o
#' mesmo caminho, sem ler a tabela.
#' @noRd
.tr_ler_resolver <- function(path, formato, membro, copia, .ctx, baixar = TRUE) {
  resolve <- function(p) if (is.null(.ctx)) p else .ctx$path(p)
  if (.tr_ler_eh_url(path)) {
    d <- .tr_ler_url_direta(path)
    fmt <- if (formato != "auto") formato else d$formato %||% .tr_ler_formato_de(d$url) %||% "csv"
    nome <- .tr_ler_nome_local(d$url, if (identical(.tr_ler_formato_de(d$url), "zip")) "zip" else fmt)
    pasta <- resolve("data")
    local <- if (baixar) .tr_ler_baixar(d$url, pasta, nome, copia) else file.path(pasta, nome)
  } else {
    local <- resolve(path)
    fmt <- if (formato != "auto") formato else .tr_ler_formato_de(local)
  }
  if (identical(.tr_ler_formato_de(local), "zip")) {
    if (!baixar) return(list(local = local, formato = NULL))
    .tr_data_arquivo(local, "path")
    local <- .tr_ler_zip(local, membro)
    fmt <- if (formato != "auto") formato else .tr_ler_formato_de(local)
  }
  if (is.null(fmt)) {
    rlang::abort(sprintf("Não reconheci o formato de '%s'. Escolha em 'Formato' (%s).",
                         basename(local), paste(.tr_ler_formatos[-1], collapse = ", ")),
                 class = "tr_data_error_bad_option")
  }
  list(local = local, formato = fmt)
}

#' Lê uma tabela de arquivo local ou de link, reconhecendo o formato.
#'
#' `path` é caminho (relativo ao projeto) ou URL. `formato = "auto"` decide
#' pela extensão; os outros forçam. `delim`/`na` valem para CSV, `sheet` para
#' Excel, `membro` para .zip com vários arquivos. `copia` é a versão da cópia
#' local de um link: aumentar baixa de novo.
#' @export
tr_read <- function(path, formato = "auto", delim = ",", na = "NA", sheet = "1",
                    membro = "", copia = 0, .ctx = NULL) {
  .tr_data_obrigatorio(path, "path")
  if (!formato %in% .tr_ler_formatos) .tr_data_option("formato", formato, .tr_ler_formatos)
  r <- .tr_ler_resolver(trimws(path), formato, membro, as.numeric(copia %||% 0), .ctx)
  switch(r$formato,
    csv = tr_read_csv(r$local, delim = delim, na = na),
    json = tr_read_json(r$local),
    rds = tr_read_rds(r$local),
    parquet = tr_read_parquet(r$local),
    excel = tr_read_excel(r$local, sheet = sheet))
}

#' Fingerprint do `data/read`: SEM rede. Para link, a chave é a URL, a `copia`
#' e o estado da cópia local — estável entre execuções e diferente quando a
#' cópia aparece, muda ou é pedida de novo. Para arquivo, o mesmo de sempre.
#' @noRd
.tr_ler_print <- function(params, ctx) {
  path <- trimws(params$path %||% "")
  if (!.tr_ler_eh_url(path)) return(.tr_data_file_print(params, ctx))
  r <- tryCatch(.tr_ler_resolver(path, params$formato %||% "auto", "", 0, ctx, baixar = FALSE),
                error = function(e) list(local = ""))
  i <- file.info(r$local)
  paste(path, params$copia %||% 0, params$membro %||% "", i$size, i$mtime, sep = "|")
}

#' Blocos por formato -> `data/read`, com o formato que eles implicavam.
#' @noRd
.tr_data_migracoes_ler <- function() {
  para <- function(f) list(to = "data/read", params = list(formato = f))
  list("data/read_csv" = para("csv"), "data/read_json" = para("json"),
       "data/read_rds" = para("rds"), "data/read_parquet" = para("parquet"),
       "data/read_excel" = para("excel"))
}
