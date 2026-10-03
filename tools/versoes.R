# Confere o piso `trama (>= x)` de cada coleção contra a API do núcleo.
#
#   Rscript tools/versoes.R            # relatório; sai 0
#   Rscript tools/versoes.R --estrito  # sai 1 se algum piso estiver atrasado
#
# Lê o NEWS.md do núcleo: toda menção a `tr_f()` ou `tr_f(arg = ...)` sob
# "# trama x.y.z" conta como API que existe a partir de x.y.z (a mais antiga
# menção vence). Uma coleção que usa essa API com piso menor é acusada.
# É heurístico: só vê API de R citada no NEWS (não vê, p.ex., `ctx` de widget
# JS) e o uso de argumento é casado por arquivo, não por chamada.

args <- commandArgs(trailingOnly = TRUE)
raiz <- normalizePath(file.path(dirname(sub("--file=", "", grep("--file=", commandArgs(), value = TRUE))), ".."))
setwd(raiz)

versao_nucleo <- read.dcf("DESCRIPTION", fields = "Version")[1, 1]

# API por versão, do NEWS do núcleo.
news <- readLines("NEWS.md", encoding = "UTF-8")
api <- list()
versao <- NA_character_
for (l in news) {
  if (grepl("^# trama ", l)) { versao <- sub("^# trama ([0-9.]+).*", "\\1", l); next }
  if (is.na(versao)) next
  m <- regmatches(l, gregexpr("`tr_[A-Za-z0-9_.]+\\([^`]*\\)`", l))[[1]]
  for (x in m) {
    fn <- sub("^`(tr_[A-Za-z0-9_.]+)\\(.*", "\\1", x)
    argumentos <- regmatches(x, gregexpr("[A-Za-z_.][A-Za-z0-9_.]*(?=\\s*=)", x, perl = TRUE))[[1]]
    chaves <- if (length(argumentos)) paste0(fn, "$", argumentos) else fn
    for (k in chaves) api[[k]] <- versao  # NEWS vai do mais novo ao mais antigo
  }
}

piso_de <- function(desc) {
  imp <- read.dcf(desc, fields = c("Imports", "Depends"))
  txt <- paste(imp[!is.na(imp)], collapse = ",")
  m <- regmatches(txt, regexpr("trama\\s*\\(>=\\s*[0-9.]+\\)", txt))
  if (!length(m)) NA_character_ else sub(".*>=\\s*([0-9.]+).*", "\\1", m)
}

# Texto de cada chamada `fn(...)`, com parênteses balanceados.
chamadas_de <- function(txt, fn) {
  inicios <- gregexpr(paste0("\\b", gsub(".", "\\.", fn, fixed = TRUE), "\\("), txt)[[1]]
  if (inicios[1] < 0) return(character())
  ch <- strsplit(txt, "")[[1]]
  vapply(inicios + attr(inicios, "match.length") - 1L, function(i) {
    nivel <- 0L; j <- i
    repeat {
      if (ch[j] == "(") nivel <- nivel + 1L
      if (ch[j] == ")") nivel <- nivel - 1L
      if (nivel == 0L || j == length(ch)) break
      j <- j + 1L
    }
    paste(ch[i:j], collapse = "")
  }, character(1))
}

atrasos <- 0L
cat(sprintf("Núcleo trama %s\n\n", versao_nucleo))
cat(sprintf("%-20s %-8s %-8s %s\n", "coleção", "versão", "piso", "observações"))
for (dir in list.dirs("collections", recursive = FALSE)) {
  desc <- file.path(dir, "DESCRIPTION")
  if (!file.exists(desc)) next
  nome <- basename(dir)
  piso <- piso_de(desc)
  obs <- character()
  if (is.na(piso)) obs <- c(obs, "sem piso trama (>=)")
  if (!file.exists(file.path(dir, "NEWS.md"))) obs <- c(obs, "sem NEWS.md")
  fontes <- list.files(file.path(dir, "R"), "\\.R$", full.names = TRUE)
  codigo <- lapply(setNames(fontes, fontes), readLines, warn = FALSE)
  for (k in names(api)) {
    v <- api[[k]]
    if (!is.na(piso) && package_version(v) <= package_version(piso)) next
    partes <- strsplit(k, "$", fixed = TRUE)[[1]]
    usa <- vapply(codigo, function(linhas) {
      chamadas <- chamadas_de(paste(linhas, collapse = "\n"), partes[1])
      if (length(partes) == 1) length(chamadas) > 0
      else any(grepl(paste0("[(,]\\s*", partes[2], "\\s*="), chamadas))
    }, logical(1))
    if (any(usa)) {
      obs <- c(obs, sprintf("usa %s (>= %s)", if (length(partes) == 1) paste0(k, "()") else sprintf("%s(%s =)", partes[1], partes[2]), v))
      atrasos <- atrasos + 1L
    }
  }
  cat(sprintf("%-20s %-8s %-8s %s\n", nome, read.dcf(desc, fields = "Version")[1, 1],
              if (is.na(piso)) "-" else piso, paste(obs, collapse = "; ")))
}

if (atrasos) cat(sprintf("\n%d uso(s) de API mais nova que o piso.\n", atrasos))
if (atrasos && "--estrito" %in% args) quit(status = 1)
