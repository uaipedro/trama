#!/usr/bin/env Rscript
# Roda só os testes afetados pelas mudanças.
#
#   Rscript tools/check.R                 # mudanças do working tree vs HEAD
#   Rscript tools/check.R --base main     # mudanças desde main (branch/PR)
#   Rscript tools/check.R --rapido        # só os arquivos de teste pareados
#   Rscript tools/check.R --plano         # mostra o que rodaria, sem rodar
#   Rscript tools/check.R --tudo          # suíte inteira
#   Rscript tools/check.R trama.series    # alvo explícito (+ dependentes)
#
# Regras:
#   collections/X/**          -> X e toda coleção que depende de X (transitivo)
#   R/, inst/ (exceto www)    -> núcleo e todas as coleções
#   inst/www/**, tests/js/**  -> testes node
#   site/**                   -> testes do site
#   docs/**, *.md             -> nada
# Em --rapido, R/foo.R roda só test-foo.R do mesmo pacote e não sobe para
# dependentes. Sem par, roda a suíte do pacote.

args <- commandArgs(trailingOnly = TRUE)
flag <- function(f) f %in% args
opt <- function(f) { i <- match(f, args); if (is.na(i)) NULL else args[i + 1] }

raiz <- normalizePath(file.path(dirname(sub("--file=", "", grep("--file=", commandArgs(), value = TRUE))), ".."))
setwd(raiz)

colecoes <- basename(list.dirs("collections", recursive = FALSE))

# Grafo: dependências de cada coleção entre os pacotes trama*.
deps <- lapply(setNames(colecoes, colecoes), function(c) {
  d <- read.dcf(file.path("collections", c, "DESCRIPTION"))
  campos <- intersect(c("Depends", "Imports", "Suggests", "LinkingTo"), colnames(d))
  nomes <- trimws(sub("\\(.*", "", unlist(strsplit(paste(d[, campos], collapse = ","), ","))))
  intersect(nomes, colecoes)
})

dependentes <- function(alvos) {
  repeat {
    novos <- names(Filter(function(d) any(d %in% alvos), deps))
    if (all(novos %in% alvos)) return(alvos)
    alvos <- union(alvos, novos)
  }
}

ordem_carga <- function(c) {
  vistos <- character()
  visita <- function(x) { for (d in deps[[x]]) visita(d); if (!x %in% vistos) vistos <<- c(vistos, x) }
  visita(c)
  vistos
}

arquivos_mudados <- function() {
  base <- opt("--base")
  git <- function(...) system2("git", c(...), stdout = TRUE)
  if (!is.null(base)) {
    mb <- git("merge-base", base, "HEAD")
    return(unique(c(git("diff", "--name-only", mb), git("ls-files", "--others", "--exclude-standard"))))
  }
  unique(c(git("diff", "--name-only", "HEAD"), git("ls-files", "--others", "--exclude-standard")))
}

# Plano: lista de pacote -> NULL (suíte inteira) ou vetor de filtros de teste.
plano <- list(pacotes = list(), js = FALSE, site = FALSE)
marca <- function(p, filtro = NULL) {
  atual <- plano$pacotes[[p]]
  if (p %in% names(plano$pacotes) && is.null(atual)) return()
  plano$pacotes[p] <<- list(if (is.null(filtro)) NULL else union(atual, filtro))
}

rapido <- flag("--rapido")
explicitos <- intersect(args, c("trama", colecoes))

if (flag("--tudo")) {
  for (p in c("trama", colecoes)) marca(p)
  plano$js <- plano$site <- TRUE
} else if (length(explicitos)) {
  for (p in explicitos) marca(p)
  if (!rapido) for (p in dependentes(setdiff(explicitos, "trama"))) marca(p)
  if ("trama" %in% explicitos && !rapido) for (p in colecoes) marca(p)
} else {
  mudados <- arquivos_mudados()
  mudados <- mudados[!grepl("(^|/)_problems/|testthat-problems\\.rds$|Rplots\\.pdf$", mudados)]
  # DESCRIPTION que só subiu Version não muda comportamento.
  so_versao <- function(f) {
    if (basename(f) != "DESCRIPTION") return(FALSE)
    ref <- if (is.null(opt("--base"))) "HEAD" else system2("git", c("merge-base", opt("--base"), "HEAD"), stdout = TRUE)
    linhas <- system2("git", c("diff", "-U0", ref, "--", f), stdout = TRUE)
    linhas <- grep("^[+-][^+-]", linhas, value = TRUE)
    length(linhas) > 0 && all(grepl("^[+-]Version:", linhas))
  }
  mudados <- mudados[!vapply(mudados, so_versao, logical(1))]
  for (f in mudados) {
    partes <- strsplit(f, "/")[[1]]
    if (partes[1] == "collections" && length(partes) >= 3) {
      c <- partes[2]
      if (!c %in% colecoes) next
      if (grepl("\\.md$", f) && partes[3] != "R") next
      pareado <- NULL
      if (partes[3] == "R") pareado <- sub("\\.R$", "", partes[4])
      if (partes[3] == "tests" && grepl("^test-.*\\.R$", partes[length(partes)]))
        pareado <- sub("^test-(.*)\\.R$", "\\1", partes[length(partes)])
      tem_par <- !is.null(pareado) &&
        file.exists(file.path("collections", c, "tests/testthat", paste0("test-", pareado, ".R")))
      marca(c, if (rapido && tem_par) pareado)
      if (!rapido && partes[3] != "tests") for (d in setdiff(dependentes(c), c)) marca(d)
      # O núcleo testa os templates de todas as coleções e usa trama.data.
      if (!rapido && partes[3] == "inst" && identical(partes[4], "templates")) marca("trama", "template")
      if (!rapido && c == "trama.data" && partes[3] == "R") marca("trama", c("etl", "pool"))
      # JS de coleção com teste em tests/js (lint do editor SQL).
      if (partes[3] == "inst" && grepl("\\.js$", partes[length(partes)])) plano$js <- TRUE
    } else if (partes[1] %in% c("R", "inst", "DESCRIPTION", "NAMESPACE") &&
               !identical(partes[1:2], c("inst", "www"))) {
      pareado <- if (partes[1] == "R") sub("\\.R$", "", partes[2])
      tem_par <- !is.null(pareado) && file.exists(file.path("tests/testthat", paste0("test-", pareado, ".R")))
      marca("trama", if (rapido && tem_par) pareado)
      if (!rapido) for (c in colecoes) marca(c)
    } else if (partes[1] == "tests" && identical(partes[2], "testthat")) {
      nome <- partes[length(partes)]
      marca("trama", if (grepl("^test-.*\\.R$", nome)) sub("^test-(.*)\\.R$", "\\1", nome))
    } else if (partes[1] == "inst" || identical(partes[1:2], c("tests", "js"))) {
      plano$js <- TRUE
    } else if (partes[1] == "site") {
      plano$site <- TRUE
    }
  }
}

# Núcleo primeiro, depois coleções na ordem de dependência.
ordem <- c("trama", unique(unlist(lapply(colecoes, ordem_carga))))
alvos <- intersect(ordem, names(plano$pacotes))

cat("Plano de testes\n")
if (!length(alvos) && !plano$js && !plano$site) { cat("  nada afetado.\n"); quit(status = 0) }
for (p in alvos) {
  f <- plano$pacotes[[p]]
  cat(sprintf("  %-18s %s\n", p, if (is.null(f)) "suíte inteira" else paste0("test-", f, ".R", collapse = " ")))
}
if (plano$js) cat("  node               tests/js\n")
if (plano$site) cat("  site               src/lib/*.test.ts\n")
if (flag("--plano")) quit(status = 0)

falhas <- character()
roda <- function(nome, cmd, argumentos) {
  cat(sprintf("\n== %s\n", nome))
  t0 <- Sys.time()
  status <- system2(cmd, argumentos)
  cat(sprintf("-- %s: %s em %.0fs\n", nome, if (status == 0) "ok" else "FALHOU",
              as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  if (status != 0) falhas <<- c(falhas, nome)
}

for (p in alvos) {
  dir_pkg <- if (p == "trama") "." else file.path("collections", p)
  cargas <- if (p == "trama") character() else ordem_carga(p)
  filtro <- plano$pacotes[[p]]
  codigo <- paste0(
    # Testes do núcleo usam internas (`.tr_*`) sem `:::`; os das coleções não
    # podem vê-las, senão passariam dependendo do que não é API.
    sprintf("suppressMessages(pkgload::load_all('.', quiet = TRUE, export_all = %s));", p == "trama"),
    paste0(sprintf("suppressMessages(pkgload::load_all('collections/%s', quiet = TRUE));", cargas), collapse = ""),
    sprintf("r <- testthat::test_dir('%s/tests/testthat', filter = %s, stop_on_failure = FALSE, reporter = 'summary', load_package = 'none');",
            dir_pkg, if (is.null(filtro)) "NULL" else deparse(paste0("^(", paste(filtro, collapse = "|"), ")$"))),
    "d <- as.data.frame(r); quit(status = as.integer(sum(d$failed) + sum(d$error) > 0))"
  )
  roda(p, "Rscript", c("-e", shQuote(codigo)))
}
if (plano$js) roda("node", "node", c("--test", shQuote("tests/js/*.test.mjs")))
if (plano$site) roda("site", "npm", c("--prefix", "site", "test", "--silent"))

if (length(falhas)) { cat("\nFalhou:", paste(falhas, collapse = ", "), "\n"); quit(status = 1) }
cat("\nTudo ok.\n")
