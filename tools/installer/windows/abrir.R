# abrir.R — o que o atalho "Trama" do menu Iniciar roda (Trama-Setup.iss,
# `Rscript.exe "{app}\abrir.R"`, sem janela de console). Acha a lib da
# release atual em estado.json (a mesma lib onde bootstrap.R instalou o
# trama.launcher, task 3.1) e delega para abrir(). Não usa jsonlite aqui de
# propósito: jsonlite só está disponível *dentro* dessa lib, e ainda não
# sabemos onde ela é.
estado_arquivo <- file.path(Sys.getenv("LOCALAPPDATA"), "Trama", "estado.json")
if (file.exists(estado_arquivo)) {
  conteudo <- paste(readLines(estado_arquivo, warn = FALSE), collapse = " ")
  encontrado <- regmatches(conteudo, regexpr('"atual"\\s*:\\s*"[^"]*"', conteudo))
  atual <- if (length(encontrado) && nzchar(encontrado)) {
    sub('.*:\\s*"([^"]*)"', "\\1", encontrado)
  } else ""
  if (nzchar(atual)) {
    lib <- file.path(Sys.getenv("LOCALAPPDATA"), "Trama", "lib", atual)
    if (dir.exists(lib)) .libPaths(c(lib, .libPaths()))
  }
}

trama.launcher::abrir()
