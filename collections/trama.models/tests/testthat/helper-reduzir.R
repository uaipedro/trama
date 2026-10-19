# Varredura do modelo reduzido (`tr_models_strip`): todo número que o objeto
# carrega, em listas, atributos, ambientes capturados por fórmula ou função e
# no corpo de chamadas. O ambiente global, o base e os de pacote ficam de
# fora: o RDS os grava como referência, sem conteúdo.
numeros_do_objeto <- function(obj) {
  vistos <- new.env()
  out <- list()
  andar <- function(o) {
    if (is.null(o)) return(invisible())
    if (is.environment(o)) {
      if (identical(o, globalenv()) || identical(o, baseenv()) || identical(o, emptyenv()) ||
          isNamespace(o) || nzchar(environmentName(o))) return(invisible())
      id <- format(o)
      if (!is.null(vistos[[id]])) return(invisible())
      assign(id, TRUE, envir = vistos)
      for (nm in ls(o, all.names = TRUE)) andar(get(nm, envir = o))
      return(invisible())
    }
    if (is.function(o)) { andar(environment(o)); return(invisible()) }
    if (is.language(o)) {
      # Argumento vazio (`x[, 1]`) é o símbolo vazio: passá-lo adiante é
      # "argumento ausente". Símbolo nenhum carrega número; só chamadas descem.
      if (is.call(o)) {
        partes <- as.list(o)
        for (i in seq_along(partes)) if (!is.symbol(partes[[i]])) andar(partes[[i]])
      }
      return(invisible())
    }
    if (is.numeric(o) || is.complex(o)) out[[length(out) + 1L]] <<- as.numeric(o)
    if (is.list(o)) for (e in o) andar(e)
    for (a in setdiff(names(attributes(o)), c("names", "row.names", "class", "dim", "dimnames", "levels"))) {
      andar(attr(o, a))
    }
    invisible()
  }
  andar(obj)
  unlist(out, use.names = FALSE)
}

# Quantos valores do treino reaparecem, idênticos, no objeto. Os dados dos
# testes são contínuos (rnorm/runif): coincidência por acaso tem probabilidade
# desprezível, e todo acerto é vazamento.
vazados <- function(obj, valores) intersect(numeros_do_objeto(obj), valores)

# Comprimentos de vetor e de dimensão de matriz, para "nada com uma posição por
# linha do treino".
comprimentos_do_objeto <- function(obj) {
  out <- integer()
  andar <- function(o) {
    if (is.environment(o) || is.function(o) || is.language(o) || is.null(o)) return(invisible())
    if (is.atomic(o)) out <<- c(out, length(o), dim(o))
    if (is.list(o)) { out <<- c(out, length(o)); for (e in o) andar(e) }
    invisible()
  }
  andar(obj)
  out
}

# Dados contínuos dos testes de reduzir e web (rnorm/runif).
dados_reduzir <- function(n = 80) {
  withr::with_seed(20261019, {
    d <- data.frame(x1 = rnorm(n, 10, 2), x2 = runif(n, 0, 5),
                    trat = factor(sample(c("a", "b", "c"), n, replace = TRUE)))
    d$y <- 1 + 0.4 * d$x1 - 0.7 * d$x2 + c(a = 0, b = 1, c = -1)[as.character(d$trat)] + rnorm(n, 0, 0.5)
    d$cont <- rpois(n, exp(0.2 + 0.1 * d$x2))
    d$classe <- factor(ifelse(d$y + rnorm(n) > stats::median(d$y), "sim", "nao"), levels = c("nao", "sim"))
    d
  })
}
novos_reduzir <- data.frame(x1 = c(8.5, 10, 12.25), x2 = c(0.5, 2.5, 4.75), trat = c("a", "b", "c"))
