skip_sem_python <- function() {
  skip_if_not_installed("reticulate")
  ok <- tryCatch({ trama.python:::.tr_py_mod(); TRUE }, error = function(e) FALSE)
  skip_if_not(ok, "Python/scikit-learn indispon\u{ED}vel")
}

py_registry <- function() {
  r <- trama::tr_registry()
  for (p in c("trama.data", "trama.view", "trama.models")) trama::tr_use(p, registry = r)
  trama::tr_use("trama.python", registry = r)
  r
}

# O sklearn chamado DIRETO, sem a ponte: o oráculo da tradução R <-> Python.
sk <- function() reticulate::import("sklearn.ensemble")
