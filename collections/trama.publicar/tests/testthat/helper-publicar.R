# Registro com as coleções de que o publicar depende, e uma coleção de teste
# que só devolve tabelas (`tst/tabela`), para os fluxos pelo motor.
publicar_registry <- function() {
  reg <- trama::tr_registry()
  trama::tr_use("trama.data", registry = reg)
  trama::tr_use("trama.view", registry = reg)
  trama::tr_use("trama.models", registry = reg)
  trama::tr_use(trama_collection(), registry = reg)
  trama::tr_use(trama::tr_collection("tst", nodes = list(
    trama::tr_node("tst/tabela", fn = function(n) utils::head(datasets::mtcars, n),
                   description = "Devolve as primeiras linhas do mtcars.",
                   outputs = list(out = "data/table"), params = list(n = trama::tr_param_int(10L))),
    trama::tr_node("tst/lm", fn = function(dados) trama.models::tr_models_lm(dados, formula = "mpg ~ wt + hp"),
                   description = "Ajusta mpg ~ wt + hp.",
                   inputs = list(dados = "data/table"), outputs = list(out = "models/fit")))),
    registry = reg)
  reg
}

material_exemplo <- function(layout = "") {
  d <- datasets::mtcars
  m <- trama.models::tr_models_lm(d, formula = "mpg ~ wt + hp")
  p <- ggplot2::ggplot(d, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  tr_publicar_material(
    list(tab = .tr_publicar_de_tabela(utils::head(d, 30)), fig = .tr_publicar_de_grafico(p),
         mod = .tr_publicar_de_modelo(m)),
    titulo = "Consumo e peso", descricao = "Material de teste.", palavras_chave = "carros, consumo",
    autores = "Josiah Carberry (0000-0002-1825-0097); Ana Lima", layout = layout)
}

# O doc de um fluxo pequeno (para flow.json e o diagrama).
doc_exemplo <- function(reg = publicar_registry()) {
  f <- trama::tr_flow(reg) |>
    trama::tr_add("tab", "tst/tabela", n = 30L) |>
    trama::tr_add("mod", "tst/lm", from = "tab") |>
    trama::tr_add("pub", "publicar/publish", titulo = "Consumo e peso", autores = "Ana Lima")
  f <- trama::tr_link(f, "tab", "pub")
  f <- trama::tr_link(f, "mod", "pub")
  list(flow = f, doc = trama::tr_flow_doc(f), reg = reg)
}
