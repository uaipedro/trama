#' Coleção SQL.
#' @export
trama_collection <- function() {
  sql_type <- trama::tr_type("sql/source", version = 1L, label = "Fonte SQL",
    preview = function(x, ctx) trama::tr_preview("sql/source", data = list(tabelas = x$tabelas)))
  trama::tr_collection("sql", version = "0.1.0", label = "SQL",
    types = list(sql_type),
    categories = list(trama::tr_category("source", "Fonte", role = "origem"),
                      trama::tr_category("transform", "Consultar", role = "preparacao")),
    nodes = list(
      trama::tr_node("sql/source", tr_sql_source, label = "Fonte SQL", category = "source",
        description = "Exp\u00f5e arquivos ou um banco local como tabelas para consulta SQL.",
        outputs = list(out = "sql/source"), params = list(caminho = trama::tr_param("text", "", "Caminho", example = "dados/")),
        pure = FALSE, fingerprint = .tr_sql_fingerprint,
        help = "## Descri\u00e7\u00e3o\n\nAbre uma pasta de arquivos, um arquivo CSV/Parquet/JSON ou um banco DuckDB/SQLite. Arquivos numa pasta viram views, sem c\u00f3pia dos dados.\n\n## Par\u00e2metros\n\n- **Caminho** \u2014 pasta ou arquivo local.\n\n## Valor\n\nA lista de tabelas e colunas dispon\u00edveis para o bloco Consultar.\n\n## Exemplos\n\nLigue sql/query a esta fonte para consultar as tabelas.\n\n## Veja tamb\u00e9m\n\nsql/query executa a consulta."),
      trama::tr_node("sql/query", tr_sql_query, label = "Consultar com SQL", category = "transform",
        description = "Executa uma consulta de leitura e devolve uma tabela.",
        inputs = list(fonte = "sql/source"), outputs = list(out = "data/table"),
        params = list(consulta = trama::tr_param("sql", "", "Consulta", example = "SELECT * FROM vendas")),
        help = "## Descri\u00e7\u00e3o\n\nExecuta SQL de leitura sobre a fonte ligada e devolve uma tabela data/table.\n\n## Par\u00e2metros\n\n- **Consulta** \u2014 instru\u00e7\u00e3o iniciada por SELECT ou WITH.\n\n## Valor\n\nUma tabela que pode seguir pelo fluxo de dados.\n\n## Exemplos\n\nSELECT * FROM vendas\n\n## Veja tamb\u00e9m\n\nsql/source define as tabelas dispon\u00edveis."))
  )
}
