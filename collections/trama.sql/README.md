# trama.sql

Coleção R de consulta SQL para fluxos trama. `sql/source` abre pastas com
arquivos CSV, Parquet e JSON, arquivos individuais ou bancos `.duckdb`,
`.sqlite` e `.db`. `sql/query` executa consultas somente de leitura e devolve
uma tabela `data/table`.

DuckDB e DBI são dependências diretas. SQLite é opcional e requer RSQLite;
Parquet e JSON são lidos pelo DuckDB sem copiar os arquivos.
