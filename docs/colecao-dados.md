# Coleção Dados

A coleção `trama.data` lê, inspeciona, transforma, reorganiza e grava tabelas. Os nós de tabelas recebem ou devolvem `data/table`.

## Escolha da operação

| Objetivo | Nó | Resultado |
|---|---|---|
| Conhecer colunas, tipos e faltantes | `data/summary` | Uma linha por coluna |
| Localizar repetidas | `data/get_dupes` | Linhas e sua contagem |
| Remover repetidas | `data/distinct` | Uma ocorrência por combinação |
| Criar medida por linha | `data/mutate` | Tabela com nova coluna |
| Calcular medida por grupo | `data/group_summarise` | Uma linha por grupo |
| Reter linhas por condição | `data/filter` | Subconjunto de linhas |
| Alterar tipos | `data/convert` | Colunas convertidas |
| Reorganizar colunas | `data/pivot_longer` ou `data/pivot_wider` | Novo formato |
| Combinar tabelas pela chave | `data/join` | Colunas das duas tabelas |
| Acrescentar tabelas | `data/bind_rows` | Linhas de todas as tabelas |

## Leitura, inspeção e limpeza

`data/read_csv`, `data/read_excel`, `data/read_json`, `data/read_rds` e
`data/read_parquet` iniciam o fluxo a partir de arquivos. A inspeção com
`data/summary` é posicionada após a leitura, pois informa tipo, faltantes,
valores distintos, extremos e exemplo de cada coluna.

No CSV, células vazias são faltantes. Textos como `-`, `n/d` ou `sem dado`
devem constar em **Marcas de faltante**, pois de outro modo permanecem como
texto. Planilhas Excel frequentemente recebem `data/clean_names`, uma vez que
títulos podem conter espaços, acentos ou nomes repetidos.

```r
tr_flow(reg) |>
  tr_add("ler", "data/read_csv", path = "dados/vendas.csv",
         delim = ";", na = "NA, -, n/d") |>
  tr_add("perfil", "data/summary", from = "ler") |>
  tr_add("faltantes", "data/arrange", cols = "faltantes", desc = TRUE,
         from = "perfil")
```

`data/get_dupes` é usado antes de `data/distinct` quando as linhas removidas
precisam ser verificadas. `data/remove_empty` elimina linhas ou colunas
inteiramente vazias; `data/drop_na` elimina linhas com faltantes em colunas
indicadas; `data/replace_na` atribui um valor aos faltantes. A escolha depende
do significado da ausência.

## Transformações sem agregação

`data/mutate` calcula uma ou mais colunas e mantém as linhas. A expressão usa
os nomes das colunas diretamente. Com **Por grupo**, o cálculo é feito dentro de
cada grupo, mas o resultado continua associado a cada linha.

```r
tr_flow(reg) |>
  tr_add("margem", "data/mutate", name = "margem, margem_pct",
         expr = "receita - custo, 100 * margem / receita", from = "ler")
```

`data/filter` retém as linhas para as quais a expressão produz `TRUE`.
`data/select` mantém ou remove colunas; `data/rename` altera nomes.
`data/convert` ajusta número, texto, data, hora, lógico e fator. Uma coluna
com valores `1.234,56` deve ser convertida com separador decimal `,`, pois a
leitura usualmente a classifica como texto.

`data/arrange` define uma ordem e `data/slice_head` retém as primeiras
`N` linhas. Com **Por grupo**, são retidas as primeiras linhas de cada grupo.

## Agrupar e resumir

`data/group_summarise` reduz várias linhas a uma linha por combinação de
**Agrupar por**. Sem agrupamento, produz uma linha para toda a tabela. **Nome**
e **Resumo** aceitam listas separadas por vírgula, correspondentes pela posição.

| Objetivo | Nome | Resumo |
|---|---|---|
| Total | `receita` | `sum(valor, na.rm = TRUE)` |
| Média | `valor_medio` | `mean(valor, na.rm = TRUE)` |
| Número de linhas | `pedidos` | `dplyr::n()` |
| Valores distintos | `clientes` | `dplyr::n_distinct(cliente)` |
| Mínimo e máximo | `minimo, maximo` | `min(valor), max(valor)` |
| Amplitude | `amplitude` | `max(valor) - min(valor)` |

```r
tr_flow(reg) |>
  tr_add("regional", "data/group_summarise", by = "regiao",
         name = "receita, pedidos, clientes",
         expr = "sum(valor, na.rm = TRUE), dplyr::n(), dplyr::n_distinct(cliente)",
         from = "ler")
```

Cada expressão de resumo devolve um valor por grupo. `valor * 2` devolve um
valor por linha e pertence a `data/mutate`. Faltantes participam de `sum()`
e `mean()` por padrão; `na.rm = TRUE` os exclui, mas reduz a base efetiva e
essa condição deve ser considerada na interpretação.

## Reorganização e combinação

`data/pivot_longer` converte várias colunas em uma coluna de nomes e outra de
valores. `data/pivot_wider` realiza a transformação inversa. Cada combinação
das demais colunas deve identificar no máximo um valor para cada novo nome;
chaves repetidas são resumidas antes com `data/group_summarise`.

```r
tr_add("mensal", "data/pivot_longer", cols = "jan, fev, mar",
       names_to = "mes", values_to = "receita", from = "largos")
```

`data/join` combina tabelas por chave. A junção interna mantém chaves das duas
tabelas; a esquerda preserva todas as linhas da esquerda; a direita preserva as
da direita; a completa preserva a união. Chaves repetidas em ambas as tabelas
multiplicam linhas, pois cada combinação correspondente é preservada.
Recomenda-se verificar a unicidade com `data/get_dupes` antes da junção.

`data/bind_rows` acrescenta linhas de tabelas com colunas compatíveis. Colunas
ausentes em uma origem recebem faltante nas linhas correspondentes.

## Saída e fluxo

`data/write_csv`, `data/write_rds` e `data/write_parquet` gravam a tabela
e a repassam. CSV é intercambiável com planilhas; RDS preserva objetos R;
Parquet é colunar, comprimido e indicado para tabelas grandes.

`data/to_stream` divide uma tabela em lotes e `data/from_stream` recompõe os
resultados. Esses nós delimitam uma região processada passo a passo.
