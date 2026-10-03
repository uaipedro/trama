# trama.data 0.4.1

* Card do `data/table` ganha uma linha de perfil sob o cabeçalho: tipo da
  coluna, barra de NA e mini-histograma (numérica) ou até três níveis mais
  frequentes (fator/texto/lógica). Medido em até 5.000 linhas espaçadas da
  tabela inteira, não nas 25 mostradas. O card recebe no máximo 30 colunas
  ("+N col" indica o resto).

# trama.data 0.4.0

* Bloco único "Ler dados" (`data/read`, função `tr_read()`) no lugar de Ler
  CSV/JSON/RDS/Parquet/Excel. Reconhece o formato pela extensão (ou pelo
  param `formato`) e abre `.zip`. Aceita link: baixa uma vez para `data/`
  (origem em `data/.origens.json`), lê a cópia dali em diante e baixa de novo
  quando `copia` sobe. Links do GitHub, Google Sheets e Google Drive
  funcionam colados como estão. Os cinco blocos antigos migram sozinhos com o
  formato correspondente; as funções `tr_read_csv()` etc. continuam
  exportadas.
* Bloco `data/public` ("Base pública"): carrega uma tabela publicada em
  qualquer pacote R (`pacote` + `dataset`). É o que o catálogo de bases insere
  no canvas. `tr_base_publica()` declara uma base carregada por ele.
* Nove bases no catálogo: sete do `datasets`, `palmerpenguins::penguins` e
  `gapminder::gapminder`.

# trama.data 0.3.0

* `data/select` aceita seletores escritos como no R, de uma lista fechada: `starts_with()`, `ends_with()`, `contains()`, `matches()`, `where(is.numeric | is.character | is.factor | is.logical | is.integer | is.double)`, `everything()`, `last_col()`, o intervalo `a:c` e os combinadores `c()`, `!`, `-`, `&`, `|`. Qualquer outra função é recusada com `tr_data_error_bad_expr`, sem avaliar. A lista de nomes de sempre e nomes com parêntese continuam iguais. Posição numérica (`c(2)`), argumento nomeado (`c(novo = valor)`) e `ignore.case` que não seja `TRUE`/`FALSE` também são recusados; `last_col(n)` aceita só inteiro não negativo.
