# trama.data 0.4.4

* Relatório Quarto exportado: `data/table` e `data/test` ganham `report`. A tabela sai em Markdown (até 20 linhas, sem colunas vazias, de pé quando é uma linha só e larga); o teste sai como linha de quadro com estatística, gl, p-valor e decisão, seguida da conclusão, da nota e da fonte — no lugar do `print` cru da lista. Novos `tr_data_report()`, `tr_data_md_table()`, `tr_data_fmt_num()`, `tr_data_fmt_p()`, base dos `report` das outras coleções.

# trama.data 0.4.3

* Templates regenerados com o formato atual do documento (saídas nomeadas e grupos do trama 0.5.10); o `data/summary` passa à versão 2. Pede `trama (>= 0.5.10)`.

# trama.data 0.4.2

* O card do `data/table` volta a mostrar só as linhas: sai a linha de perfil
  (selo de tipo, barra de NA, mini-histograma, níveis). Repetida em cada
  bloco do fluxo, poluía mais do que informava. O "+N col" fica.
* `data/summary` (versão 2) ganha a coluna `distribuicao`: histograma de 10
  classes em blocos de texto (`▁▃▇▅▂`) nas numéricas, os três níveis mais
  frequentes com a porcentagem entre os não-faltantes em texto, fator e
  lógico. Classes conferidas contra `graphics::hist(right = FALSE)`.

# trama.data 0.4.1

* Card do `data/table` ganha uma linha de perfil sob o cabeçalho: tipo da
  coluna, barra de NA e mini-histograma (numérica) ou até três níveis mais
  frequentes (fator/texto/lógica). Medido em até 5.000 linhas espaçadas da
  tabela inteira, não nas 25 mostradas. O card recebe no máximo 30 colunas
  ("+N col" indica o resto).
* Selo de delta no card: quando a tabela que sai tem outro número de linhas
  ou colunas que a única tabela que entra, aparece "−12 linhas" / "+2 col"
  (passe o mouse para ver `1.000 → 988` e a porcentagem). Exige
  trama >= 0.5.4.

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
