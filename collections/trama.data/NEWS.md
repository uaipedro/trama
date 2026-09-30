# trama.data 0.3.0

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

