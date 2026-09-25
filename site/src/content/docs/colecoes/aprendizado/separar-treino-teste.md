---
section: colecoes
title: Separar treino e teste
description: Divide as linhas em duas tabelas reprodutivelmente e pode preservar a proporção de cada classe.
collection: aprendizado
node: ml/split
related: ["ml/cart", "ml/predict"]
---

## O que o bloco faz

Divide as linhas em duas tabelas reprodutivelmente e pode preservar a proporção de cada classe.

## Quando usar

Reserve um conjunto de teste antes do ajuste para estimar desempenho em linhas não usadas no treino.

## Configuração

`alvo` identifica a resposta; `proporcao` define a fração aproximada de treino; `estratificar` mantém classes nos dois lados; `seed` reproduz a amostra. `estrategia` respeita a dependência entre linhas: `aleatoria` (padrão) sorteia linhas; `temporal` põe no treino tudo até o instante da linha ⌊n · proporção⌋ na ordem de `ordem` (instantes empatados ficam juntos) e no teste o que vem depois; `grupo` sorteia ⌊G · proporção⌋ grupos inteiros de `grupo`, de modo que nenhum indivíduo, lote ou área aparece dos dois lados. Nas duas últimas, `estratificar` é ignorado.

## Exemplo

```r
d <- trama.ml::tr_ml_example("iris_binaria")
s <- trama.ml::tr_ml_split(d, alvo = "Species", proporcao = 0.75, seed = 42)
nrow(s$treino); nrow(s$teste)

# Dados com tempo: todo o teste é posterior ao treino.
e <- data.frame(dia = rep(1:20, each = 3), y = factor(rep(c("a", "b"), 30)), x = 1:60)
t <- trama.ml::tr_ml_split(e, alvo = "y", proporcao = 0.7, estrategia = "temporal", ordem = "dia")
range(t$treino$dia); range(t$teste$dia)   # 1-14 e 15-20
```

## Como interpretar

As saídas `treino` e `teste` não duplicam nem perdem linhas. A proporção final pode variar por arredondamento em cada classe.

### Proveniência: o teste fica isolado por construção

As duas saídas levam uma marca (atributo `tr_ml_origem`, com o papel `treino`/`teste`, um id da divisão e as **impressões digitais das linhas do teste**) que sobrevive ao cache e a filtros ou colunas novas. A impressão é um hash (xxHash64) do conteúdo de cada linha nas colunas da divisão, guardado com a contagem de cópias — assim uma linha repetida que o sorteio pôs dos dois lados não é confundida com uma linha do teste que voltou ao treino. Todo modelo guarda as impressões das suas linhas de treino. Com isso:

- ajustar um modelo, o `ml/tune` ou o `ml/nested_cv` na saída `teste` é recusado (`tr_ml_error_test_leak`), e também numa tabela que junta treino e teste (`rbind`, `bind_rows`, `data/bind_rows`), mesmo que ela venha com a marca de treino;
- o `ml/predict` recusa prever o teste com um modelo que viu linhas dele no ajuste — por exemplo, ajustado na tabela inteira antes de dividir, ou numa cópia do teste sem a marca (`tr_ml_error_test_leak`) — e o teste de outra divisão com um modelo ajustado no treino desta (`tr_ml_error_split_mismatch`);
- `ml/evaluate`, `ml/confusion`, `ml/roc` e `ml/pr_curve` recusam previsões das linhas de `treino`, e também uma tabela marcada como teste que traz linhas de fora dele (previsões do treino juntadas às do teste, ou o teste repetido) (`tr_ml_error_train_eval`), a menos que se ligue `permitir_treino` — aí o resultado sai com a nota “avaliação no treino é otimista”.

Custo: em 100 mil linhas, calcular as impressões leva cerca de 1 segundo e ocupa uns 7 MB no modelo; os folds internos do `ml/tune` não as recalculam.

### O que a marca não cobre

A marca é um atributo: só viaja quando a operação copia os atributos da tabela que o leva, e a impressão só confere quando as colunas da divisão continuam lá com os mesmos valores. Ficam de fora, por construção:

- **juntar com o teste à direita** — `data/join` ou `left_join(outra, teste)` ficam com os atributos de `outra`, sem marca;
- **remodelar** — `pivot_longer`/`pivot_wider` mudam o que é uma linha e as colunas;
- **recriar à mão** — copiar os valores para uma tabela nova, exportar para planilha e ler de volta;
- **reescrever ou tirar colunas da divisão** — arredondar, converter para texto, `select` que remove uma coluna: as impressões deixam de conferir e só a checagem de papel continua;
- **dividir fora do `ml/split`**.

Nesses casos os blocos seguem como sem marca: não têm como saber de onde vieram as linhas, e o isolamento fica com você. Divida primeiro, no `ml/split`, e só então transforme cada lado.
