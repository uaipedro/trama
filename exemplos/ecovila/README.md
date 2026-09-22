# Ecovila Serra das Águas

Dados inteiramente simulados de uma ecovila agroecológica.

O conjunto cobre livro-caixa, produção de café, manejo, solo, clima,
microclima, termografia e qualidade da água. Ele não representa uma fazenda real
e não deve embasar decisões agronômicas, econômicas ou ambientais.

Para recriar os CSVs, a partir da raiz do repositório:

```r
Rscript exemplos/ecovila/gerar.R
```

Os CSVs são gravados em `exemplos/ecovila/dados/`. A semente é fixa, portanto a
execução recria exatamente os mesmos dados.

## Premissas didáticas

O prêmio de 2019 aumenta o preço de alguns lotes de café, não a produtividade.
Isso cria picos adequados para discutir média, mediana e valores atípicos. A
horta aparece em muitas transações pequenas e regulares; café permanece a maior
fonte de receita anual. Não some quantidades de produtos distintos: sacas,
quilos, cestas e mudas são unidades comerciais diferentes.

Os sistemas de cultivo já existiam antes da observação. Sistemas mais diversos
ficam um pouco mais concentrados na faixa baixa e úmida; assim, uma diferença
bruta de produtividade pode refletir manejo, solo, posição, distância ao córrego
e microclima. Estes dados são observacionais, não experimentais.

`dados/dicionario_arquivos.csv` resume a chave e granularidade de cada tabela.

## Fluxos didáticos

- `flows/estatistica_resumo.json`: uma aula visual sobre unidade de análise,
  distribuição, média, mediana, variância e desvio padrão usando a receita
  semanal do livro-caixa.
