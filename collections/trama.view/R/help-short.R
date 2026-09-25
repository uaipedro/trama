# Ajuda curta para o painel lateral. A referência extensa fica em
# `docs/colecao-graficos.md`.
.tr_view_help_curto <- function(id) {
  ajuda <- c(
    "view/points" = "## Uso principal\n\nMostra a relação entre duas medidas, com uma marca por linha.\n\n## Exemplo curto\n\n`Eixo X: idade`; `Eixo Y: renda`; `Cor por: regiao`\n\n## Usos relacionados\n\n`view/bin2d` atende a muitas observações; `view/line` é indicado quando o eixo X tem ordem.",
    "view/line" = "## Uso principal\n\nMostra a evolução de uma medida em um eixo ordenado, como tempo ou dose.\n\n## Exemplo curto\n\n`Eixo X: mes`; `Eixo Y: receita`; `Cor por: regiao`\n\n## Usos relacionados\n\n`view/area` mostra composição acumulada; `data/group_summarise` produz uma medida por período.",
    "view/area" = "## Uso principal\n\nMostra totais e composição de séries ao longo de um eixo ordenado.\n\n## Exemplo curto\n\n`Eixo X: mes`; `Eixo Y: receita`; `Empilhar por: regiao`\n\n## Usos relacionados\n\n`view/line` compara séries; `view/bars` com proporção mostra participação por período.",
    "view/bin2d" = "## Uso principal\n\nConta observações em uma grade para revelar densidade numa nuvem grande.\n\n## Exemplo curto\n\n`Eixo X: idade`; `Eixo Y: renda`; `Classes: 40`\n\n## Usos relacionados\n\n`view/points` atende a poucas observações; `view/density` mostra a distribuição de uma medida.",
    "view/heatmap" = "## Uso principal\n\nMostra uma medida em combinações de duas categorias ou escalas ordenadas.\n\n## Exemplo curto\n\n`Eixo X: mes`; `Eixo Y: regiao`; `Valor: receita`\n\n## Usos relacionados\n\n`view/bars` compara uma categoria; `data/group_summarise` calcula o valor por combinação.",
    "view/labels" = "## Uso principal\n\nMostra pontos com rótulos quando a identificação de cada observação é necessária.\n\n## Exemplo curto\n\n`Eixo X: custo`; `Eixo Y: receita`; `Rótulo: produto`\n\n## Usos relacionados\n\n`view/points` é mais legível para muitas linhas; `data/filter` reduz os rótulos mostrados.",
    "view/histogram" = "## Uso principal\n\nAgrupa uma medida numérica em faixas para mostrar sua distribuição.\n\n## Exemplo curto\n\n`Eixo X: valor`; `Classes: 30`\n\n## Usos relacionados\n\n`view/density` suaviza a forma; `view/boxplot` compara distribuições entre grupos.",
    "view/density" = "## Uso principal\n\nEstima a forma suavizada da distribuição de uma medida numérica.\n\n## Exemplo curto\n\n`Eixo X: valor`; `Cor por: regiao`\n\n## Usos relacionados\n\n`view/histogram` mostra contagens por faixa; `view/ecdf` mostra proporções acumuladas.",
    "view/boxplot" = "## Uso principal\n\nCompara mediana, quartis e valores extremos de uma medida entre grupos.\n\n## Exemplo curto\n\n`Eixo X: regiao`; `Eixo Y: valor`\n\n## Usos relacionados\n\n`view/violin` mostra a forma da distribuição; `view/means` mostra média e incerteza.",
    "view/violin" = "## Uso principal\n\nMostra a forma da distribuição de uma medida em cada grupo.\n\n## Exemplo curto\n\n`Eixo X: regiao`; `Eixo Y: valor`\n\n## Usos relacionados\n\n`view/boxplot` resume quartis; `view/strip` mostra observações individuais.",
    "view/ecdf" = "## Uso principal\n\nMostra a proporção de observações até cada valor, permitindo comparar distribuições.\n\n## Exemplo curto\n\n`Eixo X: valor`; `Cor por: regiao`\n\n## Usos relacionados\n\n`view/density` mostra a forma suavizada; `view/qq` compara quantis com uma distribuição de referência.",
    "view/qq" = "## Uso principal\n\nCompara os quantis observados aos quantis de uma distribuição normal.\n\n## Exemplo curto\n\n`Eixo Y: residuo`; `Cor por: grupo`\n\n## Usos relacionados\n\n`view/histogram` mostra a distribuição; `view/boxplot` compara grupos.",
    "view/strip" = "## Uso principal\n\nMostra observações individuais por grupo, com deslocamento para reduzir sobreposição.\n\n## Exemplo curto\n\n`Eixo X: regiao`; `Eixo Y: valor`; `Resumo: mediana`\n\n## Usos relacionados\n\n`view/boxplot` resume quartis; `view/violin` mostra densidade por grupo.",
    "view/bars" = "## Uso principal\n\nCompara contagens ou valores agregados entre categorias.\n\n## Exemplo curto\n\n`Eixo X: regiao`; `Eixo Y: receita`\n\n## Usos relacionados\n\n`data/group_summarise` calcula médias ou totais antes das barras; `view/means` estima média com incerteza.",
    "view/means" = "## Uso principal\n\nCompara médias entre grupos e apresenta barra de incerteza.\n\n## Exemplo curto\n\n`Eixo X: tratamento`; `Eixo Y: resposta`; `Barra: IC`; `Confiança (IC): 0,95`\n\n## Usos relacionados\n\n`view/boxplot` mostra a distribuição completa; `view/bars` apresenta totais ou valores já agregados.",
    "view/dotplot" = "## Uso principal\n\nCompara valores de categorias numa escala comum e ordenada.\n\n## Exemplo curto\n\n`Eixo X: valor`; `Eixo Y: produto`; `Ordenar: ligado`\n\n## Usos relacionados\n\n`view/bars` enfatiza magnitude; `view/dumbbell` compara dois valores por categoria.",
    "view/dumbbell" = "## Uso principal\n\nCompara dois valores de cada categoria por uma linha entre pontos.\n\n## Exemplo curto\n\n`Categoria: produto`; `Valor: antes, depois`\n\n## Usos relacionados\n\n`view/paired` mostra unidades pareadas; `view/dotplot` mostra um valor por categoria.",
    "view/paired" = "## Uso principal\n\nMostra a mudança de cada unidade entre duas condições ou tempos.\n\n## Exemplo curto\n\n`Eixo X: tempo`; `Eixo Y: valor`; `Unidade: paciente`\n\n## Usos relacionados\n\n`view/dumbbell` compara valores já resumidos; `view/line` mostra séries com mais de dois tempos.",
    "view/combine" = "## Uso principal\n\nJunta gráficos numa figura com painéis etiquetados (A, B, C), com um tema só para todos.\n\n## Exemplo curto\n\n`Colunas: 2`; `Etiquetas: A, B, C`; `Legenda comum: ligado`\n\n## Usos relacionados\n\nA ordem dos painéis é a ordem em que os gráficos foram ligados; `view/points`, `view/boxplot` e os demais gráficos entram como painéis.",
    "view/pareto" = "## Uso principal\n\nOrdena categorias por contribuição e mostra o percentual acumulado.\n\n## Exemplo curto\n\n`Categoria: defeito`; `Valor: ocorrencias`; `Referência: 80`\n\n## Usos relacionados\n\n`view/bars` compara categorias sem acumulado; `data/group_summarise` calcula contagens ou totais."
  )
  unname(ajuda[[id]])
}

.tr_view_aplicar_ajuda_curta <- function(colecao) {
  guias <- c(
    "view/points" = "https://uaipedro.github.io/trama/colecoes/visualizacao/disperso/",
    "view/histogram" = "https://uaipedro.github.io/trama/colecoes/visualizacao/histograma/",
    "view/combine" = "https://uaipedro.github.io/trama/colecoes/visualizacao/painel/"
  )
  colecao$nodes <- lapply(colecao$nodes, function(no) {
    no$help <- .tr_view_help_curto(no$id)
    guia <- guias[no$id]
    if (!is.na(guia)) {
      no$help <- paste0(no$help, "\n\n## Guia completo\n\n[Ver no site do trama](", guia, ").")
    }
    no
  })
  colecao
}
