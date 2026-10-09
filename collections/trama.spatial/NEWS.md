# trama.spatial 0.1.1

* Relatório Quarto exportado: `spatial/points` (resumo e quartis), `spatial/variogram` (tabela por classe), `spatial/model` (parâmetros) e `spatial/surface` (o mapa do predito) ganham `report` — tabelas em Markdown no lugar do `print` cru da lista. Pede `trama.data (>= 0.4.4)`.

# trama.spatial 0.1.0

Primeira versão. Sete blocos que levam uma tabela de pontos até um mapa de
krigagem com erro-padrão:

* `spatial/example`: três conjuntos de exemplo do IBGE (milho no Paraná, café
  em Minas Gerais, milho em Sergipe), com a fonte citada.
* `spatial/coordinates`: declara coordenadas, variável, projeção e borda a
  partir de uma tabela.
* `spatial/explore`: painel exploratório (postplot por quartil, variável contra
  cada coordenada e distribuição).
* `spatial/variogram`: variograma empírico clássico ou robusto, omnidirecional
  ou direcional, com tendência removida.
* `spatial/variogram_fit`: ajuste esférico, exponencial, gaussiano e Matérn por
  mínimos quadrados ordinários ou ponderados, com pepita, patamar parcial,
  alcance e alcance prático.
* `spatial/kriging`: krigagem ordinária ou simples numa grade recortada na
  borda, com o erro-padrão de cada célula.
* `spatial/map`: mapa do predito ou do erro-padrão da superfície.

Quatro tipos próprios (`spatial/points`, `spatial/variogram`, `spatial/model`,
`spatial/surface`), cada um com adaptador para `data/table`. Motor: `gstat` e
`sf`; `geoR` só em `Suggests`, como oráculo de teste. Template de exemplo com o
mapa do predito e o do erro-padrão lado a lado.

Fora desta versão, previstos para as próximas: validação cruzada, anisotropia,
krigagem universal e indicadora, e cokrigagem.
