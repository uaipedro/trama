# trama.spatial 0.2.0

Entrada de dados própria e as quatro krigagens que a 0.1.0 prometeu.

## Entrada de dados e borda

Antes desta versão a borda só existia nos três conjuntos de exemplo: o nó
`spatial/coordinates` não a expunha, e para dado próprio a grade da krigagem era
o retângulo da extensão, sem recorte — o mapa predizia fora da área de estudo com
cara de resultado válido, e a guarda que pega borda em escala ou projeção errada
nunca disparava.

* `spatial/read_points`: lê vetor de pontos (shapefile em zip, GeoJSON,
  GeoPackage, KML) como tabela, com as coordenadas em duas colunas. Abre o zip
  direto, lê a projeção do `.prj`, acha o `.shp` em subpasta e pede escolha
  quando há mais de uma camada. Reprojeta na leitura, porque GeoJSON é grau por
  especificação e o `spatial/coordinates` recusa grau.
* `spatial/boundary`: lê vetor de polígonos como borda, dissolvida, com os anéis
  internos contados na nota, e reprojetada para a projeção dos pontos.
* `spatial/coordinates` ganha a porta **Borda** e o param **Borda**, com a opção
  `casco convexo dos pontos`, que não precisa de arquivo nenhum. A regra de
  projeção entre borda e pontos está na ajuda do bloco, caso por caso.

## Diagnóstico e modelo

* `spatial/anisotropy`: variograma em N direções num objeto só, com faixa de
  referência opcional sob isotropia. O bloco **não** faz teste de hipótese e
  **não** estima razão nem ângulo — as três maneiras de estimá-los que testamos
  devolvem razão perto de 3 até para campos isotrópicos, e um número que não
  separa o caso do seu contrário não ajuda.
* `spatial/variogram_fit` ganha `razao` e `angulo` de anisotropia geométrica,
  lidos do card acima. `razao = 1` é idêntico ao comportamento anterior.

## Predição e validação

* `spatial/validation`: validação cruzada leave-one-out ou em k dobras, com erro
  médio, RMSE, **MSDR** e correlação. O MSDR é o que julga o mapa de erro-padrão,
  que é metade do que a krigagem entrega.
* `spatial/kriging` ganha `tipo = "universal"`, com tendência de 1ª ordem, 2ª
  ordem ou por covariável. A tendência polinomial vai em coordenada centrada e
  padronizada: em UTM cru a 2ª ordem é mal condicionada, e o `geoR` chega a
  ficar singular.
* A deriva externa exige a covariável conhecida em toda célula, pela porta
  **Grade** nova. Sem ela, erro nomeado — interpolar a covariável por dentro
  subestimaria o erro-padrão do mapa em silêncio.
* A porta **Grade** vale para qualquer tipo de krigagem e vence a resolução:
  serve também a quem quer predizer em pontos escolhidos.
* `spatial/indicator`: transforma a variável num indicador 0/1 num corte, para a
  krigagem estimar **probabilidade**. A transformação vem antes do variograma, de
  propósito: a krigagem indicadora precisa do variograma do indicador. O predito
  é recortado em [0, 1] e o mapa é lido como probabilidade.

## Rigor

Oráculos novos, com a concordância medida: `geoR::variog4` para as curvas
direcionais (5,6e-16), `geoR::xvalid` para a validação cruzada (1,07e-14),
`geoR::krige.conv` com tendência para a universal (3,7e-14 e 5,3e-14) e para a
deriva externa (8,9e-16), e `gstat::variogramLine` para a anisotropia do modelo
(razão 3,00 exata, sem otimizador). O `tendencia = "covariavel"` do variograma,
que a 0.1.0 deixou exercitado e sem referência, agora tem oráculo (5,6e-16).

Registrado em `docs/revisao-metodologica.md`, com dois achados que valem citar:
`gstat` e `geoR` classificam diferente um par cuja distância cai exatamente num
limite de classe, o que desloca gamma em ~3e-4 — os testes novos escolhem corte
sem empate, e asserem essa premissa; e o conjunto de direções `{0,45,90,135}` é
invariante sob a reflexão do azimute, então a comparação com o `geoR` é por
rótulo de direção, com um teste em conjunto assimétrico.

## Fica para a próxima

Cokrigagem, que exige objeto de pontos multivariável, variograma cruzado e
modelo linear de corregionalização.

## Adiados, declarados

`grau_dependencia` tem nome que sugere o contrário do que mede (é pepita sobre
patamar, em que menor é mais forte); `ID_COL` casa demais no JS; a detecção de
coordenada em grau só funciona com CRS declarado; a guarda de contenção na borda
é cega abaixo de ~2000 unidades de coordenada.

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
