---
title: "Box-Cox"
description: "Perfil de verossimilhança em λ: a potência da resposta que normaliza o erro, com IC e a transformação sugerida."
section: colecoes
collection: experimentos
node: experiments/boxcox
category: "Analisar"
related: [models/shapiro_residuals, models/levene, data/mutate, experiments/contrasts]
---

<!-- Gerado por tools/site/export-collection-pages.R a partir da ajuda do bloco. -->

## O que o bloco faz

Procura a potência λ da resposta que torna o erro do modelo o mais próximo de
normal com variância constante: y^(λ) = (y^λ − 1)/λ, e log(y) em λ = 0. Mantém o
MESMO modelo (o delineamento do `models/fit`) e varre λ, calculando a
log-verossimilhança perfilada em cada um, com a resposta dividida pela média
geométrica para que os valores sejam comparáveis entre λ — a conta do
`MASS::boxcox`, que o bloco reproduz ponto a ponto.

Devolve:

- **λ ótimo** — o máximo exato do perfil (não só o da grade);
- **intervalo de confiança** — os λ cuja log-verossimilhança fica a menos de
  χ²₁(confiança)/2 do máximo (razão de verossimilhança);
- **λ sugerido** — dentro do intervalo, a potência interpretável mais próxima do
  ótimo, entre −2, −1, −0,5, 0 (log), 0,5, 1 (nenhuma) e 2. Se nenhuma cair no
  intervalo, o bloco diz isso e não sugere;
- se λ = 1 (não transformar) está no intervalo;
- se λ̂ caiu na **borda da grade** (`na_borda`): aí o perfil ainda sobe além
  do limite, λ̂ não é o ótimo, e o bloco não sugere transformação — amplie a
  grade.

Na parcela subdividida, o perfil é o do modelo de efeitos fixos com bloco ×
parcela como fator (o que dá os resíduos do erro b).

## Pressupostos

- Resposta estritamente positiva: y^λ não é definido para y ≤ 0. Se houver
  zeros, some uma constante antes (`data/mutate`) e registre isso.
- Existe uma potência que normaliza e estabiliza a variância ao mesmo tempo — o
  método escolhe o λ pela verossimilhança normal; confira os resíduos do modelo
  transformado (`models/shapiro_residuals`, `models/levene`) depois.
- A transformação muda a escala da interpretação: as médias transformadas de
  volta são medianas, não médias, na escala original.

## Parâmetros

- **λ mínimo**, **λ máximo**, **Passo** — a grade do perfil; o padrão, −2 a 2
  de 0,1 em 0,1, é o do `MASS::boxcox`.
- **Confiança** — o nível do intervalo para λ (0,95).

## Valor

Três portas: `out`, o gráfico do perfil (`view/plot`) com o ótimo, o intervalo
e o λ sugerido; `resumo`, uma tabela de uma linha (`lambda_otimo`, `li`, `ls`,
`confianca`, `lambda_sugerido`, `transformacao`, `um_no_intervalo`,
`na_borda`, `nota`); e
`perfil`, a tabela (λ, log-verossimilhança) da grade.

## Exemplos

```r
tr_flow(reg) |>
  tr_add("fios", "models/example", dataset = "warpbreaks") |>
  tr_add("fat", "models/anova_factorial", resposta = "breaks", fatores = "wool, tension",
         from = "fios") |>
  tr_add("bc", "experiments/boxcox", from = "fat")
```

## Referências

- Box, G. E. P. & Cox, D. R. (1964). An analysis of transformations. *Journal of
  the Royal Statistical Society, Series B*, 26(2), 211–243 (com a discussão,
  até 252). doi:10.1111/j.2517-6161.1964.tb00553.x. (Os dados de venenos e
  tratamentos, `boot::poisons`, são deste artigo e estão nos testes.)
- Venables, W. N. & Ripley, B. D. (2002). *Modern Applied Statistics with S*,
  4th ed. Springer. (`MASS::boxcox`.)

## Veja também

`models/shapiro_residuals` e `models/levene` para conferir os resíduos antes e
depois; `data/mutate` para aplicar a transformação; `experiments/contrasts`.

### Aparência (comum a todos os gráficos)

- **Proporção** — a forma da imagem: `16:9` e `2:1` para paisagem, `1:1` para
  quadrado, `4:3` e `3:4` para o que vai numa página. A imagem sai sempre com
  1600 px no lado maior; o card só a escala, e um clique a abre em tela cheia.
  Mudar a proporção RECOMPUTA o gráfico — é o único param de aparência que
  muda mesmo o desenho, porque o ggplot recoloca a legenda e remede os rótulos.
  Arrastar a alça do card, não: aquilo é tamanho, não proporção.
- **Tema** — `padrão` segue o tema padrão do projeto; os temas (fundo, cores,
  fonte, paleta) são do projeto, e não do gráfico. Sem temas próprios valem os
  embutidos, com `escuro` como padrão; `claro` e `clássico` servem bem ao que
  sai no relatório. O tema dos gráficos não segue o claro/escuro do editor, que
  é preferência de cada pessoa: para mudar todos de uma vez, troque o tema
  padrão em ⚙ Configurações. A paleta só entra onde o
  gráfico não escolheu cores por conta própria.
- **Título**, **Rótulo do X**, **Rótulo do Y** — em branco, o gráfico usa o
  nome da coluna, que costuma ser a legenda certa.
- **Legenda** — `nenhuma` quando a cor já está explicada no título.

