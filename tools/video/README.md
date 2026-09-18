# tools/video — os vídeos de demonstração

Dois vídeos de 30s, 1920×1080, 30 fps, renderizados com
[Remotion](https://remotion.dev) — React desenhado quadro a quadro.

| composição | o que mostra |
| --- | --- |
| `TramaDemo` | a marca, as coleções, um fluxo da coleção `data` sendo montado, o mesmo fluxo em JSON e em R, o endereço |
| `TramaModelos` | uma ANOVA em blocos casualizados: dado → modelo → quadro da ANOVA → médias com letras → comparações de Tukey |

```sh
npm install
npm run render            # os dois, renderizados e masterizados
npm run render:modelos    # só o de modelos
npm run verificar         # folha de contato + medição de loudness dos dois
npm run studio            # pré-visualização interativa
```

Fica fora do pacote R: `^tools$` está no `.Rbuildignore`, então nada daqui entra
num `R CMD check` nem no tarball.

## Os cards são os do editor, não uma maquete

O canvas dos vídeos não é um desenho parecido com o trama — é o markup do editor
com a folha de estilo do editor. `src/trama/NoCard.tsx` emite as mesmas classes
que o `NdNode` de `inst/www/editor.js` emite (`.tr-node`, `.tr-preview`,
`.tr-tabs`, `.tr-params`, `.tr-ports`), os previews emitem as dos renderers
(`.tr-table`, `.tr-mf-*`, `.tr-quadro`, `.tr-sig-*`) e as arestas usam a bezier
do xyflow com a mesma curvatura. A aparência vem de `src/trama-app.css`,
`src/trama-data.css` e `src/trama-models.css`, que são **cópias** de
`inst/www/trama.css`, `collections/trama.data/inst/trama/data.css` e
`collections/trama.models/inst/trama/models.css`.

Os specs dos blocos são transcritos dos `collection.R`/`nos_*.R` das coleções —
rótulo, categoria, ícone, portas e params, com o widget que `layoutEnum` escolhe
para cada enum (por isso o `Separador` é segmentado inline, o `Tipo` do Juntar é
segmentado largo e o `Conjunto` da `models`, com onze opções, é um `select`).

**Mexeu no front do trama?** Rode `npm run sincronizar`. Ele recopia as três
folhas de estilo, a marca e reextrai os ícones do sprite do lucide. Sem isso os
vídeos continuam mostrando a aparência antiga — um defeito que não aparece lendo
o código do vídeo.

## Os números do vídeo de modelos saíram do R

Nada foi inventado. O fluxo foi rodado de verdade, com o pacote instalado, e os
valores dos cards são a saída dele:

```r
d   <- tr_models_example("milho_dbc")
fit <- tr_models_anova_dbc(d, "producao", "hibrido", "bloco")
tr_models_anova_table(fit, "I")
em  <- tr_models_emmeans(fit, especs = "hibrido", ajuste = "tukey")
tr_models_pairwise(em, "todos os pares", ajuste = "tukey")
```

O `milho_dbc` é o exemplo que a ajuda do bloco descreve como construído para
isto — cinco híbridos em quatro blocos, com o H3 acima dos demais, "para o Tukey
ter de separar o H3". É o que as letras do vídeo mostram, e quem rodar em casa vê
os mesmos R², CV, F, p e letras.

A **única** coisa redesenhada é o gráfico de médias: no app ele é um PNG que o
ggplot2 gera pelo `trama.view`, e um renderizador de vídeo não roda R. Ele é
desenhado em SVG com a geometria do `tr_models_plot_means` (pontos, barras de
IC, letras acima do limite superior, expansão de 5% embaixo e 12% em cima) e a
paleta do tema `claro` de `R/theme.R` — representação fiel do desenho, não o
arquivo que o R produz.

## O que cada arquivo decide

| arquivo | decide |
| --- | --- |
| `src/Video.tsx`, `src/VideoModelos.tsx` | a linha de tempo de cada vídeo e a trilha |
| `src/theme.ts` | cor, easing e mola — nenhum componente inventa nenhuma das três |
| `src/trama/tipos.ts` | o vocabulário que os dois catálogos falam |
| `src/trama/catalogo.ts`, `catalogo-modelos.ts` | os blocos, os resultados e **a coreografia** (em que quadro cada card entra, digita e resulta) |
| `src/trama/metricas.ts` | a geometria do card, e com ela a âncora de cada porta |
| `src/trama/previews.tsx` | o que cada card desenha: tabela, card de modelo, quadro de efeitos, gráfico |
| `src/scenes/**/Fluxo.tsx` | só o enquadramento: as marcas de câmera |
| `src/lib/` | a pilha de cinco camadas e as primitivas de movimento |

Mover um bloco no tempo é mexer no catálogo e em nada mais: a aresta, o som do
card assentando e o card ativo (o único com brilho) são derivados de lá.

## Por que a altura do card é declarada

As arestas precisam saber onde cada porta está para nascer ancoradas. Medir o
DOM para descobrir isso seria o laço "medir → mudar estado → remedir" que o
`trama.css` passa o arquivo inteiro evitando — e num renderizador quadro a
quadro ele não converge. Então `metricas.ts` guarda os números das mesmas regras
de CSS (preview 132px, abas 18px, porta 15px com gap de 3px, `.tr-ports` com 5px
em cima e 14px embaixo), e o card recebe essas alturas como estilo inline.
Desenho e âncora não podem divergir porque os dois leem do mesmo lugar.

Cards maiores que o padrão de 240px (o quadro da ANOVA, o gráfico, as
comparações) declaram `tamanho` — que é o `ui.sizes` do documento, onde mora a
alça que o usuário arrastou.

Consequência prática: **cinco linhas por tabela de preview**. Na faixa de 132px
cabem exatamente cinco linhas mais o cabeçalho; a sexta rola dentro do card no
app, e num quadro de vídeo aparece cortada ao meio.

## Som

`npm run sfx` sintetiza o kit em `public/sfx/` com Node puro — nada baixado,
nada aleatório (o ruído tem semente fixa), então o mesmo código gera o mesmo
áudio em qualquer máquina. São sete peças: `whoosh` (ruído filtrado com
varredura), `pop` e `pop-alto` (seno despencando de frequência), `thump` (o corte
de cena), `tick` (digitação), `riser` (entrada de corte) e `pad` (a cama, em
laço).

A **masterização é uma etapa separada** (`scripts/masterizar.sh`), e não um ganho
a mais na composição. A trilha é esparsa — cama baixa, golpes curtos — e a
loudness integrada da EBU R128 é gateada: ela mede os trechos altos e descarta o
que está muito abaixo. Subir a cama 7 dB mal moveu o integrado (−21,9 → −22,1
LUFS) e subir os golpes junto estourava o pico, que já estava a 2,8 dB do teto.
Quem resolve é um limitador no fim da cadeia. O script mede em duas passagens e
entrega −16 LUFS com teto de −1 dBTP, copiando o vídeo sem recodificar.

## Regras de movimento que o projeto não abre mão

Estão em `src/lib/movimento.tsx` para não precisarem ser lembradas em cada cena:

- **Nenhuma interpolação linear.** Entrada é mola; deslocamento é bezier, sempre
  com os dois `clamp`.
- **Entrada mexe três propriedades** (opacidade, subida, escala). Fade sozinho
  não diz de onde a coisa veio.
- **Tudo escalonado** — 2 a 4 quadros entre irmãos.
- **Saída mais rápida que a entrada** (10 quadros contra ~20).
- **Cenas SOBREPOSTAS em 10 quadros.** Encostadas, a que sai já está em
  opacidade zero e a que entra ainda não começou, e o corte vira um piscar de
  tela vazia — defeito que só aparece amostrando quadros em intervalos
  regulares.
- **Uma cor-herói por quadro.** `tema.cor.destaque` e o brilho ficam num
  elemento só: o card ativo, uma palavra da legenda, ou o endereço no fecho. As
  cores de categoria são cor de dado, e a régua do p-valor é a codificação de
  significância do próprio app — nenhuma das duas conta.
- **Câmera chega antes e depois FICA PARADA.** O contraste entre um
  deslocamento rápido e a imobilidade é o que lê como caro; movimento contínuo
  lê como filmado na mão.
- **CSS não anima.** `src/video.css` desarma `transition` e `animation` do CSS do
  app: as duas correm no relógio de parede, e um render quadro a quadro não tem
  relógio de parede.
- **Render sem inspeção não se entrega.** `npm run verificar` extrai 16 quadros
  do mp4 pronto (não do renderizador, para conferir também a codificação) e
  monta uma folha de contato. Foi ela que pegou a legenda cobrindo um card, a
  sexta linha de tabela cortada, o card encostando na borda e o piscar nos
  cortes.
