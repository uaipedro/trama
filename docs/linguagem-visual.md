# Linguagem visual do canvas

Diretriz de gramática e semântica visual dos cards, portas e fios do editor.
Adotada em 24/09/2026, depois de uma rodada de protótipos no laboratório
(`site/src/pages/dev/papeis.astro`, variante "C · Hardware tátil").

O objetivo é que o canvas responda perguntas diferentes por canais diferentes,
sem que um canal carregue dois significados.

| Canal | Responde | Onde se declara |
| --- | --- | --- |
| Cor do cabeçalho | **O que este bloco faz no fluxo?** (papel) | `tr_category(role =)` / `tr_node(role =)` |
| Tomada (porta) | **O que passa por este fio?** (tipo) | `tr_type(color =)` |
| LED, borda, fio vermelho | **Em que estado está?** (execução) | editor, nunca a coleção |
| Ícone e título | **Qual bloco é este?** | `tr_node(icon =, label =)` |

A regra de ouro: **a cor do bloco não diz de que coleção ele veio nem que tipo
ele produz.** Um ajuste de modelo misto e um ARIMA têm a mesma cor; um teste
de normalidade e uma matriz de confusão também. Coleção e tipo já têm onde
aparecer (paleta e portas).

---

## 1. Os sete papéis

Os papéis seguem a ordem em que o analista trabalha. Cada um responde uma
pergunta; na dúvida sobre um bloco, pergunte qual delas ele responde.

| Papel | Pergunta do analista | Exemplos |
| --- | --- | --- |
| `origem` | De onde vêm os dados? | Ler CSV, dados de exemplo, série de exemplo |
| `preparacao` | Como deixo isto usável? | Limpar nomes, filtrar, reformatar, separar treino/teste, planejar amostra |
| `inspecao` | O que tenho nas mãos? | Resumo, duplicadas, gráficos do dado bruto, correlograma, matriz de correlação |
| `ajuste` | Que objeto de análise construo? | Regressão, ANOVA, modelo misto, ARIMA, decomposição, PCA, random forest, estimadores de amostragem |
| `leitura` | O que o ajuste diz? | Quadro da ANOVA, coeficientes, médias ajustadas, efeitos, previsões, cargas, importância de variáveis |
| `avaliacao` | Posso confiar nisso? | Pressupostos, testes (Ljung-Box, Mann-Kendall), acurácia, matriz de confusão, curva ROC, comparar modelos |
| `saida` | O que levo daqui? | Gravar CSV |

### 1.1 Ajuste × Leitura — a distinção que mais importa

Esta é a separação que motivou os sete papéis (antes eram seis, com um
"Análise" que juntava tudo e deixava o canvas inteiro verde).

- **Ajuste constrói.** Recebe dados e devolve um objeto de análise: um modelo,
  uma decomposição, uma PCA. É um passo da sequência, a "máquina" do fluxo.
- **Leitura olha para um ajuste que já existe.** Recebe o objeto e devolve um
  mostrador: tabela, coeficiente, gráfico de efeitos, previsão. Não avança o
  fluxo; vários leitores costumam sair em paralelo do mesmo ajuste, como os
  mostradores de um painel.

Por isso a Leitura usa **o mesmo verde do Ajuste, invertido** (fundo claro e
tinta verde no tema claro; fundo escuro esverdeado e tinta verde clara no
escuro). O painelzinho fica visualmente ligado ao modelo de onde saiu, e o
canvas mostra o ajuste forte cercado pelos seus leitores.

### 1.2 Casos de fronteira já decididos

Registrados para que a próxima coleção siga o mesmo critério.

| Bloco | Papel | Por quê |
| --- | --- | --- |
| Gráfico do dado bruto (dispersão, histograma, boxplot, série no tempo) | `inspecao` | Descreve o que chegou; não depende de ajuste. |
| Gráfico do ajuste (efeitos, médias, decomposição, previsão, biplot) | `leitura` | É um mostrador de um objeto ajustado. |
| Diagnóstico dos resíduos (gráfico) | `avaliacao` | Serve para julgar pressupostos, não para ler o modelo. |
| Resíduos (tabela), efeitos aleatórios, medidas de ajuste | `leitura` | Informação do ajuste, sem veredito. |
| Todo teste de hipótese (normalidade, Ljung-Box, raiz unitária, F dos blocos, M de Box) | `avaliacao` | Emite um veredito. |
| Comparações de médias, contrastes, Duncan | `leitura` | Leem o modelo ajustado; o veredito é sobre os tratamentos, não sobre a confiança no modelo. |
| Prever (qualquer coleção) | `leitura` | Usa um ajuste existente. |
| Acurácia, avaliar previsões, matriz de confusão, ROC | `avaliacao` | Julgam o ajuste. |
| Ajustar hiperparâmetros (tuning) | `ajuste` | Produz o modelo escolhido. |
| Estimadores de amostragem (média, total, razão) | `ajuste` | Constroem a estimativa a partir do desenho. |
| Entrar/sair de fluxo | `preparacao` | Reorganizam como os dados passam. |
| Separar treino/teste, desenho amostral | `preparacao` | Deixam os dados prontos para o ajuste. |

Se um caso novo não couber em nenhuma linha, decida pela pergunta da tabela
de papéis e acrescente a linha aqui.

---

## 2. Como uma coleção declara

A categoria dá o papel padrão dos blocos dela; o bloco que faz outra coisa
sobrescreve só para si. A paleta continua agrupada pela categoria (é como o
usuário procura), e só a cor do card muda.

```r
trama::tr_category("serie_modelar", "Modelar", role = "ajuste")

trama::tr_node("series/arima", ...)                      # herda "ajuste"
trama::tr_node("series/forecast", role = "leitura", ...) # Prever lê o ajuste
trama::tr_node("series/accuracy", role = "avaliacao", ...)
```

- Papel fora da lista dá `tr_error_bad_role`, na declaração.
- `tr_category(color =)` continua existindo para coleção de terceiros que não
  declara papel: vale nos dois temas, com tinta escura. Coleção nossa sempre
  declara papel.
- A lista de papéis mora em `tr_roles` (`R/collection.R`) e em `PAPEIS`
  (`inst/www/papeis.js`); as duas precisam andar juntas.

**Não** criar papel por domínio ("modelagem", "séries", "ML"). Papel é
genérico: qualquer coleção futura tem que caber nos sete.

---

## 3. Cores

As cores são do editor, não da coleção: tokens `--tr-papel-<papel>` e
`--tr-papel-<papel>-ink` em `inst/www/trama.css`, redefinidos por tema.

| Papel | Claro (fundo / tinta) | Escuro (fundo / tinta) |
| --- | --- | --- |
| Origem | `#275D8C` / branco | `#7AB5E4` / `#0B0E13` |
| Preparação | `#976523` / branco | `#E4B66D` / `#0B0E13` |
| Inspeção | `#75539B` / branco | `#BEA2DF` / `#0B0E13` |
| Ajuste | `#31765A` / branco | `#83C9A8` / `#0B0E13` |
| Leitura | `#D3E9DE` / `#1D5A41` | `#1F3B31` / `#A8DCC3` |
| Avaliação | `#8B3A62` / branco | `#E39BC2` / `#0B0E13` |
| Saída | `#59636B` / branco | `#B5C3CD` / `#0B0E13` |

Regras que as cores obedecem:

- **Contraste mínimo 4,5:1** entre fundo e tinta em todos os pares (o mais
  apertado é Preparação no claro, 5,0). Ao mexer numa cor, medir de novo.
- **Tons sóbrios, não saturados.** O card é fosco; a cor do papel não pode
  competir com as tomadas nem com os estados.
- **Vermelho é só de erro.** Avaliação é vinho justamente para não ser lida
  como falha (`--tr-err`). Nenhum papel pode usar vermelho.
- **Verde do LED ≠ verde do Ajuste.** O LED "pronto" é verde saturado e
  luminoso; o Ajuste é verde fechado e fosco. Se um papel novo encostar no
  verde do LED, trocar o papel, não o LED.

---

## 4. Estado de execução

Estado nunca vem da cor do cabeçalho; tem sinais próprios.

- **LED** na ponta direita do cabeçalho: apagado (parado), âmbar (rodando ou
  pendente), verde (pronto ou em cache), vermelho (falhou ou inválido). O modo
  mini não tem LED; usa a bolinha própria.
- **Borda** do card: destaque na seleção e rodando, `--tr-err` na falha.
- **Fio quebrado**: o fio que chega num card falho fica vermelho e tracejado,
  para ver onde a trama quebrou sem abrir o card.

---

## 5. Material do canvas ("hardware tátil")

- **Bancada** no lugar dos pontos do React Flow: granulado fosco claro no tema
  claro, concreto no escuro (`--tr-bancada`). Mais escura que os cards, para
  que eles se destaquem pelo contorno e pela sombra, mas não pesada.
- **Card** fosco em relevo: gradiente sutil, contorno fino (`--tr-card-line`)
  e sombra dura curta + sombra difusa. No claro o card é quase branco.
- **Tomada** lateral: carcaça grafite saindo da borda do card, com um quadrado
  na cor do **tipo** na face. O fio encosta na face da tomada.
- **Fio** numa cor só (`--tr-fio`, grafite neutro), com um trilho largo e
  translúcido por baixo para dar volume. Uma cor única não disputa atenção
  com papéis nem tipos, e ainda deixa ler o percurso.
- **Fio de fluxo (stream)**: mesmo grafite, tracejado e **parado**. Nada no
  canvas pisca ou anima como decoração; animação fica para sinal de vida
  (barra de progresso indeterminada).
- **Hover** só no card (ele sobe um pouco). Passar o mouse não mexe nos fios.

---

## 6. Rejeitado (e por quê)

| Ideia | Motivo |
| --- | --- |
| Cor por coleção ou por categoria livre | Cada coleção escolhia a sua; um teste e uma avaliação ficavam com cores sem relação. |
| Seis papéis com "Análise" | Juntava construir e ler o modelo; o canvas de um experimento ficava todo verde. |
| Tijolo/vermelho para Avaliação | Confundia com falha. |
| Formatos por família de tipo nas tomadas (quadrado, losango, triângulo…) | Exigiria o núcleo conhecer famílias de tipo ou uma API nova em `tr_type`; ficou só o quadrado na cor do tipo. Pode voltar como `tr_type(shape =)`. |
| Fio colorido pela família do dado | Muitas cores no canvas; o tipo já está na tomada. |
| Fio animado (pulsos) | Chamava atenção o tempo todo sem informar nada novo. |
| Cabo emborrachado com plugue | Bonito isolado, pesado no canvas cheio. |
| Destacar os fios do card no hover | Mudava o canvas inteiro por um gesto casual. |

---

## 7. Pendências

- A **paleta de tipos** das portas (`tr_type(color =)` nas coleções) ainda é a
  saturada antiga. Com cabeçalhos sóbrios, as tomadas chamam mais atenção que
  o card; avaliar suavizá-la na mesma linha.
- `tools/video` tem uma cópia dos estilos do card (`video.css`, `theme.ts`) e
  ainda mostra o visual anterior.
- Molduras cheias de gráficos do dado bruto ficam todas violeta (Inspeção);
  observar se pesa.
- O modo mini herdou o material novo, mas não o layout de "placa horizontal"
  testado no laboratório.
