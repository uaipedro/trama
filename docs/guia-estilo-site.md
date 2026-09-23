# Guia de estilo do site do trama

Este guia orienta o site de documentação, as ajudas ligadas a ele, roteiros de
vídeo e materiais de apresentação. Parte do rigor do guia de redação
estatística do projeto, mas atende a outra tarefa: permitir que alguém construa
e interprete um fluxo sem que a documentação esconda sua lógica.

## Princípio editorial

O site explica o trabalho que um bloco realiza, os dados de que depende, o
resultado que produz e as condições sob as quais a interpretação muda. Não
vende o trama, não dramatiza dificuldades e não simula conversa para parecer
acessível.

O leitor é capaz de acompanhar a explicação. Quando lhe falta repertório para
isso, a **camada facilitadora** oferece apenas o contexto necessário, no ponto
em que ele se torna necessário. Ela não troca termos técnicos por eufemismos,
não reconta a página e não presume incapacidade.

## Arquitetura da explicação

Cada página de bloco segue esta ordem, ajustando os títulos ao conteúdo:

1. **O que o bloco faz** — verbo, entrada, transformação e forma do resultado.
2. **Quando usar** — pergunta analítica ou situação de trabalho que o bloco
   atende.
3. **Configuração** — parâmetros na ordem em que mudam a decisão da pessoa,
   com os nomes usados na interface.
4. **Exemplo** — fluxo curto, completo e executável, com dados plausíveis. Todo
   bloco ```r que usa `tr_add` vira automaticamente um mini-canvas (cards
   ligados por setas) com uma aba "Código R" ao lado; não há mockup separado
   para manter. Ao escrever o exemplo:
   - o id curto passado a `tr_add("id", "tipo", ...)` só aparece no `<code>`
     do card — o título vem do rótulo do bloco no catálogo, então não repita o
     nome do bloco no id;
   - cada bloco do qual o exemplo depende precisa também aparecer como um
     `tr_add` no mesmo trecho, na ordem em que é usado; um bloco citado só em
     prosa não entra no canvas;
   - o `from` tem que ser explícito em todo `tr_add` que não é a raiz do fluxo
     (`from = "id"` ou `from = c("id1", "id2")` para vários pais) — sem ele a
     aresta não é desenhada.
5. **Como interpretar** — significado das colunas, medidas, opções e
   condições que alteram a leitura.
6. **Veja também** — blocos relacionados por sequência ou pergunta, exibidos
   pelo site com título, descrição e ícone.

As seções podem ser combinadas quando a página for curta. A ordem das ideias
deve permanecer: ação, escolha, configuração, exemplo e leitura do resultado.

### Regra de foco

Descreva primeiro o que o bloco produz e como a pessoa usa esse resultado.
Não organize a página em torno de proibições ou de uma lista do que o bloco
não faz. Uma distinção necessária entra como consequência positiva da escolha:
“cada linha resume uma região” é melhor que “o bloco não mantém as linhas”.

O contexto adicional explica um conceito indispensável naquele ponto. Ele pode
definir uma medida, mostrar como uma opção muda a saída ou explicar o formato
da tabela. Não deve repetir a seção principal nem transformar a página em aula
genérica.

Guias de tarefa seguem o fluxo de trabalho real (por exemplo, ler, inspecionar,
limpar e transformar); catálogo e referência preservam a estrutura do produto
(coleção, categoria, bloco e parâmetro). Uma página não deve tentar cumprir os
dois papéis de uma vez.

## Três registros compatíveis

### Referência

Usada em páginas de bloco, parâmetros, tipos, entradas e saídas. É concisa,
declarativa e verificável. Descreve contratos e comportamento: “`data/filter`
mantém as linhas cuja expressão produz `TRUE`.”

### Guia

Usado em tarefas e receitas. Expõe a sequência e a razão de cada decisão:
“A inspeção vem após a leitura porque revela os tipos e faltantes que definem
as transformações seguintes.” Explicação não é preenchimento: cada parágrafo
deve habilitar uma escolha, uma ação ou uma interpretação.

### Camada facilitadora

Usada apenas para repertório que a página pressupõe. É curta e local:
definição, mini-diagrama, vocabulário ou consequência. Recebe um título que
declara sua função — “Antes de continuar”, “Como ler esta tabela” ou “Por que
esta escolha muda o resultado” —, nunca “Para iniciantes”.

## Voz e construção

- Usar português brasileiro, termos estáveis do domínio e verbos específicos:
  ler, inspecionar, agrupar, calcular, converter, representar, comparar,
  estimar, gravar.
- Preferir sujeito concreto. Blocos, tabelas, colunas e gráficos fazem ações;
  não “a gente”, “o usuário” ou um narrador genérico.
- Usar presente para comportamento geral. Em receitas, o imperativo pode ser
  usado apenas em instruções operacionais claras; a justificativa vem na mesma
  frase ou no parágrafo seguinte.
- Definir termo técnico na primeira ocorrência necessária e reaproveitá-lo sem
  trocar por sinônimos. A definição entra na camada facilitadora quando não for
  indispensável ao leitor experiente.
- Declarar condições de validade junto do efeito: “O gráfico de barras soma os
  valores por padrão; calcule a média antes quando a comparação for entre
  médias.”
- Nomear incerteza como incerteza: hipótese, convenção, consequência e limite
  não são escondidos em aviso genérico.

## Contra o texto genérico

Não usar preâmbulos vazios, elogios ao recurso, perguntas retóricas, frases de
efeito, metáforas, personificação, tríades decorativas ou contrastes em série.
Evitar “em resumo”, “vale destacar”, “poderoso”, “intuitivo”, “sem esforço”,
“basta”, “na prática” e “este guia irá”.

Uma afirmação abstrata precisa virar uma afirmação observável. “Organize seus
dados” não orienta; “converta `mes` de três colunas (`jan`, `fev`, `mar`) para
uma coluna quando o mês será usado como eixo do gráfico” orienta.

Não criar exemplos artificiais quando houver um exemplo realista, pequeno e
reproduzível. Exemplo tem entradas, transformação e resultado esperável. Não
usar blocos de código que não possam ser executados no estado descrito.

## Títulos, listas e destaque

Títulos são sintagmas nominais que nomeiam conteúdo ou decisão: “Dados antes
do gráfico”, “Chaves repetidas na junção”, “Parâmetros de leitura”. Não são
promessas, apelos ou perguntas de marketing.

Listas são reservadas a opções paralelas, pré-requisitos e resultados
enumeráveis. Raciocínio, interpretação de gráfico e comparação entre escolhas
ficam em prosa. Negrito marca um termo definido ou rótulo de interface na
primeira ocorrência; não é usado para criar ritmo visual em frases inteiras.

Tabela e figura devem se sustentar: título, unidades, variáveis e condições
necessárias à leitura ficam nelas ou na legenda. O texto interpreta o padrão;
não reproduz todos os valores.

## Paleta semântica de tons

O `FlowMap` (o "caminho de trabalho" mostrado nos cards de coleção e na
navegação por etapas) colore cada passo por `tone`, não por bloco individual.
Os sete tons ficam centralizados em `site/src/styles/site.css`, em `:root`,
como `--tone-source` … `--tone-sink`; `.flow-step--*` aponta para eles. Não
há cor solta redeclarada fora desses tokens — para mudar uma cor de tom,
muda-se o token.

| Tom (`tone`) | Significado no fluxo | Token CSS | Cor |
| --- | --- | --- | --- |
| `source` | Entrada: ler ou apontar a origem dos dados | `--tone-source` | `oklch(48% .14 285)` (roxo) |
| `inspect` | Inspeção: conhecer, diagnosticar, avaliar precisão | `--tone-inspect` | `oklch(50% .12 80)` (âmbar) |
| `clean` | Limpeza: corrigir, remover, padronizar | `--tone-clean` | `oklch(48% .1 182)` (verde-azulado) |
| `transform` | Transformação: reorganizar, ajustar, treinar, modelar | `--tone-transform` | `var(--blue-dark)` (azul escuro) |
| `reshape` | Remodelagem: mudar o formato da tabela ou estrutura | `--tone-reshape` | `oklch(50% .14 345)` (rosa) |
| `aggregate` | Agregação: resumir, estimar, produzir gráfico ou resultado | `--tone-aggregate` | `oklch(46% .13 305)` (violeta) |
| `sink` | Saída: gravar, exportar, encerrar o fluxo | `--tone-sink` | `oklch(46% .12 145)` (verde) |

Essa paleta é do site (navegação e narrativa), independente da cor de
categoria (`category$color`) que o registry atribui a cada bloco e que
aparece nos cards do canvas de exemplo (`--node-accent`, ver
`site/src/lib/flow-canvas-html.ts`). As duas paletas hoje não se comunicam;
a proposta de aproximá-las está em
`docs/future-ideas/paleta-semantica-e-instalador.md`.

## Revisão antes de publicar

1. O texto permite decidir, configurar, conferir ou interpretar algo?
2. O comportamento descrito é conferido no código, teste ou fluxo de exemplo?
3. Cada condição está ligada à consequência que produz?
4. O vocabulário coincide com o editor e com o glossário do projeto?
5. A camada facilitadora acrescenta repertório, em vez de resumir o principal?
6. Há frase que poderia caber em qualquer produto de dados? Se houver, ela é
   removida ou concretizada.

## Execução por coleção

Use este roteiro ao delegar uma coleção a outro agente:

1. Leia `docs/guia-estilo-site.md`, a página de visão geral da coleção e todas
   as páginas de bloco já revisadas.
2. Para cada bloco, leia o registro em `collection.R`, a implementação em
   `R/` e os testes que exercitam o comportamento. A documentação sai do
   código, dos testes e dos exemplos reais; não complete lacunas por intuição.
3. Revise a página em `site/src/content/docs/colecoes/<colecao>/`. Preserve o
   frontmatter e o identificador `node`; atualize `title`, `description` e
   `related` apenas quando isso melhorar a navegação.
4. Use a estrutura “O que o bloco faz / Quando usar / Configuração / Exemplo /
   Como interpretar”. Inclua `Antes de continuar` somente quando houver
   contexto necessário para entender a decisão.
5. Escreva exemplos compatíveis com a assinatura real. Mostre o fluxo em R;
   quando houver um mockup estático de cards disponível, use o mesmo fluxo e os
   mesmos valores para que a representação visual e o código coincidam.
6. Relacione no mínimo dois blocos próximos quando existirem. Prefira a
   sequência real da coleção a uma lista genérica.
7. Não altere implementação, testes, catálogo ou contratos para acomodar a
   redação. Se o comportamento estiver ambíguo, registre a dúvida para quem
   coordena a rodada.
8. Ao entregar, relate páginas tocadas, comportamento conferido no código e
   testes consultados. Rode `git diff --check`; não rode geradores globais ou
   `roxygenise()` em paralelo com outros agentes.

### Prompt curto para o agente

> Revise a documentação da coleção `<coleção>` seguindo
> `docs/guia-estilo-site.md`. Leia a implementação, o registro do bloco, os
> testes e os exemplos antes de editar. Atualize somente as páginas Markdown
> em `site/src/content/docs/colecoes/<coleção>/`. Para cada bloco, explique o
> que produz, quando usar, como configurar, um exemplo executável e como ler o
> resultado. Mantenha o foco no comportamento positivo do bloco. Use o contexto
> adicional apenas para conceitos necessários. Preserve frontmatter, nomes de
> parâmetros e contratos. Relacione blocos próximos em `related`. Não invente
> comportamento, não altere código e não rode geradores globais. Entregue um
> resumo dos arquivos alterados, evidências consultadas e dúvidas restantes.
