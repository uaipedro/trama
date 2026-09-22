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

Cada página responde, nesta ordem, ao que a pessoa precisa decidir:

1. **Finalidade** — qual pergunta ou transformação o bloco atende.
2. **Quando se aplica** — forma esperada da tabela, entradas e condição de uso.
3. **Como configurar** — parâmetros em ordem de consequência, não de ordem
   interna.
4. **Resultado** — o que sai do bloco e como ele se conecta ao próximo passo.
5. **Condições e limites** — o que altera o resultado, explicitamente ligado à
   consequência.
6. **Exemplo verificável** — fluxo pequeno, completo e compatível com o
   comportamento real da coleção.
7. **Próximos caminhos** — operações relacionadas por motivo, não por lista
   automática.

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

## Revisão antes de publicar

1. O texto permite decidir, configurar, conferir ou interpretar algo?
2. O comportamento descrito é conferido no código, teste ou fluxo de exemplo?
3. Cada condição está ligada à consequência que produz?
4. O vocabulário coincide com o editor e com o glossário do projeto?
5. A camada facilitadora acrescenta repertório, em vez de resumir o principal?
6. Há frase que poderia caber em qualquer produto de dados? Se houver, ela é
   removida ou concretizada.
