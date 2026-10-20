#' Orientações de narrativa para o agente: como contar uma análise no canvas.
#'
#' Fonte única: o MCP serve nas `instructions` (o resumo) e na tool `guia`
#' (inteiro), o `trama-agente guide` imprime, e o site
#' (`por-dentro/agentes.md`) repete. Curto de propósito: o agente lê a cada
#' sessão.
#' @noRd
.tr_guia_narrativa <- c(
  "Conte a an\u00e1lise como narrativa: perguntas -> etapas -> conclus\u00e3o. Quem abre o fluxo l\u00ea da esquerda para a direita, frame a frame.",
  "1. Antes de montar, escreva as perguntas e as etapas (ex.: 1. Dados e descritivas, 2. Estacionariedade, 3. Modelo, 4. Diagn\u00f3stico, 5. Interpreta\u00e7\u00e3o). Uma etapa = um frame com t\u00edtulo numerado ('frame add \"1. Dados e descritivas\"').",
  "2. Monte cada etapa dentro do seu frame: 'add ... --frame F' (MCP: 'adicionar' com 'frame'), ou 'frame put F blocos...' para quem j\u00e1 existe.",
  "3. Abra cada frame com uma nota Markdown ('note add --frame F'): o que se testa ali e por qu\u00ea, em 2 a 4 linhas.",
  "4. Prenda uma nota aos resultados-chave ('note add --node N'): o que o n\u00famero diz, com o valor lido em 'result' (nunca calculado ou inventado). Ex.: \"ADF no log do IBV: p = 0,41; n\u00e3o rejeita raiz unit\u00e1ria.\"",
  "5. D\u00ea r\u00f3tulos que se leem sozinhos ('rename N \"ADF do log do Ibovespa\"'): o r\u00f3tulo vira legenda no documento.",
  "6. Feche com um frame de conclus\u00e3o s\u00f3 com nota: a resposta a cada pergunta, com os n\u00fameros das etapas.",
  "7. Depois de montar, chame 'layout': cada frame vira uma coluna na ordem dos frames, o ramo principal fica em cima, diagn\u00f3sticos e gr\u00e1ficos embaixo, irm\u00e3os empilhados. Ajuste \u00e0 m\u00e3o com 'move' e 'frame set --order'.",
  "Evite: dezenas de blocos numa linha s\u00f3; frame sem t\u00edtulo; nota que repete o nome do bloco; n\u00famero escrito de mem\u00f3ria.")
