# Mapa de decisões — site de documentação do trama

## #1: Como o site atende diferentes bagagens sem duplicar a documentação?

Tipo: Grilling

### Pergunta

O conteúdo deve ser separado em uma trilha para iniciantes e outra para
pessoas experientes, ou a mesma página deve oferecer profundidades diferentes?

### Resposta

Cada página possui um **conteúdo principal único**, escrito com precisão e sem
condescendência. Um controle persistido no navegador ativa a **camada
facilitadora**: callouts, pequenas definições, contexto prévio, orientações de
leitura e atalhos de decisão. Essa camada não reescreve, simplifica nem troca
o conteúdo principal; ela entrega a bagagem pontual para que alguém sem
experiência prévia acompanhe a mesma explicação.

O estado é local ao navegador (por exemplo, `localStorage`) e pode influenciar
sutilmente a interface — como contraste, densidade e presença de pistas —,
mas não cria dois produtos visuais. A identidade permanece uma só.

Termos acordados:

- **Conteúdo principal**: a explicação canônica, legível por qualquer pessoa.
- **Camada facilitadora**: material adicional, opcional e contextual que torna
  explícito o conhecimento prévio necessário.
- **Leitor**: pessoa que aprende ou consulta; não se usa “iniciante” como
  rótulo de capacidade.

## #2: Qual é a voz editorial e quais são as regras contra o “AI slop”?

Blocked by: #1
Type: Research + Grilling

### Pergunta

Como transformar o guia de escrita de referência em um guia editorial próprio
do trama, aplicável a guias, catálogo de blocos, vídeos e apresentações?

### Resposta

O site adota um guia editorial próprio (`docs/guia-estilo-site.md`). A
referência estatística contribui com rigor de vocabulário, ordem de exposição,
condições de validade próximas à consequência e recusa de retórica vazia. O
site não adota sua voz impessoal de relatório em todos os casos: referência,
guia e camada facilitadora são registros distintos, com o mesmo padrão de
precisão.

O critério contra texto genérico é operacional: toda frase deve permitir uma
decisão, configuração, conferência ou interpretação específica do trama. A
camada facilitadora entrega repertório adicional, não uma cópia simplificada
da página.

## #3: Qual sistema visual pode dar uma identidade durável ao trama?

Blocked by: #1, #2
Type: Prototype

### Pergunta

Como traduzir “fluxos de computação visíveis” em marca, tipografia, paleta,
ilustrações, componentes do site, cenários de vídeo e slides, sem depender do
ícone provisório atual?

### Resposta

Direção acordada: o trama é uma ferramenta profissional de organização do
pensamento. A interface não reduz a complexidade do trabalho por aparência;
ela torna visíveis as partes, as relações e o próximo passo possível, para que
a pessoa possa conduzir o problema com confiança.

A identidade deve combinar sobriedade técnica, acabamento contemporâneo e
sinais claros de orientação. Não deve parecer infantil, corporativa genérica
nem uma promessa de automatização mágica. O piloto Astro estabelece tema claro
de leitura, tipografia editorial legível, azul como cor de ação e mapas de
fluxo como elemento estrutural. O sistema será refinado com o uso.

Referência estratégica: o trama busca ser uma entrada visual e interativa para
o R — análoga, em ambição de acessibilidade, a ferramentas de automação por
fluxos, mas enraizada no ecossistema R. A primeira experiência precisa ser
simples; o mesmo ambiente deve revelar capacidades progressivamente, sem
trocar a pessoa de ferramenta ou impor um teto artificial.

Horizonte de linguagem: “raciocínio à vista” nomeia uma direção de produto,
não uma promessa literal do estado atual. O trama não infere o raciocínio de
alguém; ele pode tornar explícitos o procedimento, as escolhas, os dados
intermediários e os resultados de uma análise. Uma formulação pública deve
preservar essa distinção.

Formulação pública adotada: **R em blocos executáveis**. Ela descreve o
produto atual de forma curta, própria e verificável: R é o motor, blocos são a
unidade de composição visual e o fluxo é executável. As ideias de organização
do pensamento e de raciocínio à vista orientam produto, experiência e
documentação, mas não substituem essa frase enquanto não forem uma promessa
mais concreta.

## #4: Qual é o formato-piloto da coleção Dados?

Blocked by: #2, #3
Type: Prototype

### Pergunta

Que páginas, componentes e exemplos compõem um primeiro recorte publicado no
GitHub Pages para `trama.data`, e como os helps do editor apontam para ele?

### Resposta

O piloto usa Astro em `site/`. Páginas públicas são Markdown em
`site/src/content/docs/`, com frontmatter validado por content collections;
componentes Astro cuidam de catálogo, rotas, diagramas e moldura. A autoria é
Markdown primeiro: a camada facilitadora é uma citação marcada por “Antes de
continuar”, que o site transforma em callout opcional e persiste no navegador.

O recorte inclui a entrada, um primeiro fluxo, catálogo e páginas iniciais de
`trama.data` e `trama.view`. O GitHub Actions constrói `site/` e publica no
GitHub Pages. Os links de help do editor entram apenas para páginas de bloco já
publicadas; a expansão ocorre bloco a bloco, evitando destinos vazios.

## #5: Como o piloto se expande para Visualização e demais coleções?

Blocked by: #4
Type: Grilling

### Pergunta

Quais elementos são invariantes entre coleções e quais pertencem ao domínio de
Dados, Visualização, Séries e outras?

### Resposta

Em aberto.
