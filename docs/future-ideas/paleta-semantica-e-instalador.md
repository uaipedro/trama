# Paleta semântica por categoria e instalador avulso com R

> **Estado: ideia parada.** Registrada em 2026-09-23, na Fase 5 (opcional) do
> plano `docs/plans/2026-09-23-site-personalidade-fast-plan.md`, junto com a
> centralização dos tokens de tom (`--tone-source` … `--tone-sink`, ver
> `docs/guia-estilo-site.md`). Não é plano de implementação.

## (a) Mapear `category$color` do registry para os tons semânticos

Hoje o site tem **duas paletas de cor que não se conversam**:

1. **Tons de etapa do `FlowMap`** (`site/src/components/FlowMap.astro`,
   `.flow-step--*` em `site/src/styles/site.css`) — sete cores fixas
   (`--tone-source` … `--tone-sink`), uma por `tone` de workflow
   (`source`, `inspect`, `clean`, `transform`, `reshape`, `aggregate`,
   `sink`). Usadas na navegação e nos cards de coleção, atribuídas à mão em
   `site/src/data/collections.ts`.
2. **Cor de categoria do registry** (`category$color`, exportada por
   `tools/site/export-node-visuals.R` como `visual$accent` em
   `site/src/data/node-visuals.json`) — uma cor por categoria de bloco
   (ex.: `modelo_fonte`, `modelo_anova`), usada como `--node-accent` no
   canvas de exemplo (`site/src/lib/flow-canvas-html.ts`) e, no editor
   (`inst/www/`), no card de cada nó.

Ou seja: a mesma ideia de "isto é uma etapa de leitura" / "isto agrega" já
existe duas vezes, com fontes de verdade diferentes — uma em TypeScript
(`FlowMap`), outra no registry R (`category$color`) — e sem garantia de que
as cores combinam. Um bloco de `modelo_fonte` pode ter um `accent` que não
lembra em nada o roxo de `--tone-source`, mesmo que ambos representem
"entrada".

**A proposta:** cada categoria do registry passaria a declarar, além da cor
livre atual, qual dos sete tons semânticos ela representa (ou herdar um
`tone` por convenção de nome de categoria). O editor e o canvas de exemplo
do site usariam `--tone-*` (ou uma cor derivada dele) em vez de uma cor
arbitrária por categoria — a cor deixaria de ser "a cor daquela categoria" e
passaria a ser "a cor daquele papel no fluxo", consistente entre editor,
canvas de exemplo e `FlowMap`.

**O que isso mexe, e por que não entrou neste plano:**

- **Coleções** (`trama`, `trama.data`, `trama.view`, `trama.models`, ...):
  cada categoria (`registry$categories`) ganharia um campo novo (`tone` ou
  equivalente) além de `color`. Isso é uma mudança de contrato do registry,
  não só de CSS — precisa de decisão sobre quantas categorias por coleção
  mapeiam bem para os sete tons (algumas, como `modelo_medias` ou
  `modelo_anova`, não são obviamente `transform` nem `aggregate`) e se o
  `color` livre desaparece ou vira um tom + variação.
- **Editor** (`inst/www/`): hoje pinta os nós com `category$color` direto;
  passaria a resolver por tom, o que muda a aparência de todo fluxo aberto
  no app, não só do site.
- **Exportação** (`tools/site/export-node-visuals.R`): passaria a emitir
  `tone` (ou uma cor já resolvida a partir dele) em vez de repassar
  `category$color` cru.

Nenhuma dessas três pontas foi tocada nesta fase — o plano manteve as cores
atuais e só centralizou os tons do `FlowMap` em tokens CSS
(`docs/guia-estilo-site.md`, seção "Paleta semântica de tons").

## (b) Instalador avulso que já traz o R

Hoje existem dois caminhos de instalação, nenhum deles "clique e pronto"
sem pré-requisito:

- **`tools/trama-cli/`** (ativo) — CLI Node/npm (`npm install -g
  @uaipedro/trama-cli`, comandos `trama install`, `create`, `add`, `open`).
  Baixa o R portátil da Posit, instala o núcleo (`trama` +
  `trama.data` + `trama.view`) e as coleções escolhidas, sobe o editor e
  abre o navegador. **Pressupõe que a pessoa já tem Node/npm instalado** —
  é essa cadeia de confiança do Node que resolve o problema de assinatura
  de executável (ver abaixo).
- **`tools/launcher/`** (**depreciado**, ver `tools/launcher/DEPRECATED.md`)
  — o launcher original, um app desktop em Go + Wails (`wails.json`,
  `main.go`, `app.go`, frontend em `frontend/`) com a mesma missão: baixar o
  R portátil (`internal/rfetch`), rodar a instalação via `pak`
  (`internal/pakrunner`), checar o ambiente (`internal/envcheck`) e abrir o
  editor (`internal/applauncher`). Foi substituído porque um executável
  nativo sem assinatura de código esbarra no Windows Defender/SmartScreen
  sem contorno gratuito e confiável (razão registrada em
  `docs/plans/2026-09-19-trama-cli-design.md`). A lógica de
  download/checksum do R portátil e o fix do Rtools foram portados quase
  verbatim para o `trama-cli`; o código do launcher fica no repositório só
  como referência, sem novas releases.

Isso deixa uma lacuna: **quem não tem Node/npm** — o público mais distante
de programação, que é também quem mais se beneficiaria de um instalador que
não pede nenhuma ferramenta prévia — ainda não tem caminho. O `trama-cli`
resolveu o problema de assinatura ao não distribuir executável nenhum, mas
trocou "precisa de R" por "precisa de Node".

**A ideia (não decidida):** um instalador avulso, de fato "clique e
funciona", que:

- não depende de Node/npm nem de R já instalados — traz (ou baixa na hora)
  tudo o que falta, R portátil incluso, reaproveitando a lógica já
  existente em `tools/launcher/internal/rfetch` e `internal/pakrunner`
  (ou a versão portada em `tools/trama-cli/src/core/rfetch.ts` e
  `installer.ts`, que é a mais recente e testada);
- resolve o problema de assinatura que matou o launcher em Go — os
  caminhos discutidos até agora (registro num programa gratuito de
  assinatura tipo SignPath, ou embutir o instalador num formato que já
  carregue confiança, como um pacote da própria Microsoft Store / winget)
  não foram avaliados a fundo;
- fica **fora do site** (ele mesmo) — o site só linka pra ele, como nota em
  `por-dentro/instalacao.md` (Fase 3, tarefa 3.3 deste mesmo plano, que já
  deixou a frase "em breve" para esse instalador).

**Perguntas em aberto:**

- Vale reviver o launcher Go (que já resolve boa parte da UX: tela de
  progresso, seleção de coleções) só trocando a forma de distribuir o
  binário, ou compensa reescrever num instalador nativo por plataforma
  (Inno Setup no Windows, `.pkg` no mac, `.deb`/`AppImage` no Linux) que
  invoque o mesmo fluxo de download?
- Assinatura de código tem custo recorrente (certificado) ou dá pra
  resolver via SignPath (gratuito para projetos open source, mas com fila
  de aprovação) — nenhuma das duas opções foi levada adiante desde a
  decisão registrada em `docs/plans/2026-09-19-trama-cli-design.md`.
- Esse instalador teria GUI própria (voltando ao Wails, ou outra stack) ou
  seria só um script/binário que, ao final, abre o navegador no editor —
  como o `trama-cli` já faz, mas sem depender do terminal?
