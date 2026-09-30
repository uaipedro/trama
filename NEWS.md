# trama 0.5.0

* Catálogo de bases públicas. As coleções declaram bases de pacotes R
  (`tr_collection(datasets = list(tr_dataset(...)))`), só com metadados:
  fonte, tamanho, temas e licença. O botão "Bases públicas" da barra abre um
  modal com busca, filtro por tema e pacote, prévia, instalação do pacote em
  um R à parte (com confirmação), "Baixar CSV" e "Adicionar ao canvas", que
  insere o bloco declarado pela base. Novas funções: `tr_dataset()`,
  `tr_datasets()` e `tr_dataset_load()`.

# trama 0.4.0

* Próximo bloco como fantasmas. O "+" do card, ou soltar um conector no vazio,
  abre uma busca e até três miniaturas translúcidas dos blocos sugeridos,
  ligadas à porta por uma conexão tracejada. Digitar filtra com busca
  aproximada (sem acento, início de palavra, letras em sequência) entre todos
  os blocos compatíveis. Enter/Tab aceita, setas navegam, Esc fecha.
  Preferência "Sugestões" liga ou desliga as três iniciais; a busca vale
  sempre. Substitui o popover de tagzinhas.
* Canal de controle para agentes. `tr_app()` sobe um servidor em 127.0.0.1,
  protegido por token (`.trama/control-agente.json`), por onde um agente lê e
  edita o fluxo aberto na tela. O CLI `trama-agente` (`inst/bin/`, ou
  `tr_cli()`) tem `state`, `catalog`, `add`, `link`, `set`, `rm`, `op`,
  `apply`, `result` e `undo`. As edições passam pelo mesmo caminho do editor:
  revisão, undo, autosave e re-execução. Desligue com
  `options(trama.controle = FALSE)`.
* `httpuv` passa a ser Imports (já vinha com o `shiny`); `curl` entra em
  Suggests, usado só pelo CLI.
