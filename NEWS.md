# trama 0.4.1

* O aviso "não existe na entrada" do campo de colunas (`sumidas`, em `colunas.js`) não acusa mais valores escritos como seletor (`starts_with("x")`, `a:c`): quem valida é o R. Necessário para o `data/select` da `trama.data` 0.3.0, que passa a aceitar seletores.

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
