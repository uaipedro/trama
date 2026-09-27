# trama 0.4.0

* Canal de controle para agentes. `tr_app()` sobe um servidor em 127.0.0.1,
  protegido por token (`.trama/control-agente.json`), por onde um agente lê e
  edita o fluxo aberto na tela. O CLI `trama-agente` (`inst/bin/`, ou
  `tr_cli()`) tem `state`, `catalog`, `add`, `link`, `set`, `rm`, `op`,
  `apply`, `result` e `undo`. As edições passam pelo mesmo caminho do editor:
  revisão, undo, autosave e re-execução. Desligue com
  `options(trama.controle = FALSE)`.
* `httpuv` passa a ser Imports (já vinha com o `shiny`); `curl` entra em
  Suggests, usado só pelo CLI.
