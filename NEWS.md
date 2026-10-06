# trama 0.5.5

* Editor: **grupos** (Ctrl+G agrupa, Ctrl+Alt+G desagrupa; gravados no fluxo em `ui.grupos`, ops `add_grupo`/`remove_grupo`), **empilhar** na vertical (V) e na horizontal (H), **alinhar** por passos com Ctrl+setas, **espaçar igualmente**, e menu de contexto no vazio do canvas. O frame da seleção passa a Ctrl+Shift+G; a ajuda vai para F1, a vista em tela cheia para P e os parâmetros para Shift+P.

# trama 0.5.4

* Robustez do motor: o fluxo é gravado de forma atômica (crash no autosave não trunca mais `flows/main.json`); worker que morre (falta de memória, daemon morto) não vira erro cacheado; `fingerprint()` que lança invalida só o nó em vez de abortar o plano; handle sem objeto não conta como cache; o GC não apaga o `tmp/` de workers em voo; uma cadeia de `pump` por run; preview parcial com caminho relativo (gráfico em branco durante o run); arquivo de progresso atômico.
* A impressão digital das funções segue helpers de outras coleções do registro (inclusive `pkg::nome`): mudar um helper de `trama.models` invalida o cache dos blocos de `trama.experiments` que o usam.
* Proveniência: cada handle registra a versão do R e de cada pacote de coleção que o produziu; `tr_provenance(projeto)` lista isso por nó.
* O Quarto exportado ganha a seção "Referências dos métodos", com as `tr_ref` dos blocos usados no fluxo.
* Editor: limite de erro por card (renderer que lança não derruba a página), aviso quando o Shiny não conecta, refazer (Ctrl+Shift+Z / Ctrl+Y), revisão das ops em voo (duas edições rápidas não são mais recusadas) e cards memoizados.
* `trama-agente`: erros sempre em JSON; `catalog`, `explain` e `validate` funcionam sem o editor aberto.
* O componente de um renderer recebe `entradas`: os handles que chegam nas portas de entrada do card (os mesmos do `ctx` dos widgets). O núcleo só repassa; é o que deixa uma coleção comparar a saída com a entrada — o `data/table` mostra "−12 linhas".

# trama 0.5.3

* Widget de coleção recebe `ctx` também fora do kind `cols`: `{ id, valores, entradas }` — o nó, os valores dos params e o handle que chega em cada porta. É o que deixa um widget (o editor SQL de `trama.sql`) saber o que está ligado no bloco sem o núcleo saber o que há no handle.
* `tr_param_int(vazio = , example = )`: `vazio` é o valor que o campo vazio representa (o "automático"). Com o param nesse valor, o campo aparece vazio com `example` em cinza, e apagar o número volta a ele em vez de acusar "obrigatório".

# trama 0.5.2

* Exportar como Quarto gera um relatório, e não um bloco de código: um chunk por card, os frames como seções (na ordem de slide), as notas como texto, as saídas que nenhum fio consome à mostra, cabeçalho com sumário, código recolhível e HTML autocontido, versões dos pacotes no início e `sessionInfo()` no fim.
* O script exportado (R ou Quarto) nomeia as variáveis pelo rótulo do card (`correlograma_acf`, não o id), trata nó de várias saídas como uma variável lida por porta (`ajuste$out`) e passa os params EFETIVOS, como o executor: antes, um param não tocado caía no default da função, que pode diferir do default do bloco. Params iguais ao default literal da função são omitidos; o tema vira uma variável só no topo.
* `tr_type(report = )`: função exportada que mostra o valor no relatório exportado (uma série vira o gráfico dela). Sem ela, o relatório imprime o objeto.
* Exportar uma imagem de frame não pinta mais as ligações de preto.

# trama 0.5.1

* `options(trama.encerrar_ao_fechar = TRUE)` faz o processo do editor encerrar alguns segundos depois de a última janela fechar. O launcher liga essa opção ao abrir um projeto.
* `trama-agente`: `catalog --busca termo` devolve só os blocos que casam (a mesma busca do "+"), em vez do catálogo inteiro; toda edição aceita `--wait N`, que espera o fluxo rodar e devolve em `efeito` o status dos nós tocados e dos que estão abaixo deles. Nó `blocked` (em `state`, `result` e `efeito`) traz `causa`: os ancestrais que falharam.

# trama 0.5.0

* Catálogo de bases públicas. As coleções declaram bases de pacotes R
  (`tr_collection(datasets = list(tr_dataset(...)))`), só com metadados:
  fonte, tamanho, temas e licença. O botão "Bases públicas" da barra abre um
  modal com busca, filtro por tema e pacote, prévia, instalação do pacote em
  um R à parte (com confirmação), "Baixar CSV" e "Adicionar ao canvas", que
  insere o bloco declarado pela base. Novas funções: `tr_dataset()`,
  `tr_datasets()` e `tr_dataset_load()`.
* Colar um link sozinho no canvas cria um bloco "Ler dados" com ele.

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
