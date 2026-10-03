# AGENTS.md

trama: editor visual de fluxos de análise estatística em R (Shiny + canvas JS). Núcleo genérico em `R/` + `inst/www/`; todo o domínio estatístico vive em coleções (`collections/trama.*`).

Antes de assumir onde algo está, leia `docs/mapa.md`. Vocabulário (papel, ajuste, leitura, tomada…) em `CONTEXT.md`.

## Regras que não se quebram

- **Núcleo sem domínio.** Nada de estatística, dplyr ou ggplot em `R/`. `tests/testthat/helper-collection.R` testa o núcleo com uma coleção falsa — mantenha assim.
- **Rigor numérico.** Bloco ou mudança estatística precisa de teste com oráculo publicado (exemplo resolvido ou pacote de referência, tolerância declarada) e de `tr_ref`/`tr_pressuposto` conferidos na fonte, nunca de memória. Mudou resultado ou padrão → sobe `version` do nó. Registrar em `docs/revisao-metodologica.md`.
- **Um verbo, um bloco.** Prever, avaliar, ROC, importância, coeficientes existem só em `trama.models`. Coleções produzem `models/fit` (contrato em `collections/trama.models/R/contrato.R`), não tipo próprio.
- **Glossário de params é travado** (`docs/glossario-parametros.md`, `tests/testthat/test-glossario.R`). Renomear param = migração (`tr_node(migracoes=)` / `tr_collection(migrations=)`).
- **Params de coluna** usam `tr_param_col(...)`; opcionais (cor, rótulo, painel) com `suggest = FALSE`.
- **Repo público.** Dados de cliente e `docs/plans/` (local, no `.gitignore`) nunca são commitados.
- **Publicar = subir `Version`** do núcleo e de cada coleção tocada; se a coleção usa API nova do núcleo, `trama (>= x.y.z)` no Imports e entrada no `NEWS.md`. O r-universe reconstrói da `main`.

## Gotchas

- Templates em `collections/*/inst/templates/*.json` são gerados por `tools/templates/gerar.R`; não edite o JSON à mão.
- `inst/www/editor.js` (~4,4 mil linhas) não carrega sob `node --test`. Lógica com ramo vai para um módulo puro (`geometria.js`, `modos.js`, `colunas.js`…) com teste em `tests/js/`.
- Teste de coleção só roda com núcleo e dependências carregados via `pkgload::load_all`, na ordem. Pacotes instalados ficam desatualizados e dão falso resultado.
- `R CMD check` só reclama de não-ASCII em strings de código; comentários e roxygen acentuados passam.
- `_problems/`, `testthat-problems.rds`, `*.Rcheck/`, `Rplots.pdf` são lixo de execução.

## Mexer na trama aberta

Com o editor de pé (`tr_app()`), `inst/bin/trama-agente` edita e lê o fluxo da
tela: `help`, `state`, `catalog`, `add`, `link`, `set`, `result`, `undo`. Sem
editor: `catalog`, `explain <tipo>`, `validate <fluxo.json>`. No
repo, rode com `TRAMA_DEV=$PWD Rscript inst/bin/trama-agente ...`. Uso e
formatos: `site/src/content/docs/por-dentro/agentes.md`.

## Verificar

Use a skill `verificar-trama` (`.claude/skills/verificar-trama/`). Resumo:

```sh
Rscript tools/check.R --plano    # o que rodaria
Rscript tools/check.R --rapido   # durante o trabalho
Rscript tools/check.R            # antes de dizer "pronto": afetados + dependentes
```

Não rode a suíte inteira por reflexo. Pronto = `tools/check.R` sem `--rapido` saiu 0, e, para mudança de interface, o editor foi aberto e o fluxo testado à mão.

## Autonomia

Siga sem perguntar quando o próximo passo não depende de decisão do usuário. Pare antes de algo destrutivo ou externo: apagar dados, force-push, push, publicar, mexer fora do repo.
