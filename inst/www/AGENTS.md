# inst/www/AGENTS.md

O editor no navegador: ES modules sem build, React e xyflow em `vendor/`.
`R/app.R` monta o importmap (`"trama"` -> `runtime.js`) e a ordem de carga:
`runtime.js` primeiro, depois o JS de cada coleção (`collections/*/inst/trama/`),
por fim `editor.js`. O editor é dirigido por catálogo: não conhece nenhum tipo
de nó, de dado ou categoria.

## Módulos

| Arquivo | Papel | React? | Teste |
| --- | --- | --- | --- |
| `runtime.js` | Contrato público: `registerRenderer`, `registerWidget`, `h`, `Segmented`, `Toggle`, `NumberField`, `Regua`, `Estrelas`, temas. | sim | indireto |
| `editor.js` | Canvas, cards, sessão (~4,6 mil linhas). Não carrega sob `node --test`. | sim | à mão |
| `params.js` | Regras dos widgets de param. | não | `params.test.mjs` |
| `colunas.js` | Sugestão de colunas. | não | `colunas.test.mjs` |
| `modos.js` / `modos-ui.js` | Modos do card e atalhos / peças React deles. | não / sim | `modos.test.mjs` |
| `geometria.js` / `frames.js` | Geometria dos frames / frames na tela. | não / sim | `geometria.test.mjs` |
| `notas.js` | Notas (markdown, imagem) no canvas. | sim | — |
| `markdown.js` | Markdown para React sem HTML cru. | via `h` | `markdown.test.mjs` |
| `ops.js` | Espelho das ops cosméticas de `R/document.R`. | não | `ops.test.mjs` |
| `sugestor.js`, `fantasmas.js`, `historico.js`, `proximo.js`, `proximo-foco.js` | Próximo bloco: ranking, previews, histórico, popover, teclado. | `proximo.js` sim | `sugestor`, `fantasmas`, `historico`, `proximo.test.mjs` |
| `rotas.js` | Rota ortogonal dos fios: desvia de todos os cards, separa fios que dividem corredor em faixas. | não | `rotas.test.mjs` |
| `percurso.js` | Setas entre blocos (pai, filho, irmãos) e ordem do Desenrolar. | não | `percurso.test.mjs` |
| `papeis.js` | Cor do card pela categoria. | não | `papeis.test.mjs` |
| `bases.js`, `links.js` | Catálogo de bases; link colado vira bloco de leitura. | não | `bases`, `links.test.mjs` |
| `teste.js` | Regras do card de teste de hipótese. | não | `teste.test.mjs` |
| `temas.js` / `settings.js` | Regras de temas / painel de configurações. | não / sim | `temas.test.mjs` |

Testes em `tests/js/*.test.mjs`, rodados por `Rscript tools/check.R` (ou
`node --test tests/js/`).

## Regras

- `runtime.js` é API das coleções: mudar assinatura exportada quebra
  `collections/*/inst/trama/*.js`. Só acrescente; renderer é despachado por
  id de renderer (string do preview), nunca por tipo de objeto.
- Lógica com ramo vai para um módulo puro (sem React, sem `"trama"`) com teste
  em `tests/js/`; `editor.js` e módulos React só desenham.
- Nada de domínio aqui: cards específicos de uma coleção vivem em
  `collections/<x>/inst/trama/` (ex. `trama.sql/inst/trama/index.js`).
- Mudança de interface: abra o editor (`tr_app()`) e teste o fluxo à mão.
