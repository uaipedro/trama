# collections/AGENTS.md

Cada `collections/trama.<x>/` é um pacote R com uma função exportada,
`trama_collection()`, que devolve um `trama::tr_collection(...)`. Todo o
domínio estatístico mora aqui; o núcleo (`R/`) não sabe de nenhum.

## Anatomia

| Caminho | O que é |
| --- | --- |
| `DESCRIPTION` | `Version`, `Imports: trama (>= x.y.z)`, `Config/trama/requires:` (coleções que precisam entrar antes, ex. `trama.data`). |
| `R/collection.R` | `trama_collection()`: tipos, categorias, lista `nodes = list(...)`, migrações, `js=`/`css=`. |
| onde ficam os `tr_node(...)` | Varia: `R/nos_*.R` em `models`, `experiments`, `sampling`; `R/collection.R` em `data`, `series`, `sql`; `R/nodes.R` em `ml`; junto da lógica (`pca.R`, `catalogo.R`...) em `multi` e `view`. Ache com `grep -n 'tr_node(' collections/trama.<x>/R/*.R` e siga o padrão da coleção. |
| demais `R/*.R` | Lógica pura: função R comum, testável sem registro. |
| `R/docs*.R` | Pressupostos e referências (`trama::tr_pressuposto`, `trama::tr_ref`) por bloco. |
| `R/type.R` | Tipos de porta próprios (`tr_type`) e o preview de cada um. |
| `R/contrato.R` | Só em `models`, `ml`, `multi`: o contrato `models/fit` (dono: `trama.models`). |
| `inst/trama/` | JS/CSS do card (renderers via `registerRenderer` de `inst/www/runtime.js`). Referenciado por `js=`/`css=` no `tr_collection`. |
| `inst/templates/*.json` | Exemplos gerados por `tools/templates/gerar.R`. Não edite à mão. |
| `tests/testthat/` | Testes; `helper-*.R` monta dados e o registro da coleção. |
| `NEWS.md` | Mudanças por versão. |

Dependências entre coleções: veja o grafo em `docs/mapa.md`.

## Rodar os testes

Nunca contra pacotes instalados (dá falso resultado). Use
`Rscript tools/check.R trama.<x>` (carrega núcleo e dependências com
`pkgload::load_all` na ordem certa) ou a skill `verificar-trama`.

## Versões (três, não confundir)

- `version` do `tr_node`: sobe quando muda resultado, padrão de param ou
  portas. Param renomeado exige `migracoes =` no nó (ou `migrations =` na
  coleção).
- `Version` do `DESCRIPTION`: sobe a cada publicação da coleção tocada.
- `trama (>= x.y.z)` no `Imports`: sobe quando a coleção passa a usar API
  nova do núcleo (e entrada no `NEWS.md` do núcleo).

## Checklist: bloco novo

Passo a passo completo na skill `.claude/skills/novo-bloco/SKILL.md`.
Arquivos que um bloco toca:

1. `R/<logica>.R` — função pura.
2. Arquivo dos nós da coleção (ver acima) — `tr_node(...)` com
   `description`, `help` (copie as seções dos vizinhos; em `data` e `view`
   são travadas por `test-help.R` e `test-catalogo.R`), params via `tr_param*`
   (coluna: `tr_param_col`, opcional com `suggest = FALSE`). Não-ASCII em
   string de código vai como `\uXXXX`.
3. Garantir que o nó entra em `nodes =` de `trama_collection()` (direto ou
   pela função `.tr_<x>_nos_*()` que a coleção junta ali).
4. `R/docs*.R` — `tr_pressuposto`/`tr_ref` conferidos na fonte.
5. `tests/testthat/test-*.R` — oráculo publicado com tolerância declarada.
6. `docs/glossario-parametros.md` se o nome de param é novo (trava:
   `tests/testthat/test-glossario.R` no núcleo).
7. `docs/revisao-metodologica.md` — registro do bloco estatístico.
8. `DESCRIPTION` (`Version`) e `NEWS.md`.
9. Exemplo? `tools/templates/gerar.R` + rodar o script.
10. Site: `site/src/content/docs/colecoes/<pasta>/<bloco>.md`.

## Coleção nova

Pacote com `DESCRIPTION` (`Config/trama/requires`), `NAMESPACE` exportando
`trama_collection`, `R/collection.R` e `tests/testthat/`. Listas fixas que
ainda precisam do nome: `colecoes_glossario` em
`tests/testthat/test-glossario.R` e `tools/site/export-node-docs.R`.
`tools/check.R` descobre sozinho.
