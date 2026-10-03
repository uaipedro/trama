---
name: novo-bloco
description: Use ao criar um bloco (nó) novo numa coleção do trama, ou ao mudar o resultado/params de um bloco existente — passo a passo com os arquivos exatos, oráculo publicado, glossário, versões, docs e verificação. Também quando pedirem "adiciona um bloco", "novo nó", "bloco de X na coleção Y".
---

# Bloco novo no trama

Anatomia da coleção e as três versões: `collections/AGENTS.md`. Regras
invioláveis: `AGENTS.md` (núcleo sem domínio, rigor numérico, um verbo um
bloco, glossário travado).

## 0. Antes de escrever

- O verbo já existe? Prever, avaliar, ROC, importância e coeficientes são só de
  `trama.models`; modelo novo produz `models/fit`
  (`collections/trama.models/R/contrato.R`), não tipo próprio.
- Ache um vizinho parecido e imite: `grep -n 'tr_node(' collections/trama.<x>/R/*.R`.
- Veja o catálogo sem abrir o editor:
  `TRAMA_DEV=$PWD Rscript inst/bin/trama-agente catalog --busca "<termo>"` e
  `... explain <tipo-vizinho>`.

## 1. Lógica pura

`collections/trama.<x>/R/<arquivo>.R`: função R comum (dados -> resultado),
testável sem registro. Nada de Shiny ou registro aqui.

## 2. Declaração

No arquivo onde a coleção declara os nós, `trama::tr_node("<x>/<id>", fn, ...)`:

- `label`, `description` (obrigatória; o catálogo é lido por máquina),
  `category`, `inputs`/`outputs` com tipos existentes.
- `help`: copie as seções do vizinho. Não-ASCII em string de código vai como
  `\uXXXX` (`R CMD check`); comentários podem ter acento.
- Params com `trama::tr_param*`; coluna com `tr_param_col(...)`, opcional
  (cor, rótulo, painel) com `suggest = FALSE`. Campo digitável precisa de
  `example`.
- Garanta que entra em `nodes =` de `trama_collection()` em `R/collection.R`.

## 3. Pressupostos e referências

`R/docs*.R` da coleção: `trama::tr_pressuposto(...)` e `trama::tr_ref(...)`
conferidos na fonte (DOI, edição, página), nunca de memória.

## 4. Teste com oráculo

`collections/trama.<x>/tests/testthat/test-<arquivo>.R`: compare com exemplo
resolvido publicado ou pacote de referência, com tolerância declarada e a
fonte citada no comentário. Teste também pelo registro se o bloco tem porta ou
param com regra.

## 5. Glossário

Nome de param novo? Confira `docs/glossario-parametros.md`; nome proibido
reprova `tests/testthat/test-glossario.R`. Renomear param existente =
`migracoes =` no `tr_node` (sobe `version`).

## 6. Registros

- `docs/revisao-metodologica.md`: o bloco, oráculo e tolerância.
- `collections/trama.<x>/DESCRIPTION`: `Version`; `NEWS.md` da coleção.
- Usou API nova do núcleo: `trama (>= x.y.z)` no `Imports` e `NEWS.md` do núcleo.
- Mudou resultado ou padrão de bloco existente: sobe `version` do nó.

## 7. Exemplo e site (quando couber)

- Template: acrescente o exemplo em `tools/templates/gerar.R` e rode
  `Rscript tools/templates/gerar.R`. Nunca edite o JSON.
- Site: `site/src/content/docs/colecoes/<pasta>/<bloco>.md` (frontmatter igual
  aos vizinhos: `node:`, `collection:`, `category:`).

## 8. Verificar

Skill `verificar-trama`: `Rscript tools/check.R` até sair 0. Valide um fluxo
com o bloco: `TRAMA_DEV=$PWD Rscript inst/bin/trama-agente validate <fluxo.json>`.
Card novo ou preview próprio: abra o editor (`tr_app()`) e teste à mão.
