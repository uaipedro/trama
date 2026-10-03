# Mapa do repositório

Onde cada coisa vive e qual documento vale. Para as regras, `AGENTS.md`.

## Pastas

| Caminho | O que é |
| --- | --- |
| `R/` | Núcleo (pacote `trama`). Sem domínio. |
| `inst/www/` | Editor no navegador (JS sem build). `runtime.js` é o contrato que as coleções importam. Mapa dos módulos: `inst/www/AGENTS.md`. |
| `inst/schema/` | JSON Schema do documento (`document-v1.json`) e do template (`template-v1.json`). |
| `tests/testthat/` | Testes do núcleo. `tests/js/` testa os módulos puros do editor com `node --test 'tests/js/*.test.mjs'` (com glob: passar o diretório faz o Node tratá-lo como arquivo e falhar). |
| `collections/trama.*` | Coleções de blocos; cada uma é um pacote R com testes próprios. Anatomia e checklist de bloco novo: `collections/AGENTS.md` e skill `novo-bloco`. |
| `site/` | Site de documentação (Astro). Publicado pelo `.github/workflows/docs.yml`. |
| `tools/check.R` | Roteador de testes (ver skill `verificar-trama`). |
| `tools/templates/gerar.R` | Gera os templates JSON das coleções (descobre as coleções; sem exemplo, pula). |
| `tools/site/` | Exporta docs e visuais dos nós para o site. |
| `tools/installer/` | Instalador e r-universe. `tools/launcher/` está **obsoleto** (`DEPRECATED.md`). |
| `tools/sugestor/` | Mineração e avaliação do sugestor de próximo bloco. |
| `tools/video/` | Vídeos de divulgação por roteiro (skill `trama-video`). |
| `exemplos/` | Projetos de exemplo abertos no editor. |
| `docs/plans/` | Planos locais de trabalho. Fora do git; histórico, não especificação. |

## Núcleo (`R/`)

| Arquivo | Responsabilidade |
| --- | --- |
| `node.R`, `param.R`, `type.R` | Declaração de blocos (`tr_node`), params (`tr_param*`) e tipos de porta. |
| `collection.R`, `registry.R`, `catalog.R` | Registro de coleções e catálogo de blocos. |
| `flow.R`, `plan.R`, `executor.R`, `scheduler.R`, `worker.R` | Fluxo em R, plano de execução e execução. |
| `stream-*.R`, `transport.R` | Regiões de fluxo (streaming) e transporte. |
| `document.R`, `document-io.R`, `migrate.R` | Documento `.trama`, leitura/gravação e migrações. |
| `project.R`, `store.R`, `run.R`, `runs.R`, `session.R` | Projeto, cache de resultados e histórico de execuções. |
| `app.R` | App Shiny que serve o editor. |
| `control.R`, `cli.R` | Canal de controle para agentes (HTTP local + token) e o CLI `trama-agente` (`inst/bin/`); `catalog`, `explain` e `validate` rodam sem editor. |
| `template.R`, `theme.R`, `export.R` | Templates, temas de gráfico, exportação. |
| `docs-bloco.R`, `test-card.R` | Documentação por bloco e `tr_test` para coleções. |

## Coleções

Grafo de dependências: `trama` ← `data` ← `view` ← {`models`, `series`, `sampling`}; `models` ← {`experiments`, `ml`, `multi`}.

| Coleção | Domínio |
| --- | --- |
| `trama.data` | Leitura, transformação, resumo de tabelas. Registra o tipo `data/test`. |
| `trama.view` | Gráficos. |
| `trama.models` | Ajuste, prever, avaliar, testes; dono do contrato `models/fit`. |
| `trama.experiments` | Delineamentos experimentais (design declarativo, contrastes). |
| `trama.multi` | Multivariada (PCA etc.). |
| `trama.ml` | Aprendizado de máquina. |
| `trama.series` | Séries temporais. |
| `trama.sampling` | Amostragem. |
| `trama.sql` | Consulta SQL local (DuckDB) devolvendo tabelas. |
| `trama.python` | Prova de conceito: modelos scikit-learn via Python. |

Dentro de uma coleção: `R/collection.R` registra tudo; os `tr_node` ficam em `R/nos_*.R`, `R/nodes.R` ou no próprio `collection.R`, conforme a coleção; os demais arquivos têm a lógica pura; `R/docs*.R` guarda a documentação; `inst/templates/` tem os templates gerados.

## Documentos: o que vale

| Documento | Status |
| --- | --- |
| `docs/manifesto.md`, `docs/design-trama.md` | Canônico: objetivo e arquitetura. |
| `docs/glossario-parametros.md` | Canônico e travado por teste. |
| `docs/linguagem-visual.md`, `docs/guia-estilo-site.md`, `docs/guia-documentacao.md` | Canônico: convenções. |
| `docs/colecao-dados.md`, `docs/colecao-graficos.md` | Canônico por coleção. |
| `docs/revisao-metodologica.md`, `docs/fontes.md`, `docs/pendencias-referencias.md` | Registro vivo de rigor e referências. |
| `docs/future-ideas/`, `docs/propostas-*.md`, `docs/visao-*.md` | Proposta, não decisão. |
| `docs/plans/` | Histórico local. Em conflito com o código, vale o código. |
