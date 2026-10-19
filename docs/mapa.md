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
| `plugins/trama.*` | Plugins: sem blocos, não são coleção nem entram no `trama.json`. Os que declaram `Config/trama/plugin: true` entram no editor (`R/plugin.R`). Hoje: `trama.github`; `trama.documentos` ainda sem interface. |
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
| `plugin.R` | Plugins do editor: acha os plugins instalados (`tr_plugins()`) e liga JS, CSS e server deles. Sem rede, sem nome de pacote. |
| `control.R`, `cli.R` | Canal de controle para agentes (HTTP local + token) e o CLI `trama-agente` (`inst/bin/`); `catalog`, `explain` e `validate` rodam sem editor. |
| `template.R`, `theme.R`, `export.R` | Templates, temas de gráfico, exportação. |
| `docs-bloco.R`, `test-card.R` | Documentação por bloco e `tr_test` para coleções. |

## Coleções

Grafo de dependências: `trama` ← `data` ← `view` ← {`models`, `series`, `sampling`, `spatial`}; `models` ← {`experiments`, `ml`, `multi`, `publicar`}.

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
| `trama.spatial` | Geoestatística: variável regionalizada, variograma, krigagem. |
| `trama.sql` | Consulta SQL local (DuckDB) devolvendo tabelas. |
| `trama.python` | Prova de conceito: modelos scikit-learn via Python. |
| `trama.publicar` | Publicação: o nó `publicar/publish` monta o pacote RO-Crate do fluxo e a página (Quarto + quarto-live/WebR); prévia local pela ação Visualizar. Adaptadores `data/table`, `view/plot`, `models/fit` → `publicar/item`. Ação Publicar no GitHub (repositório, Pages, workflow em `inst/github/`) pelo `trama.github`. |

Dentro de uma coleção: `R/collection.R` registra tudo; os `tr_node` ficam em `R/nos_*.R`, `R/nodes.R` ou no próprio `collection.R`, conforme a coleção; os demais arquivos têm a lógica pura; `R/docs*.R` guarda a documentação; `inst/templates/` tem os templates gerados.

## Plugins

| Pacote | O que faz |
| --- | --- |
| `trama.github` | Conexão com o GitHub: guia de conta, login pelo device flow, credencial no chaveiro, API interna (usuário, repositório, commit atômico, tag, Pages, Actions), versionar o projeto num repositório privado (salvar versão, histórico, restaurar; chave `versionar` no `trama.json`) e o painel GitHub do editor (conta, Projeto, Publicações). Base de `trama.publicar`, que é dependência opcional dele, nunca o contrário. Visão: `docs/visao-publicar-e-github.md`. Testes contra GitHub falso (`webfakes`). |
| `trama.documentos` | Documento vivo (fase 1, #22, sem interface): `.qmd` em `documentos/<nome>/` com regiões gerenciadas `::: {#trama-<no> .trama trama-hash=…}`; materialização em `_trama/<no>/` pelo genérico S3 `tr_documento_componente()` (tabela, gráfico, modelo sanitizado, teste; portado do `trama.publicar`); criar, inserir, estado, atualizar e resolver com as regras de dono (`R/regioes.R`); perfis e modelos (extensões Quarto) em `tools::R_user_dir("trama", "data")`, embutido Básico; render local (html, pdf via Typst, docx) e prévia. Valor do nó pelo núcleo (`tr_project_flow`, `tr_value`, `tr_plan`). Visão: `docs/visao-documentos.md`. |

Rodam pelo `tools/check.R` como as coleções (`plugins/X/**` → testes de X), sem carregar o núcleo. JS com ramo segue a regra do editor: módulo puro com teste em `tests/js/`.

## Documentos: o que vale

| Documento | Status |
| --- | --- |
| `docs/manifesto.md`, `docs/design-trama.md` | Canônico: objetivo e arquitetura. |
| `docs/glossario-parametros.md` | Canônico e travado por teste. |
| `docs/linguagem-visual.md`, `docs/guia-estilo-site.md`, `docs/guia-documentacao.md` | Canônico: convenções. |
| `docs/colecao-dados.md`, `docs/colecao-graficos.md` | Canônico por coleção. |
| `docs/revisao-metodologica.md`, `docs/fontes.md`, `docs/pendencias-referencias.md`, `docs/cobertura-oraculos.md` | Registro vivo de rigor e referências. |
| `docs/future-ideas/`, `docs/propostas-*.md`, `docs/visao-*.md` | Proposta, não decisão. |
| `docs/plans/` | Histórico local. Em conflito com o código, vale o código. |
