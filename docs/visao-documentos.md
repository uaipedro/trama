# Visão de design — `trama.documentos`

## Para que serve este documento

Registrar o desenho, combinado com o Pedro em 2026-10-19, do plugin que transforma uma análise do trama num **produto final**: relatório, artigo, slides, dashboard ou página web, com o texto escrito pela pessoa e os resultados mantidos pelo trama. Inclui a **convergência** do `trama.publicar` (feito no mesmo dia) para dentro deste motor: um só jeito de gerar documentos, com a publicação no GitHub como um dos destinos.

É uma visão, não uma especificação. Nomes de funções, campos e arquivos são exemplos para discussão. Substitui as partes de página, layout e componentes de `docs/visao-publicar-e-github.md`; as partes de GitHub (conta, versionar, release, Pages) continuam valendo.

## O problema

Hoje há três caminhos que não conversam:

1. **`tr_export_code()`** (núcleo, `R/export.R`, botão do editor): gera um `.R` ou um `.qmd` de uma vez. Exportar de novo sobrescreve; o texto que a pessoa escreveu se perde.
2. **`trama.publicar`**: gera o próprio `.qmd`, com layout em parâmetro do nó e componentes, só para a página pública com WebR.
3. **Templates de bloco**: outra coisa (fluxos prontos), fora deste desenho.

Falta o **documento vivo**: a pessoa escreve o texto, o trama cuida dos resultados, e os dois convivem quando a análise muda. E falta o **produto acabado**: um relatório no padrão da instituição ou da consultoria, com autoria preenchida, em HTML, PDF ou Word, sem montar tudo à mão.

## Ideia central

**O documento é um `.qmd` do projeto. O texto é da pessoa; as regiões do trama são do trama.**

- A pessoa cria um documento a partir de um **modelo** (UFLA ABNT, Consultoria, Artigo, Slides, Dashboard, Página web…), com o **perfil** de autor já preenchido.
- No editor, cada card ganha **"inserir no documento"**: entra no `.qmd` uma região gerenciada com o resultado daquele bloco (figura, tabela, modelo, teste).
- A pessoa escreve em volta, no editor de texto que já usa (RStudio, Positron, VS Code), e pode acrescentar os próprios chunks.
- Quando a análise muda, **"atualizar documento"** refaz só as regiões do trama que ficaram desatualizadas. O texto e os chunks da pessoa nunca são tocados.
- **"Exportar"** renderiza com Quarto no formato escolhido; **"Publicar"** leva o documento e o pacote reprodutível ao GitHub (release + Pages).

Do dado ao relatório formatado e à publicação FAIR, sem sair do trama e sem perder o texto quando a análise muda. Jamovi e JASP exportam uma vez; no Quarto puro a pessoa escreve todo o código.

## Onde mora

| Peça | Onde | Papel |
|---|---|---|
| Motor de documentos | plugin `plugins/trama.documentos` | formato do documento, regiões gerenciadas, materialização, sincronização, modelos, perfis, render, destinos |
| Ganchos mínimos | núcleo `R/` e `inst/www/` | item de plugin no menu do card; valor de um nó do projeto fora do editor |
| Conta e envio ao GitHub | `plugins/trama.github` | já existe; usado pelo destino "Publicar no GitHub" |
| Geração de código | núcleo, `tr_export_code()` | continua no núcleo (equivalência editor–documento–código); o plugin consome |

Plugin, não coleção: o documento é do projeto, não um passo do fluxo, e não precisa de bloco. O núcleo continua leve.

## O documento

### Regiões gerenciadas

Cada resultado inserido é um bloco cercado do Quarto com o id do nó:

```markdown
::: {#trama-lm .trama trama-hash="3f9a…"}
<conteúdo gerado pelo trama>
:::
```

Regras (testadas desde o primeiro dia):

- **Dono.** Fora das regiões `.trama`, tudo é da pessoa: texto, títulos, chunks próprios, YAML. O trama nunca escreve ali, exceto o YAML que ele mesmo criou na criação do documento.
- **Atualizar** refaz só regiões cujo bloco mudou (hash do resultado materializado diferente).
- **Conflito.** Se o conteúdo de uma região foi editado à mão (hash do conteúdo ≠ `trama-hash`), o trama não sobrescreve: marca conflito e pergunta (manter a versão da pessoa, que passa a ser texto dela, ou trazer a do trama).
- **Órfã.** Bloco removido do fluxo: a região fica, marcada como órfã, até a pessoa decidir.
- **Mover e apagar.** A pessoa move ou apaga regiões à vontade; a ordem é a do texto (acabou o layout em parâmetro de nó do `trama.publicar`).

### Conteúdo das regiões: dois modos

- **Resultado** (padrão): o resultado é **materializado** em `_trama/<id>/` (figura `.png`/`.svg`, tabela `.csv` + markdown, modelo sanitizado `.rds` + `web.json`, teste em markdown) e a região só referencia esses arquivos. Renderiza rápido, os números são os do editor, e o render não precisa recalcular nem ter o store (o GC do store apaga o que nenhum fluxo usa; a materialização não depende dele). Também é o que permite renderizar no GitHub Actions.
- **Código**: a região é um chunk R com o código do bloco e dos antecessores, vindo do `tr_export_code()`. Para apêndice de reprodutibilidade e para quem quer o `.qmd` autossuficiente.

A materialização reaproveita os adaptadores de hoje do `trama.publicar` (`item.R`: tabela, gráfico, modelo com `tr_models_strip()` e bloqueio de dados de treino) e passa a ser um genérico S3 por classe do valor (`tr_documento_componente()`), que coleções estendem sem o plugin conhecê-las.

## Modelos

Um modelo é uma **extensão Quarto** guardada no nível do usuário (`tools::R_user_dir("trama", "data")/modelos/<nome>/`):

- `_extension.yml` com os formatos habilitados (html, typst/pdf, docx, revealjs, dashboard, live-html);
- `_brand.yml` (cores, fontes, logo — padrão do Quarto);
- partes e templates (Typst para PDF, `reference.docx` para Word, CSS), CSL de citação;
- um `.qmd` esqueleto (seções iniciais, capa, resumo).

**PDF via Typst**, não LaTeX: leve, sem TeX instalado, e muito mais simples para quem usa o launcher no Windows.

Embutidos no v1: **Básico** (html/pdf/docx), **UFLA (ABNT)** (aproveita o modelo Quarto ABNT do kit do Pedrinho: Typst + `abnt.csl` + `referencia.docx`), **Consultoria** (marca Yucca como exemplo de identidade), **Artigo**, **Slides** (revealjs), **Dashboard**, **Página web** (live-html com WebR — o que o `trama.publicar` gera hoje).

Na interface: galeria de modelos para **criar, editar, duplicar**, importar e exportar (zip). Editar começa pelo que dá para editar sem código (nome, formatos, cores, fontes, logo, capa, CSL); quem sabe Quarto edita os arquivos da extensão direto.

## Perfis de autor

No nível do usuário (`perfis.yml` em `R_user_dir`): nome, e-mail, ORCID, instituição, curso/cargo, orientador, logo. Vários perfis (ex.: "Aluno UFLA", "Yucca Consultoria"), um padrão; um modelo pode sugerir o perfil. O perfil preenche o YAML do documento novo e o `CITATION.cff` da publicação.

## No editor

- **Painel Documentos** (do plugin): documentos do projeto; novo a partir de modelo; abrir no editor externo; estado de cada região (atualizada, desatualizada, conflito, órfã); **Atualizar**, **Visualizar** (render + prévia local servida), **Exportar** (formato), **Publicar**.
- **Menu do card: "inserir no documento"** (escolhe o documento; modo resultado ou código). Precisa de gancho no núcleo: plugin contribui item ao menu do card, recebendo o id do nó.
- Prévia local como a do `trama.publicar` de hoje: renderiza com o Quarto local e serve por servidor local (WebR não carrega de arquivo direto). Sem Quarto instalado, o painel guia a instalação.

## Destinos

| Destino | Como |
|---|---|
| HTML, PDF (Typst), Word, Slides, Dashboard | `quarto render` local no formato do modelo |
| Página web (WebR) | modelo live-html; preditor de modelo no navegador |
| Publicar no GitHub | pacote RO-Crate (documento + `_trama/` + `flow.json` + ambiente + dados ligados) → commit e tag pela API → Actions renderiza e cria release + Pages (`/vX.Y.Z/`, `/ultimo/`) — exatamente o fluxo do `trama.publicar` de hoje, agora a partir de um documento |

Sanitização e bloqueio de dados de treino continuam valendo em todo destino (são da materialização).

## Convergência com o `trama.publicar`

Zero apego: o `trama.publicar` deixa de existir como coleção.

- Materialização (`item.R`, `tr_models_strip`, `tr_models_web`), RO-Crate (`montar.R`, `material.R`), página (`pagina.R`), prévia (`previa.R`) e envio ao GitHub (`envio.R`) migram para o `trama.documentos`.
- O nó `publicar/publish` sai. **Migração**: fluxo com o nó vira um documento gerado do layout dele (cabeçalho, textos, itens na ordem), e o registro de publicações no `trama.json` passa para o documento. A migração roda ao abrir o projeto, uma vez, e avisa.
- O painel GitHub (`trama.github`, seção Publicações) passa a ler as publicações dos documentos.
- `trama.publicar` sai do r-universe e do manifesto do launcher na mesma release que coloca o `trama.documentos`.
- `docs/visao-publicar-e-github.md` ganha nota apontando para este documento.

## Fases

Todas para agora, nesta ordem; 2 e 3 podem andar em paralelo depois da 1.

1. **Motor** — pacote `plugins/trama.documentos`: formato do documento e regiões gerenciadas; materialização (portando os adaptadores do publicar para o genérico S3); criar/atualizar com conflito e órfã; render local; perfis; modelo **Básico**. Testes pesados nas regras de dono.
2. **Editor** — ganchos no núcleo (item de plugin no menu do card; valor de nó do projeto fora do editor); painel Documentos; inserir no documento; prévia; exportar.
3. **Modelos** — galeria criar/editar/duplicar/importar/exportar; embutidos UFLA ABNT, Consultoria, Artigo, Slides, Dashboard.
4. **Convergência** — destinos Página web e Publicar no GitHub dentro do plugin; migração do nó `publicar/publish`; painel GitHub lendo documentos; retirada do `trama.publicar` (r-universe, manifesto, launcher).
5. **Reprodutibilidade** — modo código por região (via `tr_export_code()`), apêndice automático; botão "Exportar Quarto" do núcleo passa a abrir "novo documento" quando o plugin está instalado.

Depois: editor de texto simples no próprio trama, compartilhar modelos pelo GitHub, citações a partir de referências do projeto.

## Riscos

- **Sincronização** é a parte difícil. A regra de dono é rígida e testada; nenhuma escrita fora das regiões `.trama`.
- **Quarto instalado** é pré-requisito para render local. O painel guia; o GitHub Actions não depende dele.
- **Referência estável**: regiões apontam para o id do nó e para a materialização em `_trama/`, nunca para hash do store.
- **Formatos**: Typst cobre PDF sem TeX; templates ABNT em Typst já existem no kit do Pedrinho.

## Decisões tomadas

| Pergunta | Decisão |
|---|---|
| Plugin ou coleção | Plugin `trama.documentos` |
| Modo padrão da região | Resultado materializado; código como opção |
| Onde se edita o texto | No editor de texto da pessoa; o trama mostra a prévia |
| PDF | Typst |
| Modelos | Extensões Quarto no nível do usuário, criar/editar/duplicar |
| Autoria | Perfis no nível do usuário |
| `trama.publicar` | Converge para dentro do `trama.documentos`, em fases, já |
| Tabela de dados em `_trama/` | Só as linhas mostradas na região; a tabela completa é opção explícita na região (mesma lógica do aceite de dados de treino) |
| `_trama/` no versionamento do projeto | Fora por padrão: é derivado e se refaz com "atualizar" |
| Tema dos gráficos no documento | Tema claro do modelo (`_brand.yml`), não o tema do editor (fase 3) |
| Como o tema claro chega ao gráfico (fase 3) | O "padrão" do projeto é trocado, nos settings do documento, por um tema derivado do `_brand.yml` do documento (fundo claro, paleta abrindo com as cores da marca que se leem no branco), como `tr_export_code()` faz no Quarto exportado. Card com tema escolhido à mão fica com o dele. O tema entra na chave do store: mudar as cores do `_brand.yml` deixa as regiões de gráfico desatualizadas |
| O que o trama guarda no modelo e o Quarto não lê | `trama-modelo.yml` ao lado do `_extension.yml`: descrição, perfil sugerido, configuração dos formatos desligados. Não vai para o documento |
| Editar um embutido | Cria a cópia do usuário com o mesmo nome, que tem precedência; remover a cópia volta ao embutido. Embutido não se remove |
| Perfil no esqueleto | Valores `"{{perfil.<campo>}}"` e `"{{ano}}"` no YAML do `template.qmd`; campo que o perfil não tem some. Perfil sugerido pelo modelo vale se existe; senão, o padrão |
| Nome do modelo Dashboard | `painel`: `dashboard-dashboard` o Quarto não resolve como formato de extensão; nomes de formato do Quarto são proibidos como nome de modelo |
| UFLA (ABNT) | NBR 14724 (margens 3/3/2/2 cm, Arial 12, 1,5), a partir do kit ABNT (UFSCar) do `pedro-e-pedro`. O manual de normalização da Biblioteca da UFLA não foi conferido: pendência |
| Consultoria | Cores da Yucca (`#003366`, `#E6EFF6`, da extensão `yucca-pdf`) e logo genérico; o logo real da Yucca não vai para o repositório público |
