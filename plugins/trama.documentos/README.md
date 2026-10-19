# trama.documentos

Plugin do trama (não é coleção: não tem blocos). Transforma uma análise num
**documento vivo**: um `.qmd` do projeto em que o texto é da pessoa e as
regiões gerenciadas são do trama. Desenho em `docs/visao-documentos.md`.

## No editor

- **Documentos**, na barra: os documentos do projeto com o estado de cada
  região; novo documento a partir de modelo + perfil; Atualizar, Visualizar
  (HTML numa aba nova, servido pelo editor), Exportar (PDF, Word, HTML);
  abrir o `.qmd` no editor de texto do sistema e a pasta; resolver regiões
  editadas à mão, órfãs ou copiadas. Sem Quarto, mostra o guia de instalação.
- **Inserir no documento…**, no menu de um card (botão direito): escolhe o
  documento, a saída, as linhas e se o CSV leva a tabela inteira; um modelo
  com dados de treino pede o aceite ali mesmo.
- **Publicar no GitHub…**, em cada documento: confere o que sai (título,
  autores, licença, próxima versão, avisos e bloqueios), envia e acompanha
  as etapas (pacote enviado, Actions, release, página no ar) com os links.
- Projeto com o bloco **Publicar** do antigo `trama.publicar`: ao abrir, o
  bloco vira um documento Página web e o painel avisa (ver "Migração").
- Aba **Modelos**: a galeria (fase 3, #24) — ver, duplicar, editar (título,
  descrição, formatos, cores, fontes, logo, capa, CSL, perfil sugerido),
  importar e exportar zip, remover ou voltar ao embutido.
  `inst/trama/modelos-ui.js` desenha; as ações `modelos_*` chegam a
  `R/painel-modelos.R`.

## O documento

```
projeto/documentos/<nome>/
  <nome>.qmd            texto da pessoa + regiões do trama
  _trama/<no>/          resultados materializados (do trama; não edite)
  _extensions/<modelo>/ cópia do modelo (extensão Quarto)
  _brand.yml, _quarto.yml, .gitignore
  _saida/               o que o render gera (slides em slides/, dashboard em painel/)
```

Uma região:

```markdown
::: {#trama-tab .trama trama-hash="3f9a…" trama-linhas="50"}
{{< include _trama/tab/tabela.md >}}
:::
```

Regras de dono:

- fora das regiões `.trama` o trama não escreve (só o YAML, ao criar);
- **atualizar** refaz só as regiões cujo resultado mudou;
- região editada à mão é **conflito**: não é sobrescrita (nem a pasta dela);
  `tr_documento_resolver()` mantém a versão da pessoa, traz a do trama ou
  remove;
- bloco que saiu do fluxo deixa a região **órfã**, até a pessoa decidir;
- a pessoa move e apaga regiões à vontade (inclusive para dentro de um callout
  ou item de lista, com recuo); a ordem é a do texto; uma cópia à mão de uma
  região é `duplicada` e não é tocada;
- "manter" a versão da pessoa congela a pasta em `_trama/<no>--mantida/`;
  remover ou trazer uma região editada que tem cercas `:::` dentro pede
  `confirmar = TRUE`;
- classes e atributos que a pessoa põe no cabeçalho ficam; os atributos `trama-*` (`trama-linhas`, `trama-formato`,
  `trama-largura`, `trama-altura`) são opções que ela pode mudar;
- o `dados.csv` de uma tabela leva só as linhas mostradas; a tabela inteira
  só com `trama-tabela="completa"`.

Modelo que carrega linhas do treino (SVM, floresta de regressão) só entra com
aceite explícito, que fica no cabeçalho (`trama-dados-treino="aceito"`).

## Uso

```r
library(trama.documentos)
tr_documento_perfil_salvar("ufla", "Ana Lima", orcid = "0000-0002-1825-0097",
                           instituicao = "UFLA", curso = "Estatística")
doc <- tr_documento_criar("relatorio", projeto = ".", modelo = "basico")
tr_documento_inserir(doc, "grafico")      # id do nó no fluxo
tr_documento_estado(doc)                  # atualizada, desatualizada, conflito, orfa
tr_documento_atualizar(doc)
tr_documento_formatos(doc)                # os do modelo: html, pdf, docx, revealjs, dashboard
tr_documento_renderizar(doc, "pdf")       # pdf é via Typst
tr_documento_previa(doc)                  # HTML servido em 127.0.0.1
```

## Destinos: Página web e Publicar no GitHub

O modelo **Página web** traz a extensão [quarto-live](https://github.com/r-wasm/quarto-live)
v0.2.0 fixada (`inst/modelos/pagina/ORIGEM.md`): num documento dele, a região
de um modelo ganha um preditor — um controle por entrada e uma célula WebR que
lê o motor da mesma origem (`_trama/<id>/motor.rds`), chama `prever` e mostra o
resultado, com as versões do treino × as do navegador. O esqueleto cita
versão, licenças e citação por `{{< var >}}`: no documento, `_variables.yml`
diz "rascunho"; no pacote publicado, os valores da versão.

**Publicar no GitHub** vale para qualquer documento com formato HTML. Os
metadados vêm do YAML: `title`, `author` (com `orcid`, conferido pelo dígito
verificador), `description`, `keywords`, e em `trama: publicar:` a `licenca`
(CC-BY-4.0, CC0-1.0, CC-BY-SA-4.0, CC-BY-NC-4.0), a `licenca-codigo` (MIT,
Apache-2.0, GPL-3.0-only), a `versao` (tipo da próxima: `menor`, `correção`,
`maior`), `privado` e `aceitar-dados-treino`.

```r
tr_documento_publicacao(doc)              # o que sai, avisos e bloqueios (não toca no GitHub)
tr_documento_pacote(doc, "pacote")        # a pasta RO-Crate, para conferir
tr_documento_publicar(doc)                # atualiza, monta, commit atômico + tag vX.Y.Z
tr_documento_acompanhar(doc)              # enviado, actions, release, no_ar ou falhou
tr_documento_publicacoes(".")             # o registro do projeto
```

O pacote é um [RO-Crate 1.1](https://w3id.org/ro/crate/1.1): o documento
como `index.qmd` com a pasta dele (modelo em `_extensions/`, `_trama/` só com
o que o texto usa), `flow.json`, `fluxo.svg`, `dados-entrada.json` (descrição
e sha256 do que não vai), `ambiente.json`, `README.md`, `CITATION.cff`,
`LICENSE` e o `ro-crate-metadata.json`, que descreve cada arquivo com tamanho
e sha256 (modelos com o vocabulário [FAIR4ML](https://w3id.org/fair4ml)). Um
repositório por documento, público por padrão; o envio é um commit atômico
(a árvore passa a ser exatamente o pacote + o workflow) e a tag `vX.Y.Z`. O
workflow (`inst/github/`, versões fixadas) confere checksums e RO-Crate,
renderiza com o Quarto, cria a release com o pacote zipado e publica no
Pages em `/vX.Y.Z/` (imutável) e `/ultimo/`, acumulados na branch `paginas`.

O endereço de cada publicação fica no `trama.json` do projeto (chave
`publicacoes`, pelo caminho do documento): nunca vai no documento nem no
pacote, e o repositório privado do projeto (versionar, do `trama.github`)
nunca é citado. O GitHub Actions instala só knitr e rmarkdown: bloco R da
pessoa que use outro pacote falha lá (a conferência avisa).

### Dados de treino

Os modelos vão reduzidos (`trama.models::tr_models_strip()`): só o que prevê.
Quando o reduzido ainda carrega linhas do treino (vetores de suporte de uma
SVM, folhas de uma floresta de regressão), a região só entra com aceite, e a
publicação pede outro, explícito, no documento (`aceitar-dados-treino: true`
em `trama: publicar:`). Aceito, o arquivo leva no RO-Crate
`trama:contemDadosTreino` e `trama:dadosTreinoEmbutidos` (o que ele carrega):
termos deste pacote, porque nem schema.org nem FAIR4ML têm um para isso.

### Migração do `trama.publicar`

O `trama.publicar` saiu (#25). Ao abrir no editor um projeto com o nó
`publicar/publish`, `tr_documento_migrar_publicar()` cria um documento
Página web por nó: os params viram o YAML (título, resumo, palavras-chave,
autores com ORCID, licenças, tipo de versão, privado, aceite de dados de
treino), e o layout vira o corpo — cabeçalho do modelo, textos, uma região
por item ligado (na ordem), o diagrama do fluxo. Item que não entra vira uma
nota no texto. A publicação já feita passa a ser do documento: a próxima sai
no mesmo repositório, na versão seguinte. O nó e as arestas dele saem do
fluxo, e a coleção sai do `trama.json`.

## Estender

O que uma região mostra vem do genérico `tr_documento_componente(x, ctx)`, pela
classe do valor. Embutidos: tabela, gráfico, modelo (`models/fit`, reduzido por
`tr_models_strip()`) e teste. Sem método, usa o `report` do tipo da porta. Uma
coleção registra um método para a classe dos seus valores e devolve
`tr_documento_parte()`.

## Modelos e perfis

Ficam em `tools::R_user_dir("trama", "data")`: `perfis.yml` e
`modelos/<nome>/` (extensões Quarto, com precedência sobre os embutidos).

| Modelo | Nome | Formatos | Perfil sugerido |
|---|---|---|---|
| Básico | `basico` | html, pdf, docx | |
| UFLA (ABNT) | `ufla` | pdf (capa, folha de rosto, sumário, NBR 14724), docx, html | `aluno-ufla` |
| Consultoria | `consultoria` | pdf (capa na cor da marca), html, docx | `yucca` |
| Artigo | `artigo` | pdf, html, docx (resumo, palavras-chave, ABNT) | |
| Slides | `slides` | revealjs (16:9) | |
| Dashboard | `painel` | dashboard (cada região vira um cartão) | |
| Página web | `pagina` | html com R no navegador (WebR): cada modelo vira preditor | |

Um modelo é uma pasta:

```
modelos/<nome>/
  _extension.yml     formatos habilitados (contributes: formats:), como em toda extensão Quarto
  _brand.yml         cores, fontes, logo (padrão do Quarto); os gráficos usam um tema claro daqui
  template.qmd       esqueleto do documento; "{{perfil.orientador}}", "{{ano}}" são preenchidos
  trama-modelo.yml   descrição, perfil sugerido, formatos desligados (não vai para o documento)
  ...                templates Typst, reference.docx, CSS, CSL, filtros
```

```r
tr_documento_modelos()                                     # a galeria (com valido e problema)
tr_documento_modelo_duplicar("ufla", "meu-tcc")
tr_documento_modelo_editar("meu-tcc", cores = c(primaria = "#1f4e79"), capa = TRUE,
                           csl = "apa.csl", perfil = "aluno-ufla", formatos = c("typst", "docx"))
tr_documento_modelo_editar("consultoria", logo = "minha-marca.svg")  # cria a cópia do usuário
tr_documento_modelo_exportar("meu-tcc", "meu-tcc.zip")
tr_documento_modelo_importar("modelo-de-alguem.zip")
tr_documento_modelo_remover("consultoria")                 # volta ao embutido
```

Documento já criado leva a cópia do modelo de quando foi criado: mudar o
modelo não muda documento existente. Para mudar só um documento, edite o
`_brand.yml` e os arquivos de `_extensions/` dele.

Licenças: o plugin é MIT; o `abnt.csl` é CC BY-SA 3.0 e os `referencia.docx`
derivam do arquivo de dados do Pandoc (`inst/COPYRIGHTS`).
