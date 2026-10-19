# trama.documentos

Plugin do trama (não é coleção: não tem blocos). Transforma uma análise num
**documento vivo**: um `.qmd` do projeto em que o texto é da pessoa e as
regiões gerenciadas são do trama. Desenho em `docs/visao-documentos.md`.

Fase 1 (#22): o motor, usável pelo R. Fase 3 (#24): a galeria de modelos e
os embutidos. O painel no editor, o "inserir no documento" do card e a prévia
pelo editor são a fase 2 (#23).

## O documento

```
projeto/documentos/<nome>/
  <nome>.qmd            texto da pessoa + regiões do trama
  _trama/<no>/          resultados materializados (do trama; não edite)
  _extensions/<modelo>/ cópia do modelo (extensão Quarto)
  _brand.yml, _quarto.yml, .gitignore
  _saida/               o que o render gera
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
  `trama-largura`, `trama-altura`) são opções que ela pode mudar.

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
