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
- Aba **Modelos**: ponto onde entra a galeria de modelos (fase 3, #24):
  `inst/trama/modelos-ui.js` desenha; as ações `modelos_*` chegam a
  `R/painel-modelos.R`.

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
tr_documento_renderizar(doc, "pdf")       # html, pdf (Typst) ou docx
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
Embutido nesta versão: **Básico** (html, pdf via Typst, docx).
