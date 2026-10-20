# trama.documentos 0.4.1

* O nome sugerido do repositório de publicação e o nome de modelo tiram o acento por `trama::tr_sem_acento()`, o mesmo em qualquer sistema; o `iconv` com `TRANSLIT` dava outro resultado no macOS ("ac-ao" no lugar de "acao"). Pede `trama (>= 0.5.26)`.

# trama.documentos 0.4.0

* Convergência do `trama.publicar` (#25), que foi retirado: o motor de documentos passa a ter os destinos de publicação.
* Modelo embutido **Página web** (`pagina`): o quarto-live v0.2.0 fixado dentro da extensão; num documento dele, a região de um modelo ganha um preditor que roda no navegador (WebR), com o motor servido da mesma origem e as versões do treino × navegador. O esqueleto cita versão, licenças e citação por `{{< var >}}` (`_variables.yml`). `tr_documento_componente()` recebe `ctx$web`.
* **Publicar no GitHub** a partir de um documento: `tr_documento_publicacao()` (conferência: metadados do YAML, regiões em dia, dados de treino com aceite em `trama: publicar:`, formato HTML), `tr_documento_pacote()` (RO-Crate com o documento como `index.qmd`, `_trama/`, fluxo, ambiente, dados de entrada, README, CITATION.cff, LICENSE), `tr_documento_publicar()` (commit atômico e tag pela API do `trama.github`; workflow com release e Pages em `/vX.Y.Z/` e `/ultimo/`) e `tr_documento_acompanhar()`. Registro por documento no `trama.json` (`tr_documento_publicacoes()`). No painel, "Publicar no GitHub…" em cada documento, com o acompanhamento.
* Migração: `tr_documento_migrar_publicar()` transforma o nó `publicar/publish` num documento Página web (YAML dos params, corpo do layout, uma região por item) e move o registro de publicações do nó para o documento; o painel roda na primeira lista da sessão, relê o projeto e avisa.
* O manifesto de cada região guarda os metadados RO-Crate da parte.
* Tabela sem `report` no tipo: os números saem pela `trama.data::tr_data_fmt_num()` (#32), que não arredonda a parte inteira (11887,03 é "11.887", não "11.890"); a `trama.data` (>= 0.4.7) passa a ser dependência.
* Depende do `trama.github` (>= 0.2.0), da `trama.data` (>= 0.4.7) e do núcleo 0.5.25 (projeto abre sem a coleção retirada).
* Para quem publica: o `trama.documentos` entra no r-universe (`packages.json`, subdir `plugins/trama.documentos`) e no manifesto do launcher; o `trama.publicar` sai dos dois.

# trama.documentos 0.3.0

* Aba **Modelos** do painel (#24): a galeria no editor — ver, editar (título, descrição, formatos, cores, fontes, logo, capa, CSL, perfil sugerido), duplicar, novo a partir de uma base, importar e exportar zip (arquivos sobem do navegador em base64), apagar ou voltar ao embutido, abrir a pasta do modelo. No painel Documentos, Exportar mostra os formatos do modelo do documento (slides, dashboard) e Visualizar usa o primeiro formato HTML; o "novo documento" parte do Básico, marca modelo com problema e troca o autor pelo perfil que o modelo sugere.
* Modelos (#24). Galeria: `tr_documento_modelo()` (detalhe), `tr_documento_modelo_criar()`, `_editar()` (título, descrição, formatos, cores, fontes, logo, capa, CSL, perfil sugerido), `_duplicar()`, `_remover()`, `_exportar()` e `_importar()` (zip, inclusive o formato das extensões Quarto publicadas). Editar um embutido cria a cópia do usuário de mesmo nome; remover a cópia volta ao embutido. Modelo inválido aparece na galeria com o problema (`valido`, `problema`) e criar documento com ele dá erro claro.
* Embutidos novos: **UFLA (ABNT)** (Typst + `abnt.csl` + `referencia.docx` + filtro, adaptados do modelo ABNT em Quarto do kit `pedro-e-pedro`; origem e licenças em `inst/modelos/ufla/ORIGEM.md` e `inst/COPYRIGHTS`), **Consultoria** (capa na cor da marca; identidade de exemplo da Yucca Data Science, com logo genérico), **Artigo**, **Slides** (revealjs) e **Dashboard** (`painel`).
* Perfis vinculados a modelos: o modelo sugere um perfil (`perfil` em `trama-modelo.yml`), usado quando existe; o esqueleto pede campos do perfil com `"{{perfil.<campo>}}"` e o ano com `"{{ano}}"`.
* Gráficos do documento num tema claro derivado do `_brand.yml` do documento, no lugar do tema "padrão" do projeto (card com tema escolhido à mão fica com o dele). O tema entra na chave do store: regiões de gráfico de documentos da 0.1.0 aparecem desatualizadas uma vez.
* `tr_documento_renderizar()` ganha `"revealjs"` e `"dashboard"`; `tr_documento_formatos()` diz os formatos do documento; a prévia usa o primeiro formato HTML do documento.
* Slides e dashboard saem em subpastas de `_saida/` (`slides/<doc>.html`, `painel/<doc>.html`): exportar um não apaga mais o HTML do documento, nem as bibliotecas dele em `<doc>_files/` (#29).
* `_brand.yml` do documento aponta o logo para a cópia da extensão; YAML gravado com `true`/`false` (o Quarto lê `yes` como texto).

# trama.documentos 0.2.0

* Plugin do editor (#23): `Config/trama/plugin: true` e `trama_plugin()`. Painel Documentos na barra (lista com estado, novo a partir de modelo + perfil, Atualizar, Visualizar em segundo plano servido pelo editor, Exportar PDF/Word/HTML, abrir no editor de texto e a pasta, resolver regiões, guia do Quarto) e "Inserir no documento…" no menu do card. Aba Modelos com a galeria (fase 3).
* O `dados.csv` de uma tabela leva só as linhas mostradas; a tabela inteira é opção explícita (`trama-tabela="completa"`).
* Requer trama 0.5.23 (item de plugin no menu do card; `ctx$servir`).

# trama.documentos 0.1.0

* Primeira versão (#22), o motor do documento vivo: `.qmd` com regiões gerenciadas `::: {#trama-<no> .trama}`, materialização em `_trama/<no>/` pelo genérico `tr_documento_componente()` (tabela, gráfico, modelo sanitizado, teste; portado do `trama.publicar`), criar/inserir/estado/atualizar/resolver/limpar com as regras de dono, perfis de autor, modelo embutido Básico, render local (html, pdf via Typst, docx) e prévia servida.
