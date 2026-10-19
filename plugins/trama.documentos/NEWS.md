# trama.documentos 0.3.0

* Modelos (#24). Galeria: `tr_documento_modelo()` (detalhe), `tr_documento_modelo_criar()`, `_editar()` (título, descrição, formatos, cores, fontes, logo, capa, CSL, perfil sugerido), `_duplicar()`, `_remover()`, `_exportar()` e `_importar()` (zip, inclusive o formato das extensões Quarto publicadas). Editar um embutido cria a cópia do usuário de mesmo nome; remover a cópia volta ao embutido. Modelo inválido aparece na galeria com o problema (`valido`, `problema`) e criar documento com ele dá erro claro.
* Embutidos novos: **UFLA (ABNT)** (Typst + `abnt.csl` + `referencia.docx` + filtro, adaptados do modelo ABNT em Quarto do kit `pedro-e-pedro`; origem e licenças em `inst/modelos/ufla/ORIGEM.md` e `inst/COPYRIGHTS`), **Consultoria** (capa na cor da marca; identidade de exemplo da Yucca Data Science, com logo genérico), **Artigo**, **Slides** (revealjs) e **Dashboard** (`painel`).
* Perfis vinculados a modelos: o modelo sugere um perfil (`perfil` em `trama-modelo.yml`), usado quando existe; o esqueleto pede campos do perfil com `"{{perfil.<campo>}}"` e o ano com `"{{ano}}"`.
* Gráficos do documento num tema claro derivado do `_brand.yml` do documento, no lugar do tema "padrão" do projeto (card com tema escolhido à mão fica com o dele). O tema entra na chave do store: regiões de gráfico de documentos da 0.1.0 aparecem desatualizadas uma vez.
* `tr_documento_renderizar()` ganha `"revealjs"` e `"dashboard"`; `tr_documento_formatos()` diz os formatos do documento; a prévia usa o primeiro formato HTML do documento.
* `_brand.yml` do documento aponta o logo para a cópia da extensão; YAML gravado com `true`/`false` (o Quarto lê `yes` como texto).

# trama.documentos 0.1.0

* Primeira versão (#22), o motor do documento vivo: `.qmd` com regiões gerenciadas `::: {#trama-<no> .trama}`, materialização em `_trama/<no>/` pelo genérico `tr_documento_componente()` (tabela, gráfico, modelo sanitizado, teste; portado do `trama.publicar`), criar/inserir/estado/atualizar/resolver/limpar com as regras de dono, perfis de autor, modelo embutido Básico, render local (html, pdf via Typst, docx) e prévia servida.
