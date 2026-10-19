# trama.documentos 0.1.0

* Primeira versão (#22), o motor do documento vivo: `.qmd` com regiões gerenciadas `::: {#trama-<no> .trama}`, materialização em `_trama/<no>/` pelo genérico `tr_documento_componente()` (tabela, gráfico, modelo sanitizado, teste; portado do `trama.publicar`), criar/inserir/estado/atualizar/resolver/limpar com as regras de dono, perfis de autor, modelo embutido Básico, render local (html, pdf via Typst, docx) e prévia servida.
