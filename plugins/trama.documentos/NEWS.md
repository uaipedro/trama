# trama.documentos 0.2.0

* Plugin do editor (#23): `Config/trama/plugin: true` e `trama_plugin()`. Painel Documentos na barra (lista com estado, novo a partir de modelo + perfil, Atualizar, Visualizar em segundo plano servido pelo editor, Exportar PDF/Word/HTML, abrir no editor de texto e a pasta, resolver regiões, guia do Quarto) e "Inserir no documento…" no menu do card. Aba Modelos como ponto da fase 3.
* O `dados.csv` de uma tabela leva só as linhas mostradas; a tabela inteira é opção explícita (`trama-tabela="completa"`).
* Requer trama 0.5.23 (item de plugin no menu do card; `ctx$servir`).

# trama.documentos 0.1.0

* Primeira versão (#22), o motor do documento vivo: `.qmd` com regiões gerenciadas `::: {#trama-<no> .trama}`, materialização em `_trama/<no>/` pelo genérico `tr_documento_componente()` (tabela, gráfico, modelo sanitizado, teste; portado do `trama.publicar`), criar/inserir/estado/atualizar/resolver/limpar com as regras de dono, perfis de autor, modelo embutido Básico, render local (html, pdf via Typst, docx) e prévia servida.
