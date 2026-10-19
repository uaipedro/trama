# Origem dos arquivos do modelo UFLA (ABNT)

Adaptado do modelo ABNT em Quarto do kit `pedro-e-pedro` (repositório privado
de Pedro Mambelli Fernandes, pasta `modelo-abnt-quarto/_modelo/`, commit
`9d0b56d`, 18/10/2026). O kit foi feito para a UFSCar (PPGADR e manual da
BCo); aqui ficou só o que é da ABNT NBR 14724, com margens de 3 cm (esquerda e
superior) e 2 cm (direita e inferior), Arial 12, entrelinha 1,5, e com
"Universidade Federal de Lavras" e "Lavras" no esqueleto. As cores do
`_brand.yml` são de exemplo, não a identidade visual oficial da UFLA. Confira
com o manual de normalização da Biblioteca da UFLA o que o seu programa pede a
mais.

| Arquivo | Origem | Licença |
|---|---|---|
| `typst-template.typ`, `typst-show.typ` | kit `pedro-e-pedro`, do mesmo autor do trama; mudanças: margens da NBR 14724, sem referências à UFSCar, campos vazios não quebram, `title`/`author` do Quarto como reserva de `titulo`/`autor`, legenda com travessão | MIT (a do trama) |
| `abnt.lua` | kit `pedro-e-pedro`; mudanças: autor e título do Quarto como reserva, orientador opcional, sem o link da UFSCar, o Word não repete o bloco de título | MIT (a do trama) |
| `referencia.docx` | kit `pedro-e-pedro`: o `reference.docx` padrão do Pandoc com os estilos ABNT acrescentados (capa, folha de rosto, resumo, fonte) | estilos derivados do arquivo de dados do Pandoc (John MacFarlane, GPL-2 ou superior) |
| `abnt.csl` | estilo "Associação Brasileira de Normas Técnicas (Português - Brasil)" do repositório oficial de estilos CSL (`http://www.zotero.org/styles/associacao-brasileira-de-normas-tecnicas`, atualizado em 20/08/2023), sem mudanças; autores e colaboradores no próprio arquivo | CC BY-SA 3.0 |
