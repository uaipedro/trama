# Origem do modelo Página web

`live.lua`, `resources/`, `templates/`, `_knitr.qmd`, `_schema.yml` e
`LICENSE.md` são a extensão [quarto-live](https://github.com/r-wasm/quarto-live)
na tag **v0.2.0** (WebR 0.6.0), copiada arquivo por arquivo da tag em
2026-10-19 (MIT, "quarto-live authors"). O `_extension.yml` da tag diz
`version: 0.1.3-dev`; vale a tag. O formato `html` deste modelo repete o
`live-html` da extensão (`ojs-engine` e o filtro `live.lua`), que resolve os
próprios arquivos a partir da pasta dele.

Fixada de propósito: a página publicada não pode mudar porque a extensão
mudou. Para atualizar, troque os arquivos por outra tag e suba
`.TD_QUARTO_LIVE` em `R/pacote.R`.
