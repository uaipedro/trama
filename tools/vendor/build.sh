#!/usr/bin/env bash
# Gera inst/www/vendor a partir do npm. Roda SÓ o mantenedor; o resultado é
# commitado. `--external` em react/react-dom é o que garante UMA instância de
# React entre núcleo e coleções: xyflow importa "react" nu, e o importmap
# resolve para o mesmo vendor/react.js que o editor usa.
#
# React 18 não publica build ESM — só CJS. Bundlado por esbuild, um pacote
# CJS que precisa de outro pacote (react-dom precisa de react; react-dom/client
# e react/jsx-runtime precisam de react-dom e react) vira uma factory CJS
# "preguiçosa" por baixo do ESM, e o `require()` de dentro dela NÃO vira um
# `import` estático — vira uma chamada real a `require()` em tempo de
# execução (única forma de o esbuild preservar semântica CJS exata: require
# pode ser lido de dentro de uma função só executada depois). Isso quebra na
# hora em qualquer browser, sem `require` global: "Dynamic require of
# 'react' is not supported". `src/react.js` e `src/react-dom.js` registram
# a si mesmos em `globalThis.__tr_modules` e definem um `require()` global
# que os resolve — desde que carreguem ANTES de quem precisa deles, que é
# exatamente a ordem em que o importmap e o editor os importam.
set -euo pipefail
cd "$(dirname "$0")"
npm install --no-audit --no-fund
OUT=../../inst/www/vendor; mkdir -p "$OUT"
EXT="--external:react --external:react-dom --external:react-dom/client --external:react/jsx-runtime"
# `export *` de um pacote CJS (react/react-dom não publicam ESM) não
# sintetiza export ESM nomeado — só `default` sobrevive, e todo
# `import { useState } from "react"` do editor voltaria `undefined`. Por
# isso `src/react.js` e `src/react-dom.js` nomeiam a API pública inteira
# à mão, em vez de `export *`.
npx esbuild src/react.js            --bundle --format=esm --minify --outfile=$OUT/react.js
npx esbuild src/react-dom.js        --bundle --format=esm --minify --external:react --outfile=$OUT/react-dom.js
# `src/react-dom-client.js` e `src/react-jsx-runtime.js` importam pelo
# CAMINHO DE ARQUIVO do pacote instalado, não pelo especificador nu
# ("react-dom/client", "react/jsx-runtime"): com o nu, `--external:react-dom`
# (ou `--external:react`) bate também no PRÓPRIO módulo que o comando gera —
# mesmo prefixo, mesmo pacote — e o esbuild trata a própria saída como
# externa: autoimportação circular, arquivo de 31 bytes nunca resolvido, sem
# aviso. Pelo caminho de arquivo o esbuild resolve o pacote de verdade, e só
# a dependência (bare, referenciada LÁ DENTRO) fica externa.
npx esbuild src/react-dom-client.js --bundle --format=esm --minify --external:react-dom --outfile=$OUT/react-dom-client.js
npx esbuild src/react-jsx-runtime.js --bundle --format=esm --minify --external:react --outfile=$OUT/react-jsx-runtime.js
npx esbuild src/xyflow.js           --bundle --format=esm --minify $EXT --outfile=$OUT/xyflow.js
npx esbuild src/dagre.js            --bundle --format=esm --minify --outfile=$OUT/dagre.js
npx esbuild src/html-to-image.js    --bundle --format=esm --minify --outfile=$OUT/html-to-image.js
cp node_modules/@xyflow/react/dist/style.css "$OUT/xyflow.css"
ls -la "$OUT"

# Mexeu em versão ou acrescentou pacote aqui? ATUALIZE inst/COPYRIGHTS e
# inst/licenses/. `--bundle` embute as dependências transitivas dentro de
# cada .js, então a lista de atribuição não sai do package.json — sai do
# que o bundle de fato contém:
#
#   npx esbuild src/xyflow.js --bundle --format=esm --minify $EXT \
#     --metafile=/tmp/meta.json --outfile=/dev/null
#
# e os `inputs` do metafile são a resposta. Foi assim que a lista nasceu, e
# é a única forma de ela não mentir.
