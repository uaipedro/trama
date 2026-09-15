// Import ESTÁTICO só pela ORDEM — mesma razão de react-dom.js: garante que
// `react.js` já registrou o shim de `require()` antes deste módulo rodar.
import "react";

// Mesma razão de react-dom-client.js: caminho de ARQUIVO, não o especificador
// nu "react/jsx-runtime" — evita a autoimportação circular de
// `--external:react` batendo no próprio módulo. `require("react")` (dentro
// de `react-jsx-runtime.production.min.js`) resolve pelo shim que
// `react.js` registra em `globalThis.__tr_modules`.
//
// `export *` a partir de um CAMINHO DE ARQUIVO (em vez de especificador de
// pacote) não gera export ESM nenhum aqui — o esbuild só sintetiza named
// exports por análise estática quando resolve via nome de pacote. API
// pública do jsx-runtime é só isto: nomear explicitamente.
import { jsx, jsxs, Fragment } from "../node_modules/react/cjs/react-jsx-runtime.production.min.js";
export { jsx, jsxs, Fragment };
