// Import ESTÁTICO só pela ORDEM: garante que `react.js` (marcado external
// abaixo) já rodou — e já registrou `globalThis.require` e
// `__tr_modules.react` — antes de este módulo avaliar o corpo de "react-dom"
// (que faz `require("react")` internamente, dentro do wrapper CJS->ESM do
// esbuild). Sem isto a ordem dependeria de quem MAIS importa "react" em
// algum lugar do grafo chegar primeiro — acidente, não garantia.
import "react";

// Mesma armadilha de react.js: `export *` de CJS não sintetiza export ESM
// nomeado — nomear explicitamente é a API pública real de "react-dom"
// (`grep -oE 'exports\.[A-Za-z_]+' .../react-dom.production.min.js`).
import ReactDOM, {
  createPortal, createRoot, findDOMNode, flushSync, hydrate, hydrateRoot,
  render, unmountComponentAtNode, unstable_batchedUpdates,
  unstable_renderSubtreeIntoContainer, version,
} from "react-dom";

// Mesmo shim de react.js, agora pro consumidor: `react-dom/client` também é
// CJS e faz `require("react-dom")` internamente.
if (typeof globalThis !== "undefined") {
  globalThis.__tr_modules = globalThis.__tr_modules || {};
  globalThis.__tr_modules["react-dom"] = Object.assign({}, ReactDOM, {
    createPortal, createRoot, findDOMNode, flushSync, hydrate, hydrateRoot,
    render, unmountComponentAtNode, unstable_batchedUpdates,
    unstable_renderSubtreeIntoContainer, version, default: ReactDOM,
  });
}

export default ReactDOM;
export {
  createPortal, createRoot, findDOMNode, flushSync, hydrate, hydrateRoot,
  render, unmountComponentAtNode, unstable_batchedUpdates,
  unstable_renderSubtreeIntoContainer, version,
};
