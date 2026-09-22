// Peças de interface dos modos do card. Separadas de `editor.js` (já grande)
// e de `modos.js` (puro, testado em Node): aqui entra React.
import { h } from "trama";
import { MODOS, dica } from "./modos.js";

// Dois retângulos empilhados, cheio = parte visível. O mini é um quadradinho
// só, porque o card mini não tem nenhuma das duas partes.
export function ModoIcone({ modo }) {
  const r = (y, cheio) => h("rect", { key: y, x: 2, y, width: 12, height: 5, rx: 1,
    fill: cheio ? "currentColor" : "none", stroke: "currentColor", strokeWidth: 1.4 });
  return h("svg", { viewBox: "0 0 16 16", width: 14, height: 14, "aria-hidden": true },
    modo === "mini"
      ? h("rect", { x: 5, y: 5, width: 6, height: 6, rx: 1, fill: "currentColor" })
      : [r(2, modo !== "params"), r(9, modo !== "preview")]);
}

// Segmentado de quatro ícones. `nodrag` porque também vive dentro do card.
export function ModoPicker({ value, onChange, className }) {
  return h("div", { className: "tr-modo-picker nodrag " + (className || ""), role: "group",
                    "aria-label": "modo do card" },
    MODOS.map((m) => h("button", {
      key: m, type: "button", title: dica(`modo-${m}`), "aria-pressed": value === m,
      className: "tr-modo-btn" + (value === m ? " tr-on" : ""),
      onClick: (e) => { e.stopPropagation(); onChange(m); },
    }, h(ModoIcone, { modo: m }))));
}
