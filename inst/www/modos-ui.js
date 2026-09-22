// Peças de interface dos modos do card. Separadas de `editor.js` (já grande)
// e de `modos.js` (puro, testado em Node): aqui entra React.
import React from "react";
import { h, getWidget } from "trama";
import { MODOS, dica, modoDe } from "./modos.js";

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

// Lista de parâmetros de um nó: mesmo corpo que morava inline em `NdNode`
// (editor.js), extraído pra ser reaproveitado pelo card (modo `params`/
// `completo`) e pelo `ParamsDock` (modo `mini`/`preview`, painel na borda da
// tela). `id` só entra pra formar a `key` do widget — o resto é `spec` +
// valores + callback, sem nada de posição no card.
export function ParamsList({ id, spec, params, onParam }) {
  return h("div", { className: "tr-params" }, (spec.params || []).map((p) => {
    const W = getWidget(p.kind);
    // `div`, e não `label`: o `<label>` repassa o clique ao primeiro
    // controle rotulável de dentro, e com botões ali (segmentado, chave)
    // clicar no texto "Tipo" escolhia a primeira opção. Sem `htmlFor`
    // de propósito: os widgets não recebem `id`, e manter a API
    // `(spec, value, onChange)` das coleções vale mais que focar o
    // campo clicando no rótulo.
    return h("div", { key: p.name, className: "tr-param" }, [
      h("span", { key: "n", title: p.name }, p.label || p.name),
      // O widget entra num Fragment com `key` porque vai num array ao lado
      // do rótulo, e o elemento que a coleção devolve não tem chave.
      W ? h(React.Fragment, { key: "w" }, W(p, params[p.name], (v) => onParam(id, p.name, v)))
        : h("input", { key: "w", className: "nodrag", type: "text",
                       defaultValue: JSON.stringify(params[p.name] ?? p.default),
                       onBlur: (e) => { try { onParam(id, p.name, JSON.parse(e.target.value)); }
                                        catch (_) {} } }),
    ]);
  }));
}

// Parâmetros de um card que não os mostra (mini ou só preview), na borda
// esquerda da TELA — não presos ao card, como o painel de propriedades do
// Excalidraw. Recolhido vira só uma alça; o recolhimento vale pra todos os
// cards, porque é uma preferência de como trabalhar, não de um bloco.
export function ParamsDock({ node, recolhido, onRecolher, categories }) {
  const { spec } = node.data;
  const cor = categories?.[spec.category]?.color || "#64748b";
  if (recolhido) {
    return h("button", { className: "tr-dock-alca nodrag", title: "mostrar parâmetros",
                         onClick: () => onRecolher(false) }, "›");
  }
  const nParams = (spec.params || []).length;
  return h("aside", { className: "tr-dock nowheel", "aria-label": "parâmetros do bloco" }, [
    h("div", { key: "hd", className: "tr-dock-head", style: { borderColor: cor } }, [
      h(ModoIcone, { key: "m", modo: modoDe(node.data) }),
      h("strong", { key: "t" }, node.data.label || spec.label),
      h("button", { key: "x", className: "tr-dock-fechar", title: "recolher",
                    onClick: () => onRecolher(true) }, "‹"),
    ]),
    nParams
      ? h(ParamsList, { key: "pl", id: node.id, spec, params: node.data.params,
                        onParam: node.data.onParam })
      : h("div", { key: "e", className: "tr-empty" }, "este bloco não tem parâmetros"),
  ]);
}
