// Peças de interface dos modos do card. Separadas de `editor.js` (já grande)
// e de `modos.js` (puro, testado em Node): aqui entra React.
import React from "react";
import ReactDOM from "react-dom";
import { h, getWidget, getRenderer, getViews } from "trama";
import { MODOS, ATALHOS, dica, modoDe } from "./modos.js";

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

// Tela cheia do card selecionado (V). Mesmo overlay do lightbox de imagem
// (`.tr-lightbox`), pra o Esc e o clique fora terem o mesmo sentido. O
// conteúdo vem do renderer: `expand` quando ele declara, senão a vista atual
// do card, maior. Funciona com o card em mini, porque não depende do preview
// estar montado no card.
export function Vista({ node, assetUrl, onClose }) {
  React.useEffect(() => {
    const onKey = (e) => {
      if (e.key === "Escape" || e.key === "v" || e.key === "V") { e.preventDefault(); onClose(); }
    };
    window.addEventListener("keydown", onKey);
    return () => window.removeEventListener("keydown", onKey);
  }, [onClose]);

  const hd = node.data.handle;
  const art = hd?.preview;
  const r = art && getRenderer(art.renderer);
  const label = node.data.label || node.data.spec?.label;

  let corpo;
  if (!r) {
    corpo = h("div", { className: "tr-empty" }, "sem preview");
  } else if (r.expand) {
    corpo = h(r.expand, { artifact: art, handle: hd, assetUrl, label });
  } else {
    const views = getViews(art.renderer, hd);
    const v = views.find((v) => v.id === node.data.view) || views[0];
    corpo = h("div", { className: "tr-vista-corpo" },
      v ? h(v.component, { artifact: art, handle: hd, assetUrl, label }) : null);
  }

  // Imagem crua (renderer `trama/image`) já vem com a classe `tr-lightbox-img`
  // do lightbox — empacotar de novo num painel ABNT poria fundo e rolagem
  // onde a imagem já cuida de proporção sozinha (`object-fit`). Todo outro
  // renderer entra no mesmo painel do overlay ABNT (`tr-abnt-panel`), que já
  // dá fundo e rolagem a uma vista maior que o card.
  const conteudo = art?.renderer === "trama/image"
    ? h("div", { key: "c", className: "tr-modal", onClick: (e) => e.stopPropagation() }, corpo)
    : h("div", { key: "c", className: "tr-abnt-panel tr-modal", onClick: (e) => e.stopPropagation() }, corpo);

  return ReactDOM.createPortal(
    h("div", { className: "tr-lightbox", role: "dialog", onClick: onClose }, [
      h("button", { key: "x", className: "tr-lightbox-close", title: "fechar (Esc ou V)",
                    onClick: (e) => { e.stopPropagation(); onClose(); } }, "×"),
      conteudo,
    ]), document.body);
}

// Painel de atalhos (H), mesma família visual do `Help` de editor.js
// (`aside.tr-help`, cabeçalho com título + ×): agrupado por `grupo` na ordem
// em que `ATALHOS` os declara, que é a mesma fonte que `dica()` usa nos
// botões — os dois nunca discordam porque leem a mesma tabela.
export function AtalhosPanel({ onClose }) {
  const grupos = [...new Set(ATALHOS.map((a) => a.grupo))];
  return h("aside", { className: "tr-help tr-atalhos" }, [
    h("div", { key: "hd", className: "tr-help-head" }, [
      h("strong", { key: "t" }, "Atalhos"),
      h("button", { key: "x", className: "tr-help-close", title: "fechar (H)",
                    onClick: onClose }, "×"),
    ]),
    h("div", { key: "b", className: "tr-help-body" },
      grupos.map((g) => h("section", { key: g }, [
        h("h4", { key: "t" }, g),
        h("dl", { key: "l" }, ATALHOS.filter((a) => a.grupo === g).flatMap((a) => [
          h("dt", { key: a.id + "k" }, a.teclas.map((t) => h("kbd", { key: t }, t))),
          h("dd", { key: a.id + "d" }, a.rotulo),
        ])),
      ]))),
  ]);
}
