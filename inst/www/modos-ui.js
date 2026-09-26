// Peças de interface dos modos do card. Separadas de `editor.js` (já grande)
// e de `modos.js` (puro, testado em Node): aqui entra React.
import React from "react";
import ReactDOM from "react-dom";
import { h, getWidget, getRenderer, getViews } from "trama";
import { ATALHOS, dica, ehMini, paramsVisiveis, LIMITE_PARAMS_CARD } from "./modos.js";
import { corDaCategoria } from "./papeis.js";

// Um botão só no cabeçalho: miniatura <-> aberto. O ícone mostra pra onde
// o clique LEVA (setas pra dentro recolhe, pra fora abre), como o botão de
// janela do sistema. `nodrag` porque vive dentro do card.
export function ModoToggle({ mini, onChange, className }) {
  const seta = mini
    ? "M9 3h4v4M13 3l-4.5 4.5M7 13H3V9M3 13l4.5-4.5"
    : "M13 7H9V3M9 7l4.5-4.5M3 9h4v4M7 9l-4.5 4.5";
  return h("button", {
    type: "button", className: "tr-modo-btn nodrag " + (className || ""),
    title: dica(mini ? "modo-completo" : "modo-mini"),
    "aria-label": mini ? "abrir card" : "miniatura",
    onClick: (e) => { e.stopPropagation(); onChange(mini ? "completo" : "mini"); },
  }, h("svg", { viewBox: "0 0 16 16", width: 14, height: 14, "aria-hidden": true, fill: "none",
                stroke: "currentColor", strokeWidth: 1.6, strokeLinecap: "round", strokeLinejoin: "round" },
       h("path", { d: seta })));
}

// Dois botões (miniatura, aberto) pra barra de seleção e a paleta, onde não
// há um estado único pra alternar.
export function ModoPicker({ value, onChange, className }) {
  return h("div", { className: "tr-modo-picker nodrag " + (className || ""), role: "group",
                    "aria-label": "modo do card" },
    [["mini", "Miniatura"], ["completo", "Aberto"]].map(([m, r]) => h("button", {
      key: m, type: "button", title: dica(`modo-${m}`), "aria-pressed": value === m,
      className: "tr-modo-txt" + (value === m ? " tr-on" : ""),
      onClick: (e) => { e.stopPropagation(); onChange(m); },
    }, r)));
}

export function Engrenagem() {
  return h("svg", { viewBox: "0 0 24 24", width: 13, height: 13, "aria-hidden": true, fill: "none",
                    stroke: "currentColor", strokeWidth: 2, strokeLinecap: "round", strokeLinejoin: "round" }, [
    h("circle", { key: "c", cx: 12, cy: 12, r: 3 }),
    h("path", { key: "p", d: "M19.4 15a1.7 1.7 0 0 0 .3 1.8l.1.1a2 2 0 1 1-2.8 2.8l-.1-.1a1.7 1.7 0 0 0-1.8-.3 1.7 1.7 0 0 0-1 1.5V21a2 2 0 1 1-4 0v-.1a1.7 1.7 0 0 0-1.1-1.5 1.7 1.7 0 0 0-1.8.3l-.1.1a2 2 0 1 1-2.8-2.8l.1-.1a1.7 1.7 0 0 0 .3-1.8 1.7 1.7 0 0 0-1.5-1H3a2 2 0 1 1 0-4h.1a1.7 1.7 0 0 0 1.5-1.1 1.7 1.7 0 0 0-.3-1.8l-.1-.1a2 2 0 1 1 2.8-2.8l.1.1a1.7 1.7 0 0 0 1.8.3H9a1.7 1.7 0 0 0 1-1.5V3a2 2 0 1 1 4 0v.1a1.7 1.7 0 0 0 1 1.5 1.7 1.7 0 0 0 1.8-.3l.1-.1a2 2 0 1 1 2.8 2.8l-.1.1a1.7 1.7 0 0 0-.3 1.8V9a1.7 1.7 0 0 0 1.5 1H21a2 2 0 1 1 0 4h-.1a1.7 1.7 0 0 0-1.5 1z" }),
  ]);
}

// Lista de parâmetros de um nó: mesmo corpo que morava inline em `NdNode`
// (editor.js), extraído pra ser reaproveitado pelo card (modo `params`/
// `completo`) e pelo `ParamsDock` (modo `mini`/`preview`, painel na borda da
// tela). `id` só entra pra formar a `key` do widget — o resto é `spec` +
// valores + callback, sem nada de posição no card.
export function ParamsList({ id, spec, params, onParam, lista }) {
  return h("div", { className: "tr-params" }, (lista || paramsVisiveis(spec, params)).map((p) => {
    const W = getWidget(p.kind);
    // `div`, e não `label`: o `<label>` repassa o clique ao primeiro
    // controle rotulável de dentro, e com botões ali (segmentado, chave)
    // clicar no texto "Tipo" escolhia a primeira opção. Sem `htmlFor`
    // de propósito: os widgets não recebem `id`, e manter a API
    // `(spec, value, onChange)` das coleções vale mais que focar o
    // campo clicando no rótulo.
    // Rótulo longo vai pra cima do campo (`.tr-param-longo`, trama.css). A
    // conta é por caracteres, e não medindo o texto: medir mudaria a altura do
    // card depois de montado, e 16 é o que cabe na coluna do card de 240px.
    const rotulo = p.label || p.name;
    return h("div", { key: p.name, className: "tr-param" + (rotulo.length > 16 ? " tr-param-longo" : "") }, [
      h("span", { key: "n", title: p.label ? `${p.label} (${p.name})` : p.name }, rotulo),
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

// Rodapé de parâmetros do card aberto. A faixa ("▸ Parâmetros") dobra e
// desdobra; aberta, mostra só os primeiros `LIMITE_PARAMS_CARD` VISÍVEIS (os
// que o `when` esconde nem contam), e a engrenagem abre o formulário inteiro
// no meio da tela. Dobrar é preferência de trabalho, não do documento.
export function ParamsRodape({ id, spec, params, onParam, dobrado, onDobrar, onTodos }) {
  const vis = paramsVisiveis(spec, params);
  if (!vis.length) return null;
  const noCard = vis.slice(0, LIMITE_PARAMS_CARD);
  const resto = vis.length - noCard.length;
  return h("div", { className: "tr-pfoot" + (dobrado ? " tr-pfoot-dobrado" : "") }, [
    h("div", { key: "bar", className: "tr-pfoot-bar" }, [
      h("button", { key: "t", type: "button", className: "tr-pfoot-toggle nodrag",
                    title: dica("params"), "aria-expanded": !dobrado,
                    onClick: (e) => { e.stopPropagation(); onDobrar(id, !dobrado); } }, [
        h("span", { key: "s", className: "tr-pfoot-seta", "aria-hidden": true }, "▸"),
        h("span", { key: "l" }, "Parâmetros"),
        h("span", { key: "n", className: "tr-pfoot-n" }, String(vis.length)),
      ]),
      resto > 0 && !dobrado
        ? h("span", { key: "r", className: "tr-pfoot-resto" }, `+${resto}`) : null,
      h("button", { key: "g", type: "button", className: "tr-pfoot-gear nodrag",
                    title: dica("params-todos"), "aria-label": "todos os parâmetros",
                    onClick: (e) => { e.stopPropagation(); onTodos(id); } }, h(Engrenagem)),
    ]),
    dobrado ? null : h(ParamsList, { key: "pl", id, spec, params, onParam, lista: noCard }),
  ]);
}

// Formulário inteiro de um card, no meio da tela (engrenagem ou P). O
// preview vai ao lado, vivo, pra ver o efeito do que se muda sem o card
// quebrar com um campo largo. Mesmo backdrop do lightbox: Esc e clique fora
// fecham.
export function ParamsModal({ node, categories, preview, onClose }) {
  React.useEffect(() => {
    const onKey = (e) => { if (e.key === "Escape") { e.preventDefault(); onClose(); } };
    window.addEventListener("keydown", onKey);
    return () => window.removeEventListener("keydown", onKey);
  }, [onClose]);
  const { spec } = node.data;
  const cor = corDaCategoria(categories?.[spec.category], spec);
  const vis = paramsVisiveis(spec, node.data.params);
  return ReactDOM.createPortal(
    h("div", { className: "tr-lightbox", role: "dialog", "aria-label": "parâmetros", onClick: onClose },
      h("div", { className: "tr-pmodal tr-modal", onClick: (e) => e.stopPropagation() }, [
        h("div", { key: "hd", className: "tr-pmodal-head", style: { borderColor: cor } }, [
          h("strong", { key: "t" }, node.data.label || spec.label),
          h("button", { key: "x", className: "tr-lightbox-close tr-pmodal-x", title: "fechar (Esc)",
                        onClick: onClose }, "×"),
        ]),
        h("div", { key: "b", className: "tr-pmodal-body" }, [
          h("div", { key: "f", className: "tr-pmodal-form" },
            vis.length
              ? h(ParamsList, { id: node.id, spec, params: node.data.params, onParam: node.data.onParam, lista: vis })
              : h("div", { className: "tr-empty" }, "este bloco não tem parâmetros")),
          preview ? h("div", { key: "p", className: "tr-pmodal-preview" }, preview) : null,
        ]),
      ])), document.body);
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
