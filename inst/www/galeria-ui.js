// Galeria: componentes React (faixa e lightbox). A regra fica em `galeria.js`
// (pura, testada em Node); aqui só entra React. O lightbox vai para o
// `<body>` por portal, pelo mesmo motivo de `useLightbox` em runtime.js: o
// `overflow` e o `transform` do canvas recortariam um overlay de card.
import React from "react";
import ReactDOM from "react-dom";
import { h } from "trama";
import { navegar, urlComVersao, DICA_GALERIA_VAZIA } from "./galeria.js";

// Miniatura de um item: imagem pronta, "gerando…" enquanto não está pronta,
// ícone com a mensagem no title quando deu erro. Os três são botões, pra
// abrir o lightbox também em erro (onde ele mostra o motivo).
function Miniatura({ item, url, onClick }) {
  const { rotulo, pronto, erro } = item;
  let conteudo;
  if (erro) {
    conteudo = h("span", { className: "tr-galeria-icone", "aria-hidden": true }, "⚠");
  } else if (!pronto) {
    conteudo = h("span", { className: "tr-galeria-gerando" }, "gerando…");
  } else {
    conteudo = h("img", { src: url, alt: "", loading: "lazy", className: "tr-galeria-img" });
  }
  return h("button", {
    type: "button",
    className: "tr-galeria-item" + (erro ? " tr-galeria-item-erro" : ""),
    title: erro ? String(erro) : rotulo,
    "aria-label": (erro ? "erro em " : "abrir ") + rotulo,
    "aria-busy": !pronto && !erro ? true : undefined,
    onClick,
  }, [
    h("span", { key: "c", className: "tr-galeria-quadro" }, conteudo),
    h("span", { key: "r", className: "tr-galeria-rotulo" }, rotulo),
  ]);
}

// Faixa horizontal com as miniaturas. A posição (fixa embaixo do canvas) é de
// quem monta o layout; este componente é só a faixa.
export function RoloGaleria({ itens, urlDe, onAbrir, onFechar }) {
  const lista = Array.isArray(itens) ? itens : [];
  return h("div", { className: "tr-galeria", role: "region", "aria-label": "galeria de imagens" }, [
    h("div", { key: "barra", className: "tr-galeria-barra" }, [
      h("span", { key: "t", className: "tr-galeria-titulo" }, "Galeria"),
      h("button", {
        key: "x", type: "button", className: "tr-galeria-fechar",
        title: "fechar galeria", "aria-label": "fechar galeria",
        onClick: () => onFechar && onFechar(),
      }, "×"),
    ]),
    lista.length === 0
      ? h("p", { key: "v", className: "tr-galeria-dica" }, DICA_GALERIA_VAZIA)
      : h("div", { key: "itens", className: "tr-galeria-itens" }, lista.map((it, i) =>
          h(Miniatura, {
            key: `${it.node}:${i}`,
            item: it,
            url: urlDe ? urlComVersao(urlDe(it), it.versao) : "",
            onClick: () => onAbrir && onAbrir(i),
          }))),
  ]);
}

// Lightbox grande de um item, com setas ←/→ (circulares), Esc para fechar e
// duas ações sobre o card gerador. O estado (`indice`) é de quem chama: aqui
// só se pede a mudança via `onIndice`.
export function LightboxGaleria({ itens, indice, urlDe, onIndice, onFechar, onIrAoCard, onTirar }) {
  const lista = Array.isArray(itens) ? itens : [];
  const total = lista.length;
  // Índice fora da faixa (item removido enquanto aberto) é puxado pro limite.
  const i = total ? Math.min(Math.max(indice | 0, 0), total - 1) : 0;
  const item = total ? lista[i] : null;
  const caixa = React.useRef(null);

  React.useEffect(() => {
    if (!total) return;
    const onKey = (e) => {
      if (e.key === "Escape") onFechar && onFechar();
      else if (e.key === "ArrowLeft") onIndice && onIndice(navegar(i, total, -1));
      else if (e.key === "ArrowRight") onIndice && onIndice(navegar(i, total, 1));
    };
    window.addEventListener("keydown", onKey);
    return () => window.removeEventListener("keydown", onKey);
  }, [total, i, onIndice, onFechar]);

  // Foco no diálogo ao abrir, pra o teclado já cair dentro dele.
  React.useEffect(() => {
    if (total && caixa.current) caixa.current.focus();
  }, [total > 0]);

  if (!item) return null;

  const url = urlDe ? urlComVersao(urlDe(item), item.versao) : "";
  const fechar = () => onFechar && onFechar();
  let corpo;
  if (item.erro) {
    corpo = h("div", { className: "tr-galeria-lb-aviso tr-galeria-lb-erro", role: "alert" },
              [h("span", { key: "i", "aria-hidden": true }, "⚠ "), String(item.erro)]);
  } else if (!item.pronto) {
    corpo = h("div", { className: "tr-galeria-lb-aviso", "aria-busy": true }, "gerando…");
  } else {
    corpo = h("img", { src: url, alt: item.rotulo, className: "tr-galeria-lb-img" });
  }

  const seta = (passo, rotulo, simbolo) => h("button", {
    type: "button", className: "tr-galeria-lb-seta", title: rotulo, "aria-label": rotulo,
    onClick: (e) => { e.stopPropagation(); onIndice && onIndice(navegar(i, total, passo)); },
  }, simbolo);

  const acao = (texto, fn) => h("button", {
    type: "button", className: "tr-galeria-btn", title: texto, "aria-label": texto,
    onClick: (e) => { e.stopPropagation(); fn(item.node); },
  }, texto);

  return ReactDOM.createPortal(
    h("div", {
      className: "tr-galeria-lb", role: "dialog", "aria-modal": true,
      "aria-label": `imagem de ${item.rotulo}`, onClick: fechar,
    }, h("div", {
      ref: caixa, tabIndex: -1, className: "tr-galeria-lb-caixa",
      onClick: (e) => e.stopPropagation(),
    }, [
      h("div", { key: "topo", className: "tr-galeria-lb-topo" }, [
        h("span", { key: "n", className: "tr-galeria-lb-nome" }, item.rotulo),
        h("span", { key: "c", className: "tr-galeria-lb-contagem" }, `${i + 1} / ${total}`),
        h("button", {
          key: "x", type: "button", className: "tr-galeria-fechar",
          title: "fechar (Esc)", "aria-label": "fechar", onClick: fechar,
        }, "×"),
      ]),
      h("div", { key: "meio", className: "tr-galeria-lb-meio" }, [
        total > 1 ? seta(-1, "imagem anterior", "‹") : null,
        h("figure", { key: "f", className: "tr-galeria-lb-figura" }, corpo),
        total > 1 ? seta(1, "próxima imagem", "›") : null,
      ]),
      (onIrAoCard || onTirar) ? h("div", { key: "acoes", className: "tr-galeria-lb-acoes" }, [
        onIrAoCard ? acao("ir ao card", onIrAoCard) : null,
        onTirar ? acao("tirar da galeria", onTirar) : null,
      ]) : null,
    ])),
    document.body);
}
