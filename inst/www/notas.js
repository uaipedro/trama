// inst/www/notas.js — notas: blocos de markdown e imagem no canvas, sem
// execução nenhuma (ver `frames.js`, o precedente que este arquivo espelha).
//
// Contrato de `data` que `NotaNode` espera (a Task 3.2 é quem fornece isto ao
// enxertar no editor.js):
//   id           — id do nó (também vem como prop separada, igual ao frame)
//   kind         — "markdown" | "imagem"
//   text         — o markdown (kind "markdown")
//   src          — a URL já resolvida da imagem (kind "imagem"), "" se vazia
//   fit          — "contain" | "cover" (object-fit da imagem)
//   escala       — "letreiro" | "nota" (tamanho de letra/moldura)
//   fundo        — "cartao" | "nenhum" (com ou sem fundo/moldura)
//   color        — um de FRAME_COLORS, ou "nenhuma" (ver decisão abaixo)
//   resolverSrc  — função (caminho) => URL, repassada ao `Markdown`
//   editing      — bool: bloco em edição (textarea aberto)
//   onNotaEditStart(id)      — chamado no duplo clique
//   onNotaEditEnd(id)        — chamado ao sair da edição (blur/commit/Esc)
//   onNotaEdit(id, patch)    — comita `{text}` (ou outro campo futuro)
//   onNotaRect(id, rect)     — comita `{x, y, width, height}` do resize
//
// Decisão sobre `color: "nenhuma"`: ao contrário do frame (que sempre tem uma
// das `FRAME_COLORS`), a nota pode não ter cor nenhuma — um bloco de texto
// "neutro" que só pega a cor do tema, sem moldura colorida. `"nenhuma"` NÃO
// vira `tr-nota-<tom>` nenhuma (nem uma classe extra): a ausência da classe de
// cor É o estado neutro, resolvido no CSS da Task 3.4 com as variáveis do
// tema. Qualquer outro valor fora de `FRAME_COLORS ∪ {"nenhuma"}` cai no
// mesmo neutro, pela mesma razão que `FrameNode` cai em "azul" pra valor
// desconhecido: um documento com dado velho ou corrompido não pode gerar uma
// classe CSS inexistente.
//
// Lightbox de imagem: reaproveita `useLightbox` extraído de `runtime.js` (a
// função `Image` de lá, que hoje sabe ler `artifact.files.png`) — ver o
// comentário ali. A nota já chega com `src` resolvido, então não duplica nada
// da leitura de artefato; só entra no mesmo hook de estado/Escape/portal.

import React from "react";
import { NodeResizer } from "@xyflow/react";
import { h, useLightbox } from "trama";
import { FRAME_COLORS } from "./geometria.js";
import { Markdown } from "./markdown.js";

// Largura e altura mínimas do bloco: piso da alça e do desenho com a
// ferramenta de nota. Bem menores que as do frame — uma nota pode ser um
// post-it pequeno — mas ainda grandes o bastante pra segurar a alça do
// resizer e não sumir num clique seco.
export const NOTA_MIN_W = 80;
export const NOTA_MIN_H = 40;

// Mesma chave de comparação do frame: o retângulo do resizer arredondado como
// a op grava, pra saber se o gesto de fato mudou alguma coisa.
const arred = (p) => [p.x, p.y, p.width, p.height].map(Math.round).join(",");

// O nó. Sem cabeçalho e sem `dragHandle`: ao contrário do frame, a nota não
// tem título nem faixa clicável separada — fora de edição, o bloco inteiro
// não tem nada que precise capturar clique pra si (o texto renderizado não é
// editável ali), então o corpo inteiro pode ser área de arrasto do xyflow.
export function NotaNode({ id, data, selected }) {
  const cor = FRAME_COLORS.includes(data.color) ? data.color : null;
  const cls = ["tr-nota",
    cor ? `tr-nota-${cor}` : "",
    data.escala === "letreiro" ? "tr-nota-letreiro" : "tr-nota-nota",
    data.fundo === "nenhum" ? "tr-nota-nenhum" : "tr-nota-cartao",
    data.kind === "imagem" ? "tr-nota-img" : "tr-nota-md",
    selected ? "tr-nota-sel" : "",
  ].filter(Boolean).join(" ");

  // Mesma guarda contra op vazia em clique seco numa alça que `FrameNode` usa
  // (ver o comentário lá): compara com o retângulo do COMEÇO do gesto, porque
  // no fim do arrasto o nó já tem o tamanho novo e todo redimensionamento
  // pareceria "sem mudança" se comparado contra o estado atual.
  const { onNotaRect } = data;
  const ini = React.useRef(null);
  const onResizeStart = React.useCallback((_e, p) => { ini.current = arred(p); }, []);
  const onResizeEnd = React.useCallback((_e, p) => {
    if (arred(p) !== ini.current) onNotaRect(id, p);
  }, [id, onNotaRect]);

  const onDoubleClick = () => {
    if (data.kind !== "markdown") return; // imagem não tem texto pra editar aqui
    data.onNotaEditStart(id);
  };

  let corpo;
  if (data.editing) {
    // `Esc` cancela: restaura o valor salvo e sai (o `blur` decorrente não
    // deve comitar de novo, por isso o `defaultValue` já volta a `data.text`
    // antes do `blur` — o `onBlur` só comita se o texto mudou em relação ao
    // que está salvo). `Ctrl+Enter` comita e sai. `Enter` sozinho é quebra de
    // linha — não faz nada especial, o textarea trata — que é a diferença em
    // relação ao título do frame (onde Enter = blur): markdown PRECISA de
    // linha em branco pra separar parágrafo.
    corpo = h("textarea", {
      key: "ta", className: "nodrag tr-nota-input", autoFocus: true,
      defaultValue: data.text,
      onBlur: (e) => {
        data.onNotaEditEnd(id);
        const t = e.target.value;
        if (t !== data.text) data.onNotaEdit(id, { text: t });
      },
      onKeyDown: (e) => {
        if (e.key === "Escape") {
          e.currentTarget.value = data.text;
          e.currentTarget.blur();
        } else if (e.key === "Enter" && (e.ctrlKey || e.metaKey)) {
          e.currentTarget.blur();
        }
      },
    });
  } else if (data.kind === "imagem") {
    corpo = h(NotaImagem, { key: "img", src: data.src, fit: data.fit });
  } else {
    corpo = h(Markdown, { key: "md", texto: data.text, resolverSrc: data.resolverSrc, h });
  }

  return h("div", { className: cls, onDoubleClick }, [
    h("div", { key: "c", className: "tr-nota-corpo" }, corpo),
    h(NodeResizer, { key: "rz", isVisible: !!selected, minWidth: NOTA_MIN_W, minHeight: NOTA_MIN_H,
                     onResizeStart, onResizeEnd }),
  ]);
}

// O corpo de uma nota de imagem: sem `src` (string vazia, o estado inicial do
// bloco antes de escolher uma imagem) é um convite, não um quadrado quebrado
// — a lista de imagens de verdade pra escolher é da Task 4.2; aqui é só o
// estado vazio, sem funcionalidade de escolha nenhuma ainda. Com `src`, abre
// o lightbox compartilhado ao clicar (ver `useLightbox` em `runtime.js`).
function NotaImagem({ src, fit }) {
  const { abrir, node } = useLightbox(src || null);
  if (!src) {
    return h("div", { className: "tr-nota-vazia" }, "clique duas vezes para escolher uma imagem");
  }
  return h("div", { className: "tr-nota-img-wrap" }, [
    h("img", { key: "i", className: "tr-nota-imgtag nodrag", src, loading: "lazy",
               style: { objectFit: fit === "cover" ? "cover" : "contain" },
               title: "clique para ampliar",
               onClick: (e) => { e.stopPropagation(); abrir(); } }),
    node,
  ]);
}

// A ferramenta de desenhar nota: mesmo padrão de `FrameDraw`, sem proporção
// fixa — nota não é slide, é sempre livre, e por isso não recebe `aspect`. No
// clique seco (sem arrasto) nasce um retângulo do tamanho MÍNIMO centrado no
// ponto, em vez dos 960×540 do frame: uma nota pequena de post-it não pede um
// bloco enorme por padrão.
const CLIQUE = 6;
export function NotaDraw({ toFlow, onDone }) {
  const [box, setBox] = React.useState(null);
  const ini = React.useRef(null);
  const local = (e) => {
    const r = e.currentTarget.getBoundingClientRect();
    return { x: e.clientX - r.left, y: e.clientY - r.top };
  };
  return h("div", {
    className: "tr-notadraw",
    onPointerDown: (e) => {
      if (e.button !== 0) return;
      e.currentTarget.setPointerCapture(e.pointerId);
      ini.current = { tela: { x: e.clientX, y: e.clientY }, loc: local(e) };
      setBox(null);
    },
    onPointerMove: (e) => {
      const a = ini.current; if (!a) return;
      const p = local(e);
      setBox({ x: Math.min(p.x, a.loc.x), y: Math.min(p.y, a.loc.y),
                w: Math.abs(p.x - a.loc.x), h: Math.abs(p.y - a.loc.y) });
    },
    onPointerUp: (e) => {
      const a = ini.current; ini.current = null;
      if (!a) return;
      const p0 = toFlow(a.tela);
      if (Math.hypot(e.clientX - a.tela.x, e.clientY - a.tela.y) < CLIQUE) {
        onDone({ x: p0.x - NOTA_MIN_W / 2, y: p0.y - NOTA_MIN_H / 2, w: NOTA_MIN_W, h: NOTA_MIN_H });
        return;
      }
      const p1 = toFlow({ x: e.clientX, y: e.clientY });
      const w = Math.max(NOTA_MIN_W, Math.abs(p1.x - p0.x));
      const hh = Math.max(NOTA_MIN_H, Math.abs(p1.y - p0.y));
      // Ancorado no ponto de partida, mesma razão de `FrameDraw`: com o piso,
      // `min(p0.x, p1.x)` faria o bloco passar do início num arrasto pra
      // esquerda menor que o mínimo.
      onDone({ x: p1.x < p0.x ? p0.x - w : p0.x, y: p1.y < p0.y ? p0.y - hh : p0.y, w, h: hh });
    },
    onPointerCancel: () => { ini.current = null; setBox(null); },
  }, box ? h("div", { className: "tr-notadraw-box",
                      style: { left: box.x, top: box.y, width: box.w, height: box.h } }) : null);
}
