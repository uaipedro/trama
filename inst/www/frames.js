// inst/www/frames.js — frames: retângulos de apresentação, sem execução.
//
// Tudo aqui é `ui` puro. O pertencimento de um card a um frame é GEOMÉTRICO
// (card inteiro dentro do retângulo), calculado no começo de cada arrasto e
// nunca gravado: o documento continua plano, com posições absolutas, e nenhum
// card fica preso a um frame esquecido.

import React from "react";
import { NodeResizer, useStore } from "@xyflow/react";
import { h, Segmented, Toggle } from "trama";
import { toBlob } from "html-to-image";
import dagre from "@dagrejs/dagre";

import { ASPECTS, ratioOf, FRAME_COLORS, rectOf, inside, containedCards, fitAspect,
         containedFrames, containedNotes, FRAME_HEAD, FRAME_PAD, donos, unidades, crescerExterno, abrirEspaco,
         gradeDeFrames, validarPrancheta, PRANCHETA_PADRAO, marcaDaAgua, notasComFrame } from "./geometria.js";
// Reexportados: o editor importa tudo de `./frames.js` e não precisa saber que
// a geometria mudou de arquivo.
export { ASPECTS, ratioOf, FRAME_COLORS, rectOf, inside, containedCards, fitAspect,
         containedFrames, containedNotes, gradeDeFrames, PRANCHETA_PADRAO };

// --- Organizar ---------------------------------------------------------------

// O único lugar que monta um grafo do dagre: o layout das posições ausentes
// (`autoLayout`, no editor) e o "Organizar" daqui dividem os mesmos ajustes,
// e um grafo arrumado no documento aberto não pode ter outra cara que o do
// botão. `items` é `[{id, w, h}]`; volta o canto de cima-esquerdo de cada um,
// inteiro, porque o dagre fala de centro e o React Flow de canto. Aresta com
// ponta fora de `items` fica de fora: o `setEdge` do dagre criaria o nó que
// falta sem tamanho, e o layout inteiro sairia NaN.
export function dagrePos(items, arestas) {
  const g = new dagre.graphlib.Graph();
  g.setGraph({ rankdir: "LR", nodesep: 40, ranksep: 90 });
  g.setDefaultEdgeLabel(() => ({}));
  items.forEach((it) => g.setNode(it.id, { width: it.w, height: it.h }));
  arestas.forEach(([a, b]) => { if (g.hasNode(a) && g.hasNode(b)) g.setEdge(a, b); });
  dagre.layout(g);
  return Object.fromEntries(items.map((it) => {
    const p = g.node(it.id);
    return [it.id, { x: Math.round(p.x - it.w / 2), y: Math.round(p.y - it.h / 2) }];
  }));
}

// "Organizar" com frames: cada frame é um BLOCO. Primeiro o dagre arruma os
// cards de dentro de cada frame, só com as ligações entre eles; o frame cresce
// se o arranjo não couber (nunca encolhe: o tamanho que o usuário escolheu
// pro slide é piso) e o arranjo fica centrado no miolo. Depois um segundo
// dagre arruma os blocos — frames e cards soltos — com cada ligação levada
// ao bloco das pontas. Card de frame anda junto com o frame, então ninguém
// sai de dentro do frame onde estava.
//
// Pertencimento é o mesmo do arrasto (`containedCards`: card INTEIRO dentro),
// com a regra de `donos`: um card em dois blocos teria duas posições, então
// ele fica com o MENOR frame que o contém (empate pela ordem de slide).
//
// Prancheta é RÍGIDA (ver `unidades`). O externo de cima e tudo que ele
// continha formam uma unidade:
//   - por dentro nada troca de lugar: células, cards soltos e externos
//     aninhados guardam o arranjo, e só os cards de cada célula são
//     arrumados, pela mesma regra de bloco. Célula que cresce abre espaço
//     (`abrirEspaco`): quem está à direita dela anda pra direita, quem está
//     abaixo anda pra baixo, e linhas e colunas continuam alinhadas;
//   - o externo só cresce (`crescerExterno`), pra direita e pra baixo, do
//     menor pro maior, pra o de fora já ver o aninhado refeito;
//   - no dagre de fora a unidade é UM nó do tamanho final do externo, com as
//     ligações dos membros levadas a ela; as de dentro somem. Por ser um nó
//     com o retângulo inteiro, o dagre não larga nada de fora lá dentro;
//   - no fim cada membro anda o mesmo tanto que a unidade andou.
//
// Sem frames, só há cards soltos no segundo dagre: é o layout de sempre; sem
// prancheta, não há unidade, e é o layout de frames de antes dela.
//
// Nota nunca entra em NADA disto: não é card nem frame, não tem ligação e não
// vira nó do dagre nenhum (nem do interno de bloco, nem do externo). No fim,
// `notasComFrame` (geometria.js) só olha `rect` (o snapshot ORIGINAL, tirado
// no topo, antes de qualquer layout) e `out.frames` (a posição NOVA de todo
// frame que passou pelo dagre) pra fazer cada nota andar o delta exato do
// frame que a contém — ver o comentário lá pra regra completa e o porquê de
// morar em `geometria.js`, e não aqui.
// Volta `{cards: {id: {x, y}}, frames: {id: {x, y, w, h}}, notes: {id: {x, y}}}`,
// tudo inteiro.
export function organizar(nodes, edges) {
  const { externos, dono } = donos(nodes);
  const { unidades: us, unidadeDe } = unidades(nodes);
  const frames = nodes.filter((n) => n.type === "trFrame" && !externos.includes(n.id))
    .sort((a, b) => (a.data?.order ?? 0) - (b.data?.order ?? 0));
  const cards = nodes.filter((n) => n.type === "ndNode");
  const rect = Object.fromEntries(nodes.map((n) => [n.id, rectOf(n)]));
  // Preview solto (`trSolto`, editor.js) é rígido em relação ao card dono:
  // o layout nunca mexe na posição de um em relação ao outro. O card ocupa,
  // pra efeito de espaço, o retângulo que envolve os dois (`rect`), e o
  // resultado volta pro canto do card somando o deslocamento `anexo`. Quem é
  // de qual frame continua decidido pelo card sozinho (`donos`/`unidades`
  // leem `nodes`): imagem pendurada pra fora do frame não tira o card dele.
  const anexo = {};
  nodes.filter((n) => n.type === "trSolto" && rect[n.data?.alvo]).forEach((s) => {
    const c = rect[s.data.alvo], r = rect[s.id];
    const x = Math.min(c.x, r.x), y = Math.min(c.y, r.y);
    const u = { x, y, w: Math.max(c.x + c.w, r.x + r.w) - x, h: Math.max(c.y + c.h, r.y + r.h) - y };
    anexo[s.data.alvo] = { dx: c.x - x, dy: c.y - y, sid: s.id, sx: r.x - c.x, sy: r.y - c.y };
    rect[s.data.alvo] = u;
  });
  const par = (e) => [e.source, e.target];

  // Por dentro: tamanho final do frame e posição de cada card relativa ao
  // canto do frame. Vale igual pra frame livre e pra célula de prancheta.
  const blocos = frames.map((f) => {
    const r = rectOf(f);
    const ids = cards.filter((c) => dono[c.id] === f.id).map((c) => c.id);
    if (ids.length === 0) return { id: f.id, w: r.w, h: r.h, rel: {} };
    const pos = dagrePos(ids.map((id) => ({ id, w: rect[id].w, h: rect[id].h })),
                         edges.filter((e) => dono[e.source] === f.id && dono[e.target] === f.id).map(par));
    const x0 = Math.min(...ids.map((id) => pos[id].x));
    const y0 = Math.min(...ids.map((id) => pos[id].y));
    const cw = Math.max(...ids.map((id) => pos[id].x + rect[id].w)) - x0;
    const ch = Math.max(...ids.map((id) => pos[id].y + rect[id].h)) - y0;
    let w = Math.max(r.w, cw + 2 * FRAME_PAD);
    let hh = Math.max(r.h, ch + 2 * FRAME_PAD + FRAME_HEAD);
    // Proporção travada: a dimensão curta cresce até ela, a regra de
    // `fitAspect`. Como as duas já são ≥ as de antes, crescer uma delas não
    // encolhe nada. `ceil`, e não `round`: arredondar pra baixo poderia
    // devolver o meio pixel que o conteúdo precisava. Só quando o frame
    // cresceu: o `ceil` deixa a proporção fora por uma fração de pixel, e
    // reaplicada a um frame que já cabia ela o esticaria mais 1px a cada
    // clique, e o segundo "Organizar" seguido não seria um "nada a fazer".
    // O lado que manda (o que não cresceu pela proporção) arredonda primeiro,
    // e o outro sai DELE: arredondar os dois cada um por si dava 211x159 num
    // 4:3 em que 212x159 é exato. Continua só crescendo: o lado derivado é o
    // `ceil` de um valor que já era ≥ o que o conteúdo pedia.
    const ra = ratioOf(f.data?.aspect);
    if (ra && (w > r.w || hh > r.h)) {
      if (w / hh < ra) { hh = Math.ceil(hh); w = Math.ceil(hh * ra); }
      else { w = Math.ceil(w); hh = Math.ceil(w / ra); }
    } else { w = Math.ceil(w); hh = Math.ceil(hh); }
    // Centro do miolo: na horizontal, o do frame; na vertical, o do trecho
    // entre o fim do cabeçalho e a borda de baixo.
    const ox = (w - cw) / 2, oy = (FRAME_HEAD + hh - ch) / 2;
    const rel = Object.fromEntries(ids.map((id) => [id, {
      x: Math.round(ox + pos[id].x - x0), y: Math.round(oy + pos[id].y - y0) }]));
    return { id: f.id, w, h: hh, rel };
  });

  // Unidades, ainda perto do lugar de antes. A célula que cresceu não pode
  // só esticar: invadiria a vizinha (ver `abrirEspaco`). Então a grade abre
  // espaço, medida nos retângulos de ANTES dos membros diretos — células,
  // externos aninhados e cards soltos, todos pela mesma régua —, e cada um
  // anda o seu (dx, dy): a célula com o tamanho novo e seus cards pelo `rel`.
  const bl = Object.fromEntries(blocos.map((b) => [b.id, b]));
  const fixo = {};
  us.forEach((u) => {
    const soltosU = u.membros.cards.filter((id) => !(id in dono));
    const lista = [...u.membros.frames, ...soltosU].map((id) => {
      const r = rect[id];
      return { id, ...r, nw: bl[id]?.w ?? r.w, nh: bl[id]?.h ?? r.h };
    });
    const d = abrirEspaco(lista);
    lista.forEach((m) => { fixo[m.id] = { x: m.x + d[m.id].dx, y: m.y + d[m.id].dy, w: m.nw, h: m.nh }; });
    u.membros.frames.filter((id) => id in bl).forEach((id) => {
      Object.entries(bl[id].rel).forEach(([k, q]) => {
        fixo[k] = { x: fixo[id].x + q.x, y: fixo[id].y + q.y, w: rect[k].w, h: rect[k].h };
      });
    });
  });
  // Externos do menor pro maior, já na posição aberta (o de cima não é
  // membro e não anda). Cada um cresce, só pra direita e pra baixo, em volta
  // do que continha no começo e é da mesma unidade (um card de frame de fora
  // que invadia o externo vai embora com o frame dele, e não conta).
  const area = (id) => rect[id].w * rect[id].h;
  [...externos].sort((a, b) => area(a) - area(b)).forEach((id) => {
    const f = nodes.find((n) => n.id === id);
    const u = unidadeDe[id] ?? id;
    const ids = [...containedFrames(f, nodes), ...containedCards(f, nodes)]
      .filter((k) => (unidadeDe[k] ?? k) === u && k in fixo);
    fixo[id] = crescerExterno(fixo[id] ?? rect[id], ids.map((k) => fixo[k]), f.data?.aspect);
  });

  // Por fora: unidades, frames livres e cards soltos como nós. Ligação dentro
  // de um bloco some (laço no próprio nó), e duas ligações entre os mesmos
  // blocos viram uma.
  const bloco = (id) => unidadeDe[id] ?? dono[id] ?? id;
  const soltos = cards.filter((c) => !(c.id in dono) && !(c.id in unidadeDe));
  const livres = blocos.filter((b) => !(b.id in unidadeDe));
  const vistas = new Set();
  const arestas = [];
  edges.forEach((e) => {
    // Separador NUL: id de nó nunca contém \0, então a chave do par não colide
    // com nenhum id. Escrito como escape (mesmo caractere pro JS) porque o byte
    // cru faz git e grep tratarem o arquivo inteiro como binário.
    const a = bloco(e.source), b = bloco(e.target), k = `${a}\0${b}`;
    if (a === b || vistas.has(k)) return;
    vistas.add(k); arestas.push([a, b]);
  });
  const pos = dagrePos([...soltos.map((c) => ({ id: c.id, w: rect[c.id].w, h: rect[c.id].h })),
                        ...livres.map((b) => ({ id: b.id, w: b.w, h: b.h })),
                        ...us.map((u) => ({ id: u.id, w: fixo[u.id].w, h: fixo[u.id].h }))], arestas);

  const out = { cards: {}, frames: {}, notes: {} };
  soltos.forEach((c) => { out.cards[c.id] = pos[c.id]; });
  livres.forEach((b) => {
    const p = pos[b.id];
    out.frames[b.id] = { x: p.x, y: p.y, w: b.w, h: b.h };
    Object.entries(b.rel).forEach(([id, d]) => { out.cards[id] = { x: p.x + d.x, y: p.y + d.y }; });
  });
  // A unidade inteira anda o que o externo andou. Tudo inteiro: `fixo` e
  // `pos` já são.
  us.forEach((u) => {
    const dx = pos[u.id].x - fixo[u.id].x, dy = pos[u.id].y - fixo[u.id].y;
    [u.id, ...u.membros.frames].forEach((id) => {
      const r = fixo[id];
      out.frames[id] = { x: r.x + dx, y: r.y + dy, w: r.w, h: r.h };
    });
    u.membros.cards.forEach((id) => { out.cards[id] = { x: fixo[id].x + dx, y: fixo[id].y + dy }; });
  });
  // Do canto do envelope pro canto do card, e a imagem no mesmo lugar em
  // relação a ele que tinha antes.
  out.soltos = {};
  Object.entries(anexo).forEach(([id, a]) => {
    const p = out.cards[id];
    if (!p) return;
    out.cards[id] = { x: p.x + a.dx, y: p.y + a.dy };
    out.soltos[a.sid] = { x: out.cards[id].x + a.sx, y: out.cards[id].y + a.sy };
  });
  out.notes = notasComFrame(nodes, rect, out.frames);
  return out;
}

// Largura e altura mínimas de um frame: piso da alça e do desenho com a
// ferramenta de frame (Shift+F; a altura só conta no desenho `livre`,
// travado, ela sai da largura).
const FRAME_MIN_W = 160, FRAME_MIN_H = 90;

// Chave de um retângulo do resizer arredondado como a op o grava: é nessa
// resolução que "mudou ou não" importa.
const arred = (p) => [p.x, p.y, p.width, p.height].map(Math.round).join(",");

// O nó. Arrastável só pelo cabeçalho (`dragHandle` em `docToFlow`); o corpo
// deixa o ponteiro passar (CSS), então arrastar no miolo de um frame cai no
// pane: anda pela tela, e com Shift começa uma caixa de seleção por cima dos
// cards que ele contém.
//
// A alça é o `NodeResizer` do xyflow, e não uma alça própria como a do card:
// lá a altura que cresce é a do PREVIEW, com params e portas abaixo; aqui o
// frame não tem layout interno, e o resizer nativo serve.
//
// As alças do resizer são de 4px em unidades do FLUXO: com o zoom em que um
// frame de 960 cabe na tela (~0,4), viravam 1,5px na tela, impossíveis de
// pegar. `--tr-z` leva o zoom ao CSS, que divide o tamanho por ele.
export function FrameNode({ id, data, selected }) {
  const zoom = useStore((s) => s.transform[2]);
  const cor = FRAME_COLORS.includes(data.color) ? data.color : "azul";
  const cls = ["tr-frame", `tr-frame-${cor}`, selected ? "tr-frame-sel" : ""].filter(Boolean).join(" ");
  // O xyflow chama `onResizeEnd` até num clique seco numa alça, e a op com os
  // mesmos valores viraria um passo vazio no desfazer. A comparação é com o
  // retângulo do COMEÇO do gesto, e não com o nó atual: no fim do arrasto o
  // estado já recebeu o tamanho novo, e aí todo redimensionamento real
  // pareceria "sem mudança". Callbacks estáveis porque o `NodeResizer` refaz
  // seus listeners quando elas mudam.
  const { onFrameRect } = data;
  const ini = React.useRef(null);
  const onResizeStart = React.useCallback((_e, p) => { ini.current = arred(p); }, []);
  const onResizeEnd = React.useCallback((_e, p) => {
    if (arred(p) !== ini.current) onFrameRect(id, p);
  }, [id, onFrameRect]);
  // O resizer vem DEPOIS do cabeçalho: os dois são absolutos e sem z-index,
  // então quem vem por último fica por cima. Antes dele, o cabeçalho cobria a
  // metade de dentro da faixa de pegar da borda de cima e as alças dos cantos
  // de cima.
  return h("div", { className: cls, style: { "--tr-z": zoom } }, [
    h("div", { key: "hd", className: "tr-frame-head",
               title: "Arraste para mover com o conteúdo · Alt+arrastar move só o frame",
               onDoubleClick: (e) => { e.stopPropagation(); data.onFrameEditStart(id); } }, [
      h("span", { key: "o", className: "tr-frame-order" }, String(data.index)),
      data.editing
        ? h("input", {
            key: "t", className: "tr-frame-input nodrag", autoFocus: true,
            defaultValue: data.title,
            onBlur: (e) => {
              data.onFrameEditEnd();
              const t = e.target.value.trim();
              if (t !== data.title) data.onFrameEdit(id, { title: t });
            },
            onKeyDown: (e) => {
              if (e.key === "Enter") e.currentTarget.blur();
              if (e.key === "Escape") { e.currentTarget.value = data.title; e.currentTarget.blur(); }
            },
          })
        : h("span", { key: "t", className: "tr-frame-title" }, data.title || "sem título"),
      h("span", { key: "a", className: "tr-frame-aspect" }, data.aspect),
    ]),
    h(NodeResizer, { key: "rz", isVisible: !!selected, minWidth: FRAME_MIN_W, minHeight: FRAME_MIN_H,
                     keepAspectRatio: ratioOf(data.aspect) != null, onResizeStart, onResizeEnd }),
  ]);
}

// A ferramenta F: overlay que só existe com ela ativa, e por isso captura o
// arrasto por cima do canvas sem que pane ou card reajam. Desenha já na
// proporção dos frames novos (`aspect`, escolhida na toolbar), com a largura
// mandando e a altura saindo dela; `livre` desenha o retângulo que a mão
// fizer. Clique seco (ponteiro andou menos de `CLIQUE` px de TELA) cria um
// frame de 960 de largura, na mesma proporção (540 de altura no `livre`),
// centrado no ponto. Em pixels de tela, e não do fluxo: o tremor da mão não
// depende do zoom, e em unidades do fluxo um arrasto curto com zoom baixo
// virava "clique" e um tremor com zoom alto virava frame minúsculo.
const CLIQUE = 6;
export function FrameDraw({ toFlow, onDone, aspect }) {
  const [box, setBox] = React.useState(null);
  const ini = React.useRef(null);
  const local = (e) => {
    const r = e.currentTarget.getBoundingClientRect();
    return { x: e.clientX - r.left, y: e.clientY - r.top };
  };
  const r = ratioOf(aspect);
  return h("div", {
    className: "tr-framedraw",
    onPointerDown: (e) => {
      // Só o botão principal desenha: o do meio é o de andar pela tela, e o
      // direito abriria um frame no lugar de um menu.
      if (e.button !== 0) return;
      e.currentTarget.setPointerCapture(e.pointerId);
      ini.current = { tela: { x: e.clientX, y: e.clientY }, loc: local(e) };
      setBox(null);
    },
    onPointerMove: (e) => {
      const a = ini.current; if (!a) return;
      const p = local(e);
      const w = Math.abs(p.x - a.loc.x), hh = r ? w / r : Math.abs(p.y - a.loc.y);
      setBox({ x: Math.min(p.x, a.loc.x), y: p.y < a.loc.y ? a.loc.y - hh : a.loc.y, w, h: hh });
    },
    onPointerUp: (e) => {
      const a = ini.current; ini.current = null;
      if (!a) return;
      const p0 = toFlow(a.tela);
      if (Math.hypot(e.clientX - a.tela.x, e.clientY - a.tela.y) < CLIQUE) {
        const hh = r ? 960 / r : 540;
        onDone({ x: p0.x - 480, y: p0.y - hh / 2, w: 960, h: hh });
        return;
      }
      const p1 = toFlow({ x: e.clientX, y: e.clientY });
      // Arrasto quase vertical passa do limiar de clique com largura ~0, e o
      // servidor recusa `w` que não seja positivo: vale o piso da alça. No
      // `livre` a altura tem o piso dela, pelo mesmo motivo no arrasto quase
      // horizontal.
      const w = Math.max(FRAME_MIN_W, Math.abs(p1.x - p0.x));
      const hh = r ? w / r : Math.max(FRAME_MIN_H, Math.abs(p1.y - p0.y));
      // Ancorado no ponto de partida: com o piso, `min(p0.x, p1.x)` faria o
      // frame passar do início quando o arrasto é para a esquerda.
      onDone({ x: p1.x < p0.x ? p0.x - w : p0.x, y: p1.y < p0.y ? p0.y - hh : p0.y, w, h: hh });
    },
    // Gesto cancelado pelo sistema (ponteiro perdido, toque virou rolagem):
    // some a caixa e não nasce frame nenhum. A ferramenta continua ativa.
    onPointerCancel: () => { ini.current = null; setBox(null); },
  }, box ? h("div", { className: "tr-framedraw-box",
                      style: { left: box.x, top: box.y, width: box.w, height: box.h } }) : null);
}

// --- Exportação --------------------------------------------------------------

const SVG_NS = "http://www.w3.org/2000/svg";
// A mesma URL que o `Icon` do editor usa: os dois módulos estão na mesma pasta.
const SPRITE = new URL("vendor/lucide.svg", import.meta.url).href;
// Mesmo motivo do sprite: `./marca.svg` num `src` seria relativo ao DOCUMENTO,
// que o Shiny serve na raiz, e cairia fora do prefixo versionado. Exportada
// porque o editor põe a mesma marca na barra — duas cópias desta linha foi o
// que duas fases desta feature escreveram sem saber uma da outra.
export const MARCA = new URL("marca.svg", import.meta.url).href;
// Só o sucesso fica guardado: uma falha (rede, 404) não pode envenenar as
// exportações seguintes, que tentam de novo.
let spriteDoc = null;
async function sprite() {
  if (!spriteDoc) {
    const res = await fetch(SPRITE);
    if (!res.ok) throw new Error("sprite dos ícones indisponível");
    spriteDoc = new DOMParser().parseFromString(await res.text(), "image/svg+xml");
  }
  return spriteDoc;
}

// O ícone do card é `<use href="…/lucide.svg#id">`, referência a arquivo
// EXTERNO. O html-to-image serializa o DOM num SVG que vira imagem, e imagem
// não carrega recurso externo: sem isto, todo ícone sairia em branco no PNG.
// Só durante a captura, os símbolos usados entram no próprio viewport e os
// `use` apontam pra eles por fragmento local. Mexe só em atributo e acrescenta
// um filho no FIM do viewport, o que o React tolera; a volta está no retorno.
async function inlineIcons(root) {
  const uses = [...root.querySelectorAll("use")]
    .map((u) => [u, u.getAttribute("href") || ""])
    .filter(([, href]) => href.startsWith(SPRITE + "#"));
  if (uses.length === 0) return () => {};
  const doc = await sprite();
  const defs = document.createElementNS(SVG_NS, "svg");
  defs.setAttribute("width", "0"); defs.setAttribute("height", "0");
  defs.setAttribute("aria-hidden", "true");
  defs.style.position = "absolute";
  new Set(uses.map(([, href]) => href.split("#")[1])).forEach((id) => {
    const s = doc.querySelector(`symbol[id="${CSS.escape(id)}"]`);
    if (s) defs.appendChild(document.importNode(s, true));
  });
  root.appendChild(defs);
  uses.forEach(([u, href]) => u.setAttribute("href", "#" + href.split("#")[1]));
  return () => { uses.forEach(([u, href]) => u.setAttribute("href", href)); defs.remove(); };
}

// O html-to-image copia o estilo computado de cada elemento HTML, mas um
// <svg> ele clona inteiro, sem descer: os filhos levam só os atributos. O
// traço da ligação vem do CSS (`.react-flow__edge-path`), então sem isto toda
// ligação sairia invisível no PNG. Só durante a captura, e depois de
// `.tr-exporting` (que tira o destaque da ligação escolhida), o traço
// computado vira estilo inline; a volta está no retorno.
function inlineEdges(root) {
  const paths = [...root.querySelectorAll(".react-flow__edge-path")];
  const antes = paths.map((p) => p.getAttribute("style"));
  paths.forEach((p) => {
    const cs = getComputedStyle(p);
    p.style.stroke = cs.stroke;
    p.style.strokeWidth = cs.strokeWidth;
  });
  return () => paths.forEach((p, i) =>
    (antes[i] == null ? p.removeAttribute("style") : p.setAttribute("style", antes[i])));
}

const slug = (s) => (s || "").normalize("NFD").replace(/[\u0300-\u036f]/g, "")
  .toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/^-|-$/g, "");

// `dim` é `[largura, altura]` opcional, e existe por causa de SVG.
//
// `marca.svg` declara só `viewBox`, sem `width`/`height` — ou seja, não tem
// dimensão INTRÍNSECA. Navegador que não aplica o sizing padrão a uma imagem
// assim desenha nada em canvas, e o modo de falhar é o problema: sem exceção.
// O `try/catch` do `carimbar` não dispara, o `toBlob` devolve o PNG
// re-codificado e sem assinatura, e a exportação sai calada e errada.
// Dar `width`/`height` ao `Image` ANTES do `src` supre a dimensão que falta.
//
// É defesa, não conserto de bug observado: no Chrome e no Firefox 155 daqui o
// desenho sai IGUAL com e sem `dim` (medido, contando pixels pintados). O
// relato é de Firefox/Safari antigos, que não temos como testar — como o custo
// é uma linha e o modo de falha seria mudo, a defesa fica.
//
// A correção mora aqui, e não no SVG: o mesmo arquivo serve de favicon, de
// marca da barra e de entrada do `tools/marca/rasterizar.sh`, que depende de
// ele NÃO ter dimensão pra renderizar em 512 px.
const carregar = (src, dim) => new Promise((ok, erro) => {
  const img = new Image();
  img.onload = () => ok(img);
  img.onerror = () => erro(new Error(`imagem não carregou: ${src}`));
  if (dim) { img.width = dim[0]; img.height = dim[1]; }
  img.src = src;
});

// O `viewBox` de `marca.svg`, que é o que o `carregar` precisa passar ao
// `Image`. Só a razão entre os dois importa (o destino do `drawImage` é
// explícito); os números em si são os do arquivo pra não inventar um terceiro.
const MARCA_DIM = [173, 200];

// Carimba o hexágono no canto do PNG. Desenha o blob num canvas e devolve
// outro blob — o `html-to-image` não tem gancho de "depois de desenhar", e o
// SVG da marca não está no DOM capturado de propósito: ele não é conteúdo do
// frame, é assinatura.
//
// Falha ao carregar a marca NÃO derruba a exportação: devolve o blob original.
// Perder a assinatura é menos grave que perder a imagem que o usuário pediu.
//
// `f` é o retângulo do frame, em unidades de CSS, e não decoração: a conta da
// posição é feita NELE, com a escala da captura passada junto.
async function carimbar(blob, f) {
  const url = URL.createObjectURL(blob);
  try {
    // Em paralelo porque são duas buscas independentes, e a da marca costuma
    // vir do cache do navegador depois da primeira exportação.
    const [img, marca] = await Promise.all([carregar(url), carregar(MARCA, MARCA_DIM)]);
    const cv = document.createElement("canvas");
    cv.width = img.naturalWidth; cv.height = img.naturalHeight;
    const ctx = cv.getContext("2d");
    ctx.drawImage(img, 0, 0);
    // A geometria é calculada no tamanho do FRAME e a escala entra como
    // PARÂMETRO dela (ver `marcaDaAgua`): medir direto no PNG, que sai em 2x,
    // daria uma marca com metade da fração pretendida. A escala vem da imagem
    // de verdade (e não de um `2` escrito à mão) pra continuar certa se o
    // `pixelRatio` da captura mudar.
    const p = marcaDaAgua(f.w, f.h, 16, cv.width / f.w);
    // Medidas de destino explícitas, e não o `naturalWidth` da marca: o SVG
    // não tem dimensão intrínseca (ver `carregar`), e o que vale é a conta.
    ctx.globalAlpha = 0.7;
    ctx.drawImage(marca, p.x, p.y, p.w, p.h);
    return await new Promise((ok) => cv.toBlob((b) => ok(b || blob), "image/png"));
  } catch (e) {
    console.warn("marca d'água não aplicada:", e);
    return blob;
  } finally {
    URL.revokeObjectURL(url);
  }
}

// Um frame vira um PNG em 2x. O viewport do React Flow é capturado com um
// `transform` que leva o retângulo do frame à origem em escala 1. O fundo
// pontilhado é IRMÃO do viewport e fica de fora sozinho; o resto do "limpo"
// (alças, `?`, seleção, badges, handles) é `.tr-exporting`, no <body> só
// durante a captura e removido no `finally`, pra uma falha não deixar o editor
// sem alças.
export async function exportFramePng(viewportEl, f, index, marca = true) {
  const volta = await inlineIcons(viewportEl);
  document.body.classList.add("tr-exporting");
  let voltaLigacoes = () => {};
  try {
    voltaLigacoes = inlineEdges(viewportEl);
    const bg = getComputedStyle(document.documentElement).getPropertyValue("--tr-bg").trim();
    const bruto = await toBlob(viewportEl, {
      width: f.w, height: f.h, pixelRatio: 2, backgroundColor: bg || "#0f1115",
      // O editor não usa fonte web; pular a varredura de @font-face só poupa tempo.
      skipFonts: true,
      style: { width: `${f.w}px`, height: `${f.h}px`,
               transform: `translate(${-f.x}px, ${-f.y}px) scale(1)` },
    });
    if (!bruto) throw new Error("a captura não gerou imagem");
    const blob = marca ? await carimbar(bruto, f) : bruto;
    // Blob, e não data: URL: um frame grande em 2x dá um PNG de megabytes, e
    // como texto base64 no `href` ele pesa um terço a mais e há navegador que
    // recusa baixar URL desse tamanho. A URL do blob é revogada depois de o
    // clique ter tempo de iniciar o download: revogada na hora, há navegador
    // que cancela o download antes de ele começar.
    const u = URL.createObjectURL(blob);
    const a = document.createElement("a");
    a.href = u;
    a.download = `${String(index).padStart(2, "0")}-${slug(f.title) || "frame"}.png`;
    a.click();
    setTimeout(() => URL.revokeObjectURL(u), 1000);
  } finally {
    document.body.classList.remove("tr-exporting");
    voltaLigacoes();
    volta();
  }
}

// --- Painel ------------------------------------------------------------------

// A prancheta: grade de frames de uma vez. Popover, e não painel: é um pedido
// de um passo, que some ao criar. Os campos guardam o TEXTO digitado (erro
// visível enquanto se digita, como o campo numérico do card) e só viram número
// no `gradeDeFrames`. A miniatura desenha a mesma lista que o Criar emite.
const MINI_W = 220, MINI_H = 130;
export function PranchetaPopover({ inicial, existentes, onCriar, onFechar }) {
  const [o, setO] = React.useState(inicial);
  const set = (k) => (v) => setO((p) => ({ ...p, [k]: v }));
  const erros = validarPrancheta(o);
  const ok = Object.keys(erros).length === 0;
  const fs = ok ? gradeDeFrames(o, { x: 0, y: 0 }, existentes) : [];
  React.useEffect(() => {
    const esc = (e) => { if (e.key === "Escape") { e.stopPropagation(); onFechar(); } };
    window.addEventListener("keydown", esc, true);
    return () => window.removeEventListener("keydown", esc, true);
  }, [onFechar]);

  const num = (k, rot) => h("label", { key: k, className: "tr-prancheta-campo" }, [
    h("span", { key: "r" }, rot),
    h("input", { key: "i", type: "text", inputMode: "decimal", value: String(o[k]),
                 className: erros[k] ? "tr-invalid" : "", title: erros[k],
                 onChange: (e) => set(k)(e.target.value) }),
  ]);

  // Miniatura: bbox da lista inteira escalada pra caber. Sem lista (campo
  // inválido), fica o retângulo vazio: o erro já está marcado no campo.
  let mini = null;
  if (fs.length) {
    const x0 = Math.min(...fs.map((f) => f.x)), y0 = Math.min(...fs.map((f) => f.y));
    const x1 = Math.max(...fs.map((f) => f.x + f.w)), y1 = Math.max(...fs.map((f) => f.y + f.h));
    const s = Math.min(MINI_W / (x1 - x0), MINI_H / (y1 - y0));
    mini = fs.map((f, i) => h("div", {
      key: i, className: `tr-prancheta-mini-f tr-frame-${f.color}` + (o.externo && i === 0 ? " tr-prancheta-mini-ext" : ""),
      style: { left: (f.x - x0) * s, top: (f.y - y0) * s, width: f.w * s, height: f.h * s } }));
  }

  return h("div", { className: "tr-prancheta" }, [
    h("div", { key: "t", className: "tr-prancheta-tit" }, "Prancheta"),
    h("div", { key: "lc", className: "tr-prancheta-linha" }, [num("linhas", "Linhas"), num("colunas", "Colunas")]),
    h(Segmented, { key: "a", options: Object.keys(ASPECTS), value: o.aspect, onChange: set("aspect"),
                   title: "proporção das células" }),
    h("div", { key: "wh", className: "tr-prancheta-linha" }, [
      num("largura", "Largura"),
      ratioOf(o.aspect) == null ? num("altura", "Altura") : null,
      num("espaco", "Espaço"),
    ]),
    // `div` e não `label`: o label em volta do botão do Toggle repassaria o
    // clique ao botão, e a chave poderia virar duas vezes.
    h("div", { key: "ex", className: "tr-prancheta-campo tr-prancheta-ext" }, [
      h("span", { key: "r" }, "Frame externo"),
      h(Toggle, { key: "t", value: o.externo, onChange: set("externo") }),
    ]),
    h("div", { key: "c", className: "tr-prancheta-cores" }, ["rodízio", ...FRAME_COLORS.filter((c) => c !== "cinza")]
      .map((c) => h("button", { key: c, type: "button", title: c,
        className: (c === "rodízio" ? "tr-prancheta-rodizio" : `tr-swatch tr-frame-${c}`) + (o.cor === c ? " tr-menu-on" : ""),
        onClick: () => set("cor")(c) }, c === "rodízio" ? "rodízio" : null))),
    h("div", { key: "m", className: "tr-prancheta-mini", style: { width: MINI_W, height: MINI_H } }, mini),
    h("div", { key: "b", className: "tr-prancheta-acoes" }, [
      h("button", { key: "x", type: "button", onClick: onFechar }, "Cancelar"),
      h("button", { key: "ok", type: "button", disabled: !ok, className: "tr-on",
                    onClick: () => onCriar(o) }, "Criar"),
    ]),
  ]);
}

// Toma a coluna da paleta, como a Ajuda. A lista está na ordem de slide;
// clicar enquadra, arrastar reordena (uma `reorder_frames` só, com a lista
// completa). Duplo clique no título renomeia ali mesmo, e a proporção é um
// `<select>` no próprio item.
//
// O item inteiro é clicável (enquadra) e arrastável (reordena), então campo
// editável dentro dele segura os próprios eventos: clique e tecla que
// subissem enquadrariam o frame (Enter e Espaço são o "clique" do item pelo
// teclado), e tecla que chegasse ao `window` cairia no xyflow. Pelo mesmo
// motivo o item deixa de ser arrastável enquanto o título está em edição:
// arrastar pra selecionar o texto viraria reordenar.
const segura = (e) => e.stopPropagation();
export function FramePanel({ frames, exportando, onGo, onReorder, onRename, onAspect,
                             onPresent, onExport, onClose }) {
  const [dragId, setDragId] = React.useState(null);
  const [editId, setEditId] = React.useState(null);
  const vazio = frames.length === 0;
  const soltar = (alvoId) => {
    if (!dragId || dragId === alvoId) return;
    const ids = frames.map((x) => x.id);
    const de = ids.indexOf(dragId), para = ids.indexOf(alvoId);
    ids.splice(de, 1);
    ids.splice(para, 0, dragId);
    onReorder(ids);
  };
  return h("aside", { className: "tr-frames" }, [
    h("div", { key: "hd", className: "tr-help-head" }, [
      h("strong", { key: "t" }, "Frames"),
      h("button", { key: "x", className: "tr-help-close", title: "voltar à paleta",
                    onClick: onClose }, "×"),
    ]),
    h("div", { key: "b", className: "tr-frames-body" }, vazio
      ? h("p", { className: "tr-frames-empty" },
          "Nenhum frame ainda. Use Shift+F, ou Ctrl+G com cards selecionados.")
      // `div` com papel de botão, e não `<button draggable>`: o Firefox não
      // começa arrasto em botão. Por isso o teclado é ensinado à mão (Tab chega
      // pelo `tabIndex`, Enter e Espaço enquadram). O `stopPropagation` segura
      // o Espaço antes do `window`, onde ele é a tecla de andar pela tela do
      // xyflow; o `preventDefault`, a rolagem da página.
      : frames.map((f, i) => h("div", {
          key: f.id, draggable: editId !== f.id, role: "button", tabIndex: 0,
          title: "clique para enquadrar; arraste para reordenar; duplo clique no título renomeia",
          className: "tr-frames-item" + (dragId === f.id ? " tr-frames-drag" : ""),
          onClick: () => onGo(f.id),
          onKeyDown: (e) => {
            if (e.key !== "Enter" && e.key !== " ") return;
            e.preventDefault(); e.stopPropagation();
            onGo(f.id);
          },
          // Sem dado nenhum no `dataTransfer`, o Firefox cancela o arrasto na
          // largada. O tipo é próprio para o canvas (que só aceita
          // `application/trama-type`) não tomar um frame por bloco solto.
          onDragStart: (e) => {
            setDragId(f.id);
            e.dataTransfer.setData("application/x-trama-frame", f.id);
            e.dataTransfer.effectAllowed = "move";
          },
          onDragEnd: () => setDragId(null),
          onDragOver: (e) => e.preventDefault(),
          onDrop: (e) => { e.preventDefault(); soltar(f.id); },
        }, [
          h("span", { key: "n", className: "tr-frames-n" }, String(i + 1)),
          // Mesma regra do título no cabeçalho do frame (`FrameNode`): Enter ou
          // sair do campo grava, Esc desiste, e título igual não vira op.
          editId === f.id
            ? h("input", {
                key: "t", className: "tr-frames-input", autoFocus: true,
                defaultValue: f.title,
                onClick: segura, onDoubleClick: segura,
                onBlur: (e) => {
                  setEditId(null);
                  const t = e.target.value.trim();
                  if (t !== (f.title ?? "")) onRename(f.id, t);
                },
                onKeyDown: (e) => {
                  e.stopPropagation();
                  if (e.key === "Enter") e.currentTarget.blur();
                  if (e.key === "Escape") { e.currentTarget.value = f.title ?? ""; e.currentTarget.blur(); }
                },
              })
            : h("span", { key: "t", className: "tr-frames-title",
                          onDoubleClick: (e) => { e.stopPropagation(); setEditId(f.id); } },
                f.title || "sem título"),
          // `draggable: false` e o `mousedown` retido: sem eles, abrir a lista
          // com o mouse podia começar o arrasto do item em volta. O `blur`
          // depois da troca é o mesmo da barra de ferramentas: com o foco
          // parado no select, digitar "1" ou "4" trocaria de novo a proporção
          // deste frame (mandando op) e os atalhos do editor ficariam mudos.
          h("select", {
            key: "a", className: "tr-frames-aspect", title: "proporção", value: f.aspect,
            draggable: false, onMouseDown: segura, onClick: segura, onKeyDown: segura,
            onChange: (e) => { onAspect(f.id, e.target.value); e.target.blur(); },
          }, Object.keys(ASPECTS).map((a) => h("option", { key: a, value: a }, a))),
        ]))),
    h("div", { key: "ft", className: "tr-frames-foot" }, [
      h("button", { key: "p", disabled: vazio, title: "Apresentar (F)", onClick: onPresent }, "▶ Apresentar"),
      h("button", { key: "e", disabled: vazio || exportando, onClick: onExport },
        exportando ? "exportando…" : "⤓ Exportar PNGs"),
    ]),
  ]);
}
