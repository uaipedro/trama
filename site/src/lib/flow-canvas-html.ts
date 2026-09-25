// Constrói o HTML do canvas estático (cards + arestas) com o MARKUP do editor
// no modo MINI (`NdNode` de inst/www/editor.js com `tr-node-mini`): sem
// preview no site, o card completo seria só uma caixa vazia. A aparência vem
// do próprio trama.css, escopado em `.tr-estatico` por scripts/css-editor.mjs.
//
// A posição dos documentos é ignorada: todo canvas (template ou exemplo das
// docs) passa pelo MESMO layout automático, em camadas por profundidade.
// `LAYOUT` troca a direção: "H" (camadas da esquerda para a direita) ou "V"
// (camadas empilhadas de cima para baixo).

import type { FlowGraph } from "./flow-example.ts";
import type { NodeVisual, NodePort } from "../data/node-visuals.ts";
import { corDaCategoria, tintaDaCategoria } from "../../../inst/www/papeis.js";
import { MODOS } from "../../../inst/www/modos.js";

export type Direcao = "H" | "V";
/** Direção padrão do layout do site. "V" cabe na coluna estreita dos cards
 *  sem rolagem nem letra miúda; troque para "H" para camadas lado a lado. */
export const LAYOUT: Direcao = "V";

// `.tr-node-mini .tr-node-head` tem 38px + 1px de borda em cima e embaixo.
const ALTURA = 40;
// Largura do card além do título: ícone (18 + 2*9 - 1), dois `gap` de 9px,
// a grade de ranhuras (14 + 4), o padding direito (10) e as bordas.
const LARGURA_FIXA = 35 + 9 + 9 + 18 + 10 + 2;
const TITULO_MAX = 160; // `.tr-node-mini .tr-node-title{max-width}`
const PORTA = 15, PORTA_GAP = 3; // `.tr-port` e o `gap` de `.tr-in/.tr-out`
const VAO = { H: { camada: 64, linha: 22 }, V: { camada: 34, linha: 28 } };
const MARGEM = 32;
const RAIO = 12;
const MARGEM_ARESTA = 16;
const OFFSET = 20;
const PORTA_PADRAO: NodePort = { name: "", color: "#64748b", required: true, multiple: false };

function escapeHtml(value: string): string {
  return value
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;")
    .replace(/'/g, "&#39;");
}

interface Spec {
  label: string;
  icon?: string;
  cor: string;
  tinta?: string;
  inputs: NodePort[];
  outputs: NodePort[];
}

function specDe(type: string, visual: NodeVisual | undefined): Spec {
  const cat = { role: visual?.role, color: visual?.accent };
  return {
    label: visual?.label ?? type,
    icon: visual?.icon,
    cor: corDaCategoria(cat, null),
    tinta: tintaDaCategoria(cat, null),
    inputs: visual?.inputs ?? (visual?.hasInput === false ? [] : [PORTA_PADRAO]),
    outputs: visual?.outputs ?? (visual?.hasOutput === false ? [] : [PORTA_PADRAO]),
  };
}

// Título em 13px/500 (Inter): ~7px por caractere, estreitos e largos à parte.
function larguraTitulo(t: string): number {
  let w = 0;
  for (const c of t) w += /[ilIj.,:;'|!]/.test(c) ? 3.6 : /[mwMW]/.test(c) ? 10.5 : /[A-Z]/.test(c) ? 8.6 : c === " " ? 3.6 : 7;
  return Math.min(TITULO_MAX, Math.ceil(w));
}

// `.tr-node-mini .tr-ports` cobre o card todo com as portas centradas na
// altura: a coluna de n portas fica no meio dos 38px do cabeçalho.
function yPorta(n: number, i: number): number {
  const coluna = n * PORTA + (n - 1) * PORTA_GAP;
  return 1 + 38 / 2 - coluna / 2 + i * (PORTA + PORTA_GAP) + PORTA / 2;
}

// --- card --------------------------------------------------------------------

const modoIcone = (m: string) =>
  m === "mini"
    ? `<rect x="5" y="5" width="6" height="6" rx="1" fill="currentColor"/>`
    : [2, 9]
        .map((y) => {
          const cheio = y === 2 ? m !== "params" : m !== "preview";
          return `<rect x="2" y="${y}" width="12" height="5" rx="1" fill="${cheio ? "currentColor" : "none"}" stroke="currentColor" stroke-width="1.4"/>`;
        })
        .join("");

const MODO_PICKER = `<div class="tr-modo-picker">${(MODOS as string[])
  .map((m) => `<span class="tr-modo-btn${m === "mini" ? " tr-on" : ""}"><svg viewBox="0 0 16 16" width="14" height="14" aria-hidden="true">${modoIcone(m)}</svg></span>`)
  .join("")}</div>`;

function cardHtml(id: string, spec: Spec, x: number, y: number, w: number): string {
  // O bloco do ícone é a cor da categoria no mini: bloco sem ícone no
  // catálogo do site ganha um genérico, senão o card perde a cor.
  const icone = `<svg class="tr-node-icon tr-node-icon-main" viewBox="0 0 24 24" aria-hidden="true"><use href="/trama/icons/lucide.svg#${escapeHtml(spec.icon ?? "box")}" /></svg>`;
  // Sem a `color` inline da tinta: no mini o título fica na placa do card
  // (`.tr-node-mini .tr-node-head{color:var(--tr-fg)}`), e a tinta só vale no
  // bloco do ícone, via `--tr-cat-ink`.
  const alca = (p: NodePort, lado: "left" | "right") =>
    `<i class="react-flow__handle react-flow__handle-${lado}" style="--porta-cor:${escapeHtml(p.color)}"></i>`;
  return `<div class="tr-node tr-state-idle tr-node-mini" data-node-id="${escapeHtml(id)}" style="left:${x}px;top:${y}px;width:${w}px;--tr-cat:${spec.cor}${spec.tinta ? `;--tr-cat-ink:${spec.tinta}` : ""}">
<div class="tr-node-head" style="background:${spec.cor}">${icone}<span class="tr-node-title" title="${escapeHtml(spec.label)}">${escapeHtml(spec.label)}</span><span class="tr-mini-dot tr-idle"></span>${MODO_PICKER}</div>
<div class="tr-ports"><div class="tr-in">${spec.inputs.map((p) => `<div class="tr-port">${alca(p, "left")}</div>`).join("")}</div><div class="tr-out">${spec.outputs.map((p) => `<div class="tr-port tr-port-out">${alca(p, "right")}</div>`).join("")}</div></div>
</div>`;
}

// --- layout ------------------------------------------------------------------

/** Camada = caminho mais longo desde uma raiz; ordem na camada pelo baricentro
 *  dos pais, para que ramos fiquem lado a lado sem cruzar à toa. */
export function camadas(graph: FlowGraph): Map<string, { camada: number; ordem: number }> {
  const ids = graph.nodes.map((n) => n.id);
  const pais = new Map(ids.map((id) => [id, [] as string[]]));
  for (const [from, to] of graph.edges) if (pais.has(to) && pais.has(from)) pais.get(to)!.push(from);
  const nivel = new Map<string, number>();
  const visitando = new Set<string>();
  const nivelDe = (id: string): number => {
    if (nivel.has(id)) return nivel.get(id)!;
    if (visitando.has(id)) return 0;
    visitando.add(id);
    const n = Math.max(-1, ...pais.get(id)!.map(nivelDe)) + 1;
    nivel.set(id, n);
    return n;
  };
  ids.forEach(nivelDe);
  const porCamada: string[][] = [];
  for (const id of ids) (porCamada[nivel.get(id)!] ??= []).push(id);
  const out = new Map<string, { camada: number; ordem: number }>();
  porCamada.forEach((grupo, c) => {
    const bari = (id: string) => {
      const ps = pais.get(id)!.map((p) => out.get(p)?.ordem ?? 0);
      return ps.length ? ps.reduce((a, b) => a + b, 0) / ps.length : 0;
    };
    const ordenado = c === 0 ? grupo : [...grupo].sort((a, b) => bari(a) - bari(b) || ids.indexOf(a) - ids.indexOf(b));
    ordenado.forEach((id, k) => out.set(id, { camada: c, ordem: k }));
  });
  return out;
}

// --- aresta (a TrAresta do editor) -------------------------------------------

type Ponto = { x: number; y: number };
type Ret = { x1: number; y1: number; x2: number; y2: number };

function caminhoContornando(s: Ponto, t: Ponto, a: Ret, b: Ret): Ponto[] | null {
  let topo: number, base: number;
  if (b.y1 - a.y2 >= MARGEM_ARESTA) { topo = a.y2; base = b.y1; }
  else if (a.y1 - b.y2 >= MARGEM_ARESTA) { topo = b.y2; base = a.y1; }
  else return null;
  const meio = (topo + base) / 2;
  const sx = s.x + MARGEM_ARESTA, tx = t.x - MARGEM_ARESTA;
  return [s, { x: sx, y: s.y }, { x: sx, y: meio }, { x: tx, y: meio }, { x: tx, y: t.y }, t];
}

function smoothstep(s: Ponto, t: Ponto): Ponto[] {
  if (t.x - s.x >= 2 * OFFSET) {
    const mx = (s.x + t.x) / 2;
    return [s, { x: mx, y: s.y }, { x: mx, y: t.y }, t];
  }
  const my = (s.y + t.y) / 2;
  return [s, { x: s.x + OFFSET, y: s.y }, { x: s.x + OFFSET, y: my }, { x: t.x - OFFSET, y: my }, { x: t.x - OFFSET, y: t.y }, t];
}

function caminhoComCantos(pontos: Ponto[], raio: number): string {
  let d = `M${pontos[0].x},${pontos[0].y}`;
  for (let i = 1; i < pontos.length - 1; i++) {
    const p0 = pontos[i - 1], p1 = pontos[i], p2 = pontos[i + 1];
    const l1x = p0.x - p1.x, l1y = p0.y - p1.y, len1 = Math.hypot(l1x, l1y);
    const l2x = p2.x - p1.x, l2y = p2.y - p1.y, len2 = Math.hypot(l2x, l2y);
    const r = Math.min(raio, len1 / 2, len2 / 2);
    if (r <= 0 || len1 === 0 || len2 === 0) { d += `L${p1.x},${p1.y}`; continue; }
    const a = { x: p1.x + (l1x / len1) * r, y: p1.y + (l1y / len1) * r };
    const b = { x: p1.x + (l2x / len2) * r, y: p1.y + (l2y / len2) * r };
    d += `L${a.x},${a.y}Q${p1.x},${p1.y} ${b.x},${b.y}`;
  }
  const fim = pontos[pontos.length - 1];
  return d + `L${fim.x},${fim.y}`;
}

// --- canvas ------------------------------------------------------------------

export interface CanvasOptions {
  /** Escala máxima do canvas; o script do site reduz até caber (com piso). */
  escala?: number;
  /** Direção do layout; o padrão é `LAYOUT`. */
  direcao?: Direcao;
}

export function renderFlowCanvas(
  graph: FlowGraph,
  visuals: Record<string, NodeVisual>,
  options: CanvasOptions = {},
): string {
  const dir = options.direcao ?? LAYOUT;
  const specs = new Map(graph.nodes.map((n) => [n.id, specDe(n.type, visuals[n.type])]));
  // Largura única no canvas: a do título mais longo, para os cards formarem grade.
  const W = LARGURA_FIXA + Math.max(0, ...[...specs.values()].map((s) => larguraTitulo(s.label)));
  const vao = VAO[dir];

  const lugar = camadas(graph);
  const porCamada = new Map<number, number>();
  for (const { camada } of lugar.values()) porCamada.set(camada, (porCamada.get(camada) ?? 0) + 1);
  const maxLinhas = Math.max(1, ...porCamada.values());
  const passoCamada = (dir === "H" ? W : ALTURA) + vao.camada;
  const passoLinha = (dir === "H" ? ALTURA : W) + vao.linha;

  const ret = new Map<string, Ret>();
  for (const n of graph.nodes) {
    const { camada, ordem } = lugar.get(n.id)!;
    // Camada com menos cards fica centrada no eixo das linhas.
    const desloc = ((maxLinhas - porCamada.get(camada)!) * passoLinha) / 2;
    const a = MARGEM + camada * passoCamada, b = MARGEM + desloc + ordem * passoLinha;
    const [x, y] = dir === "H" ? [a, b] : [b, a];
    ret.set(n.id, { x1: x, y1: y, x2: x + W, y2: y + ALTURA });
  }
  let largura = 0, altura = 0;
  for (const r of ret.values()) {
    largura = Math.max(largura, r.x2 + MARGEM);
    altura = Math.max(altura, r.y2 + MARGEM);
  }

  const cards = graph.nodes
    .map((n) => { const r = ret.get(n.id)!; return cardHtml(n.id, specs.get(n.id)!, r.x1, r.y1, W); })
    .join("\n");

  // Entrada sem porta nomeada: a ordem das ligações que chegam ao nó, como o
  // `from = c(...)` do tr_add; porta múltipla absorve o resto.
  const chegadas = new Map<string, number>();
  const arestas = graph.edges
    .map(([from, to, fromPort, toPort]) => {
      const a = specs.get(from), b = specs.get(to);
      if (!a || !b) return "";
      const iOut = Math.max(0, a.outputs.findIndex((p) => p.name === fromPort));
      let iIn = b.inputs.findIndex((p) => p.name === toPort);
      if (iIn < 0) {
        const k = chegadas.get(to) ?? 0;
        chegadas.set(to, k + 1);
        iIn = Math.min(k, Math.max(0, b.inputs.length - 1));
      }
      const ra = ret.get(from)!, rb = ret.get(to)!;
      // O centro das alças (`.react-flow__handle-right/left`, 15px de largura).
      const s = { x: ra.x2 + 1.5, y: ra.y1 + yPorta(a.outputs.length, iOut) };
      const t = { x: rb.x1 + 4.5, y: rb.y1 + yPorta(b.inputs.length, iIn) };
      // Alvo à frente: degrau simples. Alvo atrás ou embaixo (layout V):
      // contorna pelo vão entre os cards, como a TrAresta do editor.
      const pontos = t.x - s.x >= 2 * OFFSET ? smoothstep(s, t) : caminhoContornando(s, t, ra, rb) ?? smoothstep(s, t);
      const d = caminhoComCantos(pontos, RAIO);
      return `<path class="tr-fio-trilho" d="${d}" /><path class="react-flow__edge-path" fill="none" d="${d}" />`;
    })
    .join("\n");

  const escala = options.escala ?? 1;
  return `<div class="tr-estatico tr-estatico--${dir}" data-canvas-w="${largura}" data-canvas-h="${altura}" data-escala="${escala}" style="--escala:${escala};width:calc(${largura}px * var(--escala));height:calc(${altura}px * var(--escala))">
<div class="tr-estatico__mundo react-flow" style="width:${largura}px;height:${altura}px">
<div class="react-flow__background"></div>
<svg class="tr-estatico__arestas" width="${largura}" height="${altura}" viewBox="0 0 ${largura} ${altura}" aria-hidden="true">
${arestas}
</svg>
${cards}
</div>
</div>`;
}
