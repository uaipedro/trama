// Constrói o HTML do canvas estático (cards + arestas) com o MARKUP do editor:
// as classes são as de `NdNode` (inst/www/editor.js) no modo completo, antes
// de rodar ("sem preview"), e a aparência vem do próprio trama.css, escopado
// em `.tr-estatico` por scripts/css-editor.mjs. Nada de cor ou espaçamento do
// card é decidido aqui.
//
// A altura do card é DECLARADA, pelas mesmas regras de trama.css (os números
// de tools/video/src/trama/metricas.ts): assim a aresta sabe onde cada porta
// está sem medir o DOM, e o HTML sai pronto do build.

import { layoutFlow, type FlowGraph } from "./flow-example.ts";
import type { NodeVisual, NodeParam, NodePort } from "../data/node-visuals.ts";
// Módulos puros do editor: a regra do enum e a cor por papel são as dele.
import { layoutEnum } from "../../../inst/www/params.js";
import { corDaCategoria, tintaDaCategoria } from "../../../inst/www/papeis.js";
import { MODOS } from "../../../inst/www/modos.js";

const M = {
  largura: 240,
  cabeca: 29,
  preview: 133, // 132 + borda
  abas: 19,
  paramsPad: 5,
  paramsGap: 4,
  portasTopo: 5,
  portasBase: 14,
  porta: 15,
  portaGap: 3,
};
// Vão entre cards quando o grafo não traz posição (exemplos das docs): o
// mesmo passo de tools/templates/gerar.R.
const PASSO_X = 360;
const VAO_Y = 80;
const MARGEM = 40;
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
  params: NodeParam[];
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
    params: visual?.params ?? [],
  };
}

// --- geometria (trama.css) ---------------------------------------------------

type Widget = "campo" | "expr" | "largo" | "select" | "seg" | "toggle";

function widgetDe(p: NodeParam): Widget {
  if (p.kind === "expr" || p.kind === "cols") return "expr";
  if (p.kind === "boolean") return "toggle";
  if (p.kind === "theme") return "select";
  if (p.kind === "enum") {
    const l = layoutEnum(p.choices);
    return l === "select" ? "select" : l === "wide" ? "largo" : "seg";
  }
  return "campo";
}

const rotuloDe = (p: NodeParam) => p.label || p.name;
const rotuloLongo = (p: NodeParam) => rotuloDe(p).length > 16;

function alturaLinha(p: NodeParam): number {
  const w = widgetDe(p);
  const base = w === "expr" ? 38 : w === "largo" ? 46 : 22;
  return rotuloLongo(p) && w !== "largo" ? base + 17 : base;
}

function alturaParams(spec: Spec): number {
  const n = spec.params.length;
  if (!n) return 0;
  return M.paramsPad * 2 + spec.params.reduce((s, p) => s + alturaLinha(p), 0) + M.paramsGap * (n - 1);
}

function topoPortas(spec: Spec): number {
  return M.cabeca + M.preview + M.abas + alturaParams(spec) + M.portasTopo;
}

function alturaCard(spec: Spec): number {
  const n = Math.max(spec.inputs.length, spec.outputs.length, 1);
  return topoPortas(spec) + n * M.porta + (n - 1) * M.portaGap + M.portasBase;
}

const yPorta = (spec: Spec, i: number) => topoPortas(spec) + i * (M.porta + M.portaGap) + M.porta / 2;

// --- card --------------------------------------------------------------------

function valorTexto(v: unknown): string {
  if (v === undefined || v === null) return "";
  if (Array.isArray(v)) return v.map(valorTexto).join(", ");
  return String(v);
}

function widgetHtml(p: NodeParam, valor: unknown): string {
  const v = valorTexto(valor ?? p.default);
  const ph = p.example ? ` placeholder="${escapeHtml(p.example)}"` : "";
  switch (widgetDe(p)) {
    case "expr":
      return `<textarea class="tr-expr" rows="2" readonly tabindex="-1"${ph}>${escapeHtml(v)}</textarea>`;
    case "toggle": {
      const on = v === "TRUE" || v === "true";
      return `<span class="tr-toggle${on ? " tr-toggle-on" : ""}"></span>`;
    }
    case "select":
      return `<select disabled tabindex="-1"><option>${escapeHtml(v || (p.kind === "theme" ? "padrão" : ""))}</option></select>`;
    case "seg":
    case "largo":
      return `<div class="tr-seg${widgetDe(p) === "largo" ? " tr-seg-wide" : ""}">${(p.choices ?? [])
        .map((c) => `<button type="button" tabindex="-1"${c === v ? ' class="tr-seg-on"' : ""}>${escapeHtml(c)}</button>`)
        .join("")}</div>`;
    default:
      return `<input type="text" readonly tabindex="-1" value="${escapeHtml(v)}"${ph} />`;
  }
}

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
  .map((m) => `<span class="tr-modo-btn${m === "completo" ? " tr-on" : ""}"><svg viewBox="0 0 16 16" width="14" height="14" aria-hidden="true">${modoIcone(m)}</svg></span>`)
  .join("")}</div>`;

function cardHtml(id: string, spec: Spec, valores: Map<string, unknown>, x: number, y: number): string {
  const icone = spec.icon
    ? `<svg class="tr-node-icon tr-node-icon-main" viewBox="0 0 24 24" aria-hidden="true"><use href="/trama/icons/lucide.svg#${escapeHtml(spec.icon)}" /></svg>`
    : "";
  const tinta = spec.tinta ? `color:${spec.tinta}` : "";
  const params = spec.params.length
    ? `<div class="tr-params">${spec.params
        .map((p) => `<div class="tr-param${rotuloLongo(p) ? " tr-param-longo" : ""}" style="height:${alturaLinha(p)}px"><span>${escapeHtml(rotuloDe(p))}</span>${widgetHtml(p, valores.get(p.name))}</div>`)
        .join("")}</div>`
    : "";
  const porta = (p: NodePort, lado: "in" | "out") => {
    const nome = `<span>${escapeHtml(p.name + (lado === "in" && p.multiple ? " (N)" : "") + (lado === "in" && !p.required ? "?" : ""))}</span>`;
    const alca = `<i class="react-flow__handle react-flow__handle-${lado === "in" ? "left" : "right"}" style="--porta-cor:${escapeHtml(p.color)}"></i>`;
    return lado === "in"
      ? `<div class="tr-port">${alca}${nome}</div>`
      : `<div class="tr-port tr-port-out">${nome}${alca}</div>`;
  };
  return `<div class="tr-node tr-state-idle" data-node-id="${escapeHtml(id)}" style="left:${x}px;top:${y}px;height:${alturaCard(spec)}px;--tr-cat:${spec.cor}${spec.tinta ? `;--tr-cat-ink:${spec.tinta}` : ""}">
<div class="tr-node-head" style="background:${spec.cor};${tinta}">${icone}<span class="tr-node-title">${escapeHtml(spec.label)}</span>${MODO_PICKER}</div>
<div class="tr-preview tr-empty">sem preview</div>
<div class="tr-tabs"><span class="tr-tab-idle">—</span></div>
${params}
<div class="tr-ports"><div class="tr-in">${spec.inputs.map((p) => porta(p, "in")).join("")}</div><div class="tr-out">${spec.outputs.map((p) => porta(p, "out")).join("")}</div></div>
</div>`;
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
}

export function renderFlowCanvas(
  graph: FlowGraph,
  visuals: Record<string, NodeVisual>,
  options: CanvasOptions = {},
): string {
  const specs = new Map(graph.nodes.map((n) => [n.id, specDe(n.type, visuals[n.type])]));

  // Posição: a do documento (templates) ou colunas por profundidade, com a
  // linha no passo do card mais alto.
  const pos = new Map<string, Ponto>();
  if (graph.positions) {
    for (const n of graph.nodes) {
      const p = graph.positions[n.id] ?? [0, 0];
      pos.set(n.id, { x: p[0], y: p[1] });
    }
  } else {
    const grade = layoutFlow(graph);
    const passoY = Math.max(...[...specs.values()].map(alturaCard)) + VAO_Y;
    for (const n of graph.nodes) {
      const g = grade.get(n.id) ?? { col: 0, row: 0 };
      pos.set(n.id, { x: g.col * PASSO_X, y: g.row * passoY });
    }
  }
  const x0 = Math.min(...[...pos.values()].map((p) => p.x));
  const y0 = Math.min(...[...pos.values()].map((p) => p.y));
  for (const p of pos.values()) { p.x += MARGEM - x0; p.y += MARGEM - y0; }

  let largura = 0, altura = 0;
  const ret = new Map<string, Ret>();
  for (const n of graph.nodes) {
    const p = pos.get(n.id)!, h = alturaCard(specs.get(n.id)!);
    ret.set(n.id, { x1: p.x, y1: p.y, x2: p.x + M.largura, y2: p.y + h });
    largura = Math.max(largura, p.x + M.largura + MARGEM);
    altura = Math.max(altura, p.y + h + MARGEM);
  }

  const cards = graph.nodes
    .map((n) => {
      const p = pos.get(n.id)!;
      return cardHtml(n.id, specs.get(n.id)!, new Map(n.params), p.x, p.y);
    })
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
      const s = { x: ra.x2 + 1.5, y: ra.y1 + yPorta(a, iOut) };
      const t = { x: rb.x1 + 4.5, y: rb.y1 + yPorta(b, iIn) };
      const d = caminhoComCantos(caminhoContornando(s, t, ra, rb) ?? smoothstep(s, t), RAIO);
      return `<path class="tr-fio-trilho" d="${d}" /><path class="react-flow__edge-path" fill="none" d="${d}" />`;
    })
    .join("\n");

  const escala = options.escala ?? 0.8;
  return `<div class="tr-estatico" data-canvas-w="${largura}" data-canvas-h="${altura}" data-escala="${escala}" style="--escala:${escala};width:calc(${largura}px * var(--escala));height:calc(${altura}px * var(--escala))">
<div class="tr-estatico__mundo react-flow" style="width:${largura}px;height:${altura}px">
<div class="react-flow__background"></div>
<svg class="tr-estatico__arestas" width="${largura}" height="${altura}" viewBox="0 0 ${largura} ${altura}" aria-hidden="true">
${arestas}
</svg>
${cards}
</div>
</div>`;
}
