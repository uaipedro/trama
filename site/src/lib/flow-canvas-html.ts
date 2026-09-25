// Constrói o HTML do mini-canvas (cards + arestas) a partir do grafo lido em
// flow-example.ts, usando o layout em colunas por profundidade e os dados
// visuais (cor, ícone, rótulo) do catálogo exportado do R.

import { layoutFlow, type FlowGraph } from "./flow-example.ts";
import type { NodeVisual } from "../data/node-visuals.ts";

const CELL_W = 220;
// Altura da célula cabe o card mais alto: cabeçalho, id e até 3 params + "+n".
const CELL_H = 150;
const GAP = 56;
const CARD_W = CELL_W - GAP;
const CARD_H = 88;
const PORT_Y = CARD_H / 2;
const MAX_PARAMS = 3;
const FALLBACK_ACCENT = "#94a3b8";

function escapeHtml(value: string): string {
  return value
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;")
    .replace(/'/g, "&#39;");
}

export function renderFlowCanvas(graph: FlowGraph, visuals: Record<string, NodeVisual>): string {
  const positions = layoutFlow(graph);
  const cols = Math.max(0, ...[...positions.values()].map((p) => p.col)) + 1;
  const rowsByCol = new Map<number, number>();
  for (const pos of positions.values()) {
    rowsByCol.set(pos.col, Math.max(rowsByCol.get(pos.col) ?? 0, pos.row + 1));
  }
  const rows = Math.max(1, ...[...rowsByCol.values()]);

  const width = cols * CELL_W + GAP;
  const height = GAP + rows * CELL_H;

  const cardCenter = (id: string) => {
    const pos = positions.get(id) ?? { col: 0, row: 0 };
    const left = GAP + pos.col * CELL_W;
    const top = GAP + pos.row * CELL_H;
    return { left, top };
  };

  const cards = graph.nodes
    .map((node) => {
      const { left, top } = cardCenter(node.id);
      const visual = visuals[node.type];
      const accent = visual?.accent ?? FALLBACK_ACCENT;
      const label = visual?.label ?? node.type;
      const shownParams = node.params.slice(0, MAX_PARAMS);
      const extra = node.params.length - shownParams.length;
      const paramsHtml = shownParams
        .map(([k, v]) => `<li>${escapeHtml(k)}: ${escapeHtml(v)}</li>`)
        .join("");
      const extraHtml = extra > 0 ? `<li>+${extra}</li>` : "";
      const iconHtml = visual?.icon
        ? `<svg viewBox="0 0 24 24" aria-hidden="true"><use href="/trama/icons/lucide.svg#${escapeHtml(visual.icon)}" /></svg>`
        : `<i class="canvas-card__fallback" aria-hidden="true"></i>`;
      const hasInput = visual?.hasInput ?? true;
      const hasOutput = visual?.hasOutput ?? true;

      return `<div class="canvas-card" style="left:${left}px;top:${top}px;width:${CARD_W}px;--node-accent:${escapeHtml(accent)}" data-node-id="${escapeHtml(node.id)}">
${hasInput ? `<i class="canvas-card__port canvas-card__port--in" aria-hidden="true"></i>` : ""}
<header class="canvas-card__header">${iconHtml}<strong>${escapeHtml(label)}</strong></header>
<code>${escapeHtml(node.type)}</code>
${node.params.length > 0 ? `<ul class="canvas-card__params">${paramsHtml}${extraHtml}</ul>` : ""}
${hasOutput ? `<i class="canvas-card__port canvas-card__port--out" aria-hidden="true"></i>` : ""}
</div>`;
    })
    .join("\n");

  const edges = graph.edges
    .map(([from, to]) => {
      const a = cardCenter(from);
      const b = cardCenter(to);
      const x1 = a.left + CARD_W;
      const y1 = a.top + PORT_Y;
      const x2 = b.left;
      const y2 = b.top + PORT_Y;
      const mx = (x1 + x2) / 2;
      return `<path d="M ${x1} ${y1} C ${mx} ${y1}, ${mx} ${y2}, ${x2} ${y2}" fill="none" stroke="var(--node-accent, #94a3b8)" stroke-width="2" />`;
    })
    .join("\n");

  return `<div class="flow-canvas" style="width:${width}px;height:${height}px;background-image:radial-gradient(circle, var(--line) 1px, transparent 1px);background-size:20px 20px;">
<svg class="flow-canvas__edges" width="${width}" height="${height}" viewBox="0 0 ${width} ${height}" aria-hidden="true">
${edges}
</svg>
${cards}
</div>`;
}
