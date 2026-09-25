// Converte o documento (document-v1) de um template no grafo simples que o
// mini-canvas das docs desenha (flow-canvas-html.ts).

import type { FlowEdge, FlowGraph } from "./flow-example.ts";

interface TemplateDoc {
  nodes: Record<string, { type: string; params?: Record<string, unknown> }>;
  edges?: { from: { node: string; port?: string }; to: { node: string; port?: string } }[];
  ui?: { positions?: Record<string, [number, number]> };
}

function paramText(value: unknown): string {
  if (Array.isArray(value)) return value.map(paramText).join(", ");
  if (typeof value === "boolean") return value ? "TRUE" : "FALSE";
  if (value === null || value === undefined) return "";
  return typeof value === "object" ? JSON.stringify(value) : String(value);
}

export function templateToGraph(doc: TemplateDoc): FlowGraph {
  return {
    nodes: Object.entries(doc.nodes).map(([id, node]) => ({
      id,
      type: node.type,
      params: Object.entries(node.params ?? {}).map(([k, v]) => [k, paramText(v)] as [string, string])
    })),
    edges: (doc.edges ?? []).map((e) => [e.from.node, e.to.node, e.from.port, e.to.port] as FlowEdge),
    positions: doc.ui?.positions
  };
}
