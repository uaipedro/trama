// Plugin rehype: acha blocos ```r que montam um ou mais fluxos com
// tr_add(...) e troca o <pre> por um componente de abas Canvas | Código R.
// Um `tr_flow(` por bloco vira um pipeline/canvas separado; o canvas é
// gerado a partir do mesmo texto, então nunca desalinha do código mostrado.

import { visit } from "unist-util-visit";
import { toHtml } from "hast-util-to-html";
import type { Root, Element, Text } from "hast";
import type { VFile } from "vfile";
import { hasFlowCalls, parseFlowExample } from "./flow-example.ts";
import { renderFlowCanvas } from "./flow-canvas-html.ts";
import visuals from "../data/node-visuals.json" with { type: "json" };
import type { NodeVisual } from "../data/node-visuals.ts";

function collectText(node: Element): string {
  let text = "";
  visit(node, "text", (textNode: Text) => {
    text += textNode.value;
  });
  return text;
}

export function rehypeFlowExample() {
  return (tree: Root, file?: VFile) => {
    const fileLabel = file?.path ?? file?.history?.[0];
    let counter = 0;

    visit(tree, "element", (node: Element, index, parent) => {
      if (node.tagName !== "pre") return;
      if (node.properties?.dataLanguage !== "r") return;
      if (index === undefined || !parent) return;

      const code = collectText(node);
      if (!hasFlowCalls(code)) return;

      const graphs = parseFlowExample(code, { fileLabel });
      if (!graphs) return;

      counter += 1;
      const uid = `flow-example-${counter}`;

      const canvasHtml = graphs
        .map((graph, i) => {
          const label =
            graphs.length > 1
              ? `<p class="flow-canvas-label">Fluxo ${i + 1}</p>`
              : "";
          return `${label}<div class="flow-canvas-scroll">${renderFlowCanvas(graph, visuals as Record<string, NodeVisual>)}</div>`;
        })
        .join("\n");

      const originalPreHtml = toHtml(node, { allowDangerousHtml: true });

      const tabCanvasId = `${uid}-tab-canvas`;
      const tabRId = `${uid}-tab-r`;
      const panelCanvasId = `${uid}-panel-canvas`;
      const panelRId = `${uid}-panel-r`;

      const wrapperHtml = `<div class="flow-example" data-flow-example>
  <div role="tablist">
    <button type="button" role="tab" id="${tabCanvasId}" aria-selected="true" aria-controls="${panelCanvasId}" tabindex="0" data-tab="canvas">Canvas</button>
    <button type="button" role="tab" id="${tabRId}" aria-selected="false" aria-controls="${panelRId}" tabindex="-1" data-tab="r">Código R</button>
  </div>
  <div role="tabpanel" id="${panelCanvasId}" aria-labelledby="${tabCanvasId}" data-panel="canvas" class="is-active">${canvasHtml}</div>
  <div role="tabpanel" id="${panelRId}" aria-labelledby="${tabRId}" data-panel="r">${originalPreHtml}</div>
</div>`;

      parent.children[index] = { type: "raw", value: wrapperHtml } as unknown as Element;
    });
  };
}

export default rehypeFlowExample;
