// Plugin rehype: acha blocos ```r que montam um fluxo com tr_add(...) e
// troca o <pre> por um componente de abas Canvas | Código R. O canvas é
// gerado a partir do mesmo texto, então nunca desalinha do código mostrado.

import { visit } from "unist-util-visit";
import { toHtml } from "hast-util-to-html";
import type { Root, Element, Text } from "hast";
import { parseFlowExample } from "./flow-example.ts";
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
  return (tree: Root) => {
    visit(tree, "element", (node: Element, index, parent) => {
      if (node.tagName !== "pre") return;
      if (node.properties?.dataLanguage !== "r") return;
      if (index === undefined || !parent) return;

      const code = collectText(node);
      if (!code.includes("tr_add(")) return;

      const graph = parseFlowExample(code);
      if (!graph) return;

      const canvasHtml = renderFlowCanvas(graph, visuals as Record<string, NodeVisual>);
      const originalPreHtml = toHtml(node, { allowDangerousHtml: true });

      const wrapperHtml = `<div class="flow-example" data-flow-example>
  <div role="tablist">
    <button type="button" role="tab" aria-selected="true" data-tab="canvas">Canvas</button>
    <button type="button" role="tab" aria-selected="false" data-tab="r">Código R</button>
  </div>
  <div role="tabpanel" data-panel="canvas">${canvasHtml}</div>
  <div role="tabpanel" data-panel="r" hidden>${originalPreHtml}</div>
</div>`;

      parent.children[index] = { type: "raw", value: wrapperHtml } as unknown as Element;
    });
  };
}

export default rehypeFlowExample;
