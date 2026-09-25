import { test } from "node:test";
import assert from "node:assert/strict";
import { readFileSync, existsSync } from "node:fs";
import { templateToGraph } from "./template-graph.ts";
import { renderFlowCanvas } from "./flow-canvas-html.ts";

const indice: { pkg: string; arquivo: string; nome: string }[] = JSON.parse(
  readFileSync(new URL("../data/templates.json", import.meta.url), "utf8")
);

test("índice de templates não está vazio", () => {
  assert.ok(indice.length > 0);
});

test("cada entrada do índice aponta para um template publicado", () => {
  for (const { pkg, arquivo, nome } of indice) {
    const url = new URL(`../../public/templates/${pkg}/${arquivo}`, import.meta.url);
    assert.ok(existsSync(url), `${pkg}/${arquivo} ausente`);
    const tpl = JSON.parse(readFileSync(url, "utf8"));
    assert.equal(tpl.trama, "template", `${pkg}/${arquivo}`);
    assert.equal(tpl.nome, nome);
  }
});

test("documento do template vira canvas com um card por nó", () => {
  const { pkg, arquivo } = indice[0];
  const tpl = JSON.parse(readFileSync(new URL(`../../public/templates/${pkg}/${arquivo}`, import.meta.url), "utf8"));
  const graph = templateToGraph(tpl.doc);
  assert.equal(graph.nodes.length, Object.keys(tpl.doc.nodes).length);
  const html = renderFlowCanvas(graph, {});
  assert.equal(html.match(/class="canvas-card"/g)?.length, graph.nodes.length);
});
