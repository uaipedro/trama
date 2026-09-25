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
  assert.equal(html.match(/class="tr-node /g)?.length, graph.nodes.length);
});

test("card do template ignora as posições do documento: layout automático em camadas", () => {
  const graph = templateToGraph({
    nodes: { a: { type: "x/a" }, b: { type: "x/b" } },
    edges: [{ from: { node: "a", port: "teste" }, to: { node: "b", port: "dados" } }],
    ui: { positions: { a: [0, 0], b: [360, 0] } },
  });
  assert.deepEqual(graph.edges, [["a", "b", "teste", "dados"]]);
  const html = renderFlowCanvas(graph, {});
  const H = renderFlowCanvas(graph, {}, { direcao: "H" });
  const V = renderFlowCanvas(graph, {}, { direcao: "V" });
  const pos = (h: string) => [...h.matchAll(/left:(\d+)px;top:(\d+)px/g)].map((m) => [+m[1], +m[2]]);
  const [ha, hb] = pos(H), [va, vb] = pos(V);
  assert.ok(ha[1] === hb[1] && hb[0] > ha[0], "H: lado a lado");
  assert.ok(va[0] === vb[0] && vb[1] > va[1], "V: um acima do outro");
  assert.ok(html.includes("tr-node-mini"));
  assert.ok(html.includes('class="tr-estatico '));
});
