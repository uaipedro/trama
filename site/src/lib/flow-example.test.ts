import { test } from "node:test";
import assert from "node:assert/strict";
import { parseFlowExample, layoutFlow } from "./flow-example.ts";
import { renderFlowCanvas } from "./flow-canvas-html.ts";

const acf = `tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("log", "series/transform", from = "pax") |>
  tr_add("d", "series/diff", tipo = "sazonal", from = "log") |>
  tr_add("acf", "series/acf", defasagens = 36L, from = "d")`;

test("lê nós, tipos, params e from", () => {
  const g = parseFlowExample(acf)!;
  assert.deepEqual(g.nodes.map((n) => n.id), ["pax", "log", "d", "acf"]);
  assert.equal(g.nodes[3].type, "series/acf");
  assert.deepEqual(g.nodes[3].params, [["defasagens", "36"]]);
  assert.deepEqual(g.nodes[2].params, [["tipo", "sazonal"]]);
  assert.deepEqual(g.edges, [["pax", "log"], ["log", "d"], ["d", "acf"]]);
});

test("from com vários pais", () => {
  const g = parseFlowExample(`tr_add("a","t/x") |> tr_add("b","t/x") |> tr_add("j","t/join", by = c("id","ano"), from = c("a", "b"))`)!;
  assert.deepEqual(g.edges, [["a", "j"], ["b", "j"]]);
  assert.deepEqual(g.nodes[2].params, [["by", "id, ano"]]);
});

test("sem tr_add devolve null", () => {
  assert.equal(parseFlowExample(`x <- 1`), null);
});

test("layout em colunas por profundidade", () => {
  const g = parseFlowExample(`tr_add("a","t/x") |> tr_add("b","t/x", from="a") |> tr_add("c","t/x", from="a")`)!;
  const pos = layoutFlow(g);
  assert.deepEqual(pos.get("a"), { col: 0, row: 0 });
  assert.deepEqual(pos.get("b"), { col: 1, row: 0 });
  assert.deepEqual(pos.get("c"), { col: 1, row: 1 });
});

test("canvas: um card por nó, aresta por par, escapa texto", () => {
  const g = parseFlowExample(`tr_add("a","t/x", q = "<b>") |> tr_add("b","t/y", from = "a")`)!;
  const html = renderFlowCanvas(g, {});
  assert.equal(html.match(/class="canvas-card"/g)?.length, 2);
  assert.equal(html.match(/<path /g)?.length, 1);
  assert.ok(html.includes("&lt;b&gt;") && !html.includes("<b>"));
});
