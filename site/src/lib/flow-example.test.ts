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
  const [g] = parseFlowExample(acf)!;
  assert.deepEqual(g.nodes.map((n) => n.id), ["pax", "log", "d", "acf"]);
  assert.equal(g.nodes[3].type, "series/acf");
  assert.deepEqual(g.nodes[3].params, [["defasagens", "36"]]);
  assert.deepEqual(g.nodes[2].params, [["tipo", "sazonal"]]);
  assert.deepEqual(g.edges, [["pax", "log"], ["log", "d"], ["d", "acf"]]);
});

test("from com vários pais", () => {
  const [g] = parseFlowExample(`tr_add("a","t/x") |> tr_add("b","t/x") |> tr_add("j","t/join", by = c("id","ano"), from = c("a", "b"))`)!;
  assert.deepEqual(g.edges, [["a", "j"], ["b", "j"]]);
  assert.deepEqual(g.nodes[2].params, [["by", "id, ano"]]);
});

test("sem tr_add devolve null", () => {
  assert.equal(parseFlowExample(`x <- 1`), null);
});

test("layout em colunas por profundidade", () => {
  const [g] = parseFlowExample(`tr_add("a","t/x") |> tr_add("b","t/x", from="a") |> tr_add("c","t/x", from="a")`)!;
  const pos = layoutFlow(g);
  assert.deepEqual(pos.get("a"), { col: 0, row: 0 });
  assert.deepEqual(pos.get("b"), { col: 1, row: 0 });
  assert.deepEqual(pos.get("c"), { col: 1, row: 1 });
});

test("canvas: um card por nó, aresta por par, escapa texto", () => {
  const [g] = parseFlowExample(`tr_add("a","t/x", q = "<b>") |> tr_add("b","t/y", from = "a")`)!;
  const html = renderFlowCanvas(g, {});
  assert.equal(html.match(/class="canvas-card"/g)?.length, 2);
  assert.equal(html.match(/<path /g)?.length, 1);
  assert.ok(html.includes("&lt;b&gt;") && !html.includes("<b>"));
});

// --- Item 1: vários tr_flow(...) independentes viram uma lista de grafos ---

test("vários tr_flow independentes viram grafos separados, mesmo com ids repetidos entre pipelines", () => {
  const code = `
tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("f", "series/fisher", from = "pax")

tr_flow(reg) |>
  tr_add("nilo", "series/example", dataset = "Nile") |>
  tr_add("f", "series/fisher", from = "nilo")
`;
  const graphs = parseFlowExample(code)!;
  assert.equal(graphs.length, 2);
  assert.deepEqual(graphs[0].nodes.map((n) => n.id), ["pax", "f"]);
  assert.deepEqual(graphs[1].nodes.map((n) => n.id), ["nilo", "f"]);
  assert.deepEqual(graphs[0].edges, [["pax", "f"]]);
  assert.deepEqual(graphs[1].edges, [["nilo", "f"]]);
});

test("sem tr_flow, tudo é um único grafo (compatibilidade)", () => {
  const graphs = parseFlowExample(`tr_add("a","t/x") |> tr_add("b","t/x", from = "a")`)!;
  assert.equal(graphs.length, 1);
  assert.deepEqual(graphs[0].nodes.map((n) => n.id), ["a", "b"]);
});

test("id repetido dentro de UM grafo invalida tudo (fallback pre)", () => {
  const code = `tr_flow(reg) |> tr_add("a","t/x") |> tr_add("a","t/y", from = "a")`;
  assert.equal(parseFlowExample(code), null);
});

// --- Item 5: ignora tr_add(...) dentro de comentários # ---

test("ignora tr_add dentro de comentário de linha", () => {
  const code = `
tr_flow(reg) |>
  tr_add("a", "t/x") |>
  # tr_add("fantasma", "t/y", from = "a") |>
  tr_add("b", "t/x", from = "a")
`;
  const [g] = parseFlowExample(code)!;
  assert.deepEqual(g.nodes.map((n) => n.id), ["a", "b"]);
});

test("bloco só com tr_add comentado devolve null", () => {
  const code = `# tr_add("a", "t/x")`;
  assert.equal(parseFlowExample(code), null);
});

// --- Item 6: id=/type= nomeados nas duas primeiras posições ---

test("aceita id= e type= nomeados", () => {
  const [g] = parseFlowExample(`tr_add(id = "a", type = "t/x") |> tr_add(type = "t/y", id = "b", from = "a")`)!;
  assert.deepEqual(g.nodes.map((n) => n.id), ["a", "b"]);
  assert.deepEqual(g.nodes.map((n) => n.type), ["t/x", "t/y"]);
});

test("aceita mistura de posicional e nomeado", () => {
  const [g] = parseFlowExample(`tr_add("a", type = "t/x") |> tr_add(id = "b", "t/y", from = "a")`)!;
  assert.deepEqual(g.nodes.map((n) => n.id), ["a", "b"]);
  assert.deepEqual(g.nodes.map((n) => n.type), ["t/x", "t/y"]);
});

test("sem id/type determinável devolve null", () => {
  const code = `tr_add(from = "x", outro = "y")`;
  assert.equal(parseFlowExample(code), null);
});

// --- Item 7: from apontando para id inexistente ---

test("from para id inexistente invalida o grafo (fallback pre)", () => {
  const code = `tr_add("a", "t/x", from = "fantasma")`;
  assert.equal(parseFlowExample(code), null);
});
