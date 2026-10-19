import test from "node:test";
import assert from "node:assert/strict";
import { fontesPorPorta, nomesDasFontes } from "../../inst/www/fontes.js";

const nodes = [
  { id: "a", data: { label: "Tabela", nodeType: "data/read" } },
  { id: "b", data: { spec: { label: "Gráfico" }, nodeType: "view/scatter" } },
  { id: "p", data: { nodeType: "tst/juntar" } },
];

test("lista todas as origens de uma porta múltipla, na ordem do index", () => {
  const edges = [
    { source: "b", sourceHandle: "out", target: "p", targetHandle: "itens", data: { index: 2 } },
    { source: "a", sourceHandle: "out", target: "p", targetHandle: "itens", data: { index: 1 } },
  ];
  const f = fontesPorPorta(edges, nodes, (id) => ({ key: `k-${id}` }));
  assert.deepEqual(f.p.itens.map((x) => x.no), ["a", "b"]);
  assert.equal(f.p.itens[0].rotulo, "Tabela");
  assert.equal(f.p.itens[1].rotulo, "Gráfico");
  assert.equal(f.p.itens[1].tipo, "view/scatter");
  assert.equal(f.p.itens[0].handle.key, "k-a");
  assert.equal(f.p.itens[0].nome, "a");
});

test("o mesmo nó por duas saídas ganha o nome nó.saída, como no R", () => {
  assert.deepEqual(nomesDasFontes([{ no: "a", saida: "x" }, { no: "a", saida: "y" }, { no: "b", saida: "out" }]),
                   ["a.x", "a.y", "b"]);
});

test("sem arestas, nada", () => {
  assert.deepEqual(fontesPorPorta([], nodes), {});
});
