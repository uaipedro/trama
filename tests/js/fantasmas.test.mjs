import { test } from "node:test";
import assert from "node:assert/strict";
import { buscarFantasma, moverFantasma } from "../../inst/www/fantasmas.js";

const catalog = { nodes: [
  { id: "a/one", label: "Ler CSV", description: "Lê uma tabela" },
  { id: "b/two", label: "Resumo", description: "Resume tabela" },
  { id: "b/three", label: "Gráfico", description: "Visualiza tabela" },
  { id: "c/four", label: "Ajuste", description: "Ajusta modelo" },
] };
const ranked = catalog.nodes.map((n, i) => ({ id: n.id, score: 4 - i }));

test("busca mantém ranking, filtra label/descrição/id e limita a três", () => {
  assert.deepEqual(buscarFantasma(ranked, catalog).map((s) => s.id), ["a/one", "b/two", "b/three"]);
  assert.deepEqual(buscarFantasma(ranked, catalog, "VISUALIZA").map((s) => s.id), ["b/three"]);
  assert.deepEqual(buscarFantasma(ranked, catalog, "c/four").map((s) => s.id), ["c/four"]);
  assert.deepEqual(buscarFantasma(ranked, catalog, "inexistente"), []);
});

test("a lista só pode ranquear candidatos compatíveis recebidos", () => {
  const compatíveis = ranked.filter((s) => s.id !== "a/one");
  assert.deepEqual(buscarFantasma(compatíveis, catalog).map((s) => s.id), ["b/two", "b/three", "c/four"]);
});

test("setas percorrem os resultados e tratam lista vazia", () => {
  assert.equal(moverFantasma(0, 3, "ArrowDown"), 1);
  assert.equal(moverFantasma(2, 3, "ArrowRight"), 0);
  assert.equal(moverFantasma(0, 3, "ArrowUp"), 2);
  assert.equal(moverFantasma(-1, 3, "ArrowDown"), 0);
  assert.equal(moverFantasma(0, 0, "ArrowDown"), -1);
  assert.equal(moverFantasma(1, 3, "Enter"), 1);
});
