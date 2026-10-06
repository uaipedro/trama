// tests/js/percurso.test.mjs
import { test } from "node:test";
import assert from "node:assert/strict";
import { fontes, maisPerto, vizinho } from "../../inst/www/percurso.js";

const n = (id, x, y) => ({ id, x, y });
const e = (source, target) => ({ source, target });

// a -> b -> d ; a -> c ; c -> d (junção)
const nos = [n("a", 0, 100), n("b", 200, 0), n("c", 200, 200), n("d", 400, 100)];
const ar = [e("a", "c"), e("a", "b"), e("b", "d"), e("c", "d")];

test("→ vai ao filho mais acima", () => {
  assert.deepEqual(vizinho(nos, ar, "a", "right"), { id: "b", pai: "a" });
});

test("→ numa folha vai ao próximo irmão; sem irmão, sobe até achar um", () => {
  // folha d (veio de b): sem irmão adiante, mas b tem o irmão c
  assert.deepEqual(vizinho(nos, ar, "d", "right", "b"), { id: "c", pai: "a" });
  // veio de c: c é o último irmão e a é a única fonte, então acabou
  assert.equal(vizinho(nos, ar, "d", "right", "c"), null);
  // folha com irmão logo adiante
  const ns = [n("p", 0, 0), n("q", 100, 0), n("r", 100, 100), n("s", 200, 100)];
  const as = [e("p", "q"), e("p", "r"), e("r", "s")];
  assert.deepEqual(vizinho(ns, as, "q", "right", "p"), { id: "r", pai: "p" });
  // sem pai: a próxima fonte
  assert.deepEqual(vizinho([...ns, n("x", 0, 400)], as, "p", "right"), { id: "q", pai: "p" });
  assert.deepEqual(vizinho([n("a", 0, 0), n("x", 0, 50)], [], "a", "right"), { id: "x", pai: null });
  assert.equal(vizinho([n("a", 0, 0)], [], "a", "right"), null);
});

test("← volta ao pai de onde veio; sem histórico, ao mais acima", () => {
  assert.equal(vizinho(nos, ar, "d", "left").id, "b");
  assert.equal(vizinho(nos, ar, "d", "left", "c").id, "c");
  assert.equal(vizinho(nos, ar, "d", "left", "zzz").id, "b");
  assert.equal(vizinho(nos, ar, "a", "left"), null);
});

test("↑/↓ andam entre irmãos do mesmo pai, sem dar a volta", () => {
  assert.equal(vizinho(nos, ar, "b", "down").id, "c");
  assert.equal(vizinho(nos, ar, "c", "up").id, "b");
  assert.equal(vizinho(nos, ar, "b", "up"), null);
  assert.equal(vizinho(nos, ar, "c", "down"), null);
  // a junção: os irmãos de d são os filhos do pai por onde se chegou
  assert.equal(vizinho(nos, ar, "d", "down", "b"), null);
});

test("sem pai, ↑/↓ andam entre as fontes", () => {
  const dois = [...nos, n("x", 0, 400)];
  assert.deepEqual(fontes(dois, ar), ["a", "x"]);
  assert.equal(vizinho(dois, ar, "a", "down").id, "x");
  assert.equal(vizinho(dois, ar, "x", "up").id, "a");
});

test("o pai lembrado do destino é o que dá a volta certa", () => {
  const r = vizinho(nos, ar, "a", "right");           // a -> b
  const r2 = vizinho(nos, ar, r.id, "right", r.pai);  // b -> d, veio de b
  assert.deepEqual(r2, { id: "d", pai: "b" });
  assert.equal(vizinho(nos, ar, r2.id, "left", r2.pai).id, "b");
});

test("arestas soltas, repetidas, em laço ou com id inexistente são ignoradas", () => {
  const sujas = [...ar, e("a", "b"), e("b", "b"), e("a", "fantasma"), e("fantasma", "d")];
  assert.deepEqual(vizinho(nos, sujas, "a", "right"), { id: "b", pai: "a" });
  assert.equal(vizinho(nos, sujas, "fantasma", "right"), null);
  assert.equal(vizinho([], [], "a", "right"), null);
});

test("empate de altura desempata por x e depois por id", () => {
  const ns = [n("p", 0, 0), n("q", 100, 50), n("r", 100, 50), n("s", 50, 50)];
  const as = [e("p", "q"), e("p", "r"), e("p", "s")];
  assert.equal(vizinho(ns, as, "p", "right").id, "s");
  assert.equal(vizinho(ns, as, "s", "down", "p").id, "q");
  assert.equal(vizinho(ns, as, "q", "down", "p").id, "r");
});

test("bloco mais perto do centro", () => {
  assert.equal(maisPerto(nos, { x: 390, y: 90 }), "d");
  assert.equal(maisPerto([], { x: 0, y: 0 }), null);
});

test("o filho mais perto (coluna da esquerda) vem antes do mais alto, porém distante", () => {
  // q está mais alto mas 600 à direita; r está logo ali, um pouco abaixo
  const ns = [n("p", 0, 0), n("q", 600, 0), n("r", 300, 100), n("t", 300, 300)];
  const as = [e("p", "q"), e("p", "r"), e("p", "t")];
  assert.equal(vizinho(ns, as, "p", "right").id, "r");
  assert.equal(vizinho(ns, as, "r", "down", "p").id, "t");
  assert.equal(vizinho(ns, as, "t", "down", "p").id, "q");
  assert.equal(vizinho(ns, as, "q", "up", "p").id, "t");
});

test("x quase igual é a mesma coluna: manda a altura", () => {
  const ns = [n("p", 0, 0), n("q", 300, 200), n("r", 340, 0)];
  const as = [e("p", "q"), e("p", "r")];
  assert.equal(vizinho(ns, as, "p", "right").id, "r");
});
