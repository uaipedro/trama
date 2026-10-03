import { test } from "node:test";
import assert from "node:assert/strict";
import { dadosIguais, reusarNos } from "../../inst/www/reuso.js";

const f = () => {};

test("dadosIguais: um nível de objeto/array simples, o resto por identidade", () => {
  const h = { preview: 1 };
  assert.ok(dadosIguais({ a: 1, p: { x: 1 }, s: ["k"], h, f }, { a: 1, p: { x: 1 }, s: ["k"], h, f }));
  assert.ok(!dadosIguais({ p: { x: 1 } }, { p: { x: 2 } }));
  assert.ok(!dadosIguais({ s: ["k"] }, { s: ["k", "j"] }));
  assert.ok(!dadosIguais({ h: { preview: 1 } }, { h: { preview: 2 } }));
  // dois níveis abaixo é identidade
  assert.ok(!dadosIguais({ p: { x: { y: 1 } } }, { p: { x: { y: 1 } } }));
  assert.ok(!dadosIguais({ a: 1 }, { a: 1, b: undefined }));
  assert.ok(!dadosIguais({ s: [] }, { s: {} }));
  assert.ok(!dadosIguais({ f }, { f: () => {} }));
});

test("reusarNos devolve o nó antigo quando nada mudou", () => {
  const r1 = reusarNos(null, [{ id: "a", position: { x: 0 }, data: { v: 1, p: { k: 1 } } }]);
  const pos = r1.nos[0].position;
  const r2 = reusarNos(r1.mapa, [{ id: "a", position: pos, data: { v: 1, p: { k: 1 } } }]);
  assert.equal(r2.nos[0], r1.nos[0]);
});

test("reusarNos: posição nova, data antigo; data novo, nó novo", () => {
  const r1 = reusarNos(null, [{ id: "a", position: { x: 0 }, data: { v: 1 } }]);
  const r2 = reusarNos(r1.mapa, [{ id: "a", position: { x: 5 }, data: { v: 1 } }]);
  assert.notEqual(r2.nos[0], r1.nos[0]);
  assert.equal(r2.nos[0].data, r1.nos[0].data);
  assert.equal(r2.nos[0].position.x, 5);
  const r3 = reusarNos(r2.mapa, [{ id: "a", position: r2.nos[0].position, data: { v: 2 } }]);
  assert.equal(r3.nos[0].data.v, 2);
  assert.ok(!r3.mapa.has("b"));
});
