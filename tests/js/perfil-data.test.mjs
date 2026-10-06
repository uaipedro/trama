// tests/js/perfil-data.test.mjs
import { test } from "node:test";
import assert from "node:assert/strict";
import { colunasOcultas, deltaTabela } from "../../collections/trama.data/inst/trama/perfil.js";

test("colunasOcultas", () => {
  assert.equal(colunasOcultas(50, 30), 20);
  assert.equal(colunasOcultas(3, 3), 0);
  assert.equal(colunasOcultas(undefined, 0), 0);
});

const hd = (linhas, colunas) => ({ summary: { linhas, colunas } });

test("deltaTabela: filtro e mutate", () => {
  const d = deltaTabela(hd(988, 5), { dados: hd(1000, 3) });
  assert.deepEqual(d.map((x) => x.texto), ["−12 linhas", "+2 col"]);
  assert.equal(d[0].tipo, "menos");
  assert.match(d[0].titulo, /1\.000 → 988 linhas \(−1%\)/);
});

test("deltaTabela: sem mudança, duas entradas ou sem dimensões", () => {
  assert.deepEqual(deltaTabela(hd(10, 3), { a: hd(10, 3) }), []);
  assert.deepEqual(deltaTabela(hd(10, 3), { a: hd(5, 3), b: hd(5, 3) }), []);
  assert.deepEqual(deltaTabela(hd(10, 3), {}), []);
  assert.deepEqual(deltaTabela(hd(10, 3), undefined), []);
  assert.deepEqual(deltaTabela(hd(10, 3), { a: { preview: { data: {} } } }), []);
  assert.deepEqual(deltaTabela(hd(1, 3), { a: { preview: { data: { nrow: 2, ncol: 3 } } } })
    .map((x) => x.texto), ["−1 linha"]);
});
