// tests/js/perfil-data.test.mjs
import { test } from "node:test";
import assert from "node:assert/strict";
import { resumoColuna, fmtPct, colunasOcultas, deltaTabela } from "../../collections/trama.data/inst/trama/perfil.js";

test("numérica vira barras normalizadas e faixa", () => {
  const r = resumoColuna({ tipo: "num", na: 0.1, hist: [1, 2, 4, 0], min: 1, max: 9.5 });
  assert.deepEqual(r.barras, [0.25, 0.5, 1, 0]);
  assert.equal(r.naTexto, "10% NA");
  assert.equal(r.faixa, "1 – 9.50");
});

test("categórica vira top e conta o resto", () => {
  const r = resumoColuna({ tipo: "fct", na: 0, top: [{ nivel: "a", prop: 0.4 }], niveis: 4 });
  assert.deepEqual(r.top, [{ nivel: "a", prop: 0.4 }]);
  assert.equal(r.resto, 3);
  assert.equal(r.naTexto, "sem NA");
  assert.equal(r.barras, undefined);
});

test("sem perfil e coluna toda NA", () => {
  assert.equal(resumoColuna(null), null);
  const r = resumoColuna({ tipo: "data", na: 1 });
  assert.equal(r.naTexto, "100% NA");
  assert.equal(r.barras, undefined);
  assert.equal(r.top, undefined);
});

test("fmtPct e colunasOcultas", () => {
  assert.equal(fmtPct(0.004), "<1%");
  assert.equal(fmtPct(0), "0%");
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
