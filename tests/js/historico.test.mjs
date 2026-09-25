import { test } from "node:test";
import assert from "node:assert/strict";
import { lerHistorico, registrar } from "../../inst/www/historico.js";

const memoria = () => {
  const m = new Map();
  return { getItem: (k) => m.get(k) ?? null, setItem: (k, v) => m.set(k, String(v)) };
};

test("registrar duas vezes conta 2 e persiste no store", () => {
  const s = memoria();
  registrar("d/ler", "d/resumo", s);
  registrar("d/ler", "d/resumo", s);
  assert.deepEqual(lerHistorico(s), { "d/ler>d/resumo": 2 });
});

test("store que lança exceção não quebra", () => {
  const ruim = { getItem() { throw new Error("x"); }, setItem() { throw new Error("x"); } };
  assert.deepEqual(lerHistorico(ruim), {});
  assert.deepEqual(registrar("a/b", "c/d", ruim), { "a/b>c/d": 1 });
  assert.deepEqual(lerHistorico(undefined), {});
});

test("teto de 500 pares descarta os de menor contagem", () => {
  const s = memoria();
  const h = {};
  for (let i = 0; i < 500; i++) h[`a/x>b/${i}`] = 2;
  s.setItem("trama:proximo:v1", JSON.stringify(h));
  registrar("a/x", "b/novo", s);
  const r = lerHistorico(s);
  assert.equal(Object.keys(r).length, 500);
  assert.ok(!("a/x>b/novo" in r));
});
