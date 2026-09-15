// tests/js/teste.test.mjs
import { test } from "node:test";
import assert from "node:assert/strict";
import { posicao, estrelas, faixa, venceu, num, numP, eixoEfeito } from "../../inst/www/teste.js";

test("a régua é logarítmica e satura em 0,0001", () => {
  assert.equal(posicao(1), 0);
  assert.equal(posicao(0.01), 0.5);
  assert.equal(posicao(1e-4), 1);
  assert.equal(posicao(1e-12), 1);
  assert.equal(posicao(0), 1);
  assert.equal(posicao(null), 0);
});

test("estrelas na convenção do summary, e faixa pela estrela", () => {
  assert.deepEqual([0.0005, 0.005, 0.03, 0.07, 0.4].map(estrelas), ["***", "**", "*", ".", "ns"]);
  assert.equal(estrelas(null), "");
  assert.equal(faixa("***"), 4);
  assert.equal(faixa("ns"), 0);
});

test("venceu: p-valor direto, crítico na cauda do sentido", () => {
  assert.equal(venceu({ p_valor: 0.03 }, "5%", 0.05), true);
  assert.equal(venceu({ p_valor: 0.03 }, "1%", 0.01), false);
  const adf = { p_valor: null, estatistica: -3.0, sentido: "menor", criticos: { "10%": -2.57, "5%": -2.88, "1%": -3.46 } };
  assert.equal(venceu(adf, "5%", 0.05), true);
  assert.equal(venceu(adf, "1%", 0.01), false);
  const kpss = { p_valor: null, estatistica: 0.5, sentido: "maior", criticos: { "10%": 0.347, "5%": 0.463, "1%": 0.739 } };
  assert.equal(venceu(kpss, "5%", 0.05), true);
  assert.equal(venceu(kpss, "1%", 0.01), false);
  assert.equal(venceu({ p_valor: null, estatistica: 1 }, "5%", 0.05), false);
});

test("o p-valor minúsculo não vira zero e sai em potência de dez", () => {
  assert.equal(numP(2.46e-12), "2,5 × 10⁻¹²");
  assert.equal(numP(0.0159), "0,016");
  assert.equal(numP(0), "< 10⁻³⁰⁰");
  assert.equal(num(0.0002), "2,0e-4");
});

test("efeito: referência 1 na razão, e o intervalo que cruza é marcado", () => {
  const d = eixoEfeito({ rotulo: "diferença de médias", valor: 3.7, li: -0.17, ls: 7.57 });
  assert.equal(d.ref, 0);
  assert.equal(d.cruza, true);
  const r = eixoEfeito({ rotulo: "razão de chances", valor: 4, li: 1.5, ls: 9 });
  assert.equal(r.ref, 1);
  assert.equal(r.cruza, false);
  assert.ok(r.x(4) > r.x(1));
});
