// tests/js/forest-models.test.mjs
import { test } from "node:test";
import assert from "node:assert/strict";
import { escalaForest } from "../../collections/trama.models/inst/trama/forest.js";

test("domínio inclui o zero, com folga de 5%", () => {
  const s = escalaForest([{ termo: "a", est: 2, li: 1, ls: 3 }], 110);
  // domínio [0, 3] + folga 0,15 → [-0,15; 3,15]
  assert.ok(Math.abs(s.zero - 5) < 1e-9);
  assert.ok(Math.abs(s.pontos[0].xls - 105) < 1e-9);
  assert.equal(s.pontos[0].cruza, false);
});

test("IC que cruza o zero é marcado; não finitos saem", () => {
  const s = escalaForest([{ est: 0.1, li: -1, ls: 1 }, { est: NaN, li: 0, ls: 1 }], 100);
  assert.equal(s.pontos.length, 1);
  assert.equal(s.pontos[0].cruza, true);
  assert.equal(escalaForest([], 100), null);
  assert.equal(escalaForest(undefined, 100), null);
});

test("tudo em zero não divide por zero", () => {
  const s = escalaForest([{ est: 0, li: 0, ls: 0 }], 100);
  assert.ok(Math.abs(s.zero - 50) < 1e-9);
});
