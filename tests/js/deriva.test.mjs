// tests/js/deriva.test.mjs
import { test } from "node:test";
import assert from "node:assert/strict";
import { derivas, aceitos, pendentes, opAceitar, versoes, titulo } from "../../inst/www/deriva.js";

const problems = [
  { kind: "version_drift", node: "niveis", type: "series/from_table_mts", from: 1, to: 2, aceitavel: true },
  { kind: "unknown_param", node: "x", param: "y" },
  { kind: "version_drift", node: "novo", type: "t/a", from: 5, to: 2, aceitavel: false },
  { kind: "version_drift", node: "var1", type: "series/var", from: 1, to: 3 },
];

test("derivas pega só version_drift, com aceitável", () => {
  const d = derivas(problems);
  assert.deepEqual(d.map((x) => x.node), ["niveis", "novo", "var1"]);
  assert.deepEqual(d.map((x) => x.aceitavel), [true, false, true]);
  assert.deepEqual(derivas(undefined), []);
});

test("aceitos lê lista, texto único e batch", () => {
  assert.deepEqual(aceitos({ op: "accept_version", nodes: ["a", "b"] }), ["a", "b"]);
  assert.deepEqual(aceitos({ op: "accept_version", nodes: "a" }), ["a"]);
  assert.deepEqual(aceitos({ op: "batch", ops: [{ op: "move" }, { op: "accept_version", nodes: "c" }] }), ["c"]);
  assert.deepEqual(aceitos({ op: "set_param", node: "a" }), []);
  assert.deepEqual(aceitos(null), []);
});

test("pendentes tira os aceitos e os nós que saíram do fluxo", () => {
  const d = derivas(problems);
  assert.deepEqual(pendentes(d, { op: "accept_version", nodes: "niveis" }).map((x) => x.node), ["novo", "var1"]);
  assert.deepEqual(pendentes(d, null, (id) => id !== "var1").map((x) => x.node), ["niveis", "novo"]);
  // Op de outra coisa não mexe no aviso: ele nunca some sozinho.
  assert.equal(pendentes(d, { op: "set_param", node: "niveis" }).length, 3);
});

test("opAceitar manda só os aceitáveis, e nada quando não há", () => {
  assert.deepEqual(opAceitar(derivas(problems)), { op: "accept_version", nodes: ["niveis", "var1"] });
  assert.equal(opAceitar(derivas(problems).filter((x) => !x.aceitavel)), null);
  assert.equal(opAceitar([]), null);
});

test("textos do aviso", () => {
  assert.equal(versoes({ from: 1, to: 2 }), "v1 → v2");
  assert.equal(versoes({ from: null, to: 2 }), "v2");
  assert.match(titulo([{}]), /^1 bloco tem versão nova/);
  assert.match(titulo([{}, {}]), /^2 blocos têm versão nova/);
});
