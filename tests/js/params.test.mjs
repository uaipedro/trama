// tests/js/params.test.mjs
import { test } from "node:test";
import assert from "node:assert/strict";
import { layoutEnum, validarNumero, milhar, contagemDoPasso } from "../../inst/www/params.js";

test("proporção cabe ao lado do rótulo", () => {
  assert.equal(layoutEnum(["16:9", "4:3", "1:1", "3:4", "2:1"]), "inline");
});
test("opções médias descem para linha própria", () => {
  assert.equal(layoutEnum(["média", "ingênuo", "ingênuo sazonal", "deriva"]), "wide");
  assert.equal(layoutEnum(["soma_zero", "categoria_base"]), "wide");
  assert.equal(layoutEnum(["aditiva", "multiplicativa"]), "wide");
});
test("booleano disfarçado cabe ao lado", () => {
  assert.equal(layoutEnum(["auto", "sim", "não"]), "inline");
});
test("lista longa vira dropdown", () => {
  assert.equal(layoutEnum(Array.from({ length: 40 }, (_, i) => `serie${i}`)), "select");
  assert.equal(layoutEnum(["a", "b", "c", "d", "e", "f", "g"]), "select");
});
test("lista vazia ou ausente não quebra", () => {
  assert.equal(layoutEnum([]), "select");
  assert.equal(layoutEnum(undefined), "select");
});
test("número válido", () => {
  assert.deepEqual(validarNumero({ kind: "number" }, "2.5"), { ok: true, valor: 2.5 });
  assert.deepEqual(validarNumero({ kind: "number" }, " 2,5 "), { ok: true, valor: 2.5 });
});
test("vazio e texto são erro", () => {
  assert.equal(validarNumero({ kind: "number" }, "").erro, "obrigatório");
  assert.equal(validarNumero({ kind: "number" }, "abc").erro, "precisa ser número");
});
test("inteiro recusa fração", () => {
  assert.equal(validarNumero({ kind: "integer" }, "2.5").erro, "precisa ser inteiro");
  assert.deepEqual(validarNumero({ kind: "integer" }, "3"), { ok: true, valor: 3 });
});
test("limites", () => {
  assert.equal(validarNumero({ kind: "integer", min: 2 }, "1").erro, "mínimo 2");
  assert.equal(validarNumero({ kind: "integer", max: 200 }, "201").erro, "máximo 200");
  assert.deepEqual(validarNumero({ kind: "integer", min: 2, max: 200 }, "2"), { ok: true, valor: 2 });
});
test("min/max nulos são ignorados", () => {
  assert.equal(validarNumero({ kind: "number", min: null, max: null }, "-5").ok, true);
});

test("milhar pontua de três em três, à brasileira", () => {
  assert.equal(milhar(10000), "10.000");
  assert.equal(milhar(500), "500");
  assert.equal(milhar(1234567), "1.234.567");
  assert.equal(milhar(0), "0");
  assert.equal(milhar(-2500), "-2.500");
});

test("contagemDoPasso lê a mensagem que o driver publica", () => {
  // A frase exata de `ctx$progress()` em R/stream-driver.R: sprintf("ponto
  // %d de %d", i, n). Mudar essa frase sem tocar aqui é o modo de falha que
  // este teste existe pra travar — ver o contrato em test-stream-transport.R.
  assert.equal(contagemDoPasso("ponto 500 de 10000"), "passo 500 / 10.000");
  assert.equal(contagemDoPasso("ponto 1 de 3"), "passo 1 / 3");
});

test("contagemDoPasso sem número reconhecível não inventa passo", () => {
  assert.equal(contagemDoPasso("computando…"), null);
  assert.equal(contagemDoPasso(null), null);
  assert.equal(contagemDoPasso(undefined), null);
});
