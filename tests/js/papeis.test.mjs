import { test } from "node:test";
import assert from "node:assert/strict";
import { corDaCategoria, tintaDaCategoria, COR_RESERVA } from "../../inst/www/papeis.js";

test("papel vira token do tema, e a cor fixa é ignorada", () => {
  const cat = { id: "fit", role: "ajuste", color: "#ff0000" };
  assert.equal(corDaCategoria(cat), "var(--tr-papel-ajuste)");
  assert.equal(tintaDaCategoria(cat), "var(--tr-papel-ajuste-ink)");
});

test("sem papel vale a cor da coleção e a tinta padrão", () => {
  assert.equal(corDaCategoria({ color: "#123456" }), "#123456");
  assert.equal(tintaDaCategoria({ color: "#123456" }), undefined);
});

test("papel desconhecido ou categoria ausente caem na reserva", () => {
  assert.equal(corDaCategoria({ role: "modelagem" }), COR_RESERVA);
  assert.equal(corDaCategoria(undefined), COR_RESERVA);
});

test("papel do bloco vence o da categoria", () => {
  const cat = { role: "ajuste" };
  assert.equal(corDaCategoria(cat, { role: "leitura" }), "var(--tr-papel-leitura)");
  assert.equal(tintaDaCategoria(cat, { role: "leitura" }), "var(--tr-papel-leitura-ink)");
  assert.equal(corDaCategoria(cat, {}), "var(--tr-papel-ajuste)");
});
