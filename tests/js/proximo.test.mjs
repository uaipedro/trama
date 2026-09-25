import { test } from "node:test";
import assert from "node:assert/strict";
import { moverFoco } from "../../inst/www/proximo-foco.js";

test("direita e esquerda andam 1 e dão a volta", () => {
  assert.equal(moverFoco(0, 3, "ArrowRight"), 1);
  assert.equal(moverFoco(2, 3, "ArrowRight"), 0);
  assert.equal(moverFoco(0, 3, "ArrowLeft"), 2);
});

test("sem foco: avançar vai ao primeiro, voltar ao último", () => {
  assert.equal(moverFoco(-1, 4, "ArrowDown"), 0);
  assert.equal(moverFoco(-1, 4, "ArrowUp"), 3);
});

test("baixo e cima pulam colunas", () => {
  assert.equal(moverFoco(1, 10, "ArrowDown", 4), 5);
  assert.equal(moverFoco(8, 10, "ArrowDown", 4), 2);
  assert.equal(moverFoco(1, 10, "ArrowUp", 4), 7);
  assert.equal(moverFoco(1, 10, "ArrowDown"), 2);
});

test("lista vazia e tecla desconhecida", () => {
  assert.equal(moverFoco(0, 0, "ArrowDown"), -1);
  assert.equal(moverFoco(2, 5, "Enter"), 2);
  assert.equal(moverFoco(9, 5, "ArrowRight"), 0);
});
