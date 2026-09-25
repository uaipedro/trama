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

test("primeiroVao desce até caber, vaoPerto escolhe o lado mais perto", async () => {
  const { primeiroVao, vaoPerto } = await import("../../inst/www/proximo-foco.js");
  const q = { x: 0, y: 100, w: 100, h: 100 };
  assert.equal(primeiroVao([], q, 10), 100);
  const cx = [{ x: 0, y: 50, w: 100, h: 100 }];
  assert.equal(primeiroVao(cx, q, 10), 160);
  // subir custa 170 (50-100-10 = -60), descer 60: desce
  assert.equal(vaoPerto(cx, q, 10), 160);
  const alto = [{ x: 0, y: 90, w: 100, h: 500 }];
  // descer custa 510, subir 120: sobe
  assert.equal(vaoPerto(alto, q, 10), -20);
});
