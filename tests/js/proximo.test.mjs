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

// Regressão: cadeia de Tab a partir de `npk_dados` em exemplos/experimentos.
// A coluna x=1300 tem cards de ponta a ponta do documento (npk, sp, cont,
// rep): `primeiroVao` descia ~7400 unidades até o fim do canvas e o bloco
// nascia longe da origem. `vaoAoLado` desiste da coluna quando o vão fica
// longe demais e tenta a coluna seguinte à direita, ainda na altura da origem.
test("vaoAoLado não desce a coluna inteira: passa pra coluna à direita", async () => {
  const { vaoAoLado } = await import("../../inst/www/proximo-foco.js");
  const q = { x: 1300, y: 8326, w: 280, h: 220 };
  // Coluna de cards colados de 8000 a 15000 cobrindo x 1300..1580.
  const coluna = Array.from({ length: 30 }, (_, i) => ({ x: 1300, y: 8000 + i * 240, w: 280, h: 230 }));
  const r = vaoAoLado(coluna, q, 30, { limite: 600, passoX: 300, colunas: 4 });
  assert.equal(r.y, 8326);          // mesma altura da origem
  assert.ok(r.x >= 1600);           // uma coluna à direita, livre
  // Sem obstáculo longe, é o próprio primeiroVao.
  const perto = [{ x: 1300, y: 8300, w: 280, h: 100 }];
  assert.deepEqual(vaoAoLado(perto, q, 30, { limite: 600 }), { x: 1300, y: 8430 });
  // Tudo bloqueado: fica com o vão mais raso entre as colunas tentadas.
  const parede = [0, 1, 2].flatMap((k) => coluna.map((c) => ({ ...c, x: 1300 + 300 * k })));
  const b = vaoAoLado(parede, q, 30, { limite: 600, passoX: 300, colunas: 3 });
  assert.ok(b.y >= q.y);
});
