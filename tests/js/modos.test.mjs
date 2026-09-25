// tests/js/modos.test.mjs
import { test } from "node:test";
import assert from "node:assert/strict";
import { MODOS, modoDe, mostraPreview, mostraParams, precisaPainel, nomeDaTecla,
         frameMaisPerto, frameVizinho, ATALHOS, dica } from "../../inst/www/modos.js";

test("modoDe cai em 'completo' sem modo ou com valor desconhecido", () => {
  assert.equal(modoDe({}), "completo");
  assert.equal(modoDe({ modo: "mini" }), "mini");
  assert.equal(modoDe({ modo: "xx" }), "completo");
  assert.deepEqual(MODOS, ["mini", "params", "preview", "completo"]);
});

test("o que cada modo mostra", () => {
  assert.deepEqual(MODOS.map(mostraPreview), [false, false, true, true]);
  assert.deepEqual(MODOS.map(mostraParams), [false, true, false, true]);
  assert.deepEqual(MODOS.map(precisaPainel), [true, false, true, false]);
});

const ev = (key, o = {}) => ({ key, ctrlKey: false, metaKey: false, shiftKey: false, ...o });

test("nomeDaTecla monta mod+/shift+ e normaliza < > para , .", () => {
  assert.equal(nomeDaTecla(ev("W", { shiftKey: false })), "w");
  assert.equal(nomeDaTecla(ev("z", { ctrlKey: true })), "mod+z");
  assert.equal(nomeDaTecla(ev("F", { shiftKey: true })), "shift+f");
  assert.equal(nomeDaTecla(ev("<", { shiftKey: true })), ",");
  assert.equal(nomeDaTecla(ev(">", { shiftKey: true })), ".");
  assert.equal(nomeDaTecla(ev("<")), ",");
  assert.equal(nomeDaTecla(ev(".")), ".");
  assert.equal(nomeDaTecla(ev("+", { shiftKey: true })), "+");
  assert.equal(nomeDaTecla(ev("+")), "+");
});

const fr = (id, x, y) => ({ id, x, y, w: 100, h: 100 });
const frames = [fr("a", 0, 0), fr("b", 500, 0), fr("c", 1000, 0)];

test("frameMaisPerto usa o centro do frame", () => {
  assert.equal(frameMaisPerto(frames, { x: 560, y: 40 }), 1);
  assert.equal(frameMaisPerto(frames, { x: -900, y: 0 }), 0);
  assert.equal(frameMaisPerto([], { x: 0, y: 0 }), -1);
});

test("frameVizinho parte do atual, ou do mais perto sem atual, e para nas pontas", () => {
  assert.equal(frameVizinho(frames, "a", 1, { x: 0, y: 0 }), 1);
  assert.equal(frameVizinho(frames, "c", 1, { x: 0, y: 0 }), 2);
  assert.equal(frameVizinho(frames, "a", -1, { x: 0, y: 0 }), 0);
  assert.equal(frameVizinho(frames, null, 1, { x: 550, y: 50 }), 2);
  assert.equal(frameVizinho(frames, "sumiu", -1, { x: 1050, y: 50 }), 1);
  assert.equal(frameVizinho([], null, 1, { x: 0, y: 0 }), -1);
});

test("dica junta rótulo e teclas; ATALHOS tem ids únicos", () => {
  assert.equal(dica("modo-preview"), "Só preview (W)");
  assert.equal(dica("nao-existe"), "");
  const ids = ATALHOS.map((a) => a.id);
  assert.equal(new Set(ids).size, ids.length);
});
