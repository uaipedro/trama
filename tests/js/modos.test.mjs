// tests/js/modos.test.mjs
import { test } from "node:test";
import assert from "node:assert/strict";
import { MODOS, modoDe, ehMini, paramsDobradosDe, paramVisivel, paramsVisiveis, tamanhoPedido,
         nomeDaTecla, frameMaisPerto, frameVizinho, ATALHOS, dica } from "../../inst/www/modos.js";

test("dois modos; os antigos abrem o card, e 'preview' dobra os parâmetros", () => {
  assert.deepEqual(MODOS, ["mini", "completo"]);
  assert.equal(modoDe({}), "completo");
  assert.equal(modoDe({ modo: "mini" }), "mini");
  assert.equal(modoDe({ modo: "solto" }), "solto");
  assert.equal(modoDe({ modo: "params" }), "completo");
  assert.equal(modoDe({ modo: "preview" }), "completo");
  assert.equal(modoDe({ modo: "xx" }), "completo");
  assert.deepEqual(["mini", "solto", "completo"].map(ehMini), [true, true, false]);
  assert.equal(paramsDobradosDe({ modo: "preview" }), true);
  assert.equal(paramsDobradosDe({ modo: "params" }), false);
});

test("when esconde parâmetro que não vale pro seletor, contando o default", () => {
  const spec = { params: [
    { name: "metodo", default: "a" },
    { name: "alfa", default: 0.05, when: { metodo: ["b", "c"] } },
    { name: "peso", default: 1, when: { usar: [true] } },
    { name: "usar", default: false },
  ] };
  assert.deepEqual(paramsVisiveis(spec, {}).map((p) => p.name), ["metodo", "usar"]);
  assert.deepEqual(paramsVisiveis(spec, { metodo: "c", usar: true }).map((p) => p.name),
                   ["metodo", "alfa", "peso", "usar"]);
  assert.equal(paramVisivel({ when: { n: [1] } }, { n: "1" }), true);
  assert.equal(paramVisivel({ when: { a: ["x"], b: ["y"] } }, { a: "x", b: "z" }), false);
  assert.equal(paramVisivel({}, {}), true);
});

test("tamanhoPedido só cresce, na grade, com teto, e cala quando cabe", () => {
  const base = { cardW: 240, prevH: 132, clientW: 222, clientH: 132 };
  assert.equal(tamanhoPedido({ ...base, scrollW: 225, scrollH: 134 }), null);
  assert.deepEqual(tamanhoPedido({ ...base, scrollW: 400, scrollH: 132 }), { w: 432, h: 132 });
  assert.deepEqual(tamanhoPedido({ ...base, scrollW: 222, scrollH: 300 }), { w: 240, h: 304 });
  assert.deepEqual(tamanhoPedido({ ...base, scrollW: 5000, scrollH: 5000 }), { w: 720, h: 520 });
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
  assert.equal(dica("modo-mini"), "Miniatura (A)");
  assert.equal(dica("nao-existe"), "");
  const ids = ATALHOS.map((a) => a.id);
  assert.equal(new Set(ids).size, ids.length);
});
