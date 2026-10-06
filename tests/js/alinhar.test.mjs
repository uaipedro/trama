// tests/js/alinhar.test.mjs
import { test } from "node:test";
import assert from "node:assert/strict";
import { empilhar, alinhar, alinharA, distribuir, expandirGrupos, gruposDe, grupoDe, GAP_PADRAO }
  from "../../inst/www/alinhar.js";

const r = (id, x, y, w, h) => ({ id, x, y, w, h });
// aplica o resultado de volta, pra encadear passos como o editor faz
const aplicar = (rs, mv) => rs.map((q) => ({ ...q, ...(mv.find((m) => m.id === q.id) || {}) }));

test("empilhar na vertical: tamanhos diferentes, gap padrão, centralizado, ordem pela posição", () => {
  const rs = [r("b", 300, 400, 100, 50), r("a", 0, 0, 200, 80), r("c", 10, 900, 300, 120)];
  const mv = empilhar(rs, "v");
  assert.deepEqual(mv.map((m) => m.id), ["a", "b", "c"]);
  const por = Object.fromEntries(mv.map((m) => [m.id, m]));
  assert.equal(por.a.y, 0);
  assert.equal(por.b.y, 80 + GAP_PADRAO);
  assert.equal(por.c.y, 80 + GAP_PADRAO + 50 + GAP_PADRAO);
  // caixa em x: 0..400 -> centro 200
  assert.equal(por.a.x, 100); assert.equal(por.b.x, 150); assert.equal(por.c.x, 50);
});

test("empilhar na horizontal usa a largura medida e centraliza na vertical", () => {
  const rs = [r("a", 0, 0, 100, 40), r("b", 500, 100, 60, 200)];
  const mv = empilhar(rs, "h", 10);
  const por = Object.fromEntries(mv.map((m) => [m.id, m]));
  assert.equal(por.a.x, 0); assert.equal(por.b.x, 110);
  // caixa em y: 0..300 -> centro 150
  assert.equal(por.a.y, 130); assert.equal(por.b.y, 50);
});

test("menos de 2 itens: nada acontece", () => {
  assert.deepEqual(empilhar([r("a", 0, 0, 1, 1)], "v"), []);
  assert.deepEqual(alinhar([r("a", 0, 0, 1, 1)], "left"), []);
  assert.deepEqual(distribuir([r("a", 0, 0, 1, 1), r("b", 5, 5, 1, 1)], "h"), []);
});

test("alinhar: ciclo início <-> centro <-> fim no eixo x, sem mexer em y", () => {
  let rs = [r("a", 0, 0, 100, 10), r("b", 50, 70, 200, 10)]; // caixa x: 0..250
  // sem estado: seta para a esquerda vai à borda esquerda
  let mv = alinhar(rs, "left");
  rs = aplicar(rs, mv);
  assert.deepEqual(rs.map((q) => q.x), [0, 0]);
  assert.deepEqual(rs.map((q) => q.y), [0, 70]);
  // no início: esquerda não faz nada; direita vai ao centro
  assert.deepEqual(alinhar(rs, "left"), []);
  rs = aplicar(rs, alinhar(rs, "right"));
  assert.deepEqual(rs.map((q) => q.x), [50, 0]); // caixa 0..200, centro 100
  // do centro: direita vai ao fim; esquerda volta ao início
  const fimRs = aplicar(rs, alinhar(rs, "right"));
  assert.deepEqual(fimRs.map((q) => q.x), [100, 0]);
  assert.deepEqual(alinhar(fimRs, "right"), []);
  const volta = aplicar(rs, alinhar(rs, "left"));
  assert.deepEqual(volta.map((q) => q.x), [0, 0]);
});

test("alinhar no eixo y: do topo desce ao meio, ao fim, e volta", () => {
  let rs = [r("a", 0, 0, 10, 100), r("b", 0, 0, 10, 40)];
  rs = aplicar(rs, alinhar(rs, "down"));
  assert.deepEqual(rs.map((q) => q.y), [0, 30]);
  rs = aplicar(rs, alinhar(rs, "down"));
  assert.deepEqual(rs.map((q) => q.y), [0, 60]);
  rs = aplicar(rs, alinhar(rs, "up"));
  assert.deepEqual(rs.map((q) => q.y), [0, 30]);
  rs = aplicar(rs, alinhar(rs, "up"));
  assert.deepEqual(rs.map((q) => q.y), [0, 0]);
});

test("itens de mesma largura já estão em início, centro e fim: não há o que andar", () => {
  const rs = [r("a", 0, 0, 100, 10), r("b", 0, 50, 100, 10)];
  for (const d of ["left", "right"]) assert.deepEqual(alinhar(rs, d), []);
});

test("alinharA leva direto ao lado pedido", () => {
  const rs = [r("a", 0, 0, 100, 10), r("b", 50, 70, 200, 10)];
  assert.deepEqual(aplicar(rs, alinharA(rs, "dir")).map((q) => q.x + q.w), [250, 250]);
  assert.deepEqual(aplicar(rs, alinharA(rs, "base")).map((q) => q.y + q.h), [80, 80]);
  assert.deepEqual(alinharA(rs, "inexistente"), []);
});

test("distribuir iguala os vãos e mantém primeiro e último", () => {
  const rs = [r("a", 0, 0, 100, 10), r("b", 120, 0, 50, 10), r("c", 400, 0, 100, 10), r("d", 300, 0, 20, 10)];
  const out = aplicar(rs, distribuir(rs, "h"));
  const ord = [...out].sort((p, q) => p.x - q.x);
  assert.equal(ord[0].id, "a"); assert.equal(ord[0].x, 0);
  assert.equal(ord[ord.length - 1].id, "c"); assert.equal(ord[ord.length - 1].x, 400);
  const vaos = ord.slice(1).map((q, i) => q.x - (ord[i].x + ord[i].w));
  assert.ok(vaos.every((v) => Math.abs(v - vaos[0]) < 1e-9), JSON.stringify(vaos));
});

test("distribuir na vertical, com sobreposição (vão negativo) também iguala", () => {
  const rs = [r("a", 0, 0, 10, 100), r("b", 0, 10, 10, 100), r("c", 0, 150, 10, 100)];
  const out = aplicar(rs, distribuir(rs, "v"));
  assert.deepEqual(out.map((q) => q.y), [0, 75, 150]); // vão = (250 - 300) / 2 = -25
});

test("grupos: expandir seleção, ignorar membro morto, achar grupos tocados", () => {
  const grupos = { g1: ["a", "b", "x"], g2: ["c", "d"] };
  const vivos = new Set(["a", "b", "c", "d", "e"]);
  assert.deepEqual(expandirGrupos(["a"], grupos, vivos).sort(), ["a", "b"]);
  assert.deepEqual(expandirGrupos(["e"], grupos, vivos), ["e"]);
  assert.deepEqual(expandirGrupos(["a", "c"], grupos, vivos).sort(), ["a", "b", "c", "d"]);
  assert.equal(grupoDe(grupos, "d"), "g2");
  assert.deepEqual(gruposDe(["a", "b", "e"], grupos), ["g1"]);
  assert.deepEqual(gruposDe(["a", "c"], grupos).sort(), ["g1", "g2"]);
});
