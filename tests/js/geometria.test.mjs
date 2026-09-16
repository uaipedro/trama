// tests/js/geometria.test.mjs
import { test } from "node:test";
import assert from "node:assert/strict";
import { containedFrames, donos, envolver, unidades, crescerExterno, abrirEspaco, FRAME_PAD, gradeDeFrames, validarPrancheta,
         PRANCHETA_PADRAO, FRAME_HEAD, FRAME_COLORS, inside, marcaDaAgua } from "../../inst/www/geometria.js";

const frame = (id, x, y, w, hh) => ({ id, type: "trFrame", position: { x, y }, width: w, height: hh, data: {} });
const card = (id, x, y) => ({ id, type: "ndNode", position: { x, y }, measured: { width: 240, height: 190 }, data: {} });

test("containedFrames devolve só frames inteiros, nunca o próprio", () => {
  const ext = frame("E", 0, 0, 1000, 1000);
  const ns = [ext, frame("A", 10, 10, 100, 100), frame("B", 950, 950, 100, 100), card("c", 20, 20)];
  assert.deepEqual(containedFrames(ext, ns), ["A"]);
});

test("envolver abre espaço pro cabeçalho e respeita a proporção", () => {
  const r = envolver({ x: 0, y: 0, width: 1600, height: 900 }, "16:9", 100);
  assert.ok(r.y <= -(FRAME_HEAD + 100));
  assert.ok(Math.abs(r.w / r.h - 16 / 9) < 0.01);
  assert.ok(inside({ x: 0, y: 0, w: 1600, h: 900 }, r));
});

test("envolver em livre só soma folga e cabeçalho", () => {
  const r = envolver({ x: 0, y: 0, width: 100, height: 100 }, "livre", 10);
  assert.deepEqual(r, { x: -10, y: -10 - FRAME_HEAD, w: 120, h: 120 + FRAME_HEAD });
});

test("grade 2x3 16:9 com externo: externo primeiro, células linha a linha, centrada", () => {
  const o = { ...PRANCHETA_PADRAO, linhas: 2, colunas: 3, largura: 1600, espaco: 200 };
  const fs = gradeDeFrames(o, { x: 0, y: 0 }, 0);
  assert.equal(fs.length, 7);
  assert.equal(fs[0].title, "Prancheta");
  assert.equal(fs[0].color, "cinza");
  const cel = fs.slice(1);
  assert.deepEqual(cel.map((f) => f.title), ["Frame 2", "Frame 3", "Frame 4", "Frame 5", "Frame 6", "Frame 7"]);
  assert.ok(cel.every((f) => f.w === 1600 && f.h === 900 && f.aspect === "16:9"));
  assert.equal(cel[1].x - cel[0].x, 1800);
  assert.equal(cel[3].y - cel[0].y, 1100);
  assert.equal(cel[3].x, cel[0].x);
  const e = fs[0];
  assert.ok(Math.abs(e.x + e.w / 2) <= 1 && Math.abs(e.y + e.h / 2) <= 1);
  cel.forEach((f) => assert.ok(inside(f, e)));
});

test("sem externo, a grade é que fica centrada, e a numeração continua a existente", () => {
  const o = { ...PRANCHETA_PADRAO, linhas: 1, colunas: 2, largura: 1000, espaco: 100, externo: false };
  const fs = gradeDeFrames(o, { x: 500, y: 500 }, 4);
  assert.deepEqual(fs.map((f) => f.title), ["Frame 5", "Frame 6"]);
  const x0 = fs[0].x, x1 = fs[1].x + fs[1].w;
  assert.ok(Math.abs((x0 + x1) / 2 - 500) <= 1);
});

test("cores: rodízio pula o cinza; cor fixa pinta todas as células", () => {
  const rod = gradeDeFrames({ ...PRANCHETA_PADRAO, linhas: 1, colunas: 7, externo: false }, { x: 0, y: 0 }, 0);
  const semCinza = FRAME_COLORS.filter((c) => c !== "cinza");
  assert.deepEqual(rod.map((f) => f.color), [...semCinza, semCinza[0]]);
  const fixa = gradeDeFrames({ ...PRANCHETA_PADRAO, cor: "rosa" }, { x: 0, y: 0 }, 0);
  assert.ok(fixa.slice(1).every((f) => f.color === "rosa"));
  assert.equal(fixa[0].color, "cinza");
});

test("livre usa a altura digitada; tudo sai inteiro", () => {
  const fs = gradeDeFrames({ ...PRANCHETA_PADRAO, aspect: "livre", largura: 333.4, altura: 250.6,
                             linhas: 1, colunas: 1 }, { x: 0.5, y: 0.5 }, 0);
  assert.equal(fs[1].h, 251);
  assert.ok(fs.every((f) => [f.x, f.y, f.w, f.h].every(Number.isInteger)));
});

test("validarPrancheta aponta o campo e aceita o padrão", () => {
  assert.deepEqual(validarPrancheta(PRANCHETA_PADRAO), {});
  const e = validarPrancheta({ ...PRANCHETA_PADRAO, linhas: "0", largura: "abc", espaco: "-1" });
  assert.deepEqual(Object.keys(e).sort(), ["espaco", "largura", "linhas"]);
  assert.deepEqual(validarPrancheta({ ...PRANCHETA_PADRAO, colunas: "2,5" }), { colunas: "precisa ser inteiro" });
  // Altura só vale em livre: inválida num 16:9 não trava o Criar.
  assert.deepEqual(validarPrancheta({ ...PRANCHETA_PADRAO, altura: "" }), {});
  assert.ok("altura" in validarPrancheta({ ...PRANCHETA_PADRAO, aspect: "livre", altura: "" }));
});

const fr = (id, x, y, w, hh, order) => ({ ...frame(id, x, y, w, hh), data: { order } });

test("donos: o frame mais interno fica com o card; quem contém frame é externo", () => {
  const ns = [fr("E", 0, 0, 3000, 2000, 1), fr("A", 100, 100, 1000, 800, 2), fr("B", 1500, 100, 1000, 800, 3),
              card("a", 200, 200), card("b", 1600, 200), card("solto", 100, 1500)];
  const r = donos(ns);
  assert.deepEqual(r.externos, ["E"]);
  assert.deepEqual(r.dono, { a: "A", b: "B" });
});

test("donos: sem aninhamento é a regra antiga (sobrepostos, vence a ordem)", () => {
  const ns = [fr("X", 0, 0, 1000, 1000, 2), fr("Y", 0, 0, 1000, 1000, 1), card("c", 10, 10)];
  const r = donos(ns);
  assert.deepEqual(r.externos, []);
  assert.deepEqual(r.dono, { c: "Y" });
});

test("unidades: o externo de cima leva células, cards, soltos e o externo aninhado", () => {
  const ns = [fr("E", 0, 0, 6000, 4000, 1), fr("A", 100, 100, 1000, 800, 2), fr("B", 1500, 100, 1000, 800, 3),
              card("a", 200, 200), card("b", 1600, 200), card("solto", 100, 1500),
              fr("E2", 3000, 2000, 2500, 1800, 4), fr("C", 3100, 2100, 1000, 800, 5), card("c", 3200, 2200),
              fr("F", 7000, 0, 1000, 800, 6), card("f", 7100, 100), card("fora", 9000, 9000)];
  const r = unidades(ns);
  assert.equal(r.unidades.length, 1);
  assert.equal(r.unidades[0].id, "E");
  assert.deepEqual([...r.unidades[0].membros.frames].sort(), ["A", "B", "C", "E2"]);
  assert.deepEqual([...r.unidades[0].membros.cards].sort(), ["a", "b", "c", "solto"]);
  assert.deepEqual(Object.keys(r.unidadeDe).sort(), ["A", "B", "C", "E2", "a", "b", "c", "solto"]);
  assert.ok(Object.values(r.unidadeDe).every((u) => u === "E"));
});

test("unidades: sem aninhamento não há unidade", () => {
  const ns = [fr("X", 0, 0, 1000, 1000, 1), card("c", 10, 10)];
  assert.deepEqual(unidades(ns), { unidades: [], unidadeDe: {} });
});

test("crescerExterno: membros dentro, o retângulo fica como está", () => {
  const atual = { x: 0, y: 0, w: 1600, h: 900 };
  const r = crescerExterno(atual, [{ x: 100, y: 100, w: 200, h: 200 }], "16:9");
  assert.deepEqual(r, { x: 0, y: 0, w: 1600, h: 900 });
});

test("crescerExterno: membro pra fora, cresce pra direita e pra baixo na proporção, sem andar", () => {
  const atual = { x: 0, y: 0, w: 1600, h: 900 };
  const m = [{ x: 100, y: 100, w: 200, h: 200 }, { x: 1500, y: 700, w: 400, h: 300 }];
  const r = crescerExterno(atual, m, "16:9");
  assert.equal(r.x, 0); assert.equal(r.y, 0);
  assert.ok(m.every((x) => inside(x, r)));
  assert.ok(Math.abs(r.w / r.h - 16 / 9) < 0.01);
  assert.ok(r.w >= 1900 + FRAME_PAD && r.h >= 1000 + FRAME_PAD);
  assert.ok([r.w, r.h].every(Number.isInteger));
});

test("crescerExterno: livre é a borda direita/de baixo dos membros mais FRAME_PAD", () => {
  const r = crescerExterno({ x: 0, y: 0, w: 100, h: 100 }, [{ x: 50, y: 50, w: 100, h: 100 }], "livre");
  assert.deepEqual(r, { x: 0, y: 0, w: 150 + FRAME_PAD, h: 150 + FRAME_PAD });
});

const mb = (id, x, y, w, h, gw = 0, gh = 0) => ({ id, x, y, w, h, nw: w + gw, nh: h + gh });

test("abrirEspaco: grade 2x2, só a de cima-esquerda cresce", () => {
  const r = abrirEspaco([mb("a", 0, 0, 1000, 500, 50, 30), mb("b", 1100, 0, 1000, 500),
                         mb("c", 0, 600, 1000, 500), mb("d", 1100, 600, 1000, 500)]);
  assert.deepEqual(r, { a: { dx: 0, dy: 0 }, b: { dx: 50, dy: 0 }, c: { dx: 0, dy: 30 }, d: { dx: 50, dy: 30 } });
});

test("abrirEspaco: numa linha o crescimento se acumula", () => {
  const r = abrirEspaco([mb("c3", 2200, 0, 1000, 500), mb("c1", 0, 0, 1000, 500, 40),
                         mb("c2", 1100, 0, 1000, 500, 70)]);
  assert.deepEqual(r.c2, { dx: 40, dy: 0 });
  assert.deepEqual(r.c3, { dx: 110, dy: 0 });
});

test("abrirEspaco: nada cresce, nada anda", () => {
  const r = abrirEspaco([mb("a", 0, 0, 100, 100), mb("b", 200, 0, 100, 100), mb("c", 0, 200, 100, 100)]);
  assert.ok(Object.values(r).every((d) => d.dx === 0 && d.dy === 0));
});

test("abrirEspaco: card solto à direita anda com a célula; à esquerda, não", () => {
  const r = abrirEspaco([mb("cel", 500, 0, 1000, 500, 80), mb("dir", 1600, 100, 240, 190),
                         mb("esq", 0, 100, 240, 190)]);
  assert.deepEqual(r.dir, { dx: 80, dy: 0 });
  assert.deepEqual(r.esq, { dx: 0, dy: 0 });
});

test("marcaDaAgua escala com a imagem e respeita os limites", () => {
  const p = marcaDaAgua(1000, 600);
  assert.equal(Math.round(p.h), 19);              // 3,2% de 600
  assert.ok(p.x + p.w <= 1000 - p.margem);
  assert.ok(p.y + p.h <= 600 - p.margem);
  const pequena = marcaDaAgua(200, 120);
  assert.equal(pequena.h, 14);                     // piso
  const grande = marcaDaAgua(4000, 3000);
  assert.equal(grande.h, 32);                      // teto
});

test("marcaDaAgua mantém a proporção do hexágono", () => {
  const p = marcaDaAgua(1000, 600);
  assert.ok(Math.abs(p.w / p.h - 173 / 200) < 1e-9);
});
