// tests/js/rotas.test.mjs
import { test } from "node:test";
import assert from "node:assert/strict";
import { rotear, criarRoteador, simplificar, caminhoComCantos, meioDaLinha, FOLGA, PASSO_FAIXA } from "../../inst/www/rotas.js";

const W = 240, H = 190;
const card = (id, x, y, w = W, hh = H) => ({ id, x1: x, y1: y, x2: x + w, y2: y + hh });
// porta de saída à direita / entrada à esquerda, a meia altura do card.
const aresta = (id, a, b, ya = 0.5, yb = 0.5) => ({
  id, source: a.id, target: b.id,
  s: { x: a.x2, y: a.y1 + (a.y2 - a.y1) * ya, pos: "right" },
  t: { x: b.x1, y: b.y1 + (b.y2 - b.y1) * yb, pos: "left" },
});

// O segmento (p,q) entra no interior aberto do retângulo?
function entra(r, p, q) {
  const x1 = Math.min(p.x, q.x), x2 = Math.max(p.x, q.x), y1 = Math.min(p.y, q.y), y2 = Math.max(p.y, q.y);
  return x1 < r.x2 - 1e-6 && x2 > r.x1 + 1e-6 && y1 < r.y2 - 1e-6 && y2 > r.y1 + 1e-6;
}
function semCortar(pts, obst, ids) {
  for (const o of obst) {
    if (ids.includes(o.id)) continue;
    for (let i = 0; i + 1 < pts.length; i++) assert.ok(!entra(o, pts[i], pts[i + 1]), `segmento ${i} entra em ${o.id}`);
  }
}
function ortogonal(pts) {
  for (let i = 0; i + 1 < pts.length; i++) {
    assert.ok(Math.abs(pts[i].x - pts[i + 1].x) < 1e-6 || Math.abs(pts[i].y - pts[i + 1].y) < 1e-6, `segmento ${i} torto`);
  }
}

test("cards na mesma altura, lado a lado: linha reta", () => {
  const a = card("a", 0, 0), b = card("b", 400, 0);
  const r = rotear({ obstaculos: [a, b], arestas: [aresta("e", a, b)] }).get("e");
  assert.equal(r.length, 2);
  assert.deepEqual(r[0], { x: 240, y: 95 });
  assert.deepEqual(r[1], { x: 400, y: 95 });
});

test("cards em alturas diferentes: Z com a vertical no vão entre as colunas", () => {
  const a = card("a", 0, 0), b = card("b", 400, 300);
  const r = rotear({ obstaculos: [a, b], arestas: [aresta("e", a, b)] }).get("e");
  ortogonal(r);
  assert.equal(r.length, 4);
  assert.ok(r[1].x > 240 && r[1].x < 400, "vertical dentro do vão");
  assert.equal(r[1].x, r[2].x);
  semCortar(r, [a, b], []);
});

test("card no meio do caminho é contornado, nunca atravessado", () => {
  const a = card("a", 0, 0), b = card("b", 800, 0), meio = card("m", 350, -40, 200, 270);
  const r = rotear({ obstaculos: [a, b, meio], arestas: [aresta("e", a, b)] }).get("e");
  assert.ok(r, "achou rota");
  ortogonal(r);
  semCortar(r, [a, b, meio], []);
  // sai e entra na horizontal
  assert.equal(r[0].y, r[1].y);
  assert.equal(r[r.length - 1].y, r[r.length - 2].y);
});

test("fio de volta (destino à esquerda da origem) sai pela direita e entra pela esquerda", () => {
  const a = card("a", 600, 0), b = card("b", 0, 400);
  const r = rotear({ obstaculos: [a, b], arestas: [aresta("e", a, b)] }).get("e");
  assert.ok(r);
  ortogonal(r);
  semCortar(r, [a, b], []);
  assert.ok(r[1].x > r[0].x, "sai pra direita");
  assert.ok(r[r.length - 2].x < r[r.length - 1].x, "entra vindo da esquerda");
});

test("destino colado no vão: trecho reto se divide ao meio, sem voltar", () => {
  const a = card("a", 0, 0), b = card("b", 260, 300);
  const r = rotear({ obstaculos: [a, b], arestas: [aresta("e", a, b)] }).get("e");
  assert.ok(r);
  ortogonal(r);
  assert.ok(r[1].x > 240 && r[1].x < 260);
});

test("fios no mesmo corredor saem em faixas separadas", () => {
  const a = card("a", 0, 0), b = card("b", 0, 300), c = card("c", 500, 0), d = card("d", 500, 300);
  const arestas = [aresta("e1", a, c), aresta("e2", b, d), aresta("e3", a, d), aresta("e4", b, c)];
  const rs = rotear({ obstaculos: [a, b, c, d], arestas });
  const xs = [];
  for (const [id, p] of rs) {
    assert.ok(p, id);
    ortogonal(p);
    semCortar(p, [a, b, c, d], [arestas.find((e) => e.id === id).source, arestas.find((e) => e.id === id).target]);
    for (let i = 1; i + 2 < p.length; i++) if (p[i].x === p[i + 1].x) xs.push(p[i].x);
  }
  // e3 e e4 usam a vertical do vão; as duas ficam distintas em ≥ PASSO_FAIXA
  const dist = [...new Set(xs)].sort((m, n) => m - n);
  for (let i = 0; i + 1 < dist.length; i++) assert.ok(dist[i + 1] - dist[i] >= PASSO_FAIXA - 1e-6, `faixas ${dist}`);
});

test("faixa nunca invade card nem inverte o trecho vizinho", () => {
  const a = card("a", 0, 0), b = card("b", 300, 400), c = card("c", 300, 0), d = card("d", 0, 400);
  const arestas = [aresta("e1", a, b), aresta("e2", d, c), aresta("e3", a, b, 0.2, 0.8), aresta("e4", d, c, 0.2, 0.8),
                   aresta("e5", a, b, 0.8, 0.2)];
  const rs = rotear({ obstaculos: [a, b, c, d], arestas });
  for (const e of arestas) {
    const p = rs.get(e.id);
    assert.ok(p, e.id);
    ortogonal(p);
    semCortar(p, [a, b, c, d], [e.source, e.target]);
    assert.ok(p[1].x > p[0].x, "sai pra direita");
    assert.ok(p[p.length - 2].x < p[p.length - 1].x, "entra pela esquerda");
  }
});

test("porta encaixotada: null (quem chama cai no smoothstep)", () => {
  const a = card("a", 0, 0), b = card("b", 600, 0), parede = card("p", 248, -500, 40, 1200);
  const r = rotear({ obstaculos: [a, b, parede], arestas: [aresta("e", a, b)] }).get("e");
  assert.equal(r, null);
});

test("determinismo: mesma entrada, mesma saída", () => {
  const a = card("a", 0, 0), b = card("b", 800, 0), m = card("m", 350, -40, 200, 270);
  const ent = { obstaculos: [a, b, m], arestas: [aresta("e", a, b), aresta("f", a, b, 0.3, 0.7)] };
  assert.deepEqual([...rotear(ent)], [...rotear(ent)]);
});

test("roteador com memória: mexer longe não muda a rota, mexer perto muda", () => {
  const a = card("a", 0, 0), b = card("b", 500, 300), longe = card("l", 5000, 5000), perto = card("p", 250, 100, 150, 100);
  const rot = criarRoteador();
  const ar = [aresta("e", a, b)];
  const r1 = rot({ obstaculos: [a, b, longe], arestas: ar }).get("e");
  const r2 = rot({ obstaculos: [a, b, { ...longe, x1: 5100, x2: 5340 }], arestas: ar }).get("e");
  assert.deepEqual(r1, r2);
  const r3 = rot({ obstaculos: [a, b, longe, perto], arestas: ar }).get("e");
  semCortar(r3, [a, b, perto], []);
});

test("frame alheio custa: fio prefere dar a volta a atravessar frame que não é dele", () => {
  const a = card("a", 0, 0), b = card("b", 1000, 0);
  const frame = { id: "F", x1: 400, y1: 20, x2: 700, y2: 160 };
  const sem = rotear({ obstaculos: [a, b], arestas: [aresta("e", a, b, 0.5, 0.5)] }).get("e");
  const com = rotear({ obstaculos: [a, b], frames: [{ id: "F", x1: 400, y1: 40, x2: 700, y2: 150 }],
                       arestas: [aresta("e", a, b, 0.5, 0.5)] }).get("e");
  assert.equal(sem.length, 2, "sem frame, reta");
  assert.ok(com.length > 2, "com frame alheio, desvia");
  void frame;
});

test("simplificar tira repetidos e pontos no meio de trecho reto", () => {
  assert.deepEqual(simplificar([{ x: 0, y: 0 }, { x: 5, y: 0 }, { x: 5, y: 0 }, { x: 9, y: 0 }, { x: 9, y: 4 }]),
    [{ x: 0, y: 0 }, { x: 9, y: 0 }, { x: 9, y: 4 }]);
});

test("caminhoComCantos e meioDaLinha", () => {
  const p = [{ x: 0, y: 0 }, { x: 100, y: 0 }, { x: 100, y: 100 }];
  assert.ok(caminhoComCantos(p, 12).startsWith("M0,0L88,0Q100,0 100,12"));
  assert.deepEqual(meioDaLinha(p), [100, 0]);
});

test("desempenho: 30 cards, 36 arestas", () => {
  const cards = [];
  for (let r = 0; r < 5; r++) for (let c = 0; c < 6; c++) cards.push(card(`c${r}_${c}`, c * 360, r * 320));
  const arestas = [];
  let n = 0;
  for (let r = 0; r < 5; r++) for (let c = 0; c < 5; c++) arestas.push(aresta(`h${n++}`, cards[r * 6 + c], cards[r * 6 + c + 1]));
  for (let r = 0; r < 4; r++) arestas.push(aresta(`v${n++}`, cards[r * 6 + 5], cards[(r + 1) * 6 + 0]));
  arestas.push(aresta("longa", cards[0], cards[29]));
  const t0 = performance.now();
  const rs = rotear({ obstaculos: cards, arestas });
  const dt = performance.now() - t0;
  console.log(`# ${arestas.length} arestas em ${dt.toFixed(0)} ms`);
  for (const a of arestas) {
    const p = rs.get(a.id);
    assert.ok(p, a.id);
    semCortar(p, cards, [a.source, a.target]);
  }
  assert.ok(dt < 3000, `lento: ${dt} ms`);
});
