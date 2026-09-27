import { test } from "node:test";
import assert from "node:assert/strict";
import { candidatosFantasma, posicionarFantasma, acaoTeclaFantasma, alvoEditavel } from "../../inst/www/fantasmas.js";

const catalog = {
  categories: [{ id: "origem" }, { id: "leitura" }],
  nodes: [
    { id: "data/ler", label: "Ler", category: "origem", outputs: [{ name: "dados", type: "tabela" }] },
    ...Array.from({ length: 5 }, (_, i) => ({ id: `view/v${i}`, label: `Vista ${i}`,
      category: "leitura", inputs: [{ name: "dados", type: "tabela" }], outputs: [] })),
  ],
};
const flow = (selected = true) => ({
  nodes: [{ id: "n1", type: "ndNode", selected, position: { x: 10, y: 20 },
    data: { nodeType: "data/ler" } }], edges: [],
});

test("modo sob demanda considera só folha selecionada; modo ligado considera todas folhas", () => {
  const f = flow();
  assert.equal(candidatosFantasma({ catalog, ...f }).length, 0); // score sem evidência positiva
  const enriched = { ...catalog, transitions: [{ from: "data/ler", to: "view/v0", n: 1 }] };
  assert.equal(candidatosFantasma({ catalog: enriched, ...f }).length, 1);
  const branched = { ...f, nodes: [...f.nodes, { id: "n2", type: "ndNode", position: { x: 400, y: 20 },
    data: { nodeType: "data/ler" } }] };
  assert.equal(candidatosFantasma({ catalog: enriched, ...branched, modo: "demanda" }).length, 1);
  assert.equal(candidatosFantasma({ catalog: enriched, ...branched, modo: "ligado" }).length, 2);
});

test("limita a três, exige score positivo e respeita descarte da sessão", () => {
  const enriched = { ...catalog, transitions: Array.from({ length: 5 }, (_, i) =>
    ({ from: "data/ler", to: `view/v${i}`, n: i + 1 })) };
  const f = flow();
  const r = candidatosFantasma({ catalog: enriched, ...f });
  assert.equal(r.length, 3);
  assert.ok(r.every((x) => x.score > 0 && x.motivo));
  const next = candidatosFantasma({ catalog: enriched, ...f, descartados: [r[0].id] });
  assert.equal(next.length, 3);
  assert.ok(!next.some((x) => x.id === r[0].id));
  assert.deepEqual(candidatosFantasma({ catalog: enriched, ...f, modo: "desligado" }), []);
});

test("posicionamento evita cards e fantasmas já colocados", () => {
  const ocupados = [{ x: 310, y: 20, w: 250, h: 200 }];
  const a = posicionarFantasma({ x: 10, y: 20 }, ocupados);
  ocupados.push({ ...a, w: 220, h: 86 });
  const b = posicionarFantasma({ x: 10, y: 20 }, ocupados);
  assert.ok(a.y >= 238);
  assert.ok(b.y >= a.y + 104);
});

test("atalhos são Tab/Escape e inputs/editáveis ficam protegidos", () => {
  assert.equal(acaoTeclaFantasma("Tab"), "aceitar");
  assert.equal(acaoTeclaFantasma("Escape"), "descartar");
  assert.equal(acaoTeclaFantasma("Enter"), null);
  assert.equal(alvoEditavel({ closest: (s) => s.includes("input") ? {} : null }), true);
  assert.equal(alvoEditavel({ closest: () => null }), false);
});
