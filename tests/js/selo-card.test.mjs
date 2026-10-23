import test from "node:test";
import assert from "node:assert/strict";
import { selosDoCard } from "../../inst/www/selo-card.js";
import { infoDoCard } from "../../inst/www/menu-card.js";

const card = infoDoCard({ id: "fig", data: { nodeType: "view/scatter", spec: { label: "Dispersão" } } });

test("selosDoCard: só o que o plugin devolve para este card, com a ação ligada ao card", () => {
  const vistos = [];
  const defs = [
    { id: "doc", selo: (c) => (c.id === "fig" ? { icone: "file-text", titulo: "No documento relatorio", acao: (x) => vistos.push(x.id) } : null) },
    { id: "nada", selo: () => null },
    { id: "sem-funcao" },
  ];
  const r = selosDoCard(defs, card);
  assert.deepEqual(r.map((s) => [s.id, s.icone, s.titulo]), [["doc", "file-text", "No documento relatorio"]]);
  r[0].acao();
  assert.deepEqual(vistos, ["fig"]);
  assert.deepEqual(selosDoCard(defs, { ...card, id: "outro" }), []);
});

test("ícone fora do conjunto e selo sem ação", () => {
  const r = selosDoCard([
    { id: "markup", selo: () => ({ icone: "<svg onload=x>" }) },
    { id: "vazio", selo: () => ({ titulo: "sem ícone" }) },
    { id: "ok", selo: () => ({ icone: "star" }) },
  ], card);
  assert.deepEqual(r.map((s) => s.id), ["ok"]);
  assert.equal(r[0].acao, null);
  assert.equal(r[0].titulo, "");
});

test("plugin com erro perde só o próprio selo", () => {
  const avisar = console.warn;
  console.warn = () => {};
  try {
    const r = selosDoCard([{ id: "quebra", selo: () => { throw new Error("bug"); } },
                           { id: "ok", selo: () => ({ icone: "star" }) }], card);
    assert.deepEqual(r.map((s) => s.id), ["ok"]);
  } finally {
    console.warn = avisar;
  }
});
