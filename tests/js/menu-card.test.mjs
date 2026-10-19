import test from "node:test";
import assert from "node:assert/strict";
import { infoDoCard, itensDoCard, registrarItem } from "../../inst/www/menu-card.js";

const no = { id: "fig", data: { nodeType: "view/scatter", label: "", state: "done",
  spec: { label: "Dispersão", outputs: [{ name: "out", type: "view/plot" }] } } };

test("infoDoCard: id, tipo, rótulo (do card ou do bloco) e saídas", () => {
  assert.deepEqual(infoDoCard(no), { id: "fig", tipo: "view/scatter", rotulo: "Dispersão",
    saidas: [{ porta: "out", tipo: "view/plot" }], estado: "done" });
  assert.equal(infoDoCard({ ...no, data: { ...no.data, label: "Peso" } }).rotulo, "Peso");
  assert.deepEqual(infoDoCard({ id: "x" }).saidas, []);
});

test("itensDoCard filtra por `quando`, resolve o rótulo e liga a ação ao card", () => {
  const chamados = [];
  const itens = [
    { id: "a", rotulo: "Inserir no documento…", quando: (c) => c.saidas.length > 0, acao: (c) => chamados.push(c.id) },
    { id: "b", rotulo: "Nunca", quando: () => false, acao: () => {} },
    { id: "c", rotulo: (c) => `Ver ${c.rotulo}`, acao: () => {} },
    { id: "d", rotulo: "Sem ação" },
  ];
  const r = itensDoCard(itens, infoDoCard(no));
  assert.deepEqual(r.map((x) => x.rotulo), ["Inserir no documento…", "Ver Dispersão"]);
  r[0].acao();
  assert.deepEqual(chamados, ["fig"]);
});

test("plugin com erro perde só o próprio item", () => {
  const avisar = console.warn;
  console.warn = () => {};
  try {
    const itens = [
      { id: "quebra", rotulo: "x", quando: () => { throw new Error("bug"); }, acao: () => {} },
      { id: "ok", rotulo: "ok", acao: () => {} },
    ];
    assert.deepEqual(itensDoCard(itens, infoDoCard(no)).map((x) => x.id), ["ok"]);
  } finally {
    console.warn = avisar;
  }
});

test("registrarItem troca o item de mesmo id, na mesma posição", () => {
  const l = [];
  registrarItem(l, "a", { rotulo: "A" });
  registrarItem(l, "b", { rotulo: "B" });
  registrarItem(l, "a", { rotulo: "A2" });
  assert.deepEqual(l.map((x) => [x.id, x.rotulo]), [["a", "A2"], ["b", "B"]]);
});
