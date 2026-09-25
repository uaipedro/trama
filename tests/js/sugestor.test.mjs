import { test } from "node:test";
import assert from "node:assert/strict";
import { compativel, aceitantes, sugerir, relacionados, PESOS } from "../../inst/www/sugestor.js";

const cat = {
  categories: [{ id: "source" }, { id: "inspect" }, { id: "clean" }, { id: "fit" }],
  adapters: [{ from: "t/a", to: "t/b" }],
  nodes: [
    { id: "d/ler",   category: "source",  inputs: [], outputs: [{ name: "out", type: "t/a" }] },
    { id: "d/resumo",category: "inspect", inputs: [{ name: "in", type: "t/a" }], outputs: [] },
    { id: "d/limpa", category: "clean",   inputs: [{ name: "in", type: "t/a" }], outputs: [{ name: "out", type: "t/a" }] },
    { id: "m/ajuste",category: "fit",     inputs: [{ name: "d", type: "t/b" }], outputs: [] },
    { id: "x/outro", category: "fit",     inputs: [{ name: "in", type: "t/z" }], outputs: [] },
  ],
};

test("compativel aceita igual e adaptador", () => {
  assert.ok(compativel(cat, "t/a", "t/a"));
  assert.ok(compativel(cat, "t/a", "t/b"));
  assert.ok(!compativel(cat, "t/b", "t/a"));
});

test("aceitantes devolve bloco e a primeira porta compatível", () => {
  const r = aceitantes(cat, "t/a");
  assert.deepEqual(r.map((x) => [x.id, x.porta]).sort(),
    [["d/limpa", "in"], ["d/resumo", "in"], ["m/ajuste", "d"]]);
});

test("etapa: categoria logo depois da origem vem primeiro", () => {
  const r = sugerir(cat, { de: "d/ler", tipo: "t/a" });
  assert.equal(r[0].id, "d/resumo");
  assert.ok(!r.some((x) => x.id === "x/outro"));
});

const ajuda = "Lê o arquivo. Diferente de `d/resumo`.\n\n## Usos relacionados\n\n`d/limpa` corrige; `view/x` mostra.\n\n## Exemplo\n\n`d/outro` aqui não conta.";

test("relacionados lê só a seção Usos relacionados", () => {
  assert.deepEqual(relacionados({ help: ajuda }), ["d/limpa", "view/x"]);
  assert.deepEqual(relacionados({}), []);
  assert.deepEqual(relacionados(undefined), []);
});

test("sugerir dá motivo relacionado ao citado pela origem", () => {
  const c = { ...cat, nodes: cat.nodes.map((n) => n.id === "d/ler" ? { ...n, help: ajuda } : n) };
  const r = sugerir(c, { de: "d/ler", tipo: "t/a" });
  const limpa = r.find((x) => x.id === "d/limpa");
  assert.equal(limpa.motivos.relacionado, PESOS.relacionado);
  assert.equal(r[0].id, "d/limpa");
  assert.ok(!("relacionado" in r.find((x) => x.id === "d/resumo").motivos));
});

const ids = (r) => r.map((x) => x.id);

test("transição direta vence a etapa", () => {
  const c = { ...cat, transitions: [{ from: "d/ler", to: "d/limpa", n: 3 }, { from: "d/ler", to: "m/ajuste", n: 1 }] };
  const r = sugerir(c, { de: "d/ler", tipo: "t/a" });
  assert.equal(r[0].id, "d/limpa");
  assert.equal(r[0].motivos.transicao, 0.75 * PESOS.transicao);
  assert.equal(r.find((x) => x.id === "m/ajuste").motivos.transicao, 0.25 * PESOS.transicao);
});

test("sem transições da origem, faz back-off pela categoria com peso pela metade", () => {
  const c = {
    ...cat,
    nodes: [...cat.nodes, { id: "d/limpa2", category: "clean", inputs: [], outputs: [] }],
    transitions: [{ from: "d/limpa2", to: "m/ajuste", n: 2 }],
  };
  const r = sugerir(c, { de: "d/limpa", tipo: "t/a" });
  assert.equal(r[0].id, "m/ajuste");
  assert.equal(r[0].motivos.transicao, 0.5 * PESOS.transicao);
  assert.ok(!("transicao" in r.find((x) => x.id === "d/limpa").motivos));
});

test("o histórico reordena o topo", () => {
  const r = sugerir(cat, { de: "d/ler", tipo: "t/a", historico: { "d/ler>d/limpa": 3, "d/ler>d/resumo": 1, "x/y>d/resumo": 50 } });
  assert.equal(r[0].id, "d/limpa");
  assert.equal(r[0].motivos.historico, 0.75 * PESOS.historico);
});

test("contexto: bloco de ajuste já presente cai; preparação não", () => {
  const c = {
    ...cat,
    categories: cat.categories.map((k) => k.id === "clean" ? { ...k, role: "preparacao" } : k),
    nodes: cat.nodes.map((n) => n.id === "d/resumo" ? { ...n, role: "ajuste" } : n),
  };
  const r = sugerir(c, { de: "d/ler", tipo: "t/a", presentes: ["d/resumo", "d/limpa"] });
  assert.equal(r.find((x) => x.id === "d/resumo").motivos.contexto, -1);
  assert.ok(!("contexto" in r.find((x) => x.id === "d/limpa").motivos));
  assert.equal(r[0].id, "d/limpa"); // sem o contexto, d/resumo lideraria
});

test("contexto: citado em Usos relacionados de bloco presente ganha", () => {
  const c = { ...cat, nodes: [...cat.nodes, { id: "v/graf", category: "fit", help: "## Usos relacionados\n\n`m/ajuste` depois." }] };
  const r = sugerir(c, { de: "d/ler", tipo: "t/a", presentes: ["v/graf"] });
  assert.equal(r.find((x) => x.id === "m/ajuste").motivos.contexto, 0.5 * PESOS.contexto);
});

test("sem transitions nem historico nem presentes, nada quebra", () => {
  assert.deepEqual(ids(sugerir(cat, { de: "d/ler", tipo: "t/a" })), ["d/resumo", "d/limpa", "m/ajuste"]);
  assert.deepEqual(ids(sugerir(cat, { de: "nao/existe", tipo: "t/a" })), ["d/limpa", "d/resumo", "m/ajuste"]);
});
