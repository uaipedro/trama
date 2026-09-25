import { test } from "node:test";
import assert from "node:assert/strict";
import { compativel, aceitantes, sugerir, relacionados, PESOS, PENAL } from "../../inst/www/sugestor.js";

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

const comPapeis = {
  ...cat,
  categories: cat.categories.map((k) => k.id === "clean" ? { ...k, role: "preparacao" }
    : k.id === "inspect" ? { ...k, role: "inspecao" } : k),
};

test("contexto: bloco já a montante cai, inspeção inclusive", () => {
  const r = sugerir(comPapeis, { de: "d/ler", tipo: "t/a", presentes: ["d/ler", "d/resumo"] });
  assert.equal(r.find((x) => x.id === "d/resumo").motivos.contexto, -PENAL.presente * PESOS.contexto);
  assert.equal(r[0].id, "d/limpa"); // sem o contexto, d/resumo lideraria
});

test("contexto: mesmo tipo da origem cai forte; preparação encadeada não", () => {
  const c = { ...comPapeis, nodes: [...comPapeis.nodes,
    { id: "d/ver", category: "inspect", inputs: [{ name: "in", type: "t/a" }], outputs: [{ name: "out", type: "t/a" }] }] };
  const r = sugerir(c, { de: "d/ver", tipo: "t/a", presentes: ["d/ver", "d/ler"] });
  assert.equal(r.find((x) => x.id === "d/ver").motivos.contexto, -PENAL.origem * PESOS.contexto);
  const p = sugerir(c, { de: "d/limpa", tipo: "t/a", presentes: ["d/limpa", "d/ler"] });
  assert.ok(!("contexto" in p.find((x) => x.id === "d/limpa").motivos));
});

test("sem pingue-pongue: preparo -> inspeção -> não volta ao preparo", () => {
  // Converter (preparo) -> Resumo (inspeção): de Resumo, Converter já está a
  // montante e não deve liderar; de Converter, Resumo a montante também cai.
  const c = { ...comPapeis, transitions: [
    { from: "d/limpa", to: "d/resumo", n: 6 }, { from: "d/resumo", to: "d/limpa", n: 6 },
    { from: "d/limpa", to: "m/ajuste", n: 4 }, { from: "d/resumo", to: "m/ajuste", n: 4 }] };
  const r1 = sugerir(c, { de: "d/resumo", tipo: "t/a", presentes: ["d/resumo", "d/limpa", "d/ler"] });
  assert.notEqual(r1[0].id, "d/limpa");
  const r2 = sugerir(c, { de: "d/limpa", tipo: "t/a", presentes: ["d/limpa", "d/resumo", "d/ler"] });
  assert.notEqual(r2[0].id, "d/resumo");
});

test("histórico: uma escolha explícita fica no top 5 mesmo com outras escolhas", () => {
  const nodes = [cat.nodes[0], ...Array.from({ length: 8 }, (_, i) =>
    ({ id: `d/b${i}`, category: "inspect", inputs: [{ name: "in", type: "t/a" }], outputs: [] })),
    { id: "d/dup", category: "fit", inputs: [{ name: "in", type: "t/a" }], outputs: [] }];
  const c = { ...cat, nodes, transitions: Array.from({ length: 8 }, (_, i) => ({ from: "d/ler", to: `d/b${i}`, n: 10 })) };
  const historico = { "d/ler>d/dup": 1, "d/ler>d/b0": 5, "d/ler>d/b1": 5, "d/ler>d/b2": 5 };
  const r = sugerir(c, { de: "d/ler", tipo: "t/a", historico });
  const i = r.findIndex((x) => x.id === "d/dup");
  assert.ok(i >= 0 && i < 5, `d/dup em ${i}`);
  assert.equal(r[i].motivos.historico, 0.5 * PESOS.historico);
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

import { sugerirOrigem, intermediarios, emitentes } from "../../inst/www/sugestor.js";

test("emitentes: saída igual ou via adaptador", () => {
  assert.deepEqual(emitentes(cat, "t/b").map((x) => x.id).sort(), ["d/ler", "d/limpa"]);
  assert.deepEqual(emitentes(cat, "t/z"), []);
});

test("sugerirOrigem: etapa invertida põe a categoria anterior no topo", () => {
  const r = sugerirOrigem(cat, { para: "d/resumo", tipo: "t/a" });
  assert.deepEqual(r.map((x) => x.id), ["d/ler", "d/limpa"]);
  assert.equal(r[0].motivos.etapa, PESOS.etapa);
  assert.ok(!("etapa" in r[1].motivos)); // clean vem depois de inspect
});

test("sugerirOrigem: transições que chegam no alvo e back-off", () => {
  const c = { ...cat, transitions: [{ from: "d/limpa", to: "m/ajuste", n: 3 }, { from: "d/ler", to: "m/ajuste", n: 1 }] };
  const r = sugerirOrigem(c, { para: "m/ajuste", tipo: "t/b" });
  assert.equal(r[0].id, "d/limpa");
  assert.equal(r[0].motivos.transicao, 0.75 * PESOS.transicao);
  const c2 = { ...cat, nodes: [...cat.nodes, { id: "m/outro", category: "fit", inputs: [{ name: "d", type: "t/b" }] }],
               transitions: [{ from: "d/limpa", to: "m/ajuste", n: 1 }] };
  const r2 = sugerirOrigem(c2, { para: "m/outro", tipo: "t/b" });
  assert.equal(r2.find((x) => x.id === "d/limpa").motivos.transicao, 0.5 * PESOS.transicao);
});

test("sugerirOrigem: relacionado, histórico e alvo desconhecido", () => {
  const c = { ...cat, nodes: cat.nodes.map((n) => n.id === "d/limpa" ? { ...n, help: "## Usos relacionados\n\n`d/resumo`" } : n) };
  const r = sugerirOrigem(c, { para: "d/resumo", tipo: "t/a", historico: { "d/ler>d/resumo": 2, "d/ler>x/y": 9 } });
  assert.equal(r.find((x) => x.id === "d/limpa").motivos.relacionado, PESOS.relacionado);
  assert.equal(r.find((x) => x.id === "d/ler").motivos.historico, (2 / 3) * PESOS.historico);
  assert.deepEqual(sugerirOrigem(cat, { para: "nao/existe", tipo: "t/a" }).map((x) => x.id), ["d/ler", "d/limpa"]);
});

test("intermediarios: entrada aceita a origem e saída alimenta o destino", () => {
  assert.deepEqual(intermediarios(cat, "t/a", "t/b"), [{ id: "d/limpa", porta: "in", saida: "out" }]);
  assert.deepEqual(intermediarios(cat, "t/b", "t/a"), []);
  assert.deepEqual(intermediarios(cat, "t/a", "t/z"), []);
});

test("origem: bloco já a montante do alvo perde, e o histórico satura", async () => {
  const { sugerirOrigem, PENAL, PESOS } = await import("../../inst/www/sugestor.js");
  const c = {
    categories: [{ id: "source" }, { id: "clean" }],
    nodes: [
      { id: "d/ler", category: "source", inputs: [], outputs: [{ name: "out", type: "t/a" }] },
      { id: "d/limpa", category: "clean", inputs: [{ name: "in", type: "t/a" }], outputs: [{ name: "out", type: "t/a" }] },
    ],
  };
  const r = sugerirOrigem(c, { para: "d/limpa", tipo: "t/a", presentes: ["d/ler"],
                               historico: { "d/ler>d/limpa": 1 } });
  const ler = r.find((x) => x.id === "d/ler");
  assert.equal(ler.motivos.contexto, -PENAL.presente * PESOS.contexto);
  assert.equal(ler.motivos.historico, 0.5 * PESOS.historico);
});
