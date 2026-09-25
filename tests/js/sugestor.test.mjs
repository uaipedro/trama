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
