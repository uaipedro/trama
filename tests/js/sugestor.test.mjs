import { test } from "node:test";
import assert from "node:assert/strict";
import { compativel, aceitantes, sugerir } from "../../inst/www/sugestor.js";

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
