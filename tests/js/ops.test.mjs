// tests/js/ops.test.mjs
import { test } from "node:test";
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { COSMETICAS, cosmetica, afetados } from "../../inst/www/ops.js";

test("COSMETICAS espelha .tr_presentation_ops do R", () => {
  const r = readFileSync(new URL("../../R/document.R", import.meta.url), "utf8");
  const m = r.match(/\.tr_presentation_ops <- c\(([^)]*)\)/);
  assert.ok(m, ".tr_presentation_ops não encontrado em R/document.R");
  const doR = [...m[1].matchAll(/"([^"]+)"/g)].map((x) => x[1]).sort();
  assert.deepEqual([...COSMETICAS].sort(), doR);
});

test("batch é cosmético só se toda op dentro for", () => {
  assert.equal(cosmetica({ op: "update_note" }), true);
  assert.equal(cosmetica({ op: "batch", ops: [{ op: "set_mode" }, { op: "set_solto" }] }), true);
  assert.equal(cosmetica({ op: "batch", ops: [{ op: "move" }, { op: "set_param" }] }), false);
});

test("afetados: o nó mexido e só quem vem depois", () => {
  const ar = [["tab", "graf"], ["tab", "anova"], ["graf", "salvar"]];
  assert.deepEqual([...afetados({ op: "set_param", node: "graf" }, ar)].sort(), ["graf", "salvar"]);
  assert.deepEqual([...afetados({ op: "connect", to_node: "anova" }, ar)], ["anova"]);
  assert.deepEqual([...afetados({ op: "batch", ops: [{ op: "move" }, { op: "set_seed", node: "tab" }] }, ar)].sort(),
                   ["anova", "graf", "salvar", "tab"]);
  assert.equal(afetados({ op: "op_nova" }, ar), null);
});
