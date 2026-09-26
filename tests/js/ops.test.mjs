// tests/js/ops.test.mjs
import { test } from "node:test";
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { COSMETICAS, cosmetica } from "../../inst/www/ops.js";

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
