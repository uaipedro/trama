// Ops cosméticas: espelho de `.tr_presentation_ops` (R/document.R). Op
// cosmética não recomputa nada, então não pode pintar o canvas inteiro de "na
// fila" — e uma que faltasse aqui deixava todos os cards presos em "na fila",
// porque o servidor nunca mandaria a run que os tiraria de lá (foi o que
// aconteceu com as ops de nota e com `set_solto`). `tests/js/ops.test.mjs`
// lê a lista do R e falha se as duas divergirem. Batch é cosmético só se TODA
// op dentro dele for, a mesma regra de `tr_op_semantic()`.
export const COSMETICAS = new Set(["rename", "move", "resize", "set_view",
  "add_frame", "update_frame", "remove_frame", "reorder_frames", "set_mode", "set_solto",
  "add_note", "update_note", "remove_note"]);
export const cosmetica = (op) =>
  op.op === "batch" ? op.ops.every(cosmetica) : COSMETICAS.has(op.op);
