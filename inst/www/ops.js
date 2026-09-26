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

// Cards que uma op semântica pode mudar: o nó mexido e tudo que vem DEPOIS
// dele nas ligações. Quem vem antes não recomputa (a chave de cache dele não
// depende de nada a jusante), então não pode piscar "na fila". `arestas` é
// `[[origem, destino], ...]` do grafo ANTES da op. `null` = não sei (op nova
// sem regra aqui): quem chama pinta todos, que é o comportamento antigo.
function sementes(op, out) {
  switch (op.op) {
    case "batch": return op.ops.every((o) => sementes(o, out));
    case "set_param": case "set_seed": out.add(op.node); return true;
    case "connect": case "disconnect": out.add(op.to_node); return true;
    // Some do grafo: quem dependia dele é que muda.
    case "remove_node": out.add(op.node); return true;
    // Nó novo não tem estado a pintar; as ligações dele vêm em `connect`.
    case "add_node": return true;
    default: return COSMETICAS.has(op.op);
  }
}
export function afetados(op, arestas) {
  const alvo = new Set();
  if (!sementes(op, alvo)) return null;
  const saem = new Map();
  arestas.forEach(([a, b]) => { if (!saem.has(a)) saem.set(a, []); saem.get(a).push(b); });
  const fila = [...alvo];
  while (fila.length) {
    (saem.get(fila.pop()) || []).forEach((b) => { if (!alvo.has(b)) { alvo.add(b); fila.push(b); } });
  }
  return alvo;
}
