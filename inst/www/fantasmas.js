// Busca e navegação dos três previews de próximo bloco. Sem React para que
// compatibilidade, limite e teclado possam ser verificados com node:test.
export const MAX_FANTASMAS = 3;

export function buscarFantasma(ranking, catalog, termo = "", limite = MAX_FANTASMAS) {
  const q = String(termo).trim().toLowerCase();
  const byId = new Map((catalog?.nodes || []).map((n) => [n.id, n]));
  return (ranking || []).filter((s) => {
    const n = byId.get(s.id);
    return n && (!q || `${n.label || ""} ${n.description || ""} ${n.id}`.toLowerCase().includes(q));
  }).slice(0, limite);
}

export function moverFantasma(foco, total, tecla) {
  if (!total) return -1;
  if (!["ArrowDown", "ArrowRight", "ArrowUp", "ArrowLeft"].includes(tecla)) return foco;
  const delta = tecla === "ArrowDown" || tecla === "ArrowRight" ? 1 : -1;
  return ((foco < 0 ? (delta > 0 ? -1 : 0) : foco) + delta + total) % total;
}
