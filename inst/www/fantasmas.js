// Busca e navegação dos três previews de próximo bloco. Sem React para que
// compatibilidade, limite e teclado possam ser verificados com node:test.
export const MAX_FANTASMAS = 3;

// A preferência controla apenas os previews iniciais; a busca sempre pode
// mostrar resultados depois que a pessoa começa a digitar.
export function mostrarFantasma(sugestoes, termo) {
  return sugestoes !== false || String(termo).trim().length > 0;
}

// Desloca a pilha de previews para baixo dos filhos já conectados à saída.
export function yDosFantasmas(y, filhos = [], altura = 250, margem = 16) {
  const abaixo = filhos.filter((f) => Number.isFinite(f.y) && f.y >= y - margem)
    .sort((a, b) => a.y - b.y);
  let topo = y;
  for (const f of abaixo) {
    if (topo + altura + margem <= f.y) break;
    topo = Math.max(topo, f.y + (f.h || 0) + margem);
  }
  return topo;
}

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
