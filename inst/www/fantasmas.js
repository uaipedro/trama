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

// Busca aproximada: sem acento nem caixa, e cada palavra digitada tem de
// casar com o texto do bloco (rótulo, descrição, id). Início de palavra vale
// mais que trecho no meio, que vale mais que letras em sequência ("anv" acha
// "ANOVA"). Empate fica com a ordem do sugestor.
export const normalizar = (t) => String(t || "").normalize("NFD")
  .replace(/[\u0300-\u036f]/g, "").toLowerCase();

function subsequencia(q, t) {
  let i = 0;
  for (const c of t) if (c === q[i] && ++i === q.length) return true;
  return false;
}

export function pontoBusca(termo, texto) {
  const qs = normalizar(termo).split(/\s+/).filter(Boolean);
  if (!qs.length) return 0;
  const t = normalizar(texto);
  const palavras = t.split(/[^a-z0-9]+/).filter(Boolean);
  let total = 0;
  for (const q of qs) {
    if (palavras.some((w) => w.startsWith(q))) total += 3;
    else if (t.includes(q)) total += 2;
    else if (palavras.some((w) => subsequencia(q, w)) || subsequencia(q, t.replace(/\s+/g, ""))) total += 1;
    else return -1;
  }
  return total;
}

// Sem termo, só o que o sugestor pontuou. Com termo, todo bloco compatível
// entra na busca: filtrar pelo placar antes esconderia o que a pessoa pediu.
export function buscarFantasma(ranking, catalog, termo = "", limite = MAX_FANTASMAS) {
  const byId = new Map((catalog?.nodes || []).map((n) => [n.id, n]));
  const lista = (ranking || []).filter((s) => byId.has(s.id));
  if (!normalizar(termo).trim()) return lista.filter((s) => s.score > 0).slice(0, limite);
  return lista
    .map((s, i) => {
      const n = byId.get(s.id);
      return { s, i, p: pontoBusca(termo, `${n.label || ""} ${n.description || ""} ${n.id}`) };
    })
    .filter((x) => x.p >= 0)
    .sort((a, b) => b.p - a.p || a.i - b.i)
    .slice(0, limite)
    .map((x) => x.s);
}

export function moverFantasma(foco, total, tecla) {
  if (!total) return -1;
  if (!["ArrowDown", "ArrowRight", "ArrowUp", "ArrowLeft"].includes(tecla)) return foco;
  const delta = tecla === "ArrowDown" || tecla === "ArrowRight" ? 1 : -1;
  return ((foco < 0 ? (delta > 0 ? -1 : 0) : foco) + delta + total) % total;
}
