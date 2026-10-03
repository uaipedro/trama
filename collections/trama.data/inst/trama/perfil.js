// Lógica pura do cabeçalho de perfil do `data/table` (sem React, sem DOM):
// testável sob `node --test` (tests/js/perfil-data.test.mjs).

// Perfil de uma coluna (como sai de `.tr_data_perfil_coluna`, R/type.R) →
// o que o card desenha: selo de tipo, % de NA e a forma — `barras` (alturas
// 0..1 do histograma) ou `top` (nível + fração 0..1).
export function resumoColuna(p) {
  if (!p) return null;
  const na = typeof p.na === "number" ? p.na : 0;
  const out = { tipo: p.tipo || "?", na, naTexto: na > 0 ? `${fmtPct(na)} NA` : "sem NA" };
  if (Array.isArray(p.hist) && p.hist.length) {
    const max = Math.max(...p.hist);
    out.barras = p.hist.map((c) => (max > 0 ? c / max : 0));
    if (typeof p.min === "number" && typeof p.max === "number") out.faixa = `${fmtNum(p.min)} – ${fmtNum(p.max)}`;
  } else if (Array.isArray(p.top) && p.top.length) {
    out.top = p.top.map((t) => ({ nivel: String(t.nivel), prop: t.prop }));
    if (p.niveis > p.top.length) out.resto = p.niveis - p.top.length;
  }
  return out;
}

export function fmtPct(x) {
  const v = x * 100;
  if (v > 0 && v < 1) return "<1%";
  return `${Math.round(v)}%`;
}

function fmtNum(v) {
  if (Number.isInteger(v)) return String(v);
  const a = Math.abs(v);
  return a >= 1000 || a < 0.01 ? v.toPrecision(3) : v.toFixed(2);
}

// Colunas que ficaram de fora do card (teto em R): "+N col".
export function colunasOcultas(ncol, enviadas) {
  const n = (ncol || 0) - (enviadas || 0);
  return n > 0 ? n : 0;
}
