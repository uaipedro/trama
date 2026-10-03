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

// Dimensões de um handle de tabela: do `summary` (linhas/colunas, ver
// `data_table_type`) ou, sem ele, do preview (`nrow`/`ncol`).
function dims(hd) {
  if (!hd) return null;
  const s = hd.summary, d = hd.preview && hd.preview.data;
  const l = s && typeof s.linhas === "number" ? s.linhas : d && typeof d.nrow === "number" ? d.nrow : null;
  const c = s && typeof s.colunas === "number" ? s.colunas : d && typeof d.ncol === "number" ? d.ncol : null;
  return l === null || c === null ? null : { linhas: l, colunas: c };
}

const MENOS = "−";
const br = (n) => n.toLocaleString("pt-BR");

// Delta saída × entrada para o selo do card ("−12 linhas", "+2 col").
// Só com UMA entrada tabular: num join (duas tabelas) a comparação com
// qualquer uma delas engana. Lista vazia = nada mudou ou não dá pra dizer.
export function deltaTabela(saida, entradas) {
  const ins = Object.values(entradas || {}).map(dims).filter(Boolean);
  const out = dims(saida);
  if (!out || ins.length !== 1) return [];
  const [e] = ins, res = [];
  const dl = out.linhas - e.linhas, dc = out.colunas - e.colunas;
  const sinal = (n) => (n > 0 ? "+" : MENOS);
  if (dl) {
    const pct = e.linhas ? ` (${sinal(dl)}${Math.round(Math.abs(dl) / e.linhas * 100)}%)` : "";
    res.push({ texto: `${sinal(dl)}${br(Math.abs(dl))} ${Math.abs(dl) === 1 ? "linha" : "linhas"}`,
               titulo: `${br(e.linhas)} → ${br(out.linhas)} linhas${pct}`,
               tipo: dl < 0 ? "menos" : "mais" });
  }
  if (dc) {
    res.push({ texto: `${sinal(dc)}${Math.abs(dc)} col`,
               titulo: `${e.colunas} → ${out.colunas} colunas`, tipo: dc < 0 ? "menos" : "mais" });
  }
  return res;
}
