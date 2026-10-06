// Lógica pura do card do `data/table` (sem React, sem DOM):
// testável sob `node --test` (tests/js/perfil-data.test.mjs).

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
