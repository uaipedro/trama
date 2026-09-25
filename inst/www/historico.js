// Histórico pessoal de "depois de A, escolhi B". Mora só no navegador: nunca
// vai para o documento nem para o servidor (decisão de produto).
const CHAVE = "trama:proximo:v1";
const TETO = 500; // pares distintos; acima disso, descarta os de menor contagem

export function lerHistorico(store = globalThis.localStorage) {
  try { return JSON.parse(store?.getItem(CHAVE) || "{}") || {}; } catch { return {}; }
}

export function registrar(de, para, store = globalThis.localStorage) {
  const h = lerHistorico(store);
  const k = `${de}>${para}`;
  h[k] = (h[k] || 0) + 1;
  const ks = Object.keys(h);
  if (ks.length > TETO) ks.sort((a, b) => h[a] - h[b]).slice(0, ks.length - TETO).forEach((x) => delete h[x]);
  try { store?.setItem(CHAVE, JSON.stringify(h)); } catch { /* storage bloqueado: segue sem */ }
  return h;
}
