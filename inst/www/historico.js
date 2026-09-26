// Histórico pessoal de "depois de A, escolhi B". Mora só no navegador: nunca
// vai para o documento nem para o servidor (decisão de produto).
const CHAVE = "trama:proximo:v1";
const TETO = 500; // pares distintos; acima disso, descarta os de menor contagem

export function lerHistorico(store = globalThis.localStorage) {
  try {
    const v = JSON.parse(store?.getItem(CHAVE) || "{}");
    return v && typeof v === "object" && !Array.isArray(v) ? v : {};
  } catch { return {}; }
}

export function registrar(de, para, store = globalThis.localStorage) {
  const h = lerHistorico(store);
  const k = `${de}>${para}`;
  h[k] = (h[k] || 0) + 1;
  const ks = Object.keys(h);
  // O par recém-registrado nunca sai: com o teto cheio, todo par novo nasce
  // com a menor contagem e seria descartado na hora — o hábito novo nunca
  // entraria.
  if (ks.length > TETO) ks.filter((x) => x !== k).sort((a, b) => h[a] - h[b])
    .slice(0, ks.length - TETO).forEach((x) => delete h[x]);
  try { store?.setItem(CHAVE, JSON.stringify(h)); } catch { /* storage bloqueado: segue sem */ }
  return h;
}
