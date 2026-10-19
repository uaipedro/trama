// Origens de cada porta de entrada, por nó: `fontes[alvo][porta] = [{ no,
// saida, rotulo, tipo, indice, handle }]`, na ordem do `index` da aresta.
//
// Existe porque `entradas`/`handlesEntrada` (editor.js) guardam UMA origem
// por porta, e um widget de porta múltipla precisa ver todas pelo id do nó —
// o layout do `publicar` cita os componentes pelo nó de origem, o mesmo nome
// que a porta `named` dá a eles no R (`.tr_port_nomes`). Puro: sem React, com
// teste em tests/js/fontes.test.mjs.

// `nomes` repete a regra do R: o id do nó, e `nó.saída` quando o mesmo nó
// chega por mais de uma saída na mesma porta.
export function nomesDasFontes(lista) {
  const conta = {};
  for (const f of lista) conta[f.no] = (conta[f.no] || 0) + 1;
  return lista.map((f) => (conta[f.no] > 1 ? `${f.no}.${f.saida}` : f.no));
}

export function fontesPorPorta(edges, nodes, handleDe = () => null) {
  const porId = new Map((nodes || []).map((n) => [n.id, n]));
  const out = {};
  for (const e of edges || []) {
    const n = porId.get(e.source);
    const item = {
      no: e.source, saida: e.sourceHandle,
      rotulo: n?.data?.label || n?.data?.spec?.label || e.source,
      tipo: n?.data?.nodeType ?? null,
      indice: Number(e.data?.index ?? 1),
      handle: handleDe(e.source, e.sourceHandle) || null,
    };
    ((out[e.target] ||= {})[e.targetHandle] ||= []).push(item);
  }
  for (const portas of Object.values(out)) {
    for (const [porta, lista] of Object.entries(portas)) {
      lista.sort((a, b) => a.indice - b.indice);
      const nomes = nomesDasFontes(lista);
      portas[porta] = lista.map((f, i) => ({ ...f, nome: nomes[i] }));
    }
  }
  return out;
}
