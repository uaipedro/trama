// Regras puras para nomes de saídas, parte da apresentação do documento.
export function nomeDaSaida(saidas, nodeId, porta) {
  const nome = saidas?.[nodeId]?.[porta];
  return typeof nome === "string" && nome.trim() ? nome.trim() : null;
}

export function rotuloDaEntrada(entrada, edges, saidas) {
  const edge = edges.find((e) => e.target === entrada.node && e.targetHandle === entrada.port);
  const nome = edge && nomeDaSaida(saidas, edge.source, edge.sourceHandle);
  return nome ? `${entrada.port} ← ${nome}` : entrada.port;
}

export function removerSaidasDoNo(saidas, nodeId) {
  const out = { ...saidas };
  delete out[nodeId];
  return out;
}

export function limparSaidas(saidas) {
  return Object.fromEntries(Object.entries(saidas || {}).flatMap(([id, portas]) => {
    const nomes = Object.fromEntries(Object.entries(portas || {}).filter(([, nome]) =>
      typeof nome === "string" && nome.trim()).map(([porta, nome]) => [porta, nome.trim()]));
    return Object.keys(nomes).length ? [[id, nomes]] : [];
  }));
}
