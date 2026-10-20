// Itens que plugins acrescentam ao menu de contexto de um card (botão direito).
//
// Puro (sem React, sem "trama"): o registro mora no `runtime.js`
// (`registerCardMenuItem`), o editor só desenha o que `itensDoCard` devolve.
// O card chega como `infoDoCard(no)`: o plugin vê o id, o tipo, o rótulo,
// as saídas e o ícone, nunca o objeto interno do editor.

// O que um plugin sabe de um card.
export function infoDoCard(no) {
  const d = no?.data || {};
  const spec = d.spec || {};
  return {
    id: no?.id,
    tipo: d.nodeType || null,
    rotulo: d.label || spec.label || no?.id,
    saidas: (spec.outputs || []).map((p) => ({ porta: p.name, tipo: p.type })),
    estado: d.state || null,
    // O ícone do bloco como o catálogo dá (`{ kind, value }`), para listas de plugin.
    icone: spec.icon || null,
  };
}

// Os itens que valem para este card, com o rótulo resolvido. Erro de um
// plugin (no `quando` ou no rótulo) tira só o item dele do menu.
export function itensDoCard(itens, card) {
  const out = [];
  for (const it of itens || []) {
    try {
      if (typeof it.acao !== "function") continue;
      if (typeof it.quando === "function" && !it.quando(card)) continue;
      const rotulo = typeof it.rotulo === "function" ? it.rotulo(card) : it.rotulo;
      if (typeof rotulo !== "string" || !rotulo) continue;
      out.push({ id: it.id, rotulo, titulo: it.titulo || null, acao: () => it.acao(card) });
    } catch (e) {
      if (typeof console !== "undefined") console.warn(`trama: item de menu '${it?.id}' falhou`, e);
    }
  }
  return out;
}

// Registro (sem estado global aqui: o `runtime.js` guarda a lista).
export function registrarItem(lista, id, def) {
  const item = { ...def, id };
  const i = lista.findIndex((x) => x.id === id);
  if (i >= 0) lista[i] = item; else lista.push(item);
  return lista;
}
