// inst/www/fork.js — bifurcar um card: ramo paralelo com montante compartilhado.
//
// Módulo puro (o `editor.js` não carrega sob `node --test`). Espelha o modelo de
// `ui.ramos` do `inst/schema/document-v1.json` e as ops `add_ramo`,
// `remove_ramo` e `ligar_ramo` de `R/document.R`. O grafo de entrada tem o formato
// do documento: `{nodes, edges, positions, sizes, ramos}`, com edges
// `{from: {node, port}, to: {node, port}, index}`, `positions[id] = [x, y]` e
// `sizes[id] = [w, h]` (opcionais). Ids novos vêm de `novoId()`, passada pelo
// chamador, como em `opsDoGrupo` no editor.
//
// Ramo "A" é o original, implícito; os ramos guardados são B, C, ...

export const GAP_BIFURCACAO = 48;
const ALTURA_PADRAO = 120;

// Cards a jusante de `origemId`, incluindo a própria origem.
function jusante(grafo, origemId) {
  const saem = new Map();
  grafo.edges.forEach((e) => {
    if (!saem.has(e.from.node)) saem.set(e.from.node, []);
    saem.get(e.from.node).push(e.to.node);
  });
  const vistos = new Set([origemId]);
  const fila = [origemId];
  while (fila.length) {
    (saem.get(fila.pop()) || []).forEach((b) => {
      if (!vistos.has(b)) { vistos.add(b); fila.push(b); }
    });
  }
  return vistos;
}

// Próxima letra livre: A é o original (implícito), então começa em B.
export function proximaLetra(ramos) {
  const usadas = new Set(["A", ...Object.values(ramos || {}).map((r) => r.letra)]);
  for (let c = 66; c <= 90; c++) {
    const l = String.fromCharCode(c);
    if (!usadas.has(l)) return l;
  }
  throw new Error("Letras de ramo esgotadas (B a Z já usadas).");
}

// Um batch que cria o ramo: cópias de origem + tudo a jusante, com params
// iguais; arestas internas reproduzidas entre as cópias; entradas vindas de
// fora do conjunto ligadas à MESMA origem (montante compartilhado, nunca
// copiado); e o `add_ramo` por último. As cópias ficam abaixo do bloco
// original (deslocadas pela altura do conjunto + GAP_BIFURCACAO).
export function planejarBifurcacao(grafo, origemId, novoId) {
  if (!grafo.nodes[origemId]) throw new Error(`Card que não existe: '${origemId}'.`);
  const ramos = grafo.ramos || {};
  const dentro = jusante(grafo, origemId);
  // Ordem das chaves do grafo: determinística, e as cópias seguem a mesma ordem.
  const ordem = Object.keys(grafo.nodes).filter((id) => dentro.has(id));
  const copias = {};
  ordem.forEach((id) => { copias[id] = novoId(); });
  const ramoId = novoId();
  const letra = proximaLetra(ramos);

  const pos = grafo.positions || {};
  const tams = grafo.sizes || {};
  const comPos = ordem.filter((id) => pos[id]);
  let dy = 0;
  if (comPos.length) {
    const topo = Math.min(...comPos.map((id) => pos[id][1]));
    const base = Math.max(...comPos.map((id) => pos[id][1] + (tams[id] ? tams[id][1] : ALTURA_PADRAO)));
    dy = base - topo + GAP_BIFURCACAO;
  }

  const criam = ordem.map((id) => {
    const n = grafo.nodes[id];
    const op = { op: "add_node", id: copias[id], type: n.type };
    if (n.label !== undefined) op.label = n.label;
    op.params = structuredClone(n.params || {});
    if (n.seed !== undefined) op.seed = n.seed;
    if (pos[id]) op.position = [Math.round(pos[id][0]), Math.round(pos[id][1] + dy)];
    return op;
  });

  // Aresta que chega ao conjunto: de dentro (vira cópia -> cópia) ou de fora
  // (a origem fora continua a mesma). Aresta que sai do conjunto não existe:
  // o conjunto é fechado a jusante.
  const conecta = grafo.edges.filter((e) => dentro.has(e.to.node)).map((e) => {
    const op = {
      op: "connect",
      from_node: dentro.has(e.from.node) ? copias[e.from.node] : e.from.node,
      from_port: e.from.port,
      to_node: copias[e.to.node],
      to_port: e.to.port,
    };
    if (e.index !== undefined) op.index = e.index;
    return op;
  });

  const pares = Object.fromEntries(ordem.map((id) => [id, copias[id]]));
  const add = { op: "add_ramo", id: ramoId, letra, origem: origemId, pares };
  return { copias, ops: [{ op: "batch", ops: [...criam, ...conecta, add] }] };
}

// Gêmeos de um card: classe de equivalência pelos pares de todos os ramos.
// Não entram o par (origem, cópia) de cada ramo — é a decisão que difere — nem
// as cópias desligadas. Transitiva: cópia de gêmeo também é gêmea. Devolve os
// ids da classe, sem o próprio `id`, ordenados.
export function gemeosDe(ramos, id) {
  const liga = new Map();
  const une = (a, b) => {
    if (!liga.has(a)) liga.set(a, new Set());
    liga.get(a).add(b);
  };
  Object.values(ramos || {}).forEach((r) => {
    const off = new Set(r.desligados || []);
    Object.entries(r.pares || {}).forEach(([orig, copia]) => {
      if (orig === r.origem || off.has(copia)) return;
      une(orig, copia);
      une(copia, orig);
    });
  });
  if (!liga.has(id)) return [];
  const vistos = new Set([id]);
  const fila = [id];
  while (fila.length) {
    liga.get(fila.pop()).forEach((b) => {
      if (!vistos.has(b)) { vistos.add(b); fila.push(b); }
    });
  }
  vistos.delete(id);
  return [...vistos].sort();
}

// Espelhamento de uma op: para cada `set_param` (sozinho ou dentro de batch),
// devolve um `set_param` igual para cada gêmeo do card. Não duplica o que a
// própria op já põe num gêmeo (o valor explícito vence). Não é recursivo: as
// extras não geram mais espelhos, então não há laço.
export function espelhar(op, ramos) {
  const sets = [];
  const colhe = (o) => {
    if (o.op === "batch") o.ops.forEach(colhe);
    else if (o.op === "set_param") sets.push(o);
  };
  colhe(op);
  const usados = new Set(sets.map((s) => `${s.node} ${s.name}`));
  const extras = [];
  sets.forEach((s) => {
    gemeosDe(ramos, s.node).forEach((g) => {
      const k = `${g} ${s.name}`;
      if (usados.has(k)) return;
      usados.add(k);
      extras.push({ ...s, node: g });
    });
  });
  return extras;
}

// Diferenças de params entre o card de origem de `ramo` e a sua cópia:
// [{param, de, para}], ordenadas pelo nome do param. `ramo` é `ramos[letraId]`.
export function diferencas(grafo, ramo) {
  const a = grafo.nodes[ramo.origem]?.params || {};
  const b = grafo.nodes[ramo.pares[ramo.origem]]?.params || {};
  const nomes = [...new Set([...Object.keys(a), ...Object.keys(b)])].sort();
  return nomes
    .filter((p) => JSON.stringify(a[p] ?? null) !== JSON.stringify(b[p] ?? null))
    .map((p) => ({ param: p, de: a[p] ?? null, para: b[p] ?? null }));
}

// Selo de um card: a letra do ramo a que ele pertence — a de quem o criou
// como cópia; "A" se ele só aparece como original; null fora de qualquer
// ramo. `decide` marca o card onde a decisão difere (origem de um ramo ou a
// cópia dela), que é onde o editor mostra o "difere".
export function selo(ramos, id) {
  let letra = null;
  let original = false;
  let decide = false;
  Object.values(ramos || {}).forEach((r) => {
    if (Object.values(r.pares).includes(id)) letra = r.letra;
    if (Object.hasOwn(r.pares, id)) original = true;
    if (r.origem === id || r.pares[r.origem] === id) decide = true;
  });
  return { letra: letra ?? (original ? "A" : null), decide };
}

// Correspondentes de um card em TODOS os ramos, para comparar (Vista lado a
// lado, destaque ao selecionar): a classe transitiva pelos pares, agora
// incluindo o par de origem e as cópias desligadas — comparar o modelo A com
// o modelo B é justamente o ponto. Devolve os ids sem o próprio, ordenados.
export function correspondentes(ramos, id) {
  const viz = new Map();
  const liga = (a, b) => {
    if (!viz.has(a)) viz.set(a, new Set());
    if (!viz.has(b)) viz.set(b, new Set());
    viz.get(a).add(b); viz.get(b).add(a);
  };
  Object.values(ramos || {}).forEach((r) =>
    Object.entries(r.pares).forEach(([a, b]) => liga(a, b)));
  const vistos = new Set([id]);
  const fila = [id];
  while (fila.length) {
    (viz.get(fila.pop()) || []).forEach((b) => {
      if (!vistos.has(b)) { vistos.add(b); fila.push(b); }
    });
  }
  vistos.delete(id);
  return [...vistos].sort();
}

// Ramo (id) do qual `id` é cópia, e se ela está desligada do espelhamento.
export function ramoDaCopia(ramos, id) {
  for (const [rid, r] of Object.entries(ramos || {})) {
    if (Object.values(r.pares).includes(id))
      return { ramo: rid, letra: r.letra, desligado: (r.desligados || []).includes(id) };
  }
  return null;
}
