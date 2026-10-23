// Blocos que subiram de versão desde que o fluxo foi salvo (`version_drift`,
// `tr_doc_validate()` em R/document-io.R). Módulo puro pelo motivo de sempre:
// `editor.js` não carrega sob `node --test`, e é aqui que mora a decisão — o
// editor só desenha o aviso e despacha a op.
//
// Subir `version` quer dizer que o resultado ou o padrão do bloco pode ter
// mudado. Sem renome de param não há migração, e o documento guarda a versão
// velha: o aviso volta a cada abertura até alguém dizer "vi". Aceitar é a op
// `accept_version`, que grava a versão nova; o aviso nunca some sozinho (#32).

// `problems` do servidor -> uma entrada por nó em deriva. `aceitavel` vem do
// R: falso quando o documento é mais novo que a coleção instalada (aceitar
// seria rebaixar) — esse caso só se resolve atualizando a coleção.
export function derivas(problems) {
  return (problems || [])
    .filter((p) => p && p.kind === "version_drift" && p.node)
    .map((p) => ({ node: p.node, type: p.type, from: p.from ?? null, to: p.to,
                   aceitavel: p.aceitavel !== false }));
}

// Ids que a op aceitou (o eco do servidor manda `nodes` como lista, ou como
// texto quando é um só — `auto_unbox`). Batch soma as de dentro.
export function aceitos(op) {
  if (!op) return [];
  if (op.op === "batch") return (op.ops || []).flatMap(aceitos);
  if (op.op !== "accept_version") return [];
  return Array.isArray(op.nodes) ? op.nodes : op.nodes == null ? [] : [op.nodes];
}

// O que sobra do aviso: sem os aceitos e sem nó que saiu do fluxo.
export function pendentes(lista, op, existe = () => true) {
  const fora = new Set(aceitos(op));
  return (lista || []).filter((d) => !fora.has(d.node) && existe(d.node));
}

// `null` quando não há o que aceitar: o botão não manda op vazia.
export function opAceitar(lista) {
  const ids = (lista || []).filter((d) => d.aceitavel).map((d) => d.node);
  return ids.length ? { op: "accept_version", nodes: ids } : null;
}

export function versoes(d) {
  return d.from == null ? `v${d.to}` : `v${d.from} → v${d.to}`;
}

export function titulo(lista) {
  const n = (lista || []).length;
  return n === 1
    ? "1 bloco tem versão nova desde que o fluxo foi salvo: o resultado pode ter mudado."
    : `${n} blocos têm versão nova desde que o fluxo foi salvo: o resultado pode ter mudado.`;
}
