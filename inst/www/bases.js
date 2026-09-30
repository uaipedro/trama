// Busca e filtros do catálogo de bases. Módulo puro (sem React), pelo mesmo
// motivo de `colunas.js`: `editor.js` não carrega sob `node --test`.
//
// Uma base é o que `tr_datasets()` manda: {id, pacote, nome, titulo, node,
// params, descricao?, fonte?, temas[], n?, variaveis?, licenca?, url?,
// instalado}.

import { normalizar } from "./fantasmas.js";

// Cada palavra do termo tem que aparecer: começo de palavra vale 2, trecho no
// meio vale 1. Sem a subsequência do `pontoBusca` dos fantasmas: num texto
// longo (título + fonte + temas) ela casa quase qualquer coisa.
function ponto(termo, texto) {
  const qs = normalizar(termo).split(/\s+/).filter(Boolean);
  const t = normalizar(texto);
  const palavras = t.split(/[^a-z0-9]+/).filter(Boolean);
  let total = 0;
  for (const q of qs) {
    if (palavras.some((w) => w.startsWith(q))) total += 2;
    else if (t.includes(q)) total += 1;
    else return -1;
  }
  return total;
}

const textoDe = (b) => [b.titulo, b.nome, b.pacote, b.descricao, b.fonte, ...(b.temas || [])]
  .filter(Boolean).join(" ");

// Com termo, ordena pela pontuação da busca aproximada (o título pesa o
// dobro: é o que a pessoa lê). Sem termo, a ordem do servidor, que é a das
// coleções. `tema` e `pacote` filtram por igualdade; vazio é "todos".
export function filtrarBases(bases, { termo = "", tema = "", pacote = "" } = {}) {
  const lista = (bases || []).filter((b) =>
    (!tema || (b.temas || []).includes(tema)) && (!pacote || b.pacote === pacote));
  if (!normalizar(termo).trim()) return lista;
  return lista
    .map((b, i) => {
      const p = ponto(termo, textoDe(b));
      const t = ponto(termo, b.titulo);
      return { b, i, p: p < 0 ? -1 : p + Math.max(t, 0) };
    })
    .filter((x) => x.p >= 0)
    .sort((a, c) => c.p - a.p || a.i - c.i)
    .map((x) => x.b);
}

// Contagem por valor, do mais frequente ao menos (empate em ordem alfabética):
// os chips de tema e a lista de pacotes do modal.
function contar(valores) {
  const n = new Map();
  for (const v of valores) n.set(v, (n.get(v) || 0) + 1);
  return [...n].map(([valor, total]) => ({ valor, total }))
    .sort((a, b) => b.total - a.total || a.valor.localeCompare(b.valor, "pt"));
}
export const temasDe = (bases) => contar((bases || []).flatMap((b) => b.temas || []));
export const pacotesDe = (bases) => contar((bases || []).map((b) => b.pacote));

// "344 × 8" ou "" — o tamanho ajuda a decidir antes de instalar.
export function dimensao(b) {
  if (b.n == null) return "";
  const n = Number(b.n).toLocaleString("pt-BR");
  return b.variaveis == null ? `${n} linhas` : `${n} × ${b.variaveis}`;
}
