// Cor do cabeçalho de um card a partir da categoria. Módulo puro (sem React),
// pelo mesmo motivo de `modos.js`: `editor.js` não carrega sob `node --test`.
//
// O papel do BLOCO (`tr_node(role = ...)`) vence o da categoria
// (`tr_category(role = ...)`), que é o padrão dos blocos dela. Papel não traz
// cor: a cor é do editor e muda com o tema, então aqui só se aponta pro token
// do CSS (`--tr-papel-<papel>` e a tinta `--tr-papel-<papel>-ink`). Sem papel
// nenhum, vale a cor fixa que a coleção mandou, com a tinta padrão.

export const PAPEIS = ["origem", "preparacao", "inspecao", "ajuste", "leitura", "avaliacao", "saida"];
export const COR_RESERVA = "#64748b";

const papelDe = (cat, bloco) =>
  [bloco?.role, cat?.role].find((p) => PAPEIS.includes(p)) || null;

// `bloco` é o spec do nó (catálogo); sem ele, a cor é a da categoria, como
// na bolinha do cabeçalho de grupo da paleta.
export function corDaCategoria(cat, bloco) {
  const p = papelDe(cat, bloco);
  return p ? `var(--tr-papel-${p})` : cat?.color || COR_RESERVA;
}

// `undefined` deixa valer a tinta do CSS (`--tr-ink`), sem sobrescrever.
export function tintaDaCategoria(cat, bloco) {
  const p = papelDe(cat, bloco);
  return p ? `var(--tr-papel-${p}-ink)` : undefined;
}
