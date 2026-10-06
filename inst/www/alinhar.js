// inst/www/alinhar.js — empilhar, alinhar e distribuir, sem React nem xyflow.
//
// Módulo puro (o `editor.js` não carrega sob `node --test`). Tudo recebe
// retângulos `{id, x, y, w, h}` já MEDIDOS (largura/altura de verdade, não a
// estimada: card em mini, completo ou redimensionado tem tamanhos distintos) e
// devolve só o que mudou de lugar, `[{id, x, y}]`. Quem emite os `move`
// /`update_*` num batch só é o editor.

export const GAP_PADRAO = 32;
const EPS = 0.5;

// Eixo "x" (largura) ou "y" (altura): os dois lados do código são o mesmo,
// só trocam as chaves.
const EIXO = { x: { pos: "x", tam: "w" }, y: { pos: "y", tam: "h" } };
const caixa = (rs) => ({
  x0: Math.min(...rs.map((r) => r.x)), y0: Math.min(...rs.map((r) => r.y)),
  x1: Math.max(...rs.map((r) => r.x + r.w)), y1: Math.max(...rs.map((r) => r.y + r.h)),
});
const ini = (b, e) => (e === "x" ? b.x0 : b.y0);
const fim = (b, e) => (e === "x" ? b.x1 : b.y1);

// Posição de `r` no eixo `e` para o estado de alinhamento `k`
// (0 início, 1 centro, 2 fim) dentro da caixa `b`.
function posNoEstado(r, e, k, b) {
  const { tam } = EIXO[e];
  if (k === 0) return ini(b, e);
  if (k === 1) return (ini(b, e) + fim(b, e)) / 2 - r[tam] / 2;
  return fim(b, e) - r[tam];
}

// Empilha na ordem em que os itens estão hoje (de cima para baixo em "v", da
// esquerda para a direita em "h"), com `gap` entre eles. A caixa da seleção
// fica ancorada no canto superior esquerdo, e o eixo cruzado vai centralizado
// no centro dessa caixa.
export function empilhar(rs, sentido, gap = GAP_PADRAO) {
  if (rs.length < 2) return [];
  const e = sentido === "v" ? "y" : "x";
  const c = e === "y" ? "x" : "y";
  const b = caixa(rs);
  const ord = [...rs].sort((a, z) => a[e] - z[e] || a[c] - z[c]);
  let cursor = ini(b, e);
  return ord.map((r) => {
    const o = { id: r.id, [e]: cursor, [c]: posNoEstado(r, c, 1, b) };
    cursor += r[EIXO[e].tam] + gap;
    return o;
  });
}

// Estados (0 início, 1 centro, 2 fim) em que a seleção JÁ está no eixo `e`.
// Itens de mesma largura estão em todos ao mesmo tempo.
function estadosAtuais(rs, e, b) {
  const { pos } = EIXO[e];
  return [0, 1, 2].filter((k) => rs.every((r) => Math.abs(r[pos] - posNoEstado(r, e, k, b)) <= EPS));
}

// Um passo de alinhamento. `dir` é a seta: "left"/"up" andam para o início,
// "right"/"down" para o fim, e a seta horizontal mexe no eixo x, a vertical no
// y. Do centro vai-se ao início, do fim ao centro, e assim por diante; sem
// estado reconhecido, vai direto à borda da seta. Já na borda (ou com todos
// iguais em tudo), não mexe em nada.
export function alinhar(rs, dir) {
  if (rs.length < 2) return [];
  const e = dir === "left" || dir === "right" ? "x" : "y";
  const paraInicio = dir === "left" || dir === "up";
  const b = caixa(rs);
  const S = estadosAtuais(rs, e, b);
  let alvo;
  if (!S.length) alvo = paraInicio ? 0 : 2;
  else alvo = paraInicio ? Math.min(...S) - 1 : Math.max(...S) + 1;
  if (alvo < 0 || alvo > 2) return [];
  const { pos } = EIXO[e];
  return rs.map((r) => ({ id: r.id, [pos]: posNoEstado(r, e, alvo, b) }))
           .filter((o, i) => Math.abs(o[pos] - rs[i][pos]) > EPS);
}

// Alinha em UM estado dado (menu e barra: "alinhar à esquerda", "ao meio"...).
// `lado`: "esq" | "centro-h" | "dir" | "topo" | "meio" | "base".
const LADOS = { esq: ["x", 0], "centro-h": ["x", 1], dir: ["x", 2],
                topo: ["y", 0], meio: ["y", 1], base: ["y", 2] };
export function alinharA(rs, lado) {
  if (rs.length < 2 || !LADOS[lado]) return [];
  const [e, k] = LADOS[lado];
  const b = caixa(rs);
  const { pos } = EIXO[e];
  return rs.map((r) => ({ id: r.id, [pos]: posNoEstado(r, e, k, b) }))
           .filter((o, i) => Math.abs(o[pos] - rs[i][pos]) > EPS);
}

// Primeiro e último ficam onde estão; os vãos entre todos viram iguais. Pede 3
// ou mais (com 2 não há o que igualar).
export function distribuir(rs, sentido) {
  if (rs.length < 3) return [];
  const e = sentido === "v" ? "y" : "x";
  const { pos, tam } = EIXO[e];
  const ord = [...rs].sort((a, z) => a[pos] - z[pos]);
  const ultimo = ord[ord.length - 1];
  const soma = ord.reduce((s, r) => s + r[tam], 0);
  const vao = (ultimo[pos] + ultimo[tam] - ord[0][pos] - soma) / (ord.length - 1);
  let cursor = ord[0][pos];
  return ord.map((r) => {
    const o = { id: r.id, [pos]: cursor };
    cursor += r[tam] + vao;
    return o;
  }).filter((o) => {
    const r = rs.find((q) => q.id === o.id);
    return Math.abs(o[pos] - r[pos]) > EPS;
  });
}

// Grupos: `grupos` é `{idGrupo: [ids]}` (o `ui.grupos` do documento). Um id
// pertence a no máximo um grupo, então o grupo de um id é único.
export function grupoDe(grupos, id) {
  for (const g of Object.keys(grupos || {})) if ((grupos[g] || []).includes(id)) return g;
  return null;
}

// Fecho da seleção: quem tem grupo leva o grupo inteiro. `existentes` é o
// conjunto de ids vivos no canvas; membro que não existe mais não conta (o
// servidor já poda, mas o eco pode atrasar).
export function expandirGrupos(ids, grupos, existentes) {
  const vivo = existentes ? (id) => existentes.has(id) : () => true;
  const out = new Set(ids);
  for (const id of ids) {
    const g = grupoDe(grupos, id);
    if (g) grupos[g].filter(vivo).forEach((m) => out.add(m));
  }
  return [...out];
}

// Ids de grupo tocados por `ids`: o que `agrupar` precisa desfazer antes de
// criar o grupo novo (não aninha), e o que `desagrupar` remove.
export function gruposDe(ids, grupos) {
  return [...new Set(ids.map((id) => grupoDe(grupos, id)).filter(Boolean))];
}
