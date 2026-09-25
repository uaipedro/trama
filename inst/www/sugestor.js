// Ranking do "próximo bloco". Módulo puro (sem React), pelo mesmo motivo de
// `modos.js`: `editor.js` não carrega sob `node --test`.
//
// A pontuação é uma soma de camadas, cada uma com o seu peso, e cada item
// guarda os motivos, para o popover poder dizer POR QUE sugeriu.

export const PESOS = { etapa: 1, relacionado: 2, transicao: 3, contexto: 1.5, historico: 2.5 };

export function compativel(cat, de, para) {
  if (de === para) return true;
  return (cat.adapters || []).some((a) => a.from === de && a.to === para);
}

// Todo bloco com alguma entrada que aceita `tipo`, com a porta que a conexão
// automática vai usar: a primeira compatível, na ordem declarada.
export function aceitantes(cat, tipo) {
  const out = [];
  for (const n of cat.nodes || []) {
    const p = (n.inputs || []).find((i) => compativel(cat, tipo, i.type));
    if (p) out.push({ id: n.id, porta: p.name, spec: n });
  }
  return out;
}

const ordemCat = (cat) => Object.fromEntries((cat.categories || []).map((c, i) => [c.id, i]));

// Etapa: a categoria SEGUINTE à do bloco de origem pontua 1, a mesma 0.5,
// e as demais para frente decaem. Categorias de coleções diferentes não
// têm ordem entre si, então pontuam 0 (nem ganham nem perdem).
function pontoEtapa(ordem, deCat, paraCat, mesmaColecao) {
  if (!mesmaColecao || !(deCat in ordem) || !(paraCat in ordem)) return 0;
  const d = ordem[paraCat] - ordem[deCat];
  if (d === 1) return 1;
  if (d === 0) return 0.5;
  if (d > 1) return 1 / d;
  return 0;
}

const colecao = (id) => id.split("/")[0];

export function sugerir(cat, ctx) {
  const { de, tipo } = ctx;
  const byId = Object.fromEntries((cat.nodes || []).map((n) => [n.id, n]));
  const ordem = ordemCat(cat);
  const origem = byId[de];
  return aceitantes(cat, tipo).map((a) => {
    const motivos = {};
    if (origem) {
      const e = pontoEtapa(ordem, origem.category, a.spec.category,
                           colecao(de) === colecao(a.id));
      if (e) motivos.etapa = e * PESOS.etapa;
    }
    const score = Object.values(motivos).reduce((s, v) => s + v, 0);
    return { id: a.id, porta: a.porta, score, motivos };
  }).sort((x, y) => y.score - x.score || x.id.localeCompare(y.id));
}
