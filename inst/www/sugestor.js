import { PAPEIS } from "./papeis.js";

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

// Seção "## Usos relacionados" da ajuda curta: ids entre crases no formato
// `colecao/bloco`. Só essa seção conta. Citação no meio da descrição costuma
// ser contraste ("diferente de X"), não sequência.
export function relacionados(spec) {
  const m = /##\s*Usos relacionados\s*\n([\s\S]*?)(?=\n##\s|$)/.exec(spec?.help || "");
  if (!m) return [];
  return [...m[1].matchAll(/`([a-z][a-z0-9_]*\/[a-z0-9_]+)`/g)].map((x) => x[1]);
}

const colecao = (id) => id.split("/")[0];

// Fração de `n` de cada destino entre as transições filtradas, por chave.
function fracoes(pares, chaveDe) {
  const soma = {};
  let total = 0;
  for (const t of pares) {
    const k = chaveDe(t);
    if (k == null) continue;
    soma[k] = (soma[k] || 0) + (t.n || 0);
    total += t.n || 0;
  }
  return total ? (k) => (soma[k] || 0) / total : () => 0;
}

// Transição: as observadas a partir do próprio bloco de origem. Sem nenhuma,
// back-off para categoria -> categoria (agregando os blocos das duas pontas),
// valendo metade, porque é evidência mais fraca.
function pontoTransicao(cat, byId, de) {
  const ts = cat.transitions || [];
  const diretas = ts.filter((t) => t.from === de);
  if (diretas.length) {
    const f = fracoes(diretas, (t) => t.to);
    return (spec) => f(spec.id);
  }
  const deCat = byId[de]?.category;
  if (deCat == null) return () => 0;
  const f = fracoes(ts.filter((t) => byId[t.from]?.category === deCat),
                    (t) => byId[t.to]?.category);
  return (spec) => 0.5 * f(spec.category);
}

// Papel pela mesma regra de `papeis.js`: o do bloco vence o da categoria.
const papel = (catPorId, spec) =>
  [spec.role, catPorId[spec.category]?.role].find((p) => PAPEIS.includes(p)) || null;

export function sugerir(cat, ctx) {
  const { de, tipo, presentes = [], historico = {} } = ctx;
  const byId = Object.fromEntries((cat.nodes || []).map((n) => [n.id, n]));
  const catPorId = Object.fromEntries((cat.categories || []).map((c) => [c.id, c]));
  const ordem = ordemCat(cat);
  const origem = byId[de];
  const rel = new Set(relacionados(origem));
  const trans = pontoTransicao(cat, byId, de);
  const hist = fracoes(
    Object.entries(historico || {}).filter(([k]) => k.startsWith(`${de}>`))
      .map(([k, n]) => ({ to: k.slice(de.length + 1), n })),
    (t) => t.to);
  const presentesSet = new Set(presentes);
  const citadosNoFluxo = new Set(presentes.flatMap((id) => relacionados(byId[id])));
  return aceitantes(cat, tipo).map((a) => {
    const motivos = {};
    if (origem) {
      const e = pontoEtapa(ordem, origem.category, a.spec.category,
                           colecao(de) === colecao(a.id));
      if (e) motivos.etapa = e * PESOS.etapa;
    }
    if (rel.has(a.id)) motivos.relacionado = PESOS.relacionado;
    const t = trans(a.spec);
    if (t) motivos.transicao = t * PESOS.transicao;
    const h = hist(a.id);
    if (h) motivos.historico = h * PESOS.historico;
    // Contexto: um segundo bloco de análise igual raramente faz sentido (uma
    // segunda `anova`), mas preparação repete à vontade. A penalidade usa o
    // mesmo peso do bônus, para ficar na mesma escala. Isentar também leitura e
    // saída (gráficos, exportações) foi medido e não mudou nada (hit@1/3/5
    // iguais em 210 arestas): elas quase nunca estão a montante, pois fecham o
    // fluxo. Fica a regra mais simples.
    let c = 0;
    if (presentesSet.has(a.id) && papel(catPorId, a.spec) !== "preparacao") c -= PESOS.contexto;
    if (citadosNoFluxo.has(a.id)) c += 0.5 * PESOS.contexto;
    if (c) motivos.contexto = c;
    const score = Object.values(motivos).reduce((s, v) => s + v, 0);
    return { id: a.id, porta: a.porta, score, motivos };
  }).sort((x, y) => y.score - x.score || x.id.localeCompare(y.id));
}
