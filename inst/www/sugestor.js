import { PAPEIS } from "./papeis.js";

// Ranking do "próximo bloco". Módulo puro (sem React), pelo mesmo motivo de
// `modos.js`: `editor.js` não carrega sob `node --test`.
//
// A pontuação é uma soma de camadas, cada uma com o seu peso, e cada item
// guarda os motivos, para o popover poder dizer POR QUE sugeriu.

export const PESOS = { etapa: 1, relacionado: 2, transicao: 3, contexto: 1.5, historico: 2.5 };

// Multiplicadores de PESOS.contexto para blocos já a montante. Medido em
// avaliar.mjs: penalizar preparo igual à origem piora (filtro -> filtro é
// real); penalizar preparo mais acima da origem melhora hit@3.
export const PENAL = { origem: 2, origemPrep: 0, presente: 1.5, presentePrep: 1.5 };

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
  // Histórico pessoal: contagem própria de cada destino, sem normalizar pela
  // soma (normalizar diluía uma escolha explícita quando havia outras).
  // count/(count+1) satura em PESOS.historico: 1 escolha já vale metade.
  const contagem = Object.fromEntries(Object.entries(historico || {})
    .filter(([k, n]) => k.startsWith(`${de}>`) && n > 0)
    .map(([k, n]) => [k.slice(de.length + 1), n]));
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
    const nh = contagem[a.id] || 0;
    if (nh) motivos.historico = (nh / (nh + 1)) * PESOS.historico;
    // Contexto: um segundo bloco de análise igual raramente faz sentido (uma
    // segunda `anova`); preparação só repete logo em seguida. A penalidade usa o
    // mesmo peso do bônus, para ficar na mesma escala. Isentar também leitura e
    // saída (gráficos, exportações) foi medido e não mudou nada (hit@1/3/5
    // iguais em 210 arestas): elas quase nunca estão a montante, pois fecham o
    // fluxo. Fica a regra mais simples.
    let c = 0;
    // Voltar a um bloco já a montante gera o pingue-pongue do Tab (Converter
    // -> Resumo -> Converter...), então preparação também cai quando está
    // acima da origem. Inspeção não é isenta: costuma ser uma por caminho.
    const prep = papel(catPorId, a.spec) === "preparacao";
    if (a.id === de) c -= PESOS.contexto * (prep ? PENAL.origemPrep : PENAL.origem);
    else if (presentesSet.has(a.id)) c -= PESOS.contexto * (prep ? PENAL.presentePrep : PENAL.presente);
    if (citadosNoFluxo.has(a.id)) c += 0.5 * PESOS.contexto;
    if (c) motivos.contexto = c;
    const score = Object.values(motivos).reduce((s, v) => s + v, 0);
    return { id: a.id, porta: a.porta, score, motivos };
  }).sort((x, y) => y.score - x.score || x.id.localeCompare(y.id));
}

// Todo bloco com alguma SAÍDA que alimenta `tipo`: o espelho de `aceitantes`,
// para a sugestão de origem (arrasto a partir de uma porta de entrada).
export function emitentes(cat, tipo) {
  const out = [];
  for (const n of cat.nodes || []) {
    const p = (n.outputs || []).find((o) => compativel(cat, o.type, tipo));
    if (p) out.push({ id: n.id, porta: p.name, spec: n });
  }
  return out;
}

// Origem: o que viria ANTES de `para`, cuja entrada espera `tipo`. As mesmas
// camadas de `sugerir`, com as pontas trocadas: etapa conta da categoria do
// candidato até a de `para`, a transição olha as que CHEGAM em `para`, e o
// relacionado é o candidato citar `para` na própria ajuda.
export function sugerirOrigem(cat, ctx) {
  const { para, tipo, presentes = [], historico = {} } = ctx;
  const byId = Object.fromEntries((cat.nodes || []).map((n) => [n.id, n]));
  const ordem = ordemCat(cat);
  const alvo = byId[para];
  const ts = cat.transitions || [];
  const diretas = ts.filter((t) => t.to === para);
  let trans;
  if (diretas.length) {
    const f = fracoes(diretas, (t) => t.from);
    trans = (spec) => f(spec.id);
  } else if (alvo?.category != null) {
    const f = fracoes(ts.filter((t) => byId[t.to]?.category === alvo.category),
                      (t) => byId[t.from]?.category);
    trans = (spec) => 0.5 * f(spec.category);
  } else trans = () => 0;
  // Mesma saturação de `sugerir`: normalizar pela soma diluiria a escolha
  // explícita a cada escolha nova.
  const contagem = Object.fromEntries(
    Object.entries(historico || {}).filter(([k]) => k.endsWith(`>${para}`))
      .map(([k, n]) => [k.slice(0, k.length - para.length - 1), n]));
  const presentesSet = new Set(presentes);
  return emitentes(cat, tipo).map((a) => {
    const motivos = {};
    if (alvo) {
      const e = pontoEtapa(ordem, a.spec.category, alvo.category,
                           colecao(para) === colecao(a.id));
      if (e) motivos.etapa = e * PESOS.etapa;
    }
    if (relacionados(a.spec).includes(para)) motivos.relacionado = PESOS.relacionado;
    const t = trans(a.spec);
    if (t) motivos.transicao = t * PESOS.transicao;
    const nh = contagem[a.id];
    if (nh) motivos.historico = (nh / (nh + 1)) * PESOS.historico;
    // `presentes` é o que já alimenta o alvo: uma segunda cópia a montante
    // repete trabalho, então pesa como em `sugerir`, não como bônus.
    if (presentesSet.has(a.id)) motivos.contexto = -PENAL.presente * PESOS.contexto;
    const score = Object.values(motivos).reduce((s, v) => s + v, 0);
    return { id: a.id, porta: a.porta, score, motivos };
  }).sort((x, y) => y.score - x.score || x.id.localeCompare(y.id));
}

// Blocos que cabem no MEIO de uma aresta `tipoDe -> tipoPara`: alguma entrada
// aceita `tipoDe` e alguma saída alimenta `tipoPara`. `porta` é a entrada e
// `saida` a saída que a inserção vai ligar (as primeiras compatíveis).
export function intermediarios(cat, tipoDe, tipoPara) {
  const out = [];
  for (const n of cat.nodes || []) {
    const i = (n.inputs || []).find((p) => compativel(cat, tipoDe, p.type));
    const o = (n.outputs || []).find((p) => compativel(cat, p.type, tipoPara));
    if (i && o) out.push({ id: n.id, porta: i.name, saida: o.name });
  }
  return out;
}
