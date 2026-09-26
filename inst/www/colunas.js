// Regras de sugestão de colunas. Módulo puro (sem React), pelo mesmo motivo
// de `sugestor.js`: `editor.js` não carrega sob `node --test`, e é aqui que
// mora a decisão — o editor só desenha o select e despacha as ops.
//
// `schema` é o do handle de um data.frame (`{colunas, truncado, amostra}`)
// ou null quando a entrada ainda não rodou. Com null, tudo devolve vazio:
// sem tabela não há o que oferecer, e sugerir às cegas seria pior que nada.

const NOME_PAPEL = { numerica: "numérica", categorica: "categórica", tempo: "de tempo", qualquer: "" };

// Params `cols` antigos não declaram role/multi: servem para qualquer coluna,
// aceitam várias, e nunca recebem sugestão (não sabemos o que querem).
export const anotado = (spec) => spec?.kind === "cols" && !!spec.role;
const papelDe = (spec) => spec.role || "qualquer";
const serve = (spec, c) => papelDe(spec) === "qualquer" || c.papel === papelDe(spec);
const vazio = (v) => v === "" || v === null || v === undefined;
const nomes = (v) => (vazio(v) ? [] : String(v).split(",").map((s) => s.trim()).filter(Boolean));

export function opcoes(spec, schema) {
  if (!schema) return [];
  return (schema.colunas || []).map((c) => ({ nome: c.nome, papel: c.papel, serve: serve(spec, c) }));
}

// Categórica com 2..30 níveis é agrupador; acima disso quase sempre é id
// (150 linhas, 150 valores) e renderia um boxplot por linha. Sem agrupador,
// não sugerimos nada: um id no lugar do grupo é pior que o campo vazio.
const agrupador = (c) => c.n_distintos != null && c.n_distintos >= 2 && c.n_distintos <= 30;

// `forcar`: nome de um param para calcular MESMO com `suggest: false` — é o
// "sugerir: X" a pedido do select. Ao conectar, param opcional (cor, rótulo,
// grupo) nunca é preenchido sozinho: mudaria a análise sem o usuário pedir.
export function sugerir(params, valores, sugeridos, schema, { forcar } = {}) {
  if (!schema) return [];
  const val = valores || {};
  const re = new Set(sugeridos || []);
  const alvo = (p) => anotado(p) && !p.multi && (p.suggest !== false || p.name === forcar) &&
    (vazio(val[p.name]) || re.has(p.name));
  // Usadas: o que o usuário escolheu (params anotados que não vamos mexer).
  // As sugestões desta passada entram conforme são feitas; quem vai ser
  // re-sugerido não bloqueia a si mesmo — senão nunca trocaria de tabela.
  const usadas = new Set();
  for (const p of params) if (anotado(p) && !alvo(p)) nomes(val[p.name]).forEach((n) => usadas.add(n));

  const out = [];
  for (const p of params) {
    if (!alvo(p)) continue;
    const livres = (schema.colunas || []).filter((c) => serve(p, c) && !usadas.has(c.nome));
    const escolha = p.role === "categorica" ? livres.find(agrupador) : livres[0];
    if (!escolha) continue; // não limpa: melhor um valor velho que um vazio mudo
    usadas.add(escolha.nome);
    if (escolha.nome === val[p.name]) continue;
    const papel = NOME_PAPEL[papelDe(p)];
    const motivo = p.role === "categorica"
      ? `1ª coluna categórica com poucos níveis ainda não usada`
      : `1ª coluna${papel ? " " + papel : ""} ainda não usada`;
    out.push({ name: p.name, value: escolha.nome, motivo });
  }
  return out;
}

// Truncado = a lista de colunas é parcial; acusar "sumiu" seria falso alarme.
export function sumidas(params, valores, schema) {
  if (!schema || schema.truncado) return [];
  const existe = new Set((schema.colunas || []).map((c) => c.nome));
  const out = [];
  for (const p of params) {
    if (p.kind !== "cols") continue;
    const faltam = nomes((valores || {})[p.name]).filter((n) => !existe.has(n));
    if (faltam.length) out.push({ name: p.name, faltam });
  }
  return out;
}

// Palpite para "a coluna X sumiu — quis dizer Y?". Deliberadamente simples:
// mesma grafia sem caixa, depois prefixo comum ≥ 3 ou um contém o outro.
export function alternativa(spec, faltando, schema) {
  if (!schema || vazio(faltando)) return null;
  const f = String(faltando).toLowerCase();
  const cands = (schema.colunas || []).filter((c) => serve(spec, c) && c.nome !== faltando);
  const igual = cands.find((c) => c.nome.toLowerCase() === f);
  if (igual) return igual.nome;
  const prefixo = (a, b) => { let i = 0; while (i < a.length && a[i] === b[i]) i++; return i; };
  const par = cands.find((c) => {
    const n = c.nome.toLowerCase();
    return n.includes(f) || f.includes(n) || prefixo(n, f) >= 3;
  });
  return par ? par.nome : null;
}

export function opsDeSugestao(nodeId, params, valores, sugeridos, schema) {
  return sugerir(params, valores, sugeridos, schema).map(({ name, value }) => ({
    op: "set_param", node: nodeId, name, value, origem: "sugestao",
  }));
}
