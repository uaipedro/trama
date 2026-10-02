// Análise do SQL do editor: puro, sem DOM nem CodeMirror, testado em
// `tests/js/sql-lint.test.mjs`. O parser chega de fora (`node-sql-parser`,
// gramática PostgreSQL, no vendor) para o teste rodar com o mesmo bundle.
//
// O editor cobra SQL PADRÃO de propósito: o motor é o DuckDB, mas os extras
// dele (`FROM` sem `SELECT`, `QUALIFY`, arquivo no `FROM`) aparecem como erro,
// porque o bloco é para ensinar o SQL que roda em qualquer banco.
//
// Cada diagnóstico é `{ from, to, mensagem, grave? }` em offsets do texto;
// `grave` bloqueia rodar (sintaxe, tabela inexistente), o resto é aviso. Erro de
// sintaxe é um só (o parser para no primeiro); nomes desconhecidos podem ser
// vários. Na dúvida, não acusa: um aviso falso ensina errado.

const OPCOES = { database: "PostgresQL" };

const EXTRAS = [
  [/\bqualify\b/i, "QUALIFY é extensão do DuckDB, não SQL padrão. Filtre a janela numa subconsulta (WITH)."],
  [/\b(un)?pivot\b/i, "PIVOT é extensão do DuckDB, não SQL padrão. Use GROUP BY com CASE WHEN."],
  [/\*\s*exclude\b/i, "SELECT * EXCLUDE é extensão do DuckDB. Liste as colunas que quer."],
];

export function analisar(sql, { parser, tabelas = [] } = {}) {
  const texto = sql ?? "";
  if (!texto.trim()) return { ok: true, diagnosticos: [] };

  const inicio = texto.search(/\S/);
  const primeira = (texto.slice(inicio).match(/^[a-z_]+/i) || [""])[0].toLowerCase();
  if (primeira !== "select" && primeira !== "with" && primeira !== "(") {
    const msg = primeira === "from"
      ? "Comece com SELECT: \"FROM tabela\" sozinho é extensão do DuckDB. Escreva SELECT * FROM tabela."
      : "A consulta precisa começar com SELECT (ou WITH). Este bloco só lê dados.";
    return erro(inicio, inicio + Math.max(primeira.length, 1), msg);
  }
  for (const [re, msg] of EXTRAS) {
    const m = re.exec(texto);
    if (m) return erro(m.index, m.index + m[0].length, msg);
  }

  const virgula = /,(\s*)(from|where|group\s+by|order\s+by|having|limit)\b/i.exec(texto);
  if (virgula) {
    return erro(virgula.index, virgula.index + 1,
                `Vírgula sobrando antes de ${virgula[2].replace(/\s+/, " ").toUpperCase()}.`);
  }

  let ast;
  try {
    ast = parser.astify(texto, OPCOES);
  } catch (e) {
    return erroDeSintaxe(texto, e);
  }
  if (Array.isArray(ast) && ast.length > 1) {
    const i = texto.indexOf(";");
    return erro(i, i + 1, "Uma consulta por bloco: tire o que vem depois do ;");
  }
  // Tabela que não existe é erro certo (bloqueia rodar); coluna desconhecida
  // fica como aviso, porque a checagem de coluna é a parte aproximada.
  const ds = nomes(texto, parser, ast, tabelas);
  return { ok: !ds.some((d) => d.grave), diagnosticos: ds };
}

function erro(from, to, mensagem) {
  return { ok: false, diagnosticos: [{ from, to: Math.max(to, from + 1), mensagem, grave: true }] };
}

// O parser (peggy) diz o que esperava; isso não ensina nada a quem aprende.
// Os casos frequentes ganham frase própria; o resto aponta o lugar.
function erroDeSintaxe(texto, e) {
  const pos = e.location?.start?.offset ?? texto.length;
  const antes = texto.slice(0, pos);
  const achou = texto.slice(pos).match(/^\S+/)?.[0] ?? null;
  const fim = texto.trimEnd().length;
  if (achou && /^(from|where|group|order|having|limit)$/i.test(achou) && /,\s*$/.test(antes)) {
    const v = antes.lastIndexOf(",");
    return erro(v, v + 1, `Vírgula sobrando antes de ${achou.toUpperCase()}.`);
  }
  if (!achou || pos >= fim) {
    const ult = (antes.trimEnd().match(/[a-z_]+$/i) || [""])[0].toUpperCase();
    return erro(Math.max(fim - 1, 0), fim, ult
      ? `A consulta terminou logo depois de ${ult}: falta completar.`
      : "A consulta terminou no meio: falta completar.");
  }
  return erro(pos, pos + achou.length, `SQL inválido perto de "${achou}".`);
}

// Tabelas e colunas que não existem na fonte. Só acusa o que tem certeza:
// nomes de CTE contam como tabela; coluna sem tabela só é conferida quando o
// FROM tem apenas tabelas da fonte; apelidos do SELECT (`AS k`) valem no
// ORDER BY.
function nomes(texto, parser, ast, tabelas) {
  if (!tabelas.length) return [];
  const fonte = new Map(tabelas.map((t) => [t.nome.toLowerCase(), t]));
  const ctes = new Set();
  const apelidos = new Set();
  let derivadas = false;
  visitar(ast, (n) => {
    if (n && Array.isArray(n.with)) for (const w of n.with) {
      const nome = w?.name?.value ?? w?.name;
      if (typeof nome === "string") ctes.add(nome.toLowerCase());
    }
    if (n && Array.isArray(n.columns)) for (const c of n.columns) {
      const as = typeof c?.as === "string" ? c.as : c?.as?.value;
      if (as) apelidos.add(String(as).toLowerCase());
    }
    if (n && Array.isArray(n.from)) for (const f of n.from) if (f?.expr?.ast || f?.expr?.type) derivadas = true;
  });

  const out = [];
  const usadas = [];
  let lista = [];
  try { lista = parser.tableList(texto, OPCOES); } catch (_) { return out; }
  for (const item of lista) {
    const nome = item.split("::").slice(2).join("::");
    const chave = nome.toLowerCase();
    if (ctes.has(chave)) continue;
    if (fonte.has(chave)) { usadas.push(fonte.get(chave)); continue; }
    const pos = achar(texto, nome);
    const msg = /\.(csv|parquet|json)$/i.test(nome)
      ? `Ler arquivo direto no FROM é extensão do DuckDB. Use a tabela da fonte${sugestao(nome.replace(/\.[^.]+$/, ""), [...fonte.keys()])}.`
      : `A tabela "${nome}" não existe na fonte.${sugestao(nome, [...fonte.keys()], " Quis dizer")}`;
    out.push({ from: pos, to: pos + nome.length, mensagem: msg, grave: true });
  }
  if (out.length || ctes.size || derivadas) return out;

  let cols = [];
  try { cols = parser.columnList(texto, OPCOES); } catch (_) { return out; }
  const todas = new Set(usadas.flatMap((t) => t.colunas.map((c) => c.nome.toLowerCase())));
  for (const item of cols) {
    const [, tab, col] = item.split("::");
    if (!col || col === "(.*)" || col === "*") continue;
    const chave = col.toLowerCase();
    if (apelidos.has(chave)) continue;
    // `columnList` lista também literais de texto ('MG') como coluna.
    if (new RegExp(`'${col.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")}'`, "i").test(texto)) continue;
    let existe, candidatas;
    if (tab && tab !== "null" && fonte.has(tab.toLowerCase())) {
      candidatas = fonte.get(tab.toLowerCase()).colunas.map((c) => c.nome);
      existe = candidatas.some((c) => c.toLowerCase() === chave);
    } else if (!tab || tab === "null") {
      candidatas = [...todas];
      existe = todas.has(chave);
    } else continue;
    if (existe) continue;
    const pos = achar(texto, col);
    if (pos < 0) continue;
    out.push({ from: pos, to: pos + col.length,
               mensagem: `A coluna "${col}" não existe${tab && tab !== "null" ? ` em ${tab}` : " nas tabelas do FROM"}.${sugestao(col, candidatas, " Quis dizer")}` });
  }
  return out;
}

function visitar(n, f) {
  if (!n || typeof n !== "object") return;
  f(n);
  for (const v of Object.values(n)) {
    if (Array.isArray(v)) v.forEach((x) => visitar(x, f));
    else if (v && typeof v === "object") visitar(v, f);
  }
}

// Primeira ocorrência do nome como palavra inteira (com ou sem aspas).
function achar(texto, nome) {
  const esc = nome.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
  const m = new RegExp(`(^|[^\\w])["'\`]?(${esc})(?![\\w])`, "i").exec(texto);
  return m ? m.index + m[0].length - m[2].length : 0;
}

function sugestao(nome, candidatos, prefixo = " Quis dizer") {
  const p = maisProximo(nome, candidatos);
  return p ? `${prefixo} "${p}"?` : "";
}

export function maisProximo(nome, candidatos) {
  const alvo = nome.toLowerCase();
  let melhor = null, dist = Infinity;
  for (const c of candidatos) {
    const d = levenshtein(alvo, c.toLowerCase());
    if (d < dist) { dist = d; melhor = c; }
  }
  return melhor && dist <= Math.max(2, Math.floor(alvo.length / 3)) ? melhor : null;
}

function levenshtein(a, b) {
  const d = Array.from({ length: b.length + 1 }, (_, i) => i);
  for (let i = 1; i <= a.length; i++) {
    let prev = d[0]; d[0] = i;
    for (let j = 1; j <= b.length; j++) {
      const tmp = d[j];
      d[j] = Math.min(d[j] + 1, d[j - 1] + 1, prev + (a[i - 1] === b[j - 1] ? 0 : 1));
      prev = tmp;
    }
  }
  return d[b.length];
}

// Esquema que o editor usa: vem do handle da porta `fonte` (preview do tipo
// `sql/source`). Também é o formato do autocompletar do CodeMirror.
export function tabelasDoHandle(handle) {
  const t = handle?.preview?.data?.tabelas;
  if (!Array.isArray(t)) return [];
  return t.map((x) => ({ nome: String(x.nome), colunas: (x.colunas || []).map((c) => ({ nome: String(c.nome), tipo: c.tipo })) }));
}

export function esquemaCompletar(tabelas) {
  const out = {};
  for (const t of tabelas) out[t.nome] = t.colunas.map((c) => c.nome);
  return out;
}
