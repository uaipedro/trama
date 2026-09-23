// Lê um trecho de código R que monta um fluxo com tr_add(...) e devolve um
// grafo simples: nós (id, tipo, params) e arestas (from -> to). Usado para
// desenhar o canvas de exemplo nas páginas de doc, a partir do bloco ```r.

export interface FlowNode {
  id: string;
  type: string;
  params: [string, string][];
}

export interface FlowGraph {
  nodes: FlowNode[];
  edges: [string, string][];
}

export interface FlowPosition {
  col: number;
  row: number;
}

/** Divide uma lista de argumentos por vírgulas de nível 0 (fora de (), [], "", ''). */
function splitTopLevel(text: string): string[] {
  const parts: string[] = [];
  let depth = 0;
  let quote: string | null = null;
  let current = "";
  for (let i = 0; i < text.length; i++) {
    const ch = text[i];
    if (quote) {
      current += ch;
      if (ch === quote && text[i - 1] !== "\\") quote = null;
      continue;
    }
    if (ch === '"' || ch === "'") {
      quote = ch;
      current += ch;
      continue;
    }
    if (ch === "(" || ch === "[") {
      depth++;
      current += ch;
      continue;
    }
    if (ch === ")" || ch === "]") {
      depth--;
      current += ch;
      continue;
    }
    if (ch === "," && depth === 0) {
      parts.push(current);
      current = "";
      continue;
    }
    current += ch;
  }
  if (current.trim().length > 0) parts.push(current);
  return parts.map((p) => p.trim()).filter((p) => p.length > 0);
}

/** Localiza cada chamada `tr_add(...)` no código, devolvendo o texto entre parênteses. */
function findTrAddCalls(code: string): string[] {
  const calls: string[] = [];
  const marker = "tr_add(";
  let searchFrom = 0;
  while (true) {
    const start = code.indexOf(marker, searchFrom);
    if (start === -1) break;
    const argsStart = start + marker.length;
    let depth = 1;
    let quote: string | null = null;
    let i = argsStart;
    for (; i < code.length && depth > 0; i++) {
      const ch = code[i];
      if (quote) {
        if (ch === quote && code[i - 1] !== "\\") quote = null;
        continue;
      }
      if (ch === '"' || ch === "'") {
        quote = ch;
        continue;
      }
      if (ch === "(") depth++;
      else if (ch === ")") depth--;
    }
    calls.push(code.slice(argsStart, i - 1));
    searchFrom = i;
  }
  return calls;
}

/** Tira aspas, sufixo L de inteiro, e junta c(...) numa string legível. */
function normalizeValue(raw: string): string {
  const value = raw.trim();
  const cMatch = value.match(/^c\((.*)\)$/s);
  if (cMatch) {
    return splitTopLevel(cMatch[1])
      .map((item) => normalizeValue(item))
      .join(", ");
  }
  const stringMatch = value.match(/^(["'])(.*)\1$/s);
  if (stringMatch) return stringMatch[2];
  if (/^-?\d+L$/.test(value)) return value.slice(0, -1);
  return value;
}

export function parseFlowExample(code: string): FlowGraph | null {
  const calls = findTrAddCalls(code);
  if (calls.length === 0) return null;

  const nodes: FlowNode[] = [];
  const edges: [string, string][] = [];

  for (const call of calls) {
    const args = splitTopLevel(call);
    if (args.length < 2) continue;

    const id = normalizeValue(args[0]);
    const type = normalizeValue(args[1]);
    const params: [string, string][] = [];
    let from: string[] = [];

    for (const arg of args.slice(2)) {
      const eq = arg.indexOf("=");
      if (eq === -1) continue;
      const key = arg.slice(0, eq).trim();
      const rawValue = arg.slice(eq + 1).trim();
      if (key === "from") {
        const cMatch = rawValue.match(/^c\((.*)\)$/s);
        from = cMatch
          ? splitTopLevel(cMatch[1]).map((item) => normalizeValue(item))
          : [normalizeValue(rawValue)];
        continue;
      }
      params.push([key, normalizeValue(rawValue)]);
    }

    nodes.push({ id, type, params });
    for (const parent of from) edges.push([parent, id]);
  }

  return { nodes, edges };
}

export function layoutFlow(graph: FlowGraph): Map<string, FlowPosition> {
  const parentsOf = new Map<string, string[]>();
  for (const node of graph.nodes) parentsOf.set(node.id, []);
  for (const [from, to] of graph.edges) {
    const parents = parentsOf.get(to);
    if (parents) parents.push(from);
  }

  const positions = new Map<string, FlowPosition>();
  const rowCountByCol = new Map<number, number>();

  for (const node of graph.nodes) {
    const parents = parentsOf.get(node.id) ?? [];
    let col = 0;
    for (const parent of parents) {
      const parentPos = positions.get(parent);
      if (parentPos) col = Math.max(col, parentPos.col + 1);
    }
    const row = rowCountByCol.get(col) ?? 0;
    rowCountByCol.set(col, row + 1);
    positions.set(node.id, { col, row });
  }

  return positions;
}
