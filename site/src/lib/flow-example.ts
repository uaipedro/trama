// Lê um trecho de código R que monta um ou mais fluxos com tr_flow(...) |>
// tr_add(...) e devolve uma lista de grafos simples: nós (id, tipo, params) e
// arestas (from -> to). Usado para desenhar os mini-canvas de exemplo nas
// páginas de doc, a partir do bloco ```r. Cada `tr_flow(` inicia um novo
// grafo; sem nenhum `tr_flow(` no bloco, todo o código é um único grafo.

export interface FlowNode {
  id: string;
  type: string;
  params: [string, string][];
}

/** Aresta: origem, destino e, quando se sabe, a porta de cada lado. */
export type FlowEdge = [string, string, string?, string?];

export interface FlowGraph {
  nodes: FlowNode[];
  edges: FlowEdge[];
  /** Posição de cada nó no canvas (`ui.positions` do documento). */
  positions?: Record<string, [number, number]>;
}

export interface FlowPosition {
  col: number;
  row: number;
}

export interface ParseFlowExampleOptions {
  /** Nome do arquivo de origem, usado só para identificar avisos no build. */
  fileLabel?: string;
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

/**
 * Apaga comentários de linha (`#` até o fim da linha, fora de strings),
 * trocando os caracteres por espaços — preserva o comprimento e os índices
 * do texto original, então tudo que já procurava por índice continua válido.
 */
function blankComments(code: string): string {
  let out = "";
  let quote: string | null = null;
  let i = 0;
  while (i < code.length) {
    const ch = code[i];
    if (quote) {
      out += ch;
      if (ch === quote && code[i - 1] !== "\\") quote = null;
      i++;
      continue;
    }
    if (ch === '"' || ch === "'") {
      quote = ch;
      out += ch;
      i++;
      continue;
    }
    if (ch === "#") {
      while (i < code.length && code[i] !== "\n") {
        out += " ";
        i++;
      }
      continue;
    }
    out += ch;
    i++;
  }
  return out;
}

/** Diz se o código tem uma chamada `tr_add(` fora de comentários. */
export function hasFlowCalls(code: string): boolean {
  return blankComments(code).includes("tr_add(");
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

/** Divide o código (já sem comentários) em um trecho por pipeline `tr_flow(`. */
function splitPipelines(maskedCode: string): string[] {
  const marker = "tr_flow(";
  const indices: number[] = [];
  let searchFrom = 0;
  while (true) {
    const idx = maskedCode.indexOf(marker, searchFrom);
    if (idx === -1) break;
    indices.push(idx);
    searchFrom = idx + marker.length;
  }
  if (indices.length === 0) return [maskedCode];

  const segments: string[] = [];
  for (let i = 0; i < indices.length; i++) {
    const start = i === 0 ? 0 : indices[i];
    const end = i + 1 < indices.length ? indices[i + 1] : maskedCode.length;
    segments.push(maskedCode.slice(start, end));
  }
  return segments;
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

/** Se `arg` é da forma `chave = valor` (nomeado), devolve {key, value}; senão null. */
function parseNamedArg(arg: string): { key: string; value: string } | null {
  const m = arg.match(/^([A-Za-z_.][A-Za-z0-9_.]*)\s*=(?!=)([\s\S]*)$/);
  if (!m) return null;
  return { key: m[1], value: m[2].trim() };
}

/**
 * Determina id e tipo a partir dos argumentos de um `tr_add(...)`, aceitando
 * posição (id, type, ...) ou nomeados `id =` / `type =` em qualquer uma das
 * duas primeiras posições. Devolve null quando não dá para determinar os dois.
 */
function extractIdType(
  args: string[],
): { id: string; type: string; rest: string[] } | null {
  let id: string | undefined;
  let type: string | undefined;
  const rest: string[] = [];

  for (let i = 0; i < args.length; i++) {
    const arg = args[i];
    if (i >= 2) {
      rest.push(arg);
      continue;
    }
    const named = parseNamedArg(arg);
    if (named && named.key === "id") {
      id = normalizeValue(named.value);
      continue;
    }
    if (named && named.key === "type") {
      type = normalizeValue(named.value);
      continue;
    }
    if (named) {
      // nomeado, mas não é id/type (ex.: from na posição 0/1) — vira param.
      rest.push(arg);
      continue;
    }
    // posicional: preenche o primeiro slot ainda livre.
    if (id === undefined) {
      id = normalizeValue(arg);
      continue;
    }
    if (type === undefined) {
      type = normalizeValue(arg);
      continue;
    }
    rest.push(arg);
  }

  if (id === undefined || type === undefined) return null;
  return { id, type, rest };
}

/** Lê os `tr_add(...)` de um trecho de pipeline; null se o grafo for inválido. */
function parsePipeline(segment: string): FlowGraph | null {
  const calls = findTrAddCalls(segment);
  if (calls.length === 0) return null;

  const nodes: FlowNode[] = [];
  const edges: FlowEdge[] = [];
  const seenIds = new Set<string>();

  for (const call of calls) {
    const args = splitTopLevel(call);
    const parsed = extractIdType(args);
    if (!parsed) return null;
    const { id, type, rest } = parsed;
    if (seenIds.has(id)) return null;
    seenIds.add(id);

    const params: [string, string][] = [];
    let from: string[] = [];

    for (const arg of rest) {
      const named = parseNamedArg(arg);
      if (!named) continue;
      const { key, value: rawValue } = named;
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
    // `from = "separar:teste"` escolhe a porta de saída.
    for (const parent of from) {
      const [no, porta] = parent.split(":");
      edges.push(porta ? [no, id, porta] : [no, id]);
    }
  }

  const idSet = new Set(nodes.map((n) => n.id));
  for (const [from] of edges) {
    if (!idSet.has(from)) return null;
  }

  return { nodes, edges };
}

export function parseFlowExample(
  code: string,
  options: ParseFlowExampleOptions = {},
): FlowGraph[] | null {
  const masked = blankComments(code);
  if (!masked.includes("tr_add(")) return null;

  const segments = splitPipelines(masked);
  const graphs: FlowGraph[] = [];

  for (const segment of segments) {
    if (!segment.includes("tr_add(")) continue;
    const graph = parsePipeline(segment);
    if (!graph) {
      const label = options.fileLabel ? ` (${options.fileLabel})` : "";
      console.warn(
        `[flow-example] grafo inválido${label} — id repetido, from sem destino ou id/type indeterminado; mantendo o bloco de código original.`,
      );
      return null;
    }
    graphs.push(graph);
  }

  return graphs.length > 0 ? graphs : null;
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
