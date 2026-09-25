// Plugin rehype: nas páginas de bloco (frontmatter `node:`), insere as seções
// Pressupostos e Referências logo depois de "Quando usar" — ou, sem ela,
// depois de "O que o bloco faz". O conteúdo vem de src/data/node-docs.json,
// gerado do núcleo por tools/site/export-node-docs.R.

import { existsSync, readFileSync, statSync } from "node:fs";
import { join } from "node:path";
import type { Element, ElementContent, Root, RootContent } from "hast";
import type { VFile } from "vfile";
import { frontmatterField, nodePages } from "./rehype-doc-links.ts";
import { pressupostosHast, referenciasHast, SITE_LANG, type NodeDocs, type ResolveBlock } from "./node-docs.ts";

const text = (node: ElementContent | RootContent): string =>
  node.type === "text" ? node.value : "children" in node ? node.children.map(text).join("") : "";

const isH = (node: RootContent, re: RegExp) => node.type === "element" && re.test(node.tagName);

/** Índice logo após a seção de título `titulo` (até o próximo h1/h2), ou -1. */
export function afterSection(tree: Root, titulo: string): number {
  const start = tree.children.findIndex((n) => isH(n, /^h2$/) && text(n).trim() === titulo);
  if (start < 0) return -1;
  let end = start + 1;
  while (end < tree.children.length && !isH(tree.children[end], /^h[12]$/)) end += 1;
  return end;
}

export function insertNodeDocs(tree: Root, docs: NodeDocs, resolve: ResolveBlock, lang = SITE_LANG): void {
  const secoes: Element[] = [...pressupostosHast(docs.pressupostos, resolve, lang), ...referenciasHast(docs.referencias, lang)];
  if (!secoes.length) return;
  let at = afterSection(tree, "Quando usar");
  if (at < 0) at = afterSection(tree, "O que o bloco faz");
  if (at < 0) at = tree.children.length;
  tree.children.splice(at, 0, ...secoes);
}

function titleOf(page: string): string | undefined {
  const dir = join(process.cwd(), "src/content/docs");
  const path = [".md", ".mdx"].map((ext) => join(dir, page + ext)).find((f) => existsSync(f));
  return path ? frontmatterField(readFileSync(path, "utf8"), "title") : undefined;
}

export interface RehypeNodeDocsOptions {
  /** `base` do astro.config (ex.: "/trama"); os links de "Verificar" o usam. */
  base?: string;
  /** Idioma das páginas. */
  lang?: string;
  /** Caminho do JSON (padrão: src/data/node-docs.json). */
  dataPath?: string;
}

// Cache pelo mtime do JSON, não pela vida do processo: com `astro dev` aberto,
// rodar de novo tools/site/export-node-docs.R muda o arquivo e a próxima
// página já lê o novo. (O export também apaga o data-store do Astro.)
let cache: { mtime: number; docs: Record<string, NodeDocs> } | undefined;
function loadDocs(path: string): Record<string, NodeDocs> {
  const mtime = statSync(path).mtimeMs;
  if (cache?.mtime !== mtime) cache = { mtime, docs: JSON.parse(readFileSync(path, "utf8")) };
  return cache.docs;
}

export function blockResolver(base = "/"): ResolveBlock {
  const prefix = base.replace(/\/+$/, "");
  const pages = nodePages();
  return (id) => {
    const page = pages.get(id);
    return page ? { href: `${prefix}/${page}/`, label: titleOf(page) ?? id } : undefined;
  };
}

export function rehypeNodeDocs(options: RehypeNodeDocsOptions = {}) {
  const dataPath = options.dataPath ?? join(process.cwd(), "src/data/node-docs.json");
  return (tree: Root, file?: VFile) => {
    const node = (file?.data as { astro?: { frontmatter?: { node?: string } } })?.astro?.frontmatter?.node;
    if (!node) return;
    const docs = loadDocs(dataPath)[node];
    if (!docs) return;
    insertNodeDocs(tree, docs, blockResolver(options.base), options.lang);
  };
}
