// Plugin rehype: nas páginas de bloco (frontmatter `node:`), insere as seções
// Pressupostos e Referências logo depois de "Quando usar" — ou, sem ela,
// depois de "O que o bloco faz". O conteúdo vem de src/data/node-docs.json,
// gerado do núcleo por tools/site/export-node-docs.R.

import { readFileSync } from "node:fs";
import { join } from "node:path";
import type { Element, ElementContent, Root, RootContent } from "hast";
import type { VFile } from "vfile";
import { nodePages } from "./rehype-doc-links.ts";
import { pressupostosHast, referenciasHast, type NodeDocs, type ResolveBlock } from "./node-docs.ts";

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

export function insertNodeDocs(tree: Root, docs: NodeDocs, resolve: ResolveBlock): void {
  const secoes: Element[] = [...pressupostosHast(docs.pressupostos, resolve), ...referenciasHast(docs.referencias)];
  if (!secoes.length) return;
  let at = afterSection(tree, "Quando usar");
  if (at < 0) at = afterSection(tree, "O que o bloco faz");
  if (at < 0) at = tree.children.length;
  tree.children.splice(at, 0, ...secoes);
}

function titleOf(page: string): string | undefined {
  const src = readFileSync(join(process.cwd(), "src/content/docs", `${page}.md`), "utf8");
  return /^title:\s*"?(.+?)"?\s*$/m.exec(src)?.[1];
}

export function rehypeNodeDocs() {
  let docs: Record<string, NodeDocs> | undefined;
  let resolve: ResolveBlock | undefined;
  return (tree: Root, file?: VFile) => {
    const node = (file?.data as { astro?: { frontmatter?: { node?: string } } })?.astro?.frontmatter?.node;
    if (!node) return;
    docs ??= JSON.parse(readFileSync(join(process.cwd(), "src/data/node-docs.json"), "utf8"));
    if (!docs![node]) return;
    if (!resolve) {
      const pages = nodePages();
      resolve = (id) => {
        const page = pages.get(id);
        return page ? { href: `/trama/${page}/`, label: titleOf(page) ?? id } : undefined;
      };
    }
    insertNodeDocs(tree, docs![node], resolve);
  };
}
