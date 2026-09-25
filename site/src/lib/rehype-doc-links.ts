// Plugin rehype das páginas de documentação:
// - `pacote/bloco` em código inline vira link para a página do bloco;
// - a seção "Veja também" do markdown sai quando o frontmatter tem `related`,
//   porque a página já desenha esses blocos como cartões no fim.

import { readdirSync, readFileSync } from "node:fs";
import { join, relative } from "node:path";
import { SKIP, visit } from "unist-util-visit";
import type { Element, ElementContent, Root, Text } from "hast";
import type { VFile } from "vfile";

const docsDir = join(process.cwd(), "src/content/docs");

// Mapa bloco → caminho da página, lido do frontmatter `node:` dos .md.
function nodePages(): Map<string, string> {
  const pages = new Map<string, string>();
  const walk = (dir: string) => {
    for (const entry of readdirSync(dir, { withFileTypes: true })) {
      const path = join(dir, entry.name);
      if (entry.isDirectory()) walk(path);
      else if (/\.mdx?$/.test(entry.name)) {
        const node = /^node:\s*(\S+)\s*$/m.exec(readFileSync(path, "utf8"))?.[1];
        if (node) pages.set(node, relative(docsDir, path).replace(/\.mdx?$/, ""));
      }
    }
  };
  walk(docsDir);
  return pages;
}

const text = (node: ElementContent): string =>
  node.type === "text" ? node.value : "children" in node ? node.children.map(text).join("") : "";

function dropSeeAlso(tree: Root) {
  const start = tree.children.findIndex(
    (node) => node.type === "element" && node.tagName === "h2" && text(node).trim() === "Veja também"
  );
  if (start < 0) return;
  let end = start + 1;
  while (end < tree.children.length) {
    const node = tree.children[end];
    if (node.type === "element" && /^h[12]$/.test(node.tagName)) break;
    end += 1;
  }
  tree.children.splice(start, end - start);
}

export function rehypeDocLinks() {
  let pages: Map<string, string> | undefined;
  return (tree: Root, file?: VFile) => {
    pages ??= nodePages();
    const frontmatter = (file?.data as { astro?: { frontmatter?: { related?: unknown[] } } })?.astro?.frontmatter;
    if (frontmatter?.related?.length) dropSeeAlso(tree);

    visit(tree, "element", (node: Element, index, parent) => {
      if (node.tagName === "pre" || node.tagName === "a") return SKIP;
      if (node.tagName !== "code" || index === undefined || !parent) return;
      const page = pages!.get((node.children[0] as Text | undefined)?.value ?? "");
      if (!page) return;
      parent.children[index] = {
        type: "element",
        tagName: "a",
        properties: { href: `/trama/${page}/`, className: ["node-ref"] },
        children: [node]
      };
      return SKIP;
    });
  };
}
