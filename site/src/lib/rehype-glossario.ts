// Plugin rehype: nas páginas de bloco (frontmatter `node:`), insere depois de
// "O que o bloco faz" uma caixa "Antes de continuar" com os termos do
// glossário mais citados na página, e marca a primeira menção de cada um.
// Roda depois do rehype-node-docs, para contar também os pressupostos.

import type { Root } from "hast";
import type { VFile } from "vfile";
import { GLOSSARIO, TERMOS_DE_BLOCO } from "../data/glossario.ts";
import { caixaGlossario, marcarPrimeiraMencao, selecionarTermos, textoDaPagina } from "./glossario.ts";
import { afterSection } from "./rehype-node-docs.ts";

/** `extra`: texto que não está na árvore mas descreve a página (título, descrição). */
export function insertGlossario(tree: Root, node: string, glossarioHref: string, extra = ""): void {
  const termos = selecionarTermos(`${extra} ${textoDaPagina(tree)}`, node, GLOSSARIO, { deBloco: TERMOS_DE_BLOCO });
  if (!termos.length) return;
  marcarPrimeiraMencao(tree, termos, glossarioHref);
  const at = afterSection(tree, "O que o bloco faz");
  tree.children.splice(at < 0 ? 0 : at, 0, caixaGlossario(termos, glossarioHref));
}

export function rehypeGlossario(options: { base?: string } = {}) {
  const href = `${(options.base ?? "/").replace(/\/+$/, "")}/glossario/`;
  return (tree: Root, file?: VFile) => {
    const fm = (file?.data as { astro?: { frontmatter?: { node?: string; title?: string; description?: string } } })?.astro?.frontmatter;
    if (fm?.node) insertGlossario(tree, fm.node, href, `${fm.title ?? ""}. ${fm.description ?? ""}`);
  };
}
