// Glossário nas páginas de bloco: escolhe os termos mais citados na página,
// monta a caixa "Antes de continuar" (que o toggle Contexto mostra/oculta) e
// marca a primeira menção de cada termo no texto. Funções puras sobre HAST;
// o plugin em rehype-glossario.ts só as encadeia.

import type { Element, ElementContent, Root, RootContent } from "hast";
import type { TermoGlossario } from "../data/glossario.ts";

const LETRA = "\\p{L}\\p{N}";
const escapar = (s: string) => s.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");

/** Regex que casa qualquer forma do termo, sem caixa e com fronteira de palavra (com acentos). */
export function regexTermo(t: TermoGlossario, flags = "giu"): RegExp {
  const formas = [...t.termos].sort((a, b) => b.length - a.length).map(escapar).join("|");
  return new RegExp(`(?<![${LETRA}])(?:${formas})(?![${LETRA}])`, flags);
}

export function contarMencoes(texto: string, t: TermoGlossario): number {
  return texto.match(regexTermo(t))?.length ?? 0;
}

export interface OpcoesSelecao {
  max?: number;
  min?: number;
  /** id do termo → blocos em que ele é central (TERMOS_DE_BLOCO). */
  deBloco?: Record<string, string[]>;
}

/**
 * Até `max` termos para a página do bloco `node`: pontua pelas menções no
 * texto (no máximo 3 por termo), mais 2 se o termo é central na coleção do
 * bloco, mais 4 se é central no próprio bloco (`deBloco`; entra mesmo sem
 * menção). Termos sem menção e não centrais nunca entram. Com menos de `min`
 * termos, devolve vazio (a página fica sem caixa).
 */
export function selecionarTermos(texto: string, node: string, glossario: TermoGlossario[],
                                 { max = 5, min = 2, deBloco = {} }: OpcoesSelecao = {}): TermoGlossario[] {
  const prefixo = node.split("/")[0];
  const pontuados = glossario
    .filter((t) => !t.fora?.includes(prefixo))
    .map((t, ordem) => {
      const mencoes = contarMencoes(texto, t);
      const afim = t.colecoes?.includes(prefixo) ?? false;
      const central = deBloco[t.id]?.includes(node) ?? false;
      return { t, ordem, mencoes, afim, central, nota: Math.min(mencoes, 3) + (afim ? 2 : 0) + (central ? 4 : 0) };
    });
  const citados = pontuados.filter((p) => p.mencoes > 0 || p.central)
    .sort((a, b) => b.nota - a.nota || a.ordem - b.ordem)
    .slice(0, max);
  if (citados.length < min) return [];
  // Na caixa, a ordem do glossário (que agrupa por assunto) lê melhor que a nota.
  return citados.sort((a, b) => a.ordem - b.ordem).map((p) => p.t);
}

const el = (tagName: string, className: string | null, children: ElementContent[],
            properties: Record<string, unknown> = {}): Element => ({
  type: "element", tagName,
  properties: className ? { className: className.split(" "), ...properties } : properties,
  children
});
const tx = (value: string): ElementContent => ({ type: "text", value });

/** A caixa "Antes de continuar" com os termos e o link para o glossário. */
export function caixaGlossario(termos: TermoGlossario[], glossarioHref: string): Element {
  const itens = termos.flatMap((t) => [
    el("dt", null, [el("a", null, [tx(t.termo)], { href: `${glossarioHref}#${t.id}` })]),
    el("dd", null, [tx(t.explicacao)])
  ]);
  return el("blockquote", "facilitador glossario-caixa", [
    el("p", null, [el("strong", null, [tx("Antes de continuar")])]),
    el("p", null, [tx("Termos que esta página usa, em poucas palavras. As definições completas estão no "),
      el("a", null, [tx("glossário")], { href: glossarioHref }), tx(".")]),
    el("dl", "glossario-caixa__lista", itens)
  ]);
}

const classes = (n: Element) => (n.properties?.className as string[] | undefined) ?? [];
const PULAR = new Set(["pre", "code", "script", "style", "a", "dfn", "h1", "h2", "h3", "h4", "h5", "h6", "blockquote", "svg"]);
const pular = (n: Element) => PULAR.has(n.tagName) || classes(n).some((c) => /^(flow-example|tr-estatico|glossario-caixa)/.test(c));

/** Texto corrido da página, sem títulos, código, exemplos-canvas nem caixas do glossário. */
export function textoDaPagina(tree: Root): string {
  const partes: string[] = [];
  const visitar = (n: RootContent | ElementContent) => {
    if (n.type === "text") partes.push(n.value);
    else if (n.type === "element") {
      if (["pre", "code", "script", "style", "svg", "h1", "h2", "h3", "h4", "h5", "h6"].includes(n.tagName) || classes(n).some((c) => /^(flow-example|tr-estatico|glossario-caixa)/.test(c))) return;
      n.children.forEach(visitar);
    }
  };
  tree.children.forEach(visitar);
  return partes.join(" ");
}

/**
 * Envolve a primeira menção de cada termo, em parágrafos e itens de lista,
 * num `<dfn class="glossario-termo" title="...">`. O CSS só o destaca com o
 * Contexto ligado; desligado, é texto comum. Com `glossarioHref`, a marca vai
 * dentro de um link para o verbete (`<glossario>#<id>`).
 */
export function marcarPrimeiraMencao(tree: Root, termos: TermoGlossario[], glossarioHref?: string): void {
  const pendentes = new Map(termos.map((t) => [t.id, t]));
  const visitar = (pai: Root | Element, emProsa: boolean) => {
    for (let i = 0; i < pai.children.length && pendentes.size; i += 1) {
      const n = pai.children[i];
      if (n.type === "element") {
        if (!pular(n)) visitar(n, emProsa || n.tagName === "p" || n.tagName === "li");
        continue;
      }
      if (n.type !== "text" || !emProsa) continue;
      // A menção mais cedo no texto entre os termos ainda pendentes.
      let melhor: { t: TermoGlossario; idx: number; len: number } | undefined;
      for (const t of pendentes.values()) {
        const m = regexTermo(t, "iu").exec(n.value);
        if (m && (!melhor || m.index < melhor.idx)) melhor = { t, idx: m.index, len: m[0].length };
      }
      if (!melhor) continue;
      const { t, idx, len } = melhor;
      const antes = n.value.slice(0, idx), achado = n.value.slice(idx, idx + len), depois = n.value.slice(idx + len);
      const novos: ElementContent[] = [];
      if (antes) novos.push(tx(antes));
      const dfn = el("dfn", "glossario-termo", [tx(achado)], { title: `${t.termo}: ${t.explicacao}` });
      // Com o endereço do glossário, a primeira menção leva ao verbete.
      novos.push(glossarioHref ? el("a", "glossario-link", [dfn], { href: `${glossarioHref}#${t.id}` }) : dfn);
      if (depois) novos.push(tx(depois));
      (pai.children as ElementContent[]).splice(i, 1, ...novos);
      pendentes.delete(t.id);
      // Volta ao texto que sobrou depois da marca (pode citar outro termo).
      i += novos.length - (depois ? 2 : 1);
    }
  };
  visitar(tree, false);
}
