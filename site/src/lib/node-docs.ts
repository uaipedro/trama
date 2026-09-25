// Pressupostos e referências de um bloco, como HAST — a mesma ordem, os
// mesmos rótulos e o mesmo agrupamento da ajuda do editor (inst/www/editor.js,
// `Help`/`Referencia`). Vira HAST, e não componente Astro, porque as seções
// entram NO MEIO do markdown (depois de "Quando usar"), e quem mexe no meio do
// markdown é o rehype (ver rehype-node-docs.ts).

import type { Element, ElementContent } from "hast";

/** Texto i18n cru, como sai do exportador: string ou {pt, en, ...}. */
export type I18nText = string | Record<string, string>;

export interface Pressuposto {
  texto: I18nText;
  verificar?: string[];
  se_falhar?: I18nText;
}

export interface Referencia {
  papel: "teoria" | "livro-texto" | "implementacao" | "complementar";
  autores?: string[];
  ano?: number;
  titulo?: I18nText;
  fonte?: I18nText;
  doi?: string;
  url?: string;
  pacote?: string;
  funcao?: string;
  versao?: string;
  nota?: I18nText;
}

export interface NodeDocs {
  pressupostos: Pressuposto[];
  referencias: Referencia[];
}

/** Idioma do site. Só português por ora; o inglês entra trocando isto. */
export const SITE_LANG = "pt";

/** Mesma regra do `tr_text()` do R: idioma pedido, senão pt, senão o primeiro. */
export function resolveText(x: I18nText | undefined, lang = SITE_LANG): string {
  if (x === undefined) return "";
  if (typeof x === "string") return x;
  return x[lang] ?? x.pt ?? Object.values(x)[0] ?? "";
}

export const PAPEIS_REF: [Referencia["papel"], string][] = [
  ["teoria", "Teoria"], ["livro-texto", "Livro-texto"],
  ["implementacao", "Implementação"], ["complementar", "Complementar"]
];

/** DOI na forma que o núcleo aceita (tr_ref): `10.xxxx/...`, sem URL. */
export const DOI_RE = /^10\.[0-9]{4,9}\/\S*[^\s.,;]$/;

const el = (tagName: string, className: string | null, children: ElementContent[],
            properties: Record<string, unknown> = {}): Element => ({
  type: "element", tagName,
  properties: className ? { ...properties, className: className.split(" ") } : properties,
  children
});
const t = (value: string): ElementContent => ({ type: "text", value });

// `**negrito**` e `código`, como o mdInline do editor.
function inline(text: string): ElementContent[] {
  const out: ElementContent[] = [];
  const re = /\*\*([^*]+)\*\*|`([^`]+)`/g;
  let last = 0;
  let m: RegExpExecArray | null;
  while ((m = re.exec(text))) {
    if (m.index > last) out.push(t(text.slice(last, m.index)));
    out.push(m[1] ? el("strong", null, [t(m[1])]) : el("code", null, [t(m[2])]));
    last = re.lastIndex;
  }
  if (last < text.length) out.push(t(text.slice(last)));
  return out;
}

export interface BlockLink { href: string; label: string }
/** Resolve um id de bloco na página dele; `undefined` se não houver página. */
export type ResolveBlock = (id: string) => BlockLink | undefined;

export function pressupostosHast(items: Pressuposto[], resolve: ResolveBlock, lang = SITE_LANG): Element[] {
  if (!items.length) return [];
  const li = items.map((p) => {
    const corpo: ElementContent[] = [el("p", "node-press__texto", inline(resolveText(p.texto, lang)))];
    if (p.verificar?.length) {
      corpo.push(el("p", "node-press__verif", [
        el("span", "node-docs__rot", [t("Verificar:")]),
        ...p.verificar.map((id) => {
          const alvo = resolve(id);
          return alvo
            ? el("a", "node-press__chip", [t(alvo.label)], { href: alvo.href, title: id })
            : el("code", "node-press__chip", [t(id)]);
        })
      ]));
    }
    if (p.se_falhar) {
      corpo.push(el("p", "node-press__falha", [
        el("span", "node-docs__rot", [t("Se falhar: ")]), ...inline(resolveText(p.se_falhar, lang))
      ]));
    }
    return el("li", null, [el("span", "node-press__ic", [], { ariaHidden: "true" }), el("div", null, corpo)]);
  });
  return [el("h2", null, [t("Pressupostos")], { id: "pressupostos" }), el("ul", "node-press", li)];
}

function referenciaHast(r: Referencia, lang: string): Element {
  const partes: ElementContent[] = [];
  if (r.papel === "implementacao" && r.pacote) {
    partes.push(el("code", null, [t(`${r.pacote}::${r.funcao ? r.funcao + "()" : ""}`)]));
    if (r.versao) partes.push(el("span", "node-ref__ver", [t(` versão ${r.versao}`)]));
    if (r.autores?.length || r.titulo) partes.push(el("br", null, []));
  }
  const cab: string[] = [];
  if (r.autores?.length) cab.push(r.autores.join("; "));
  if (r.ano) cab.push(`(${r.ano}).`);
  else if (cab.length) cab[cab.length - 1] += ".";
  if (cab.length) partes.push(t(cab.join(" ") + " "));
  const titulo = resolveText(r.titulo, lang);
  if (titulo) partes.push(el("em", null, [t(titulo.replace(/\.$/, ""))]), t(". "));
  const fonte = resolveText(r.fonte, lang);
  if (fonte) partes.push(t(fonte.replace(/\.$/, "") + ". "));
  const href = r.doi ? `https://doi.org/${r.doi}` : r.url;
  if (href) partes.push(el("a", null, [t(r.doi ? `doi:${r.doi}` : r.url!)], { href, rel: "noopener" }));
  const nota = resolveText(r.nota, lang);
  if (nota) partes.push(el("span", "node-ref__nota", inline(nota)));
  return el("li", "node-ref", partes);
}

export function referenciasHast(items: Referencia[], lang = SITE_LANG): Element[] {
  if (!items.length) return [];
  const grupos = PAPEIS_REF.flatMap(([papel, rot]) => {
    const grupo = items.filter((r) => r.papel === papel);
    return grupo.length
      ? [el("h3", null, [t(rot)]), el("ul", "node-refs", grupo.map((r) => referenciaHast(r, lang)))]
      : [];
  });
  return [el("h2", null, [t("Referências")], { id: "referencias" }), ...grupos];
}
