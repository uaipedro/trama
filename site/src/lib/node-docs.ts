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

/** Uma chamada a função de outro pacote, achada no código do bloco. */
export interface Chamada {
  pacote: string;
  funcao: string;
  codigo: string;
  arquivo?: string;
  linha?: number;
  /** Função do pacote do bloco em que a chamada aparece. */
  dentro: string;
  /** Coincide com uma referência de implementação. */
  principal: boolean;
}

/** Raio-x (tr_node_raiox): a função do bloco e onde a conta acontece. */
export interface Raiox {
  funcao: string;
  arquivo?: string;
  linha?: number;
  chamadas: Chamada[];
}

export interface NodeDocs {
  pressupostos: Pressuposto[];
  referencias: Referencia[];
  raiox?: Raiox;
}

/** Código no GitHub, na main (o site é reconstruído a partir dela). */
export const REPO_BLOB = "https://github.com/uaipedro/trama/blob/main/";

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

export function referenciaHast(r: Referencia, lang = SITE_LANG): Element {
  const partes: ElementContent[] = [];
  if (r.papel === "implementacao" && r.pacote) {
    // Como o editor: sem função, só o pacote.
    partes.push(el("code", null, [t(r.funcao ? `${r.pacote}::${r.funcao}()` : r.pacote)]));
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
  // Só https vira link (javascript:, http: etc. ficam texto); DOI é codificado.
  if (r.doi) partes.push(el("a", null, [t(`doi:${r.doi}`)], { href: `https://doi.org/${encodeURI(r.doi)}`, rel: "noopener" }));
  else if (r.url && /^https:\/\//i.test(r.url)) partes.push(el("a", null, [t(r.url)], { href: r.url, rel: "noopener" }));
  else if (r.url) partes.push(t(r.url));
  const nota = resolveText(r.nota, lang);
  if (nota) partes.push(el("span", "node-ref__nota", inline(nota)));
  return el("li", "node-ref", partes);
}

/** `bibHref`: página da bibliografia completa; se dado, um link fecha a seção. */
const linkCodigo = (arquivo?: string, linha?: number): ElementContent[] =>
  arquivo
    ? [el("a", "node-raiox__onde", [t(`${arquivo.replace(/^collections\//, "")}${linha ? `:${linha}` : ""}`)],
        { href: `${REPO_BLOB}${arquivo}${linha ? `#L${linha}` : ""}`, rel: "noopener" })]
    : [];

function chamadaHast(c: Chamada): Element {
  return el("li", null, [
    el("pre", "node-raiox__codigo", [el("code", null, [t(c.codigo)])]),
    el("p", "node-raiox__meta", [t(`em ${c.dentro}() · `), ...linkCodigo(c.arquivo, c.linha)])
  ]);
}

/**
 * "No código": a chamada que faz a conta (a que coincide com a referência de
 * implementação) e, recolhidas, as outras funções de pacotes que o bloco usa.
 * Sem chamada principal, a conta é do próprio trama: aponta a função.
 */
export function raioxHast(rx: Raiox | undefined): Element[] {
  if (!rx) return [];
  const principais = rx.chamadas.filter((c) => c.principal);
  const outras = rx.chamadas.filter((c) => !c.principal);
  const out: Element[] = [el("p", "node-raiox__titulo", [
    el("span", "node-docs__rot", [t("No código")]),
    t(" O bloco chama "), el("code", null, [t(`${rx.funcao}()`)]), t(", em "), ...linkCodigo(rx.arquivo, rx.linha), t(".")
  ])];
  if (principais.length) {
    out.push(el("p", null, [t(principais.length > 1 ? "A conta é feita nestas chamadas:" : "A conta é feita nesta chamada:")]));
    out.push(el("ul", "node-raiox", principais.map(chamadaHast)));
  } else {
    out.push(el("p", null, [t("A conta é implementada pelo próprio trama, sem um pacote R de referência por trás.")]));
  }
  if (outras.length) {
    out.push(el("details", "node-raiox__outras", [
      el("summary", null, [t(`Outras funções de pacotes que o bloco usa (${outras.length})`)]),
      el("ul", "node-raiox", outras.map(chamadaHast))
    ]));
  }
  return out;
}

/**
 * "Implementação": o pacote cuja conta o bloco reproduz e, logo abaixo, o
 * raio-x com a chamada em que ela acontece. Vem antes das referências.
 */
export function implementacaoHast(items: Referencia[], raiox?: Raiox, lang = SITE_LANG): Element[] {
  const impl = items.filter((r) => r.papel === "implementacao");
  if (!impl.length && !raiox) return [];
  return [
    el("h2", null, [t("Implementação")], { id: "implementacao" }),
    ...(impl.length ? [el("ul", "node-refs", impl.map((r) => referenciaHast(r, lang)))] : []),
    ...raioxHast(raiox)
  ];
}

export function referenciasHast(items: Referencia[], lang = SITE_LANG, bibHref?: string): Element[] {
  const obras = items.filter((r) => r.papel !== "implementacao");
  if (!obras.length) return [];
  const grupos = PAPEIS_REF.flatMap(([papel, rot]) => {
    const grupo = obras.filter((r) => r.papel === papel);
    return grupo.length ? [el("h3", null, [t(rot)]), el("ul", "node-refs", grupo.map((r) => referenciaHast(r, lang)))] : [];
  });
  const bib = bibHref
    ? [el("p", "node-refs__bib", [el("a", null, [t("Ver a bibliografia completa do trama")], { href: bibHref })])]
    : [];
  return [el("h2", null, [t("Referências")], { id: "referencias" }), ...grupos, ...bib];
}
