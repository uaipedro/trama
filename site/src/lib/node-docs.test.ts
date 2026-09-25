import { test } from "node:test";
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { toHtml } from "hast-util-to-html";
import type { Root } from "hast";
import { DOI_RE, resolveText, type NodeDocs } from "./node-docs.ts";
import { insertNodeDocs } from "./rehype-node-docs.ts";
import { nodePages } from "./rehype-doc-links.ts";

process.chdir(new URL("../..", import.meta.url).pathname);
const docs: Record<string, NodeDocs> = JSON.parse(readFileSync("src/data/node-docs.json", "utf8"));
const pages = nodePages();

test("todo `verificar` aponta para um bloco que tem página", () => {
  for (const [id, d] of Object.entries(docs)) {
    assert.ok(pages.has(id), `${id} tem docs mas não tem página`);
    for (const p of d.pressupostos) for (const v of p.verificar ?? []) {
      assert.ok(pages.has(v), `${id}: verificar '${v}' sem página`);
    }
  }
});

test("todo DOI tem forma válida", () => {
  for (const [id, d] of Object.entries(docs)) for (const r of d.referencias) {
    if (r.doi) assert.match(r.doi, DOI_RE, `${id}: doi '${r.doi}'`);
  }
});

test("resolveText escolhe o idioma e cai para pt", () => {
  assert.equal(resolveText("oi"), "oi");
  assert.equal(resolveText({ pt: "oi", en: "hi" }, "en"), "hi");
  assert.equal(resolveText({ pt: "oi", en: "hi" }, "es"), "oi");
  assert.equal(resolveText({ en: "hi" }), "hi");
});

const fixture: NodeDocs = {
  pressupostos: [{ texto: { pt: "Resíduos **normais**.", en: "Normal residuals." },
                   verificar: ["models/qq", "x/sem_pagina"], se_falhar: "Use `models/kruskal`." }],
  referencias: [
    { papel: "implementacao", pacote: "stats", funcao: "aov", versao: "4.6.0" },
    { papel: "teoria", autores: ["Fisher, R. A."], ano: 1925, titulo: "Statistical methods.", doi: "10.1000/xyz" },
    { papel: "livro-texto", autores: ["Banzatto, D. A.", "Kronka, S. N."], ano: 2006, titulo: "Experimentação agrícola", fonte: "Funep" }
  ]
};
const h2 = (s: string) => ({ type: "element" as const, tagName: "h2", properties: {}, children: [{ type: "text" as const, value: s }] });
const p = (s: string) => ({ type: "element" as const, tagName: "p", properties: {}, children: [{ type: "text" as const, value: s }] });
const resolve = (id: string) => id === "models/qq" ? { href: "/trama/colecoes/modelos/qq/", label: "QQ" } : undefined;

test("seções entram depois de 'Quando usar', na ordem e com os rótulos do editor", () => {
  const tree: Root = { type: "root", children: [h2("O que o bloco faz"), p("a"), h2("Quando usar"), p("b"), h2("Exemplo")] };
  insertNodeDocs(tree, fixture, resolve);
  const html = toHtml(tree);
  const pos = (s: string) => html.indexOf(s);
  assert.ok(pos("<p>b</p>") < pos("Pressupostos") && pos("Pressupostos") < pos("Referências") && pos("Referências") < pos(">Exemplo"));
  assert.ok(pos(">Teoria<") < pos(">Livro-texto<") && pos(">Livro-texto<") < pos(">Implementação<"));
  assert.match(html, /Verificar:.*href="\/trama\/colecoes\/modelos\/qq\/"/);
  assert.match(html, /<code class="node-press__chip">x\/sem_pagina<\/code>/);
  assert.match(html, /Se falhar: .*<code>models\/kruskal<\/code>/);
  assert.match(html, /<strong>normais<\/strong>/);
  assert.match(html, /href="https:\/\/doi.org\/10.1000\/xyz"/);
  assert.match(html, /<code>stats::aov\(\)<\/code><span class="node-ref__ver"> versão 4.6.0<\/span>/);
  assert.match(html, /Fisher, R. A. \(1925\). <em>Statistical methods<\/em>/);
});

test("sem 'Quando usar', entra depois de 'O que o bloco faz'", () => {
  const tree: Root = { type: "root", children: [h2("O que o bloco faz"), p("a"), h2("Exemplo")] };
  insertNodeDocs(tree, { pressupostos: fixture.pressupostos, referencias: [] }, resolve);
  const html = toHtml(tree);
  assert.ok(html.indexOf("<p>a</p>") < html.indexOf("Pressupostos") && html.indexOf("Pressupostos") < html.indexOf(">Exemplo"));
  assert.ok(!html.includes("Referências"));
});

const render = (d: NodeDocs, lang?: string) => {
  const tree: Root = { type: "root", children: [h2("Quando usar"), p("b")] };
  insertNodeDocs(tree, d, resolve, lang);
  return toHtml(tree);
};

test("implementação sem função mostra só o pacote, como o editor", () => {
  const html = render({ pressupostos: [], referencias: [{ papel: "implementacao", pacote: "lme4" }] });
  assert.match(html, /<code>lme4<\/code>/);
  assert.ok(!html.includes("lme4::"));
});

test("url que não é https vira texto, não link; DOI é codificado", () => {
  for (const url of ["http://exemplo.org", "javascript:alert(1)"]) {
    const html = render({ pressupostos: [], referencias: [{ papel: "complementar", titulo: "X", url }] });
    assert.ok(!html.includes("<a"), `${url} virou link`);
    assert.ok(html.includes(url.replace("(", "(")), `${url} sumiu`);
  }
  const ok = render({ pressupostos: [], referencias: [{ papel: "complementar", url: "https://exemplo.org" }] });
  assert.match(ok, /<a href="https:\/\/exemplo.org"/);
  const doi = render({ pressupostos: [], referencias: [{ papel: "teoria", doi: "10.1000/a<b>" }] });
  assert.match(doi, /href="https:\/\/doi.org\/10.1000\/a%3Cb%3E"/);
});

test("idioma en atravessa todo o caminho até o HAST", () => {
  const html = render(fixture, "en");
  assert.match(html, /Normal residuals\./);
  assert.ok(!html.includes("normais"));
});
