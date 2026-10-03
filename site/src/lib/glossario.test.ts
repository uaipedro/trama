import { test } from "node:test";
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { toHtml } from "hast-util-to-html";
import type { Root } from "hast";
import { GLOSSARIO, type TermoGlossario } from "../data/glossario.ts";
import { agregarReferencias, encontrarReferencia } from "./bibliografia.ts";
import { contarMencoes, marcarPrimeiraMencao, selecionarTermos } from "./glossario.ts";
import { insertGlossario } from "./rehype-glossario.ts";
import type { NodeDocs } from "./node-docs.ts";

const termo = (id: string) => GLOSSARIO.find((t) => t.id === id)!;

test("glossário: ids únicos, explicação curta, 40+ termos", () => {
  assert.ok(GLOSSARIO.length >= 40, `só ${GLOSSARIO.length} termos`);
  assert.equal(new Set(GLOSSARIO.map((t) => t.id)).size, GLOSSARIO.length);
  for (const t of GLOSSARIO) {
    const frases = t.explicacao.split(/(?<=[.!?])\s+(?=[A-ZÀ-Ú“])/).length;
    assert.ok(frases >= 1 && frases <= 3, `${t.id}: ${frases} frases`);
    assert.ok(t.termos.length > 0, `${t.id} sem formas`);
    for (const f of t.termos) assert.equal(f, f.toLowerCase(), `${t.id}: forma '${f}' com maiúscula`);
  }
});

test("glossário: 'onde aprofundar' só aponta para obras que estão em node-docs.json", () => {
  process.chdir(new URL("../..", import.meta.url).pathname);
  const docs: Record<string, NodeDocs> = JSON.parse(readFileSync("src/data/node-docs.json", "utf8"));
  const grupos = agregarReferencias(docs);
  for (const t of GLOSSARIO) if (t.aprofundar) {
    assert.ok(encontrarReferencia(grupos, t.aprofundar.autor, t.aprofundar.ano),
      `${t.id}: ${t.aprofundar.autor} ${t.aprofundar.ano} não está nas referências dos blocos`);
  }
});

test("contarMencoes ignora caixa e respeita fronteira de palavra com acento", () => {
  assert.equal(contarMencoes("O p-valor e o P-VALOR.", termo("p-valor")), 2);
  assert.equal(contarMencoes("resíduos e residuais", termo("residuo")), 2);
  assert.equal(contarMencoes("Os blocos do trama", termo("bloco")), 0, "'bloco' do trama não conta");
  assert.equal(contarMencoes("delineamento em blocos casualizados", termo("bloco")), 1);
});

test("selecionarTermos prioriza menções e a coleção, e respeita `fora`", () => {
  const texto = "A análise de variância testa a interação entre fatores. O resíduo deve ter normalidade; resíduos, resíduos.";
  const ids = selecionarTermos(texto, "experiments/x", GLOSSARIO).map((t) => t.id);
  assert.ok(ids.length >= 3 && ids.length <= 5);
  for (const id of ["anova", "interacao", "residuo"]) assert.ok(ids.includes(id), id);
  assert.ok(!selecionarTermos("o fator latente e as cargas fatoriais", "multi/fa", GLOSSARIO).some((t) => t.id === "fator"));
  assert.deepEqual(selecionarTermos("nada estatístico aqui", "data/filter", GLOSSARIO), []);
  assert.deepEqual(selecionarTermos("série com tendência", "series/x", GLOSSARIO), [], "um termo só não faz caixa");
  assert.equal(selecionarTermos("tendência e sazonalidade", "series/x", GLOSSARIO).length, 2);
});

const p = (s: string) => ({ type: "element" as const, tagName: "p", properties: {}, children: [{ type: "text" as const, value: s }] });
const h2 = (s: string) => ({ type: "element" as const, tagName: "h2", properties: {}, children: [{ type: "text" as const, value: s }] });

test("marcarPrimeiraMencao marca só a primeira menção e não entra em código", () => {
  const t: TermoGlossario[] = [termo("residuo"), termo("p-valor")];
  const tree: Root = { type: "root", children: [
    { type: "element", tagName: "p", properties: {}, children: [{ type: "element", tagName: "code", properties: {}, children: [{ type: "text", value: "resíduo" }] }] },
    p("O p-valor sai do resíduo; outro resíduo."), p("mais um p-valor")
  ] };
  marcarPrimeiraMencao(tree, t);
  const html = toHtml(tree);
  assert.equal((html.match(/<dfn/g) ?? []).length, 2);
  assert.match(html, /<code>resíduo<\/code>/);
  assert.match(html, /O <dfn class="glossario-termo" title="p-valor: [^"]+">p-valor<\/dfn> sai do <dfn[^>]+>resíduo<\/dfn>; outro resíduo\./);
});

test("insertGlossario põe a caixa depois de 'O que o bloco faz', marcada como facilitador", () => {
  const tree: Root = { type: "root", children: [
    h2("O que o bloco faz"), p("Ajusta a análise de variância e testa a interação entre os tratamentos pelo resíduo."),
    h2("Quando usar"), p("x")
  ] };
  insertGlossario(tree, "experiments/x", "/trama/glossario/");
  const html = toHtml(tree);
  assert.ok(html.indexOf("glossario-caixa") > html.indexOf("Ajusta") && html.indexOf("glossario-caixa") < html.indexOf("Quando usar"));
  assert.match(html, /<blockquote class="facilitador glossario-caixa"><p><strong>Antes de continuar<\/strong>/);
  assert.match(html, /href="\/trama\/glossario\/#anova"/);
});

test("TERMOS_DE_BLOCO: termos existem e blocos têm página", async () => {
  const { TERMOS_DE_BLOCO } = await import("../data/glossario.ts");
  const { nodePages } = await import("./rehype-doc-links.ts");
  process.chdir(new URL("../..", import.meta.url).pathname);
  const pages = nodePages();
  for (const [id, blocos] of Object.entries(TERMOS_DE_BLOCO)) {
    assert.ok(GLOSSARIO.some((t) => t.id === id), `termo '${id}' não existe`);
    for (const b of blocos) assert.ok(pages.has(b), `${id}: bloco '${b}' sem página`);
  }
  const ids = selecionarTermos("Ajusta tratamento e blocos.", "models/anova_dbc", GLOSSARIO, { deBloco: TERMOS_DE_BLOCO }).map((t) => t.id);
  for (const id of ["anova", "bloco", "delineamento"]) assert.ok(ids.includes(id), id);
});
