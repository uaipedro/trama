import { test } from "node:test";
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import type { NodeDocs } from "./node-docs.ts";
import { agregarReferencias, chaveReferencia, encontrarReferencia, resumoReferencias } from "./bibliografia.ts";

const fixture: Record<string, NodeDocs> = {
  "models/b": { pressupostos: [], referencias: [
    { papel: "livro-texto", autores: ["Fisher, R. A."], ano: 1925, titulo: "Statistical methods", doi: "10.1000/XYZ", nota: "nota do bloco b" },
    { papel: "implementacao", pacote: "stats", funcao: "aov", versao: "4.6.0" }
  ] },
  "models/a": { pressupostos: [{ texto: "x" }], referencias: [
    { papel: "teoria", autores: ["Fisher, R. A."], ano: 1925, titulo: "Statistical Methods.", doi: "10.1000/xyz" },
    { papel: "livro-texto", autores: ["Banzatto, D. A.", "Kronka, S. N."], ano: 2006, titulo: "Experimentação agrícola" },
    { papel: "livro-texto", autores: ["Banzatto, D. A.", "Kronka, S. N."], ano: 2006, titulo: "Experimentacao agricola." }
  ] },
  "models/c": { pressupostos: [], referencias: [] }
};

test("deduplica por DOI (sem caixa) e por autores+ano+título normalizados", () => {
  const grupos = agregarReferencias(fixture);
  const todas = grupos.flatMap((g) => g.entradas);
  assert.equal(todas.length, 3);
  const fisher = todas.find((e) => e.ref.doi)!;
  assert.deepEqual(fisher.blocos, ["models/a", "models/b"]);
  assert.equal(fisher.ref.papel, "teoria", "o papel de maior precedência vence");
  assert.equal(fisher.ref.nota, undefined, "a nota do bloco não vai para a bibliografia");
  assert.equal(chaveReferencia(fixture["models/a"].referencias[1]), chaveReferencia(fixture["models/a"].referencias[2]));
});

test("agrupa na ordem dos papéis e gera âncoras únicas", () => {
  const grupos = agregarReferencias(fixture);
  assert.deepEqual(grupos.map((g) => g.papel), ["teoria", "livro-texto", "implementacao"]);
  assert.equal(grupos[2].entradas[0].id, "ref-stats-aov");
  assert.match(grupos[0].entradas[0].id, /^ref-fisher-1925-statistical$/);
});

test("encontrarReferencia acha pelo sobrenome e ano", () => {
  const grupos = agregarReferencias(fixture);
  assert.equal(encontrarReferencia(grupos, "Banzatto", 2006)?.blocos[0], "models/a");
  assert.equal(encontrarReferencia(grupos, "Banzatto", 2007), undefined);
});

test("resumo conta blocos, obras e DOI", () => {
  assert.deepEqual(resumoReferencias(fixture), {
    blocos: 3, blocosComReferencias: 2, blocosComPressupostos: 1, obras: 2, obrasComDoi: 1, citacoes: 5
  });
});

test("dados reais: âncoras únicas e nenhuma obra perdida", () => {
  process.chdir(new URL("../..", import.meta.url).pathname);
  const docs: Record<string, NodeDocs> = JSON.parse(readFileSync("src/data/node-docs.json", "utf8"));
  const todas = agregarReferencias(docs).flatMap((g) => g.entradas);
  assert.equal(new Set(todas.map((e) => e.id)).size, todas.length);
  const chaves = new Set(Object.values(docs).flatMap((d) => d.referencias.map(chaveReferencia)));
  assert.equal(todas.length, chaves.size);
});
