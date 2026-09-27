import { test } from "node:test";
import assert from "node:assert/strict";
import { buscarFantasma, moverFantasma, mostrarFantasma, pontoBusca, yDosFantasmas } from "../../inst/www/fantasmas.js";

const catalog = { nodes: [
  { id: "a/one", label: "Ler CSV", description: "Lê uma tabela" },
  { id: "b/two", label: "Resumo", description: "Resume tabela" },
  { id: "b/three", label: "Gráfico", description: "Visualiza tabela" },
  { id: "c/four", label: "Ajuste", description: "Ajusta modelo" },
] };
const ranked = catalog.nodes.map((n, i) => ({ id: n.id, score: 4 - i }));

test("busca mantém ranking, filtra label/descrição/id e limita a três", () => {
  assert.deepEqual(buscarFantasma(ranked, catalog).map((s) => s.id), ["a/one", "b/two", "b/three"]);
  assert.deepEqual(buscarFantasma(ranked, catalog, "VISUALIZA").map((s) => s.id), ["b/three"]);
  assert.deepEqual(buscarFantasma(ranked, catalog, "c/four").map((s) => s.id), ["c/four"]);
  assert.deepEqual(buscarFantasma(ranked, catalog, "inexistente"), []);
});

test("a lista só pode ranquear candidatos compatíveis recebidos", () => {
  const compatíveis = ranked.filter((s) => s.id !== "a/one");
  assert.deepEqual(buscarFantasma(compatíveis, catalog).map((s) => s.id), ["b/two", "b/three", "c/four"]);
});

test("setas percorrem os resultados e tratam lista vazia", () => {
  assert.equal(moverFantasma(0, 3, "ArrowDown"), 1);
  assert.equal(moverFantasma(2, 3, "ArrowRight"), 0);
  assert.equal(moverFantasma(0, 3, "ArrowUp"), 2);
  assert.equal(moverFantasma(-1, 3, "ArrowDown"), 0);
  assert.equal(moverFantasma(0, 0, "ArrowDown"), -1);
  assert.equal(moverFantasma(1, 3, "Enter"), 1);
});

test("preferência desligada oculta previews até a busca receber texto", () => {
  assert.equal(mostrarFantasma(false, "  "), false);
  assert.equal(mostrarFantasma(false, "anova"), true);
  assert.equal(mostrarFantasma(true, ""), true);
});

test("previews descem para não cobrir filhos existentes da saída", () => {
  assert.equal(yDosFantasmas(100, [{ y: 120, h: 180 }], 250), 316);
  assert.equal(yDosFantasmas(100, [{ y: 500, h: 180 }], 250), 100);
  assert.equal(yDosFantasmas(100, []), 100);
});

test("busca acha bloco que o sugestor não pontuou (ANOVA saindo do CSV)", () => {
  const cat = { nodes: [
    { id: "data/filter", label: "Filtrar", description: "Mantém linhas" },
    { id: "models/anova_dbc", label: "ANOVA · DBC", description: "Blocos casualizados" },
  ] };
  const r = [{ id: "data/filter", score: 3 }, { id: "models/anova_dbc", score: 0 }];
  assert.deepEqual(buscarFantasma(r, cat).map((s) => s.id), ["data/filter"]);
  assert.deepEqual(buscarFantasma(r, cat, "anova").map((s) => s.id), ["models/anova_dbc"]);
  assert.deepEqual(buscarFantasma(r, cat, "anv").map((s) => s.id), ["models/anova_dbc"]);
  assert.deepEqual(buscarFantasma(r, cat, "dbc anova").map((s) => s.id), ["models/anova_dbc"]);
});

test("busca ignora acento e caixa e põe início de palavra na frente", () => {
  assert.deepEqual(buscarFantasma(ranked, catalog, "grafico").map((s) => s.id), ["b/three"]);
  assert.equal(pontoBusca("res", "Resumo"), 3);
  assert.equal(pontoBusca("sum", "Resumo"), 2);
  assert.equal(pontoBusca("rsm", "Resumo"), 1);
  assert.equal(pontoBusca("xyz", "Resumo"), -1);
});
