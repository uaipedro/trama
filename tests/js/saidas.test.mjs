import { test } from "node:test";
import assert from "node:assert/strict";
import { limparSaidas, nomeDaSaida, removerSaidasDoNo, rotuloDaEntrada } from "../../inst/www/saidas.js";

test("entrada ligada mostra porta e nome da saída; aresta removida volta ao rótulo base", () => {
  const edges = [{ source: "a", sourceHandle: "out", target: "b", targetHandle: "serie" }];
  const saidas = { a: { out: "Vendas SP" } };
  assert.equal(rotuloDaEntrada({ node: "b", port: "serie" }, edges, saidas), "serie ← Vendas SP");
  assert.equal(rotuloDaEntrada({ node: "b", port: "serie" }, [], saidas), "serie");
});

test("remove nó limpa seus nomes e remove aresta não deixa rótulo pendente", () => {
  const saidas = { a: { out: "Vendas SP" }, b: { x: "Outra" } };
  const restantes = removerSaidasDoNo(saidas, "a");
  assert.equal(nomeDaSaida(restantes, "a", "out"), null);
  assert.equal(rotuloDaEntrada({ node: "b", port: "serie" }, [], saidas), "serie");
});

test("ui.saidas normaliza e sobrevive a ida e volta no JSON", () => {
  const doc = { ui: { saidas: limparSaidas({ a: { out: " Vendas SP ", vazia: " " }, b: {} }) } };
  assert.deepEqual(JSON.parse(JSON.stringify(doc)).ui.saidas, { a: { out: "Vendas SP" } });
});
