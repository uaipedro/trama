// tests/js/galeria.test.mjs
import { test } from "node:test";
import assert from "node:assert/strict";
import { naGaleria, opAlternarGaleria, navegar, urlComVersao, DICA_GALERIA_VAZIA }
  from "../../inst/www/galeria.js";

test("regra de pertencimento: marca explícita vence; sem marca vale o padrão e o renderer", () => {
  // sem marca: só imagem com o padrão ligado
  assert.equal(naGaleria(undefined, "trama/image", true), true);
  assert.equal(naGaleria(undefined, "trama/image", false), false);
  assert.equal(naGaleria(undefined, "trama/tabela", true), false);
  assert.equal(naGaleria(null, "trama/image", true), true);
  // marca explícita vence o padrão nos dois sentidos
  assert.equal(naGaleria(true, "trama/tabela", false), true);
  assert.equal(naGaleria(false, "trama/image", true), false);
});

test("op de alternar, caso 1: padrão dentro, card sem marca -> tirar grava false", () => {
  const op = opAlternarGaleria({ node: "n1", renderer: "trama/image", imagensPadrao: true });
  assert.deepEqual(op, { op: "set_galeria", node: "n1", valor: false });
});

test("op de alternar, caso 2: padrão fora, card sem marca -> pôr grava true", () => {
  const op = opAlternarGaleria({ node: "n2", renderer: "trama/tabela", imagensPadrao: false });
  assert.deepEqual(op, { op: "set_galeria", node: "n2", valor: true });
});

test("op de alternar, caso 3: marca explícita que coincide com a regra -> null (volta ao padrão)", () => {
  // padrão fora (tabela), marca TRUE (está na galeria); alternar tira -> igual ao padrão -> null
  const op = opAlternarGaleria({ node: "n3", marca: true, renderer: "trama/tabela", imagensPadrao: false });
  assert.deepEqual(op, { op: "set_galeria", node: "n3", valor: null });
  // padrão dentro (imagem), marca FALSE; alternar põe -> igual ao padrão -> null
  const op2 = opAlternarGaleria({ node: "n3b", marca: false, renderer: "trama/image", imagensPadrao: true });
  assert.deepEqual(op2, { op: "set_galeria", node: "n3b", valor: null });
});

test("op de alternar, caso 4: marca explícita divergente do padrão -> grava o desejado", () => {
  // padrão dentro, marca FALSE (fora); alternar põe -> true == padrão -> null
  const op = opAlternarGaleria({ node: "n4", marca: false, renderer: "trama/image", imagensPadrao: true });
  assert.equal(op.valor, null);
  // padrão dentro, marca TRUE; alternar tira -> false, divergente do padrão -> grava false
  const op2 = opAlternarGaleria({ node: "n4b", marca: true, renderer: "trama/image", imagensPadrao: true });
  assert.deepEqual(op2, { op: "set_galeria", node: "n4b", valor: false });
});

test("op de alternar aceita 'quer' explícito", () => {
  const op = opAlternarGaleria({ node: "n5", renderer: "trama/tabela", imagensPadrao: false, quer: false });
  assert.equal(op.valor, null); // desejado false == padrão false
  const op2 = opAlternarGaleria({ node: "n5b", marca: true, renderer: "trama/image", imagensPadrao: true, quer: true });
  assert.equal(op2.valor, null); // desejado true == padrão true
});

test("navegação circular no lightbox", () => {
  assert.equal(navegar(0, 3, 1), 1);
  assert.equal(navegar(2, 3, 1), 0);   // último -> primeiro
  assert.equal(navegar(0, 3, -1), 2);  // primeiro -> último
  assert.equal(navegar(1, 3, -1), 0);
  assert.equal(navegar(0, 1, 1), 0);   // um item só: fica
  assert.equal(navegar(0, 1, -1), 0);
  assert.equal(navegar(0, 0, 1), 0);   // sem itens
  assert.equal(navegar(4, 5, 7), 1);   // passo maior que o total dá a volta
});

test("URL com versão para cache-busting", () => {
  assert.equal(urlComVersao("/arq/preview.png", 3), "/arq/preview.png?v=3");
  assert.equal(urlComVersao("/arq/preview.png?x=1", "abc"), "/arq/preview.png?x=1&v=abc");
  assert.equal(urlComVersao("/arq/preview.png", null), "/arq/preview.png");
  assert.equal(urlComVersao("/arq/preview.png", undefined), "/arq/preview.png");
  assert.equal(urlComVersao(null, 2), "");
  assert.equal(urlComVersao("", 2), "");
  assert.equal(urlComVersao("/a b.png", "1 2"), "/a b.png?v=1%202");
});

test("dica da faixa vazia", () => {
  assert.equal(DICA_GALERIA_VAZIA, "Marque um card com ☆ para trazê-lo à galeria");
});
