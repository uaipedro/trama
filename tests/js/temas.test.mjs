// tests/js/temas.test.mjs
import { test } from "node:test";
import assert from "node:assert/strict";
import { nomeLivre, erroNome, duplicar, renomear, apagar, HEX } from "../../inst/www/temas.js";

const t = (fundo) => ({ base: "minimal", tamanho: 11, fonte: "sans", fundo, texto: "#000000",
  eixos: "#000000", grade: "#000000", paleta: ["#111111"], continua: "viridis" });
const est = () => ({ temas: { claro: t("#ffffff"), escuro: t("#000000"), sepia: t("#eeddcc") },
                     tema_padrao: "escuro" });

test("nome livre numera a partir de 2", () => {
  assert.equal(nomeLivre("novo tema", ["claro"]), "novo tema");
  assert.equal(nomeLivre("novo tema", ["novo tema"]), "novo tema 2");
  assert.equal(nomeLivre("novo tema", ["novo tema", "novo tema 2"]), "novo tema 3");
});
test("nome vazio, reservado ou repetido é recusado", () => {
  assert.equal(erroNome("  ", ["a"], "a"), "nome vazio");
  assert.match(erroNome("padrão", ["a"], "a"), /reservado/);
  assert.match(erroNome("b", ["a", "b"], "a"), /já existe/);
  assert.equal(erroNome("a", ["a", "b"], "a"), null);
  assert.equal(erroNome("c", ["a", "b"], "a"), null);
});
test("duplicar copia fundo e não compartilha a paleta", () => {
  const s = est();
  const { estado, novo } = duplicar(s, "sepia");
  assert.equal(novo, "novo tema");
  assert.equal(estado.temas[novo].fundo, "#eeddcc");
  estado.temas[novo].paleta.push("#222222");
  assert.equal(s.temas.sepia.paleta.length, 1);
});
test("renomear mantém a ordem e leva o padrão junto", () => {
  const r = renomear(est(), "escuro", "noite");
  assert.deepEqual(Object.keys(r.temas), ["claro", "noite", "sepia"]);
  assert.equal(r.tema_padrao, "noite");
  assert.equal(renomear(est(), "claro", "dia").tema_padrao, "escuro");
});
test("apagar o padrão promove o primeiro que sobra", () => {
  const r = apagar(est(), "escuro");
  assert.deepEqual(Object.keys(r.temas), ["claro", "sepia"]);
  assert.equal(r.tema_padrao, "claro");
  assert.equal(apagar(est(), "sepia").tema_padrao, "escuro");
});
test("a marca atravessa as operações da lista de temas", () => {
  const s = { ...est(), marca: false };
  assert.equal(duplicar(s, "sepia").estado.marca, false);
  assert.equal(renomear(s, "escuro", "noite").marca, false);
  assert.equal(apagar(s, "escuro").marca, false);
  // Rascunho sem a chave (antes da primeira mensagem `themes`) vale ligada,
  // como no servidor — nunca `undefined`, que o servidor recusaria.
  assert.equal(apagar(est(), "escuro").marca, true);
});
test("hex só aceita #rrggbb", () => {
  assert.ok(HEX.test("#a1B2c3"));
  assert.ok(!HEX.test("#abc"));
  assert.ok(!HEX.test("a1b2c3"));
});
