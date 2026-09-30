// tests/js/bases.test.mjs
import { test } from "node:test";
import assert from "node:assert/strict";
import { filtrarBases, temasDe, pacotesDe, dimensao } from "../../inst/www/bases.js";

const B = [
  { id: "datasets::mtcars", pacote: "datasets", nome: "mtcars", titulo: "Consumo de 32 carros",
    temas: ["regressão", "didático"], n: 32, variaveis: 11, instalado: true },
  { id: "agridat::yates.oats", pacote: "agridat", nome: "yates.oats",
    titulo: "Parcelas subdivididas de aveia", fonte: "Rothamsted", temas: ["parcelas subdivididas", "experimentos"],
    n: 72, variaveis: 8, instalado: false },
  { id: "MASS::oats", pacote: "MASS", nome: "oats", titulo: "Aveia em parcelas subdivididas (Yates)",
    temas: ["experimentos"], n: 72, instalado: true },
];

test("sem termo nem filtro devolve tudo na ordem do servidor", () => {
  assert.deepEqual(filtrarBases(B).map((b) => b.id), B.map((b) => b.id));
});
test("termo ignora acento e caixa, e título pesa mais", () => {
  assert.deepEqual(filtrarBases(B, { termo: "REGRESSAO" }).map((b) => b.id), ["datasets::mtcars"]);
  const aveia = filtrarBases(B, { termo: "aveia" }).map((b) => b.id);
  assert.deepEqual(aveia.sort(), ["MASS::oats", "agridat::yates.oats"]);
  assert.deepEqual(filtrarBases(B, { termo: "rothamsted" }).map((b) => b.id), ["agridat::yates.oats"]);
  assert.deepEqual(filtrarBases(B, { termo: "zzz" }), []);
});
test("tema e pacote filtram por igualdade e combinam com o termo", () => {
  assert.deepEqual(filtrarBases(B, { tema: "experimentos" }).map((b) => b.id),
    ["agridat::yates.oats", "MASS::oats"]);
  assert.deepEqual(filtrarBases(B, { tema: "experimentos", pacote: "MASS" }).map((b) => b.id), ["MASS::oats"]);
  assert.deepEqual(filtrarBases(B, { termo: "carros", tema: "experimentos" }), []);
});
test("temas e pacotes contados, mais frequente primeiro", () => {
  assert.deepEqual(temasDe(B)[0], { valor: "experimentos", total: 2 });
  assert.deepEqual(pacotesDe(B).map((p) => p.valor), ["agridat", "datasets", "MASS"]);
});
test("dimensao formata linhas e colunas", () => {
  assert.equal(dimensao(B[0]), "32 × 11");
  assert.equal(dimensao(B[2]), "72 linhas");
  assert.equal(dimensao({ n: 10000, variaveis: 4 }), "10.000 × 4");
  assert.equal(dimensao({}), "");
});
