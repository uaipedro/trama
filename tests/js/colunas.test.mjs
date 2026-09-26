// tests/js/colunas.test.mjs
import { test } from "node:test";
import assert from "node:assert/strict";
import {
  anotado, opcoes, sugerir, sumidas, alternativa, opsDeSugestao,
} from "../../inst/www/colunas.js";

const col = (nome, papel, n_distintos = 10) => ({ nome, papel, classe: "", n_distintos, tem_na: false });
const iris = {
  colunas: [
    col("Sepal.Length", "numerica", 35), col("Sepal.Width", "numerica", 23),
    col("Petal.Length", "numerica", 43), col("Petal.Width", "numerica", 22),
    col("Species", "categorica", 3),
  ],
  truncado: false, amostra: null,
};
const mtcars = { colunas: [col("mpg", "numerica"), col("cyl", "numerica"), col("disp", "numerica")], truncado: false };
const num = (name, extra = {}) => ({ name, kind: "cols", role: "numerica", multi: false, ...extra });
const cat = (name) => ({ name, kind: "cols", role: "categorica", multi: false });

test("anotado exige kind cols e role", () => {
  assert.equal(anotado(num("x")), true);
  assert.equal(anotado({ name: "x", kind: "cols" }), false);
  assert.equal(anotado({ name: "x", kind: "text", role: "numerica" }), false);
});
test("opcoes marca quem serve, na ordem da tabela", () => {
  const o = opcoes(cat("g"), iris);
  assert.deepEqual(o.map((c) => c.nome), iris.colunas.map((c) => c.nome));
  assert.deepEqual(o.filter((c) => c.serve).map((c) => c.nome), ["Species"]);
  assert.ok(opcoes({ name: "c", kind: "cols" }, iris).every((c) => c.serve));
  assert.deepEqual(opcoes(cat("g"), null), []);
});
test("dispersão no iris: x e y numéricas distintas", () => {
  const r = sugerir([num("x"), num("y")], {}, [], iris);
  assert.deepEqual(r.map(({ name, value }) => [name, value]), [["x", "Sepal.Length"], ["y", "Sepal.Width"]]);
  assert.match(r[0].motivo, /numérica/);
});
test("boxplot: x categórica, y numérica", () => {
  const r = sugerir([cat("x"), num("y")], { x: "", y: null }, [], iris);
  assert.deepEqual(r.map(({ name, value }) => [name, value]), [["x", "Species"], ["y", "Sepal.Length"]]);
});
test("não sobrescreve escolha e a escolha conta como usada", () => {
  const r = sugerir([num("x"), num("y")], { x: "Sepal.Length" }, [], iris);
  assert.deepEqual(r.map(({ name, value }) => [name, value]), [["y", "Sepal.Width"]]);
  const r2 = sugerir([num("x"), num("y")], { y: "Sepal.Length" }, [], iris);
  assert.deepEqual(r2.map(({ name, value }) => [name, value]), [["x", "Sepal.Width"]]);
});
test("re-sugere o que veio de sugestão quando a tabela muda", () => {
  const r = sugerir([num("x"), num("y")], { x: "Sepal.Length", y: "Sepal.Width" }, ["x", "y"], mtcars);
  assert.deepEqual(r.map(({ name, value }) => [name, value]), [["x", "mpg"], ["y", "cyl"]]);
});
test("re-sugestão sem mudança não gera entrada", () => {
  assert.deepEqual(sugerir([num("x"), num("y")], { x: "Sepal.Length", y: "Sepal.Width" }, ["x", "y"], iris), []);
});
test("categórica pula coluna com cara de id", () => {
  const s = { colunas: [col("id", "categorica", 150), col("grupo", "categorica", 4)], truncado: false };
  assert.equal(sugerir([cat("g")], {}, [], s)[0].value, "grupo");
  const s2 = { colunas: [col("id", "categorica", 150)], truncado: false };
  assert.equal(sugerir([cat("g")], {}, [], s2)[0].value, "id");
});
test("sem coluna que sirva não sugere nem limpa", () => {
  assert.deepEqual(sugerir([cat("g")], {}, [], mtcars), []);
  assert.deepEqual(sugerir([cat("g")], { g: "x" }, ["g"], mtcars), []);
});
test("multi e não anotado nunca são sugeridos", () => {
  assert.deepEqual(sugerir([num("x", { multi: true })], {}, [], iris), []);
  assert.deepEqual(sugerir([{ name: "c", kind: "cols" }], {}, [], iris), []);
});
test("sumidas separa por vírgula", () => {
  const ps = [{ name: "c", kind: "cols" }, num("x")];
  assert.deepEqual(sumidas(ps, { c: "a, Species, ,b", x: "Sepal.Length" }, iris), [{ name: "c", faltam: ["a", "b"] }]);
});
test("tabela truncada não acusa sumidas", () => {
  assert.deepEqual(sumidas([num("x")], { x: "zz" }, { ...iris, truncado: true }), []);
});
test("alternativa por nome parecido e mesmo papel", () => {
  assert.equal(alternativa(num("x"), "MPG", mtcars), "mpg");
  assert.equal(alternativa(num("x"), "Sepal.Len", iris), "Sepal.Length");
  assert.equal(alternativa(num("x"), "Species2", iris), null);
  assert.equal(alternativa(num("x"), "qqq", iris), null);
});
test("schema nulo: nada em lugar nenhum", () => {
  assert.deepEqual(sugerir([num("x")], {}, [], null), []);
  assert.deepEqual(sumidas([num("x")], { x: "a" }, null), []);
  assert.equal(alternativa(num("x"), "a", null), null);
  assert.deepEqual(opsDeSugestao("n1", [num("x")], {}, [], null), []);
});
test("opsDeSugestao no formato do set_param", () => {
  assert.deepEqual(opsDeSugestao("n1", [num("x")], {}, undefined, iris), [
    { op: "set_param", node: "n1", name: "x", value: "Sepal.Length", origem: "sugestao" },
  ]);
});
