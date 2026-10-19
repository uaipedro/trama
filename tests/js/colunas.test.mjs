// tests/js/colunas.test.mjs
import { test } from "node:test";
import assert from "node:assert/strict";
import {
  anotado, opcoes, sugerir, sumidas, alternativa, opsDeSugestao, colsTexto, colsValor,
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
const cat = (name, extra = {}) => ({ name, kind: "cols", role: "categorica", multi: false, ...extra });

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
test("categórica pula coluna com cara de id e não cai nela sem alternativa", () => {
  const s = { colunas: [col("id", "categorica", 150), col("grupo", "categorica", 4)], truncado: false };
  assert.equal(sugerir([cat("g")], {}, [], s)[0].value, "grupo");
  const s2 = { colunas: [col("id", "categorica", 150)], truncado: false };
  assert.deepEqual(sugerir([cat("g")], {}, [], s2), []);
});
test("sem coluna que sirva não sugere nem limpa", () => {
  assert.deepEqual(sugerir([cat("g")], {}, [], mtcars), []);
  assert.deepEqual(sugerir([cat("g")], { g: "x" }, ["g"], mtcars), []);
});
test("suggest: false fica fora ao conectar, mas sai a pedido com forcar", () => {
  const ps = [num("x"), cat("cor", { suggest: false })];
  assert.deepEqual(sugerir(ps, {}, [], iris).map((r) => r.name), ["x"]);
  const r = sugerir(ps, {}, [], iris, { forcar: "cor" });
  assert.equal(r.find((s) => s.name === "cor").value, "Species");
  assert.deepEqual(opsDeSugestao("n", ps, {}, [], iris).map((o) => o.name), ["x"]);
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

test("sumidas não acusa seletor escrito como R, mas ainda acusa nome sumido", () => {
  const p = { name: "cols", kind: "cols", multi: true };
  assert.deepEqual(sumidas([p], { cols: 'starts_with("Sepal", ignore.case = TRUE)' }, iris), []);
  assert.deepEqual(sumidas([p], { cols: "Sepal.Length:Petal.Width" }, iris), []);
  assert.deepEqual(sumidas([p], { cols: "Sepal.Length, mpg" }, iris), [{ name: "cols", faltam: ["mpg"] }]);
});

test("sumidas acusa o nome simples sumido mesmo ao lado de seletor ou de nome com parêntese", () => {
  const p = { name: "cols", kind: "cols", multi: true };
  assert.deepEqual(sumidas([p], { cols: 'starts_with("Sepal"), zz' }, iris), [{ name: "cols", faltam: ["zz"] }]);
  const com = { colunas: [col("Preço (R$)", "numerica")], truncado: false };
  assert.deepEqual(sumidas([p], { cols: "Preço (R$), zz" }, com), [{ name: "cols", faltam: ["zz"] }]);
  const dois = { colunas: [col("a:b", "numerica")], truncado: false };
  assert.deepEqual(sumidas([p], { cols: "a:b" }, dois), []);
});

// #32: fluxo salvo com `valores: ["ibv", "sp"]` saía do editor com "ibv,sp"
// e o bloco recusava. O campo mostra texto, mas grava na forma do documento.
test("colsTexto mostra lista como texto separado por vírgula", () => {
  assert.equal(colsTexto(["ibv", "sp"]), "ibv, sp");
  assert.equal(colsTexto([]), "");
  assert.equal(colsTexto("a, b"), "a, b");
  assert.equal(colsTexto(""), "");
  assert.equal(colsTexto(null), "");
  assert.equal(colsTexto(undefined), "");
});
test("colsValor devolve na forma do valor atual", () => {
  // lista continua lista, aparada e sem item vazio
  assert.deepEqual(colsValor("ibv, sp", ["ibv", "sp"], []), ["ibv", "sp"]);
  assert.deepEqual(colsValor(" ibv ,sp, x, ", ["ibv"], ""), ["ibv", "sp", "x"]);
  assert.deepEqual(colsValor("", ["ibv", "sp"], []), []);
  // texto continua texto, como o usuário digitou
  assert.equal(colsValor("a,b", "a", ""), "a,b");
  // vazio segue o padrão da spec
  assert.deepEqual(colsValor("ibv, sp", undefined, []), ["ibv", "sp"]);
  assert.deepEqual(colsValor("ibv, sp", "", []), ["ibv", "sp"]);
  assert.equal(colsValor("ibv, sp", undefined, ""), "ibv, sp");
  assert.equal(colsValor("ibv, sp", null, undefined), "ibv, sp");
});
test("o que o campo mostra volta igual ao documento (#32)", () => {
  // O campo só grava se o texto mudou; e se gravar o mesmo texto, a forma e
  // o conteúdo voltam iguais.
  for (const [atual, padrao] of [[["ibv", "sp"], []], [["a"], ""], ["ibv, sp", ""], ["a", "a"]]) {
    assert.deepEqual(colsValor(colsTexto(atual), atual, padrao), atual);
  }
});
