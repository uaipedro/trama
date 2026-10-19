import test from "node:test";
import assert from "node:assert/strict";
import { automatico, ler, gravar, mover, tirar, por, diferencas, rotuloDoBloco }
  from "../../collections/trama.publicar/inst/trama/layout.js";

const fontes = [{ nome: "a", rotulo: "Tabela" }, { nome: "b", rotulo: "Modelo" }];

test("vazio é o automático, igual ao do R", () => {
  const r = ler("", fontes);
  assert.equal(r.automatico, true);
  assert.deepEqual(r.blocos, [{ tipo: "cabecalho" }, { tipo: "item", id: "a" }, { tipo: "item", id: "b" }, { tipo: "fluxo" }]);
  assert.deepEqual(automatico([]), [{ tipo: "cabecalho" }, { tipo: "fluxo" }]);
});

test("JSON inválido não apaga: volta o automático com o erro", () => {
  const r = ler("[{", fontes);
  assert.equal(r.automatico, true);
  assert.match(r.erro, /JSON inválido/);
  assert.match(ler("{}", fontes).erro, /não é uma lista/);
});

test("mover, tirar, pôr e gravar", () => {
  const b = ler(gravar([{ tipo: "cabecalho" }, { tipo: "item", id: "b" }]), fontes).blocos;
  assert.deepEqual(mover(b, 1, -1).map((x) => x.tipo), ["item", "cabecalho"]);
  assert.deepEqual(mover(b, 0, -1), b);
  assert.deepEqual(tirar(b, 0), [{ tipo: "item", id: "b" }]);
  assert.deepEqual(por(b, { tipo: "texto", texto: "oi" }, 0)[1], { tipo: "texto", texto: "oi" });
  assert.equal(JSON.parse(gravar(b)).length, 2);
});

test("diferenças: ligado fora do layout e layout citando o que não está ligado", () => {
  const d = diferencas([{ tipo: "item", id: "b" }, { tipo: "item", id: "z" }], fontes);
  assert.deepEqual(d.fora.map((f) => f.nome), ["a"]);
  assert.deepEqual(d.soltos, ["z"]);
  assert.equal(rotuloDoBloco({ tipo: "item", id: "z" }, fontes), "z (não ligado)");
  assert.equal(rotuloDoBloco({ tipo: "item", id: "a" }, fontes), "Tabela");
});
