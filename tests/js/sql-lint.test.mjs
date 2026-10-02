// Análise do SQL do editor da coleção `trama.sql` (lint.js), com o mesmo
// parser do vendor que o navegador carrega.
import { test } from "node:test";
import assert from "node:assert/strict";
import { analisar, maisProximo, tabelasDoHandle } from "../../collections/trama.sql/inst/trama/lint.js";
import { Parser } from "../../collections/trama.sql/inst/trama/vendor/sql-vendor.js";

const parser = new Parser();
const tabelas = [
  { nome: "vendas", colunas: [{ nome: "id" }, { nome: "cliente_id" }, { nome: "valor" }, { nome: "ano" }] },
  { nome: "clientes", colunas: [{ nome: "id" }, { nome: "nome" }, { nome: "uf" }] },
];
const lint = (q) => analisar(q, { parser, tabelas });
const msgs = (q) => lint(q).diagnosticos.map((d) => d.mensagem);

test("consultas padrão de aula passam sem aviso", () => {
  for (const q of [
    "SELECT * FROM vendas",
    "select v.valor, c.nome from vendas v join clientes c on v.cliente_id = c.id where v.ano > 2020",
    "SELECT uf, count(*) AS n FROM clientes GROUP BY uf HAVING count(*) > 1 ORDER BY n DESC",
    "WITH t AS (SELECT ano, sum(valor) AS total FROM vendas GROUP BY ano) SELECT * FROM t",
    "SELECT ano, avg(valor) OVER (PARTITION BY ano) FROM vendas",
    "SELECT CASE WHEN valor > 10 THEN 'alto' ELSE 'baixo' END AS faixa FROM vendas",
    "SELECT DISTINCT uf FROM clientes UNION SELECT 'MG'",
    "",
  ]) assert.deepEqual(lint(q), { ok: true, diagnosticos: [] }, q);
});

test("extras do DuckDB são recusados com explicação", () => {
  assert.match(msgs("FROM vendas")[0], /Comece com SELECT/);
  assert.match(msgs("SELECT * FROM vendas QUALIFY row_number() OVER () = 1")[0], /QUALIFY/);
  assert.match(msgs("SELECT * FROM 'vendas.csv'")[0], /arquivo direto.*"vendas"/);
});

test("só leitura: escrita é recusada antes do parser", () => {
  assert.match(msgs("DELETE FROM vendas")[0], /SELECT/);
  assert.equal(lint("DROP TABLE vendas").ok, false);
});

test("erros de sintaxe em português, no lugar certo", () => {
  const q = "SELECT valor, FROM vendas";
  const d = lint(q).diagnosticos[0];
  assert.equal(d.mensagem, "Vírgula sobrando antes de FROM.");
  assert.equal(d.grave, true);
  assert.equal(q.slice(d.from, d.to), ",");
  assert.match(msgs("SELECT valor FROM vendas WHERE")[0], /terminou logo depois de WHERE/);
  assert.match(msgs("SELECT 1; SELECT 2")[0], /Uma consulta por bloco/);
});

test("nomes que não existem na fonte, com sugestão", () => {
  const q = "SELECT * FROM venda";
  const d = lint(q).diagnosticos[0];
  assert.equal(q.slice(d.from, d.to), "venda");
  assert.match(d.mensagem, /"venda" não existe.*"vendas"/);
  assert.equal(lint(q).ok, false, "tabela inexistente bloqueia rodar");
  assert.equal(lint("SELECT valr FROM vendas").ok, true, "coluna desconhecida é só aviso");
  assert.match(msgs("SELECT valr FROM vendas")[0], /"valr" não existe.*"valor"/);
  assert.match(msgs("SELECT v.nome FROM vendas v")[0], /"nome" não existe em vendas/);
  // Apelido do SELECT vale no ORDER BY.
  assert.equal(lint("SELECT valor * 2 AS dobro FROM vendas ORDER BY dobro").ok, true);
  assert.deepEqual(lint("SELECT dobro FROM vendas ORDER BY dobro").diagnosticos.length, 1);
});

test("sem esquema (fonte desligada) só cobra sintaxe", () => {
  assert.deepEqual(analisar("SELECT x FROM qualquer", { parser }), { ok: true, diagnosticos: [] });
});

test("utilitários", () => {
  assert.equal(maisProximo("clientez", ["vendas", "clientes"]), "clientes");
  assert.equal(maisProximo("zzz", ["vendas"]), null);
  assert.deepEqual(tabelasDoHandle({ preview: { data: { tabelas: [{ nome: "a", colunas: [{ nome: "x", tipo: "INTEGER" }] }] } } }),
                   [{ nome: "a", colunas: [{ nome: "x", tipo: "INTEGER" }] }]);
  assert.deepEqual(tabelasDoHandle(null), []);
});
