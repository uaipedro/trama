// tests/js/fork.test.mjs
import { test } from "node:test";
import assert from "node:assert/strict";
import { planejarBifurcacao, gemeosDe, espelhar, diferencas, selo, proximaLetra }
  from "../../inst/www/fork.js";

// Fluxo de referência: dados -> filtro -> modelo -> resíduos -> gráfico, mais
// uma entrada de fora (dados -> gráfico, porta y) que o gráfico copiado
// precisa manter ligada ao MESMO dados.
const grafoBase = () => ({
  nodes: {
    dados: { type: "data/tabela", label: "Dados", params: { arquivo: "x.csv" }, seed: 1 },
    filtro: { type: "data/filtro", params: { cond: "a>0" } },
    modelo: { type: "models/linear", params: { formula: "y~x", alpha: 0.05 }, seed: 7 },
    residuos: { type: "models/residuos", params: { tipo: "padr" } },
    grafico: { type: "view/dispersao", params: { cor: "azul" } },
  },
  edges: [
    { from: { node: "dados", port: "out" }, to: { node: "filtro", port: "in" } },
    { from: { node: "filtro", port: "out" }, to: { node: "modelo", port: "in" } },
    { from: { node: "modelo", port: "out" }, to: { node: "residuos", port: "in" } },
    { from: { node: "residuos", port: "out" }, to: { node: "grafico", port: "x" } },
    { from: { node: "dados", port: "out" }, to: { node: "grafico", port: "y" } },
  ],
  positions: { dados: [0, 0], filtro: [0, 150], modelo: [0, 300], residuos: [0, 450], grafico: [0, 600] },
  sizes: {},
  ramos: {},
});

const contador = () => { let n = 0; return () => `n${++n}`; };

// Aplica um batch no grafo (o mínimo que o planejamento precisa): add_node,
// connect, add_ramo, set_param. Espelha o efeito das ops no documento.
function aplicar(grafo, op) {
  const g = structuredClone(grafo);
  const um = (o) => {
    if (o.op === "batch") return o.ops.forEach(um);
    if (o.op === "add_node") {
      g.nodes[o.id] = { type: o.type, label: o.label, params: o.params, seed: o.seed };
      if (o.position) g.positions[o.id] = o.position;
    } else if (o.op === "connect") {
      g.edges.push({ from: { node: o.from_node, port: o.from_port },
                     to: { node: o.to_node, port: o.to_port }, index: o.index });
    } else if (o.op === "add_ramo") {
      g.ramos[o.id] = { letra: o.letra, origem: o.origem, pares: o.pares, desligados: [] };
    } else if (o.op === "set_param") {
      g.nodes[o.node].params[o.name] = o.value;
    }
  };
  um(op);
  return g;
}

const conexoes = (op) => {
  const out = [];
  const um = (o) => (o.op === "batch" ? o.ops.forEach(um) : o.op === "connect" && out.push(o));
  um(op);
  return out;
};
const criados = (op) => {
  const out = [];
  const um = (o) => (o.op === "batch" ? o.ops.forEach(um) : o.op === "add_node" && out.push(o));
  um(op);
  return out;
};
const ramoDe = (op) => {
  let r = null;
  const um = (o) => (o.op === "batch" ? o.ops.forEach(um) : o.op === "add_ramo" && (r = o));
  um(op);
  return r;
};
const porId = (lista) => Object.fromEntries(lista.map((o) => [o.id, o]));

test("bifurcar modelo: cópias só de modelo, resíduos e gráfico", () => {
  const g = grafoBase();
  const { copias, ops } = planejarBifurcacao(g, "modelo", contador());
  assert.deepEqual(Object.keys(copias), ["modelo", "residuos", "grafico"]);
  assert.equal(ops.length, 1);
  assert.equal(ops[0].op, "batch");
  const nos = porId(criados(ops[0]));
  assert.deepEqual(Object.keys(nos).sort(), Object.values(copias).sort());
  // Params e tipo iguais; seed copiada; o original não é tocado.
  assert.equal(nos[copias.modelo].type, "models/linear");
  assert.deepEqual(nos[copias.modelo].params, { formula: "y~x", alpha: 0.05 });
  assert.equal(nos[copias.modelo].seed, 7);
  assert.deepEqual(nos[copias.residuos].params, { tipo: "padr" });
  assert.equal(g.nodes.modelo.params.alpha, 0.05);
});

test("bifurcar modelo: resíduos' ligado a modelo', modelo' ligado ao MESMO filtro", () => {
  const g = grafoBase();
  const { copias, ops } = planejarBifurcacao(g, "modelo", contador());
  const ligs = conexoes(ops[0]);
  // filtro é montante: não é copiado, e a entrada de modelo' sai dele.
  const entradaModelo = ligs.find((o) => o.to_node === copias.modelo);
  assert.deepEqual([entradaModelo.from_node, entradaModelo.from_port], ["filtro", "out"]);
  assert.equal(ligs.some((o) => o.to_node === "modelo"), false);
  // Internas reproduzidas entre as cópias.
  assert.ok(ligs.some((o) => o.from_node === copias.modelo && o.to_node === copias.residuos
    && o.from_port === "out" && o.to_port === "in"));
  assert.ok(ligs.some((o) => o.from_node === copias.residuos && o.to_node === copias.grafico
    && o.to_port === "x"));
  // Entrada de fora do conjunto (dados) é compartilhada com a cópia do gráfico.
  assert.ok(ligs.some((o) => o.from_node === "dados" && o.to_node === copias.grafico && o.to_port === "y"));
  // Nenhuma aresta de cópia para original, nem cópia como fonte de montante.
  assert.equal(ligs.length, 4);
  assert.equal(ligs.some((o) => o.from_node === "filtro" && o.to_node !== copias.modelo), false);
});

test("bifurcar modelo: add_ramo com letra B, origem e pares", () => {
  const g = grafoBase();
  const { copias, ops } = planejarBifurcacao(g, "modelo", contador());
  const r = ramoDe(ops[0]);
  assert.equal(r.letra, "B");
  assert.equal(r.origem, "modelo");
  assert.deepEqual(r.pares, copias);
  assert.ok(r.id && !Object.keys(g.nodes).includes(r.id));
});

test("cópias ficam abaixo do bloco original, sem sobrepor", () => {
  const g = grafoBase();
  const { copias, ops } = planejarBifurcacao(g, "modelo", contador());
  const nos = porId(criados(ops[0]));
  // Conjunto original vai de y=300 até 600+120=720; cópias começam depois.
  const fim = 720;
  Object.values(copias).forEach((c) => assert.ok(nos[c].position[1] >= fim + 48));
  // Mantém o layout relativo: resíduos' fica 150 abaixo de modelo'.
  assert.equal(nos[copias.residuos].position[1] - nos[copias.modelo].position[1], 150);
});

test("bifurcação aninhada: copiar resíduos' gera C sobre o grafo já bifurcado", () => {
  const g0 = grafoBase();
  const novo = contador();
  const p1 = planejarBifurcacao(g0, "modelo", novo);
  const g1 = aplicar(g0, p1.ops[0]);
  const b = p1.copias;
  const p2 = planejarBifurcacao(g1, b.residuos, novo);
  const c = p2.copias;
  assert.deepEqual(Object.keys(c), [b.residuos, b.grafico]);
  const ramoC = ramoDe(p2.ops[0]);
  assert.equal(ramoC.letra, "C");
  assert.equal(ramoC.origem, b.residuos);
  assert.deepEqual(Object.keys(ramoC.pares), [b.residuos, b.grafico]);
  // Montante compartilhado: modelo' (de B) alimenta resíduos'' sem copiá-lo.
  const ligs = conexoes(p2.ops[0]);
  assert.ok(ligs.some((o) => o.from_node === b.modelo && o.to_node === c[b.residuos]));
  assert.equal(ligs.some((o) => o.to_node === b.modelo), false);
  assert.ok(ligs.some((o) => o.from_node === "dados" && o.to_node === c[b.grafico]));
  // Aplicado, o documento tem dois ramos com letras distintas.
  const g2 = aplicar(g1, p2.ops[0]);
  assert.deepEqual(Object.values(g2.ramos).map((r) => r.letra).sort(), ["B", "C"]);
});

test("letras: próxima livre, sem contar ramo removido", () => {
  assert.equal(proximaLetra({}), "B");
  assert.equal(proximaLetra({ r1: { letra: "B" } }), "C");
  assert.equal(proximaLetra({ r1: { letra: "B" }, r2: { letra: "C" } }), "D");
  assert.equal(proximaLetra({ r2: { letra: "C" } }), "B");
});

test("gêmeos: classe pelos pares, pula origem e não inclui o próprio card", () => {
  const g = grafoBase();
  const { copias, ops } = planejarBifurcacao(g, "modelo", contador());
  const ramos = aplicar(g, ops[0]).ramos;
  assert.deepEqual(gemeosDe(ramos, "residuos"), [copias.residuos]);
  assert.deepEqual(gemeosDe(ramos, copias.residuos), ["residuos"]);
  assert.deepEqual(gemeosDe(ramos, "modelo"), []);
  assert.deepEqual(gemeosDe(ramos, copias.modelo), []);
  assert.deepEqual(gemeosDe(ramos, "filtro"), []);
});

test("gêmeos: aninhado é transitivo (gráfico'' entra na classe do gráfico)", () => {
  const g0 = grafoBase();
  const novo = contador();
  const p1 = planejarBifurcacao(g0, "modelo", novo);
  const g1 = aplicar(g0, p1.ops[0]);
  const p2 = planejarBifurcacao(g1, p1.copias.residuos, novo);
  const ramos = aplicar(g1, p2.ops[0]).ramos;
  const { residuos: rB, grafico: gB } = p1.copias;
  const gC = p2.copias[gB];
  assert.deepEqual(gemeosDe(ramos, "grafico"), [gB, gC].sort());
  assert.deepEqual(gemeosDe(ramos, gB), ["grafico", gC].sort());
  assert.deepEqual(gemeosDe(ramos, "residuos"), [rB]);
  // Regra literal do enunciado: o par (origem C = resíduos', resíduos'') não
  // liga resíduos'' à classe. Ver relatório: decisão em aberto.
  assert.deepEqual(gemeosDe(ramos, p2.copias[rB]), []);
});

test("gêmeos: cópia desligada sai da classe", () => {
  const g = grafoBase();
  const { copias, ops } = planejarBifurcacao(g, "modelo", contador());
  const ramos = aplicar(g, ops[0]).ramos;
  const id = Object.keys(ramos)[0];
  ramos[id].desligados = [copias.residuos];
  assert.deepEqual(gemeosDe(ramos, "residuos"), []);
  assert.deepEqual(gemeosDe(ramos, copias.residuos), []);
  // Desligar o resíduos não afeta o gráfico' (outro par).
  assert.deepEqual(gemeosDe(ramos, "grafico"), [copias.grafico]);
});

test("gêmeos: ciclo de pares não trava", () => {
  // Triângulo c-d, d-a, a-c (cada ramo com origem fora do par, então valem).
  const ramos = {
    r1: { letra: "B", origem: "x", pares: { c: "d" }, desligados: [] },
    r2: { letra: "C", origem: "y", pares: { d: "a" }, desligados: [] },
    r3: { letra: "D", origem: "z", pares: { a: "c" }, desligados: [] },
  };
  assert.deepEqual(gemeosDe(ramos, "c"), ["a", "d"]);
  assert.deepEqual(gemeosDe(ramos, "a"), ["c", "d"]);
});

test("espelhar: set_param no gêmeo vai para o outro; origem e cópia não se espelham", () => {
  const g = grafoBase();
  const { copias, ops } = planejarBifurcacao(g, "modelo", contador());
  const ramos = aplicar(g, ops[0]).ramos;
  const op = { op: "set_param", node: "residuos", name: "tipo", value: "stud" };
  assert.deepEqual(espelhar(op, ramos), [{ ...op, node: copias.residuos }]);
  const pulaOrigem = { op: "set_param", node: "modelo", name: "alpha", value: 0.1 };
  assert.deepEqual(espelhar(pulaOrigem, ramos), []);
  assert.deepEqual(espelhar({ ...pulaOrigem, node: copias.modelo }, ramos), []);
});

test("espelhar: pula cópia desligada", () => {
  const g = grafoBase();
  const { copias, ops } = planejarBifurcacao(g, "modelo", contador());
  const ramos = aplicar(g, ops[0]).ramos;
  ramos[ramoDe(ops[0]).id].desligados = [copias.residuos];
  const op = { op: "set_param", node: "residuos", name: "tipo", value: "stud" };
  assert.deepEqual(espelhar(op, ramos), []);
});

test("espelhar: batch espelha cada set_param; não duplica o que já é explícito", () => {
  const g = grafoBase();
  const { copias, ops } = planejarBifurcacao(g, "modelo", contador());
  const ramos = aplicar(g, ops[0]).ramos;
  // Explícitos em residuos e residuos': nada a espelhar entre eles.
  const par = { op: "batch", ops: [
    { op: "set_param", node: "residuos", name: "tipo", value: "stud" },
    { op: "set_param", node: copias.residuos, name: "tipo", value: "stud" },
  ] };
  assert.deepEqual(espelhar(par, ramos), []);
  // Só um dos dois explícito: o outro recebe, uma vez.
  const misto = { op: "batch", ops: [
    { op: "move", id: "dados", x: 1, y: 1 },
    { op: "set_param", node: "residuos", name: "tipo", value: "stud" },
    { op: "set_param", node: "grafico", name: "cor", value: "verde" },
  ] };
  const extras = espelhar(misto, ramos);
  assert.deepEqual(extras.map((e) => [e.node, e.name, e.value]).sort(),
    [[copias.grafico, "cor", "verde"], [copias.residuos, "tipo", "stud"]].sort());
  // Não toca no original nem devolve o card da própria op.
  assert.ok(extras.every((e) => e.node !== "residuos" && e.node !== "grafico"));
});

test("espelhar: op que não é set_param não gera nada", () => {
  const g = grafoBase();
  const { ops } = planejarBifurcacao(g, "modelo", contador());
  const ramos = aplicar(g, ops[0]).ramos;
  assert.deepEqual(espelhar({ op: "set_seed", node: "residuos", seed: 3 }, ramos), []);
  assert.deepEqual(espelhar({ op: "batch", ops: [{ op: "rename", id: "residuos", label: "x" }] }, ramos), []);
});

test("diferenças: origem contra a cópia, por param", () => {
  const g = grafoBase();
  const { copias, ops } = planejarBifurcacao(g, "modelo", contador());
  const ramo = ramoDe(ops[0]);
  const gr = aplicar(g, ops[0]);
  assert.deepEqual(diferencas(gr, { ...ramo, pares: ramo.pares }), []);
  gr.nodes[copias.modelo].params = { formula: "y~x+z", alpha: 0.05, extra: 2 };
  assert.deepEqual(diferencas(gr, { ...ramo, pares: ramo.pares }), [
    { param: "extra", de: null, para: 2 },
    { param: "formula", de: "y~x", para: "y~x+z" },
  ]);
});

test("selo: letra do ramo de quem criou a cópia; decide na origem e na cópia dela", () => {
  const g0 = grafoBase();
  const novo = contador();
  const p1 = planejarBifurcacao(g0, "modelo", novo);
  const g1 = aplicar(g0, p1.ops[0]);
  const p2 = planejarBifurcacao(g1, p1.copias.residuos, novo);
  const ramos = aplicar(g1, p2.ops[0]).ramos;
  assert.deepEqual(selo(ramos, "modelo"), { letra: "A", decide: true });
  assert.deepEqual(selo(ramos, "grafico"), { letra: "A", decide: false });
  assert.deepEqual(selo(ramos, p1.copias.modelo), { letra: "B", decide: true });
  assert.deepEqual(selo(ramos, p1.copias.residuos), { letra: "B", decide: true });
  assert.deepEqual(selo(ramos, p2.copias[p1.copias.residuos]), { letra: "C", decide: true });
  assert.deepEqual(selo(ramos, p2.copias[p1.copias.grafico]), { letra: "C", decide: false });
  assert.deepEqual(selo(ramos, "filtro"), { letra: null, decide: false });
});

test("erro claro para origem inexistente", () => {
  assert.throws(() => planejarBifurcacao(grafoBase(), "nao_existe", contador()), /nao_existe/);
});

test("correspondentes inclui o par de origem e desligados; ramoDaCopia", async () => {
  const { correspondentes, ramoDaCopia } = await import("../../inst/www/fork.js");
  const ramos = { r1: { letra: "B", origem: "m", pares: { m: "m2", g: "g2" }, desligados: ["g2"] },
                  r2: { letra: "C", origem: "m2", pares: { m2: "m3" }, desligados: [] } };
  assert.deepEqual(correspondentes(ramos, "m"), ["m2", "m3"]);
  assert.deepEqual(correspondentes(ramos, "g"), ["g2"]);
  assert.deepEqual(correspondentes(ramos, "x"), []);
  assert.deepEqual(ramoDaCopia(ramos, "g2"), { ramo: "r1", letra: "B", desligado: true });
  assert.equal(ramoDaCopia(ramos, "m"), null);
});
