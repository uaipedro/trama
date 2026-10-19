import test from "node:test";
import assert from "node:assert/strict";
import { INICIAL, reduzir, resumo, pendencias, pedidoInserir, ehTabela, nomeValido, resolucoes, textoOcupado }
  from "../../plugins/trama.documentos/inst/trama/painel.js";

const lista = { tipo: "lista", quarto: true, perfil_padrao: "ufla",
  documentos: [{ nome: "rel", caminho: "documentos/rel/rel.qmd", modelo: "basico", resumo: { atualizada: 2, conflito: 1 } }],
  modelos: [{ nome: "basico", titulo: "Básico" }], perfis: [{ perfil: "ufla", nome: "Ana" }] };
const card = { id: "tab", rotulo: "Tabela", saidas: [{ porta: "out", tipo: "data/table" }, { porta: "b", tipo: "view/plot" }] };
const passo = (s, ...as) => as.reduce(reduzir, s);

test("lista: um documento só já vem selecionado; sumiu, deseleciona", () => {
  let s = passo(INICIAL, lista);
  assert.equal(s.fase, "pronto");
  assert.equal(s.selecionado, "documentos/rel/rel.qmd");
  s = passo(s, { ...lista, documentos: [] });
  assert.equal(s.selecionado, null);
  assert.equal(passo(INICIAL, { ...lista, quarto: false }).quarto, false);
});

test("inserir pelo card: abre o painel, escolhe o documento e a primeira saída", () => {
  const s = passo(INICIAL, lista, { tipo: "inserir_card", card });
  assert.equal(s.aberto, true);
  assert.equal(s.inserir.documento, "documentos/rel/rel.qmd");
  assert.equal(s.inserir.porta, "out");
  assert.ok(ehTabela(s.inserir));
  assert.ok(!ehTabela({ ...s.inserir, porta: "b" }));
  const p = pedidoInserir(passo(s, { tipo: "inserir_campo", campo: "linhas", valor: "50" },
                                   { tipo: "inserir_campo", campo: "completa", valor: true }).inserir);
  assert.deepEqual(p, { documento: "documentos/rel/rel.qmd", no: "tab", porta: "out",
                        opcoes: { linhas: 50, completa: true }, aceitar: false });
  assert.equal(pedidoInserir({ ...s.inserir, documento: null }), null);
  assert.equal(pedidoInserir({ ...s.inserir, enviando: true }), null);
  assert.equal(pedidoInserir({ ...s.inserir, linhas: "-3" }).opcoes.linhas, null);
});

test("dados de treino: o erro vira pedido de aceite, não beco sem saída", () => {
  let s = passo(INICIAL, lista, { tipo: "inserir_card", card }, { tipo: "inserir_enviando" });
  s = passo(s, { tipo: "erro", acao: "inserir", classe: "tr_documento_erro_dados_treino", mensagem: "leva dados de treino" });
  assert.equal(s.inserir.pedirAceite, true);
  assert.equal(s.inserir.enviando, false);
  s = passo(s, { tipo: "inserir_campo", campo: "aceitar", valor: true });
  assert.equal(pedidoInserir(s.inserir).aceitar, true);
  s = passo(s, { tipo: "inserido", documento: "documentos/rel/rel.qmd", regiao: "tab", avisos: [] });
  assert.equal(s.inserir, null);
  assert.match(s.aviso, /#trama-tab/);
});

test("novo documento criado no meio de um inserir vira o destino", () => {
  let s = passo(INICIAL, { ...lista, documentos: [] }, { tipo: "inserir_card", card });
  assert.equal(s.inserir.documento, null);
  s = passo(s, { tipo: "novo_abrir" });
  assert.deepEqual(s.novo, { nome: "", titulo: "", modelo: "basico", perfil: "ufla" });
  s = passo(s, { tipo: "criado", documento: "documentos/novo/novo.qmd" });
  assert.equal(s.novo, null);
  assert.equal(s.inserir.documento, "documentos/novo/novo.qmd");
});

test("ocupado some ao terminar ou falhar; atualizado resume", () => {
  const d = "documentos/rel/rel.qmd";
  let s = passo(INICIAL, lista, { tipo: "ocupado", documento: d, tarefa: "pdf" });
  assert.equal(textoOcupado(s.ocupado[d]), "Exportando PDF…");
  s = passo(s, { tipo: "exportado", documento: d, formato: "pdf", arquivo: "documentos/rel/_saida/rel.pdf" });
  assert.equal(s.ocupado[d], undefined);
  assert.equal(s.exportados[d].arquivo, "documentos/rel/_saida/rel.pdf");
  s = passo(s, { tipo: "ocupado", documento: d, tarefa: "atualizar" },
              { tipo: "atualizado", documento: d, resultado: [{ estado: "atualizada" }, { estado: "conflito" }, { estado: "igual" }] });
  assert.equal(s.aviso, "1 região atualizada. 1 precisa de você.");
  s = passo(s, { tipo: "ocupado", documento: d, tarefa: "previa" }, { tipo: "erro", acao: "render", documento: d, mensagem: "falhou" });
  assert.equal(s.ocupado[d], undefined);
  assert.equal(s.erro, "falhou");
});

test("reabrir o painel limpa o aviso e o erro da vez anterior", () => {
  const s = passo(INICIAL, { tipo: "erro", mensagem: "x" }, { tipo: "fechar" }, { tipo: "abrir" });
  assert.equal(s.erro, null);
  assert.equal(s.aviso, null);
});

test("resumo, pendências, nome e resoluções", () => {
  assert.equal(resumo({ atualizada: 2, conflito: 1 }), "3 regiões · 1 editada à mão");
  assert.equal(resumo({}), "sem regiões ainda");
  assert.equal(pendencias(lista.documentos), 1);
  assert.ok(nomeValido("relatorio-final"));
  assert.ok(!nomeValido("../x"));
  assert.ok(!nomeValido(""));
  assert.deepEqual(resolucoes("conflito").map((r) => r.como), ["manter", "trazer"]);
  assert.deepEqual(resolucoes("atualizada"), []);
});

test("confirmar: o R pede, o painel mostra a confirmação forte", () => {
  let s = passo(INICIAL, { tipo: "pedir_confirmacao", documento: "d", regiao: "tab", como: "remover" });
  s = passo(s, { tipo: "erro", acao: "resolver", classe: "tr_documento_erro_confirmar", mensagem: "apagaria" });
  assert.equal(s.confirmar.forte, true);
  assert.equal(passo(s, { tipo: "resolvido", regiao: "tab" }).confirmar, null);
});
