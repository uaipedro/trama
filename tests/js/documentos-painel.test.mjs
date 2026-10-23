import test from "node:test";
import assert from "node:assert/strict";
import { INICIAL, reduzir, resumo, pendencias, pedidoInserir, ehTabela, nomeValido, resolucoes, textoOcupado, pedidoPublicar,
         nomeSaida, filtrarCards, regioesDoCard, indiceSelos, tituloSelo, vaiSair }
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
  assert.equal(s.aviso, "Exportado: rel.pdf");
  // Slides e dashboard ficam em subpastas: o nome mostrado diz qual é qual.
  s = passo(s, { tipo: "exportado", documento: d, formato: "revealjs", arquivo: "documentos/rel/_saida/slides/rel.html" });
  assert.equal(s.aviso, "Exportado: slides/rel.html");
  assert.equal(nomeSaida("documentos/rel/_saida/painel/rel.html"), "painel/rel.html");
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

test("migração do bloco Publicar: o painel abre com o aviso e o documento novo escolhido", () => {
  const s = passo(INICIAL, lista, { tipo: "migrado", documentos: [{ fluxo: "main", no: "pub",
    documento: "documentos/consumo/consumo.qmd", regioes: 2 }] });
  assert.equal(s.aberto, true);
  assert.equal(s.selecionado, "documentos/consumo/consumo.qmd");
  assert.match(s.aviso, /bloco Publicar saiu do trama e virou o documento consumo\.qmd/);
});

test("publicar: conferir, bloqueio desliga, confirmar e enviar", () => {
  const doc = "documentos/rel/rel.qmd";
  const conf = { tipo: "publicacao", documento: doc, titulo: "T", autores: ["Ana"], licenca: "CC-BY-4.0",
                 tipo_versao: "menor", regioes: [{ chave: "tab" }], avisos: [], erros: [], repo: "t", registrado: false };
  let s = passo(INICIAL, lista, { tipo: "publicar_abrir", documento: doc });
  assert.equal(s.publicar.carregando, true);
  assert.equal(pedidoPublicar(s.publicar), null);
  s = passo(s, conf);
  assert.deepEqual(pedidoPublicar(s.publicar), { documento: doc, repositorio: "t", confirmado: false });
  s = passo(s, { tipo: "publicar_campo", campo: "repo", valor: "  " });
  assert.equal(pedidoPublicar(s.publicar).repositorio, null);
  // Conferência de outro documento não troca a caixa aberta.
  assert.equal(passo(s, { ...conf, documento: "documentos/x/x.qmd", repo: "x" }).publicar.repo, "  ");
  // Bloqueio: o botão desliga.
  const b = passo(s, { tipo: "publicar_enviando" }, { tipo: "publicar_bloqueado", documento: doc, erros: ["Falta o título."] });
  assert.equal(b.publicar.enviando, false);
  assert.equal(pedidoPublicar(b.publicar), null);
  // Confirmação: a pergunta aparece e o pedido segue confirmado.
  s = passo(s, { tipo: "publicar_enviando" }, { tipo: "ocupado", documento: doc, tarefa: "publicar" },
            { tipo: "publicar_confirmar", documento: doc, texto: "Criar privado?", repo: "p/t" });
  assert.equal(s.publicar.confirmar, "Criar privado?");
  assert.equal(s.ocupado[doc], undefined);
  assert.equal(pedidoPublicar(s.publicar, true).confirmado, true);
  // "Sim, publicar" e a resposta: a caixa fecha e o envio fica no documento.
  s = passo(s, { tipo: "publicar_enviando" },
            { tipo: "envio", documento: doc, estado: { repo: "p/t", tag: "v0.1.0", etapa: "enviado" } });
  assert.equal(s.publicar, null);
  assert.equal(s.envios[doc].tag, "v0.1.0");
  assert.match(s.aviso, /v0\.1\.0 enviada a p\/t/);
  s = passo(s, { tipo: "envio", documento: doc, estado: { repo: "p/t", tag: "v0.1.0", etapa: "actions" } });
  assert.equal(s.envios[doc].etapa, "actions");
  // Erro do R no envio destrava a caixa.
  s = passo(s, { tipo: "publicar_abrir", documento: doc }, conf, { tipo: "publicar_enviando" },
            { tipo: "erro", acao: "publicar", documento: doc, mensagem: "GitHub fora" });
  assert.equal(s.publicar.enviando, false);
  assert.equal(s.erro, "GitHub fora");
});

test("o menu GitHub abre o painel num documento", () => {
  const s = passo(INICIAL, lista, { tipo: "abrir_documento", documento: "documentos/rel/rel.qmd" });
  assert.equal(s.aberto, true);
  assert.equal(s.aba, "documentos");
  assert.equal(s.selecionado, "documentos/rel/rel.qmd");
});

test("acompanhamento de um envio anterior não fecha a caixa de publicar", () => {
  const doc = "documentos/rel/rel.qmd";
  let s = passo(INICIAL, lista, { tipo: "publicar_abrir", documento: doc },
                { tipo: "publicacao", documento: doc, erros: [], avisos: [], repo: "t" },
                { tipo: "envio", documento: doc, estado: { repo: "p/t", tag: "v0.1.0", etapa: "actions" } });
  assert.notEqual(s.publicar, null);
  assert.equal(s.envios[doc].etapa, "actions");
});

// #37: a janela grande, "+ Adicionar bloco" e o selo dos cards.

const dois = { ...lista, fluxo: "main", documentos: [
  { nome: "rel", caminho: "documentos/rel/rel.qmd", fluxo: "main", resumo: { atualizada: 2 },
    regioes: [{ regiao: "tab", no: "tab", estado: "atualizada" }, { regiao: "fig", no: "fig", estado: "desatualizada" }] },
  { nome: "art", caminho: "documentos/art/art.qmd", fluxo: "main", resumo: { atualizada: 1, duplicada: 1 },
    regioes: [{ regiao: "tab", no: "tab", estado: "atualizada" }, { regiao: "tab", no: "tab", estado: "duplicada" }] },
  { nome: "outro", caminho: "documentos/outro/outro.qmd", fluxo: "simulacao", resumo: { atualizada: 1 },
    regioes: [{ regiao: "fig", no: "fig", estado: "atualizada" }] }] };

test("janela: sem escolha vale o primeiro documento; escolher não desmarca, e o inserir segue a escolha", () => {
  let s = passo(INICIAL, dois);
  assert.equal(s.selecionado, "documentos/rel/rel.qmd");
  assert.equal(s.fluxo, "main");
  s = passo(s, { tipo: "selecionar", documento: "documentos/art/art.qmd" }, { tipo: "selecionar", documento: "documentos/art/art.qmd" });
  assert.equal(s.selecionado, "documentos/art/art.qmd");
  s = passo(s, { tipo: "inserir_card", card }, { tipo: "selecionar", documento: "documentos/rel/rel.qmd" });
  assert.equal(s.inserir.documento, "documentos/rel/rel.qmd");
  assert.equal(passo(INICIAL, { ...dois, fluxo: {} }).fluxo, "main");
});

test("adicionar bloco: abre a lista, busca, escolhe (inserir no documento da janela) e continua aberta", () => {
  const cards = [card, { id: "fig", rotulo: "Dispersão", tipo: "view/scatter", saidas: [{ porta: "out", tipo: "view/plot" }] }];
  let s = passo(INICIAL, dois, { tipo: "abrir" }, { tipo: "adicionar_abrir", cards });
  assert.equal(s.adicionar.cards.length, 2);
  s = passo(s, { tipo: "adicionar_busca", busca: "disp" }, { tipo: "inserir_card", card: cards[1], documento: "documentos/art/art.qmd" });
  assert.equal(s.inserir.documento, "documentos/art/art.qmd");
  assert.equal(s.selecionado, "documentos/art/art.qmd");
  assert.equal(s.adicionar.busca, "disp");
  s = passo(s, { tipo: "inserido", documento: "documentos/art/art.qmd", regiao: "fig", avisos: [] });
  assert.equal(s.inserir, null);
  assert.equal(s.foco, "fig");
  assert.notEqual(s.adicionar, null);
  s = passo(s, { tipo: "adicionar_fechar" });
  assert.equal(s.adicionar, null);
  assert.equal(passo(s, { tipo: "adicionar_abrir", cards }, { tipo: "novo_abrir" }).adicionar, null);
});

test("filtrarCards: só blocos com saída; busca sem acento no rótulo, tipo e id", () => {
  const cards = [{ id: "lm1", rotulo: "Regressão", tipo: "models/lm", saidas: [{ porta: "out" }] },
                 { id: "csv", rotulo: "Ler CSV", tipo: "data/csv", saidas: [{ porta: "out" }] },
                 { id: "nota", rotulo: "Nota", tipo: "ui/nota", saidas: [] }];
  assert.deepEqual(filtrarCards(cards, "").map((c) => c.id), ["lm1", "csv"]);
  assert.deepEqual(filtrarCards(cards, "regressao").map((c) => c.id), ["lm1"]);
  assert.deepEqual(filtrarCards(cards, "DATA csv").map((c) => c.id), ["csv"]);
  assert.deepEqual(filtrarCards(cards, "lm1").map((c) => c.id), ["lm1"]);
  assert.deepEqual(filtrarCards(null, "x"), []);
});

test("selos: por nó, só documentos do fluxo na tela, sem cópias; tooltip e região de destino", () => {
  const idx = indiceSelos(dois.documentos, "main");
  assert.deepEqual(idx.get("tab"), [{ documento: "documentos/rel/rel.qmd", nome: "rel", regiao: "tab" },
                                    { documento: "documentos/art/art.qmd", nome: "art", regiao: "tab" }]);
  assert.deepEqual(idx.get("fig"), [{ documento: "documentos/rel/rel.qmd", nome: "rel", regiao: "fig" }]);
  assert.deepEqual(indiceSelos(dois.documentos, "simulacao").get("fig")[0].nome, "outro");
  assert.equal(indiceSelos(dois.documentos, "simulacao").has("tab"), false);
  assert.equal(indiceSelos([{ caminho: "d", nome: "d" }]).size, 0);
  assert.match(tituloSelo(idx.get("fig")), /no documento rel/);
  assert.match(tituloSelo(idx.get("tab")), /nos documentos rel, art/);
  assert.equal(tituloSelo([]), "");
  assert.equal(regioesDoCard(dois.documentos[1].regioes, "tab").length, 1);
});

test("o selo abre a janela no documento, com a região em foco", () => {
  let s = passo(INICIAL, dois, { tipo: "adicionar_abrir", cards: [] },
                { tipo: "abrir_regiao", documento: "documentos/art/art.qmd", regiao: "tab" });
  assert.equal(s.aberto, true);
  assert.equal(s.aba, "documentos");
  assert.equal(s.selecionado, "documentos/art/art.qmd");
  assert.equal(s.foco, "tab");
  assert.equal(s.adicionar, null);
  assert.equal(passo(s, { tipo: "fechar" }).foco, null);
  assert.equal(passo(s, { tipo: "selecionar", documento: "documentos/rel/rel.qmd" }).foco, null);
});

test("vai sair", () => {
  assert.equal(vaiSair(3), "Vai sair: 3 regiões + o fluxo, os dados ligados e o ambiente");
  assert.equal(vaiSair(1), "Vai sair: 1 região + o fluxo, os dados ligados e o ambiente");
});
