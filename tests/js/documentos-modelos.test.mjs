import test from "node:test";
import assert from "node:assert/strict";
import { reduzirModelos, formularioDe, pedidoEditar, pedidoDuplicar, pedidoImportar, formatosDoFormulario,
         nomeModeloValido, nomeDe, origemDe, formatosLegiveis } from "../../plugins/trama.documentos/inst/trama/modelos.js";
import { INICIAL, reduzir, formatosExportar } from "../../plugins/trama.documentos/inst/trama/painel.js";

const ufla = { nome: "ufla", titulo: "UFLA (ABNT)", origem: "embutido", sobre_embutido: false,
  descricao: "ABNT", formatos: ["typst", "docx", "html"], cores: { primaria: "#1f4e79", fundo: "#ffffff" },
  fontes: {}, capa: true, perfil: "aluno-ufla", csl: "abnt.csl", valido: true };
const lista = [{ nome: "basico", titulo: "Básico", formatos: "html, typst, docx", origem: "embutido", valido: true },
               { nome: "ufla", titulo: "UFLA (ABNT)", formatos: "typst, docx, html", origem: "embutido", valido: true }];
const passo = (s, ...as) => as.reduce(reduzir, s);

test("nome de modelo: o que o R aceita", () => {
  for (const ok of ["meu", "meu-tcc", "abnt2"]) assert.ok(nomeModeloValido(ok), ok);
  for (const ruim of ["", "Meu", "a b", "-x", "dashboard", "html", "meu_tcc"]) assert.ok(!nomeModeloValido(ruim), ruim);
  assert.equal(nomeDe("Relatório do Laboratório"), "relatorio-do-laboratorio");
  assert.equal(nomeDe("Dashboard"), "dashboard-modelo");
  assert.equal(nomeDe(""), "");
});

test("editar: o formulário vem do detalhe, e só o que mudou vai ao R", () => {
  const f = formularioDe(ufla);
  assert.equal(pedidoEditar(f), null);  // nada mudou
  let s = passo({ ...INICIAL, modelos: lista }, { tipo: "modelo", modelo: ufla });
  assert.equal(s.modeloForm.nome, "ufla");
  s = passo(s, { tipo: "modelo_editar_campo", campo: "titulo", valor: "Meu TCC" },
               { tipo: "modelo_editar_campo", grupo: "cores", campo: "primaria", valor: "#7B2D26" },
               { tipo: "modelo_editar_campo", grupo: "fontes", campo: "texto", valor: " Lora " },
               { tipo: "modelo_formato", formato: "docx" },
               { tipo: "modelo_editar_campo", campo: "capa", valor: false },
               { tipo: "modelo_editar_campo", campo: "perfil", valor: "" },
               { tipo: "modelo_editar_campo", campo: "semCsl", valor: true });
  assert.deepEqual(pedidoEditar(s.modeloForm), {
    nome: "ufla", titulo: "Meu TCC", formatos: ["typst", "html"], cores: { primaria: "#7b2d26" },
    fontes: { texto: "Lora" }, capa: false, perfil: false, csl: false });
  // Sem formato, não salva; religar põe no fim.
  const sem = passo(s, { tipo: "modelo_formato", formato: "typst" }, { tipo: "modelo_formato", formato: "html" });
  assert.equal(pedidoEditar(sem.modeloForm), null);
  assert.deepEqual(passo(sem, { tipo: "modelo_formato", formato: "revealjs" }).modeloForm.formatos, ["revealjs"]);
  // Logo novo vai em base64 como veio do navegador.
  const logo = { nome: "marca.svg", conteudo: "data:image/svg+xml;base64,PHN2Zy8+" };
  const comLogo = passo(s, { tipo: "modelo_editar_campo", campo: "novoLogo", valor: logo });
  assert.deepEqual(pedidoEditar(comLogo.modeloForm).logo, logo);
  // Enviando: não manda de novo; erro destrava.
  const env = passo(s, { tipo: "modelo_enviando" });
  assert.equal(pedidoEditar(env.modeloForm), null);
  const err = passo(env, { tipo: "erro", acao: "modelos_editar", mensagem: "Cor inválida" });
  assert.equal(err.erro, "Cor inválida");
  assert.ok(pedidoEditar(err.modeloForm));
});

test("salvo: fecha o formulário, mostra o aviso e mantém o modelo aberto na lista", () => {
  const s = passo({ ...INICIAL, modelos: lista }, { tipo: "modelo", modelo: ufla },
                  { tipo: "modelo_salvo", nome: "ufla", mensagem: "Salvo como cópia sua." });
  assert.equal(s.modeloForm, null);
  assert.equal(s.modeloAberto, "ufla");
  assert.equal(s.aviso, "Salvo como cópia sua.");
  // Duplicar: o R manda modelo_salvo e logo o detalhe; o aviso fica.
  const d = passo(s, { tipo: "modelo", modelo: { ...ufla, nome: "copia" } });
  assert.equal(d.aviso, "Salvo como cópia sua.");
  assert.equal(d.modeloForm.nome, "copia");
});

test("duplicar e novo modelo: nome curto acompanha o título até a pessoa mexer nele", () => {
  let s = passo({ ...INICIAL, modelos: lista }, { tipo: "modelo_duplicar_abrir", de: "ufla" });
  assert.equal(s.modeloDuplicar.titulo, "UFLA (ABNT) (cópia)");
  assert.deepEqual(pedidoDuplicar(s.modeloDuplicar), { de: "ufla", nome: "ufla-copia", titulo: "UFLA (ABNT) (cópia)" });
  s = passo(s, { tipo: "modelo_duplicar_campo", campo: "titulo", valor: "Meu TCC" });
  assert.equal(s.modeloDuplicar.nome, "meu-tcc");
  s = passo(s, { tipo: "modelo_duplicar_campo", campo: "nome", valor: "tcc" },
               { tipo: "modelo_duplicar_campo", campo: "titulo", valor: "Outro" });
  assert.equal(s.modeloDuplicar.nome, "tcc");
  assert.equal(pedidoDuplicar({ ...s.modeloDuplicar, nome: "Ruim" }), null);
  const n = passo({ ...INICIAL, modelos: lista }, { tipo: "modelo_duplicar_abrir", de: "basico", novo: true });
  assert.equal(n.modeloDuplicar.novo, true);
  assert.equal(pedidoDuplicar(n.modeloDuplicar), null);  // falta o nome
  assert.equal(passo(n, { tipo: "modelo_cancelar" }).modeloDuplicar, null);
});

test("remover pede confirmação; voltar ao embutido é o mesmo gesto", () => {
  let s = passo(INICIAL, { tipo: "modelo_remover_pedir", nome: "ufla", voltar: true });
  assert.deepEqual(s.modeloConfirmar, { nome: "ufla", voltar: true });
  s = passo(s, { tipo: "modelo_remover_cancelar" });
  assert.equal(s.modeloConfirmar, null);
});

test("galeria: a lista vem na mensagem 'modelos' e na 'lista'; rótulos", () => {
  assert.deepEqual(passo(INICIAL, { tipo: "modelos", modelos: lista }).modelos, lista);
  assert.equal(origemDe({ origem: "embutido" }), "embutido");
  assert.equal(origemDe({ origem: "usuario", sobre_embutido: true }), "editado");
  assert.equal(origemDe({ origem: "usuario" }), "seu");
  assert.equal(formatosLegiveis("typst, docx, html, revealjs"), "PDF · Word · HTML · Slides");
  assert.equal(reduzirModelos(INICIAL, { tipo: "modelo_desconhecido" }), INICIAL);
});

test("novo documento: Básico por padrão; o modelo escolhido traz o perfil que sugere", () => {
  const l = { tipo: "lista", documentos: [], perfis: [{ perfil: "aluno-ufla", nome: "Ana" }, { perfil: "yucca", nome: "Pedro" }],
              perfil_padrao: "yucca", modelos: [{ nome: "artigo", titulo: "Artigo" }, { nome: "basico", titulo: "Básico" },
                                               { nome: "ufla", titulo: "UFLA", perfil: "aluno-ufla" },
                                               { nome: "consultoria", titulo: "Consultoria", perfil: "nao-existe" }] };
  let s = passo(INICIAL, l, { tipo: "novo_abrir" });
  assert.equal(s.novo.modelo, "basico");
  assert.equal(s.novo.perfil, "yucca");
  s = passo(s, { tipo: "novo_campo", campo: "modelo", valor: "ufla" });
  assert.equal(s.novo.perfil, "aluno-ufla");
  s = passo(s, { tipo: "novo_campo", campo: "modelo", valor: "consultoria" });
  assert.equal(s.novo.perfil, "aluno-ufla");  // o sugerido não existe: fica o que estava
});

test("exportar: os formatos do documento, e os três de sempre sem a lista", () => {
  assert.deepEqual(formatosExportar({ formatos: ["html", "pdf", "docx"] }).map(([k]) => k), ["pdf", "docx", "html"]);
  assert.deepEqual(formatosExportar({ formatos: ["revealjs"] }), [["revealjs", "Slides"]]);
  assert.deepEqual(formatosExportar({}).map(([k]) => k), ["pdf", "docx", "html"]);
});

test("importar: nome que já existe vira escolha entre substituir e outro nome, sem escolher o arquivo de novo", () => {
  const dados = { nome: "meu.zip", conteudo: "data:application/zip;base64,UEsDBA==" };
  let s = passo(INICIAL, { tipo: "modelo_importar_abrir" });
  assert.deepEqual(pedidoImportar({ ...s.modeloImportar, dados }), { arquivo: dados });
  s = passo(s, { tipo: "modelo_importar_enviando", dados });
  assert.equal(pedidoImportar(s.modeloImportar), null);  // enviando
  s = passo(s, { tipo: "erro", acao: "modelos_importar", classe: "tr_documento_erro_modelo_existe", mensagem: "Já existe" });
  assert.equal(s.modeloImportar.conflito, true);
  assert.equal(s.modeloImportar.arquivo, "meu.zip");
  assert.deepEqual(pedidoImportar(s.modeloImportar, "substituir"), { arquivo: dados, substituir: true });
  assert.equal(pedidoImportar(s.modeloImportar, "nome"), null);
  s = passo(s, { tipo: "modelo_importar_nome", valor: "meu-2" });
  assert.deepEqual(pedidoImportar(s.modeloImportar, "nome"), { arquivo: dados, nome: "meu-2" });
  // Outro erro (zip inválido) não é conflito.
  const outro = passo(s, { tipo: "modelo_importar_enviando" },
                      { tipo: "erro", acao: "modelos_importar", classe: "tr_documento_erro_modelo", mensagem: "x" });
  assert.equal(outro.modeloImportar.conflito, false);
});

test("formatos que o modelo já tem e a galeria não configura aparecem no formulário", () => {
  const f = formularioDe({ ...ufla, formatos: ["pdf", "html"], formatos_desligados: ["beamer"] });
  assert.deepEqual(formatosDoFormulario(f).map(([k]) => k), ["html", "typst", "docx", "revealjs", "dashboard", "pdf", "beamer"]);
  assert.equal(formatosDoFormulario(formularioDe(ufla)).length, 5);
});

test("apagar: um envio só; depois de apagar, nada fica aberto", () => {
  let s = passo(INICIAL, { tipo: "modelo_abrir", nome: "meu" }, { tipo: "modelo_remover_pedir", nome: "meu" },
                { tipo: "modelo_remover_enviando" });
  assert.equal(s.modeloConfirmar.enviando, true);
  s = passo(s, { tipo: "modelo_salvo", nome: null, mensagem: "Modelo 'meu' removido." });
  assert.equal(s.modeloAberto, null);
  assert.equal(s.modeloConfirmar, null);
});
