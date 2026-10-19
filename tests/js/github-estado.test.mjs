// Estado do menu GitHub do plugin `trama.github` (estado.js).
import { test } from "node:test";
import assert from "node:assert/strict";
import { INICIAL, reduzir, rotuloFonte, podeSair, indicador, dica } from "../../plugins/trama.github/inst/trama/estado.js";

const passos = (s, ...as) => as.reduce(reduzir, s);
const pesquisadora = { login: "pesquisadora", nome: "Pesquisadora" };

test("começa carregando, fechado, sem indicador", () => {
  assert.equal(INICIAL.fase, "carregando");
  assert.equal(INICIAL.aberto, false);
  assert.equal(indicador(INICIAL), null);
});

test("estado do R define conectado ou desconectado", () => {
  const off = reduzir(INICIAL, { tipo: "estado", conectado: false, login_disponivel: true, persistente: true });
  assert.equal(off.fase, "desconectado");
  assert.equal(dica(off), "GitHub: entrar");
  const on = reduzir(off, { tipo: "estado", conectado: true, fonte: "chaveiro", usuario: pesquisadora });
  assert.equal(on.fase, "conectado");
  assert.equal(indicador(on), "ok");
  assert.equal(dica(on), "GitHub: conectado como pesquisadora");
});

test("login: pedindo, código, copiado, conectado", () => {
  const s = passos(INICIAL,
    { tipo: "estado", conectado: false },
    { tipo: "entrando" },
    { tipo: "codigo", codigo: "ABCD-1234", link: "https://github.com/login/device" },
    { tipo: "copiado" });
  assert.equal(s.fase, "aguardando");
  assert.equal(s.codigo, "ABCD-1234");
  assert.equal(s.copiado, true);
  const fim = reduzir(s, { tipo: "estado", conectado: true, fonte: "chaveiro", usuario: pesquisadora });
  assert.equal(fim.codigo, null);
  assert.equal(fim.copiado, false);
});

test("erro no login volta a desconectado e mostra a mensagem", () => {
  const s = passos(INICIAL, { tipo: "estado", conectado: false }, { tipo: "entrando" },
    { tipo: "codigo", codigo: "X", link: "l" }, { tipo: "erro", mensagem: "O código expirou" });
  assert.equal(s.fase, "desconectado");
  assert.equal(s.erro, "O código expirou");
  assert.equal(s.codigo, null);
  assert.equal(indicador(s), "erro");
  assert.equal(reduzir(s, { tipo: "entrando" }).erro, null);
});

test("credencial vencida: desconectado com motivo e fonte", () => {
  const s = reduzir(INICIAL, { tipo: "estado", conectado: false, fonte: "GITHUB_PAT", erro: "expirou" });
  assert.equal(s.fase, "desconectado");
  assert.equal(s.fonte, "GITHUB_PAT");
  assert.equal(indicador(s), "erro");
});

test("copiado fora da espera não muda nada", () => {
  const s = reduzir(INICIAL, { tipo: "estado", conectado: false });
  assert.equal(reduzir(s, { tipo: "copiado" }), s);
});

test("sair só quando a credencial é do trama", () => {
  const com = (fonte) => reduzir(INICIAL, { tipo: "estado", conectado: true, fonte, usuario: pesquisadora });
  assert.equal(podeSair(com("chaveiro")), true);
  assert.equal(podeSair(com("GITHUB_PAT")), false);
  assert.equal(podeSair(com("gh")), false);
  assert.equal(rotuloFonte("gh"), "pelo GitHub CLI (gh)");
  assert.equal(rotuloFonte("desconhecida"), "");
});

test("sem Client ID e sem chaveiro persistente vêm do R", () => {
  const s = reduzir(INICIAL, { tipo: "estado", conectado: false, login_disponivel: false, persistente: false });
  assert.equal(s.loginDisponivel, false);
  assert.equal(s.persistente, false);
});

test("guia abre e fecha; entrar fecha o guia", () => {
  const s = reduzir(INICIAL, { tipo: "guia", passos: [{ titulo: "a", texto: "b" }] });
  assert.equal(s.guia.length, 1);
  assert.equal(reduzir(s, { tipo: "fechar_guia" }).guia, null);
  assert.equal(reduzir(s, { tipo: "entrando" }).guia, null);
  assert.deepEqual(reduzir(INICIAL, { tipo: "guia", passos: null }).guia, []);
});

test("abrir e fechar o menu; ação desconhecida não muda nada", () => {
  const a = reduzir(INICIAL, { tipo: "abrir" });
  assert.equal(a.aberto, true);
  assert.equal(reduzir(a, { tipo: "fechar" }).aberto, false);
  assert.equal(reduzir(a, { tipo: "???" }), a);
});
