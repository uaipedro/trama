// Seções Projeto e Publicações do painel GitHub do plugin `trama.github` (painel.js).
import { test } from "node:test";
import assert from "node:assert/strict";
import { PAINEL_INICIAL, reduzirPainel, podeSalvar, nomeRepo, tamanho, resumoArquivos,
         dataCurta, situacaoExecucao } from "../../plugins/trama.github/inst/trama/painel.js";

const passos = (s, ...as) => as.reduce(reduzirPainel, s);
const arquivos = { projeto_n: 3, projeto_bytes: 2048,
                   dados: [{ caminho: "vendas.csv", tamanho: 10, incluido: false }] };

test("carregar e receber um projeto sem repositório", () => {
  const p = passos(PAINEL_INICIAL, { tipo: "carregar" },
    { tipo: "projeto", ligado: false, nome_sugerido: "minha-tese", arquivos });
  assert.equal(p.projeto.fase, "pronto");
  assert.equal(p.projeto.ligado, false);
  assert.equal(nomeRepo(p.projeto), "minha-tese");
  assert.equal(nomeRepo(reduzirPainel(p, { tipo: "nome", texto: " outro " }).projeto), "outro");
  assert.equal(p.publicacoes.fase, "carregando");
});

test("recarregar não apaga o projeto já na tela", () => {
  const p = passos(PAINEL_INICIAL, { tipo: "projeto", ligado: true, repo: "a/b", versoes: [{ sha: "1" }] },
    { tipo: "carregar" });
  assert.equal(p.projeto.fase, "pronto");
  assert.equal(p.projeto.versoes.length, 1);
});

test("salvar: rascunho, ocupado, salvo limpa o rascunho e avisa", () => {
  let p = passos(PAINEL_INICIAL, { tipo: "projeto", ligado: true, repo: "a/b" });
  assert.equal(podeSalvar(p.projeto), false);
  p = reduzirPainel(p, { tipo: "rascunho", texto: "Novo modelo" });
  assert.equal(podeSalvar(p.projeto), true);
  p = reduzirPainel(p, { tipo: "ocupar", o: "salvando" });
  assert.equal(podeSalvar(p.projeto), false);
  p = reduzirPainel(p, { tipo: "salvo", mudou: true });
  assert.equal(p.projeto.rascunho, "");
  assert.equal(p.projeto.ocupado, null);
  assert.equal(p.projeto.aviso, "Versão salva.");
  assert.equal(reduzirPainel(p, { tipo: "salvo", mudou: false }).projeto.aviso, "Nada mudou desde a última versão.");
  assert.match(reduzirPainel(p, { tipo: "salvo", primeira: true }).projeto.aviso, /primeira versão/);
});

test("restaurar pede confirmação; restaurado avisa onde ficou a cópia", () => {
  let p = passos(PAINEL_INICIAL, { tipo: "projeto", ligado: true, repo: "a/b" },
    { tipo: "confirmar", c: { tipo: "restaurar", sha: "abc", mensagem: "v1" } });
  assert.equal(p.projeto.confirmar.sha, "abc");
  assert.equal(reduzirPainel(p, { tipo: "cancelar_confirmacao" }).projeto.confirmar, null);
  p = passos(p, { tipo: "ocupar", o: "restaurando" },
    { tipo: "restaurado", escritos: 2, removidos: 1, copia: ".trama/restauracao/20261019-120000" });
  assert.equal(p.projeto.confirmar, null);
  assert.match(p.projeto.aviso, /\.trama\/restauracao/);
  assert.match(reduzirPainel(p, { tipo: "restaurado", copia: null }).projeto.aviso, /iguais/);
});

test("erro com contexto fica na seção certa; sem contexto é da conta", () => {
  const p = passos(PAINEL_INICIAL, { tipo: "carregar" }, { tipo: "ocupar", o: "salvando" },
    { tipo: "erro", contexto: "projeto", mensagem: "falhou" });
  assert.equal(p.projeto.erro, "falhou");
  assert.equal(p.projeto.ocupado, null);
  assert.equal(p.projeto.fase, "erro");
  const q = reduzirPainel(p, { tipo: "erro", contexto: "publicacoes", mensagem: "x" });
  assert.equal(q.publicacoes.erro, "x");
  assert.equal(reduzirPainel(p, { tipo: "erro", mensagem: "da conta" }), p);
});

test("sair da conta zera o painel", () => {
  const p = passos(PAINEL_INICIAL, { tipo: "projeto", ligado: true, repo: "a/b" });
  assert.equal(reduzirPainel(p, { tipo: "estado", conectado: false }), PAINEL_INICIAL);
  assert.equal(reduzirPainel(p, { tipo: "estado", conectado: true }), p);
});

test("publicações e dados abertos", () => {
  const p = passos(PAINEL_INICIAL, { tipo: "publicacoes", disponivel: true, itens: [{ no: "n1" }] },
    { tipo: "alternar_dados" });
  assert.equal(p.publicacoes.itens.length, 1);
  assert.equal(p.projeto.dadosAbertos, true);
  assert.equal(reduzirPainel(p, { tipo: "publicacoes", disponivel: false }).publicacoes.disponivel, false);
  assert.equal(reduzirPainel(p, { tipo: "arquivos", arquivos }).projeto.arquivos, arquivos);
});

test("tamanho, resumo e data", () => {
  assert.equal(tamanho(500), "500 B");
  assert.equal(tamanho(2048), "2,0 KB");
  assert.equal(tamanho(35 * 1024 ** 2), "35 MB");
  assert.equal(resumoArquivos(arquivos), "3 arquivos do projeto (2,0 KB) · 0 de 1 arquivo de dados");
  assert.equal(resumoArquivos({ projeto_n: 1, projeto_bytes: 10, dados: [] }),
               "1 arquivo do projeto (10 B) · nenhum arquivo de dados");
  const d = new Date(2026, 9, 19, 14, 5);
  assert.equal(dataCurta(d.toISOString()), "19/10/2026 14:05");
  assert.equal(dataCurta(null), "");
});

test("situação do Actions", () => {
  assert.equal(situacaoExecucao(null), null);
  assert.deepEqual(situacaoExecucao({ estado: "completed", conclusao: "success" }), { rotulo: "publicada", tom: "ok" });
  assert.equal(situacaoExecucao({ estado: "completed", conclusao: "failure" }).tom, "erro");
  assert.equal(situacaoExecucao({ estado: "in_progress" }).rotulo, "publicando…");
  assert.equal(situacaoExecucao({ estado: "queued" }).rotulo, "na fila");
});
