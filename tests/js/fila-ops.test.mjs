import { test } from "node:test";
import assert from "node:assert/strict";
import { criarFilaOps } from "../../inst/www/fila-ops.js";

// Servidor de mentira com a mesma regra de `tr_submit`: aplica se `base_rev`
// é igual à revisão atual (sobe 1), senão recusa e informa a atual.
function servidor(rev = 0) {
  return {
    rev,
    receber(env) {
      if (env.base_rev !== this.rev) return { tipo: "rejected", seq: env.seq, rev: this.rev };
      this.rev += 1;
      return { tipo: "applied", seq: env.seq, rev: this.rev };
    },
  };
}

test("duas ops antes do eco saem com revisões consecutivas e as duas entram", () => {
  const f = criarFilaOps(); f.documento(5);
  const srv = servidor(5);
  const e1 = { seq: 1, base_rev: f.base() }; f.enviada(1);
  const e2 = { seq: 2, base_rev: f.base() }; f.enviada(2);
  assert.deepEqual([e1.base_rev, e2.base_rev], [5, 6]);
  const r1 = srv.receber(e1), r2 = srv.receber(e2);
  assert.equal(r1.tipo, "applied"); assert.equal(r2.tipo, "applied");
  f.aplicada(r1.seq, r1.rev);
  assert.equal(f.base(), 7);
  f.aplicada(r2.seq, r2.rev);
  assert.equal(f.base(), 7); assert.equal(f.emVoo(), 0);
});

test("documento estrutural entre ecos não esquece quem está em voo", () => {
  const f = criarFilaOps(); f.documento(1);
  f.enviada(1); f.enviada(2);
  f.aplicada(1, 2); f.documento(2);
  assert.equal(f.base(), 3);
});

test("recusa zera o voo e recomeça da revisão informada", () => {
  const f = criarFilaOps(); f.documento(3);
  f.enviada(1); f.enviada(2); f.enviada(3);
  f.recusada(1, 4);
  assert.equal(f.base(), 4); assert.equal(f.emVoo(), 0);
  // a recusa atrasada das de trás é inócua
  f.enviada(4);
  f.recusada(2, 4);
  assert.equal(f.base(), 4);
});

test("eco de op alheia (agente) só anda a revisão", () => {
  const f = criarFilaOps(); f.documento(1);
  f.aplicada("agente-x", 2);
  assert.equal(f.base(), 2);
});

test("seq nulo (Shiny fora do ar) não entra no voo", () => {
  const f = criarFilaOps(); f.enviada(null);
  assert.equal(f.emVoo(), 0);
});
