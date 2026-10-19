import test from "node:test";
import assert from "node:assert/strict";
import { linhas, links, terminou } from "../../collections/trama.publicar/inst/trama/envio.js";

const base = { repo: "p/pub", tag: "v0.1.0", url: "https://github.com/p/pub", pagina: "https://p.github.io/pub/v0.1.0/",
               ultimo: "https://p.github.io/pub/ultimo/", release: "https://github.com/p/pub/releases/tag/v0.1.0" };

test("etapas: feitas até a atual, pendentes depois", () => {
  assert.deepEqual(linhas({ ...base, etapa: "actions" }).map((l) => l.situacao), ["feita", "atual", "pendente", "pendente"]);
  assert.deepEqual(linhas({ ...base, etapa: "no_ar" }).map((l) => l.situacao), ["feita", "feita", "feita", "feita"]);
  assert.deepEqual(linhas(null).map((l) => l.situacao), ["pendente", "pendente", "pendente", "pendente"]);
});

test("falha marca o Actions e oferece o log", () => {
  const e = { ...base, etapa: "falhou", log: "https://log/7" };
  assert.deepEqual(linhas(e).map((l) => l.situacao), ["feita", "falhou", "pendente", "pendente"]);
  assert.deepEqual(links(e).map((l) => l.rotulo), ["log da falha", "repositório"]);
  assert.equal(terminou(e), true);
});

test("no ar: página, última versão, release e repositório", () => {
  assert.deepEqual(links({ ...base, etapa: "no_ar" }).map((l) => l.rotulo), ["página", "última versão", "release", "repositório"]);
  assert.equal(terminou({ etapa: "release" }), false);
});
