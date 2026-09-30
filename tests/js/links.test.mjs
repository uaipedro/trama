// tests/js/links.test.mjs
import { test } from "node:test";
import assert from "node:assert/strict";
import { linkDeDados } from "../../inst/www/links.js";

test("aceita um link sozinho, aparado", () => {
  assert.equal(linkDeDados("  https://exemplo.org/a.csv\n"), "https://exemplo.org/a.csv");
  assert.equal(linkDeDados("http://127.0.0.1:8000/x"), "http://127.0.0.1:8000/x");
});
test("recusa frase com link, texto, vazio e outros esquemas", () => {
  assert.equal(linkDeDados("veja https://exemplo.org/a.csv"), null);
  assert.equal(linkDeDados("https://a.org/x https://b.org/y"), null);
  assert.equal(linkDeDados("dados.csv"), null);
  assert.equal(linkDeDados(""), null);
  assert.equal(linkDeDados(null), null);
  assert.equal(linkDeDados("ftp://exemplo.org/a.csv"), null);
});
