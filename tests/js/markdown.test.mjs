// tests/js/markdown.test.mjs
import { test } from "node:test";
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { tokensDe, Markdown } from "../../inst/www/markdown.js";

// `tokensDe` é a árvore intermediária (não React, não HTML): o teste mora
// aqui porque é o ponto onde a decisão "isto está no subconjunto ou não" é
// tomada. O mapeamento pra elementos (`Markdown`) é a parte fina — testada
// à parte, com um `h` falso, só pra confirmar que o fallback literal chega
// inteiro até a árvore de elementos (nunca vira tag).

test("texto vazio ou só espaço não estoura, devolve nada", () => {
  assert.deepEqual(tokensDe(""), []);
  assert.deepEqual(tokensDe("   \n  "), []);
  assert.deepEqual(tokensDe(undefined), []);
});

test("heading nível 1 vira título; nível 6 (fora do subconjunto) sai literal e visível", () => {
  const [um] = tokensDe("# Um");
  assert.deepEqual(um, { tipo: "titulo", nivel: 1, filhos: [{ tipo: "texto", texto: "Um" }] });

  const [seis] = tokensDe("###### Seis");
  assert.equal(seis.tipo, "literal");
  assert.match(seis.raw, /Seis/);
  assert.match(seis.raw, /######/);
});

test("parágrafo com negrito, itálico e código inline", () => {
  const [p] = tokensDe("**a** *b* `c`");
  assert.equal(p.tipo, "paragrafo");
  assert.deepEqual(p.filhos[0], { tipo: "negrito", filhos: [{ tipo: "texto", texto: "a" }] });
  assert.deepEqual(p.filhos[2], { tipo: "italico", filhos: [{ tipo: "texto", texto: "b" }] });
  assert.deepEqual(p.filhos[4], { tipo: "codigo_inline", texto: "c" });
});

test("lista com e sem número, aninhada um nível", () => {
  const [semNumero] = tokensDe("- um\n  - dois\n- tres");
  assert.equal(semNumero.tipo, "lista");
  assert.equal(semNumero.ordenada, false);
  assert.equal(semNumero.itens.length, 2);
  const aninhada = semNumero.itens[0].filhos.find((f) => f.tipo === "lista");
  assert.ok(aninhada, "item aninhado deveria conter uma sub-lista");
  assert.equal(aninhada.itens[0].filhos[0].texto, "dois");
  assert.equal(semNumero.itens[1].filhos[0].texto, "tres");

  const [comNumero] = tokensDe("1. um\n2. dois");
  assert.equal(comNumero.tipo, "lista");
  assert.equal(comNumero.ordenada, true);
  assert.deepEqual(comNumero.itens.map((it) => it.filhos[0].texto), ["um", "dois"]);
});

test("bloco de código preserva linguagem e quebras de linha", () => {
  const [c] = tokensDe("```js\nlinha1\nlinha2\n```");
  assert.deepEqual(c, { tipo: "codigo", lingua: "js", texto: "linha1\nlinha2" });
});

test("citação, régua e tabela simples", () => {
  const [cita] = tokensDe("> uma citação");
  assert.equal(cita.tipo, "citacao");
  assert.equal(cita.filhos[0].tipo, "paragrafo");
  assert.equal(cita.filhos[0].filhos[0].texto, "uma citação");

  const [regua] = tokensDe("---");
  assert.deepEqual(regua, { tipo: "regua" });

  const [tabela] = tokensDe("| a | b |\n|---|---|\n| 1 | 2 |");
  assert.equal(tabela.tipo, "tabela");
  assert.deepEqual(tabela.cabecalho.map((c) => c[0].texto), ["a", "b"]);
  assert.deepEqual(tabela.linhas.map((r) => r.map((c) => c[0].texto)), [["1", "2"]]);
});

test("link http(s) vira link; javascript: sai literal, nunca clicável", () => {
  const [p] = tokensDe("[rótulo](https://x)");
  assert.deepEqual(p.filhos[0], { tipo: "link", href: "https://x", filhos: [{ tipo: "texto", texto: "rótulo" }] });

  const [pBad] = tokensDe("[rótulo](javascript:alert(1))");
  assert.equal(pBad.filhos[0].tipo, "literal");
  assert.match(pBad.filhos[0].raw, /javascript:alert/);
});

test("link relativo (sem esquema) é permitido", () => {
  const [p] = tokensDe("[home](./pagina)");
  assert.equal(p.filhos[0].tipo, "link");
  assert.equal(p.filhos[0].href, "./pagina");
});

test("imagem resolve src pela função injetada", () => {
  const resolverSrc = (rel) => `trama-imagens/${rel}`;
  const [p] = tokensDe("![alt](logo.png)", resolverSrc);
  assert.deepEqual(p.filhos[0], { tipo: "imagem", alt: "alt", src: "trama-imagens/logo.png" });
});

test("html cru — <script> em bloco e <b> inline — sai literal, nunca elemento", () => {
  const [scriptTok] = tokensDe("<script>alert(1)</script>");
  assert.equal(scriptTok.tipo, "literal");
  assert.match(scriptTok.raw, /<script>/);

  const [p] = tokensDe("<b>x</b>");
  assert.equal(p.tipo, "paragrafo");
  assert.ok(p.filhos.every((f) => f.tipo !== "html"));
  const literais = p.filhos.filter((f) => f.tipo === "literal");
  assert.ok(literais.some((f) => f.raw === "<b>"));
  assert.ok(literais.some((f) => f.raw === "</b>"));
});

// --- Markdown (mapeamento pra elementos) -------------------------------------
// `h` falso: só registra type/props/children, sem depender de React nem do
// especificador nu "trama" (que só resolve no bundle do navegador — ver
// tests/js/editor-streamcontrols.test.mjs). O que importa aqui é confirmar
// que o literal chega como TEXTO dentro de um elemento, nunca como tag.
const hFalso = (type, props, ...children) => ({ type, props, children: children.flat() });

test("Markdown mapeia heading suportado e devolve null pra texto vazio", () => {
  assert.equal(Markdown({ texto: "", h: hFalso }), null);
  const [el] = Markdown({ texto: "# Um", h: hFalso });
  assert.equal(el.type, "h1");
});

test("Markdown nunca produz uma tag <script> — o raw vira texto dentro de um span", () => {
  const [el] = Markdown({ texto: "<script>alert(1)</script>", h: hFalso });
  assert.equal(el.type, "span");
  assert.equal(el.children[0], "<script>alert(1)</script>");
});

test("Markdown resolve src da imagem com resolverSrc e nunca usa innerHTML/dangerouslySetInnerHTML", () => {
  const resolverSrc = (rel) => `trama-imagens/${rel}`;
  const [p] = Markdown({ texto: "![alt](logo.png)", resolverSrc, h: hFalso });
  const img = p.children[0];
  assert.equal(img.type, "img");
  assert.equal(img.props.src, "trama-imagens/logo.png");

  // checa USO, não a mera menção: o cabeçalho do arquivo cita
  // "dangerouslySetInnerHTML"/"innerHTML" em prosa, ao explicar por que eles
  // não aparecem em código — daí o regex exigir `=`/`:` depois do nome.
  const fonte = readFileSync(new URL("../../inst/www/markdown.js", import.meta.url), "utf8");
  assert.ok(!/dangerouslySetInnerHTML\s*[:=]/.test(fonte));
  assert.ok(!/\.innerHTML\s*=/.test(fonte));
});
