// Gera src/styles/editor-card.css a partir das folhas do editor
// (inst/www/trama.css e o data.css da coleção data), com toda regra
// escopada em `.tr-estatico`: o card estático do site usa o MESMO CSS do
// editor, sem copiar estilo à mão e sem vazar para o resto do site.
//
// `:root` vira `.tr-estatico` (tokens do tema escuro, o padrão do editor);
// `:root[data-tema="claro"]` vale quando o sistema não pede tema escuro.
// Roda antes de dev, build e test; o arquivo gerado fica fora do git.

import { readFileSync, writeFileSync } from "node:fs";
import { join, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const site = join(dirname(fileURLToPath(import.meta.url)), "..");
const raiz = join(site, "..");
const fontes = ["inst/www/trama.css", "collections/trama.data/inst/trama/data.css"];
const ESCOPO = ".tr-estatico";

function semComentarios(css) {
  return css.replace(/\/\*[\s\S]*?\*\//g, "");
}

// Divide no nível 0: devolve [{prelude, corpo}] (corpo sem as chaves).
function blocos(css) {
  const out = [];
  let i = 0;
  while (i < css.length) {
    const abre = css.indexOf("{", i);
    if (abre < 0) break;
    const prelude = css.slice(i, abre).trim();
    let prof = 1, j = abre + 1;
    while (j < css.length && prof > 0) {
      if (css[j] === "{") prof++;
      else if (css[j] === "}") prof--;
      j++;
    }
    out.push({ prelude, corpo: css.slice(abre + 1, j - 1) });
    i = j;
  }
  return out;
}

// Vírgulas fora de parênteses (`:is(a,b)`, `:not(...)`).
function dividirSeletores(s) {
  const partes = [];
  let prof = 0, atual = "";
  for (const c of s) {
    if (c === "(") prof++;
    if (c === ")") prof--;
    if (c === "," && prof === 0) { partes.push(atual.trim()); atual = ""; }
    else atual += c;
  }
  if (atual.trim()) partes.push(atual.trim());
  return partes;
}

function escopar(sel) {
  if (sel === ":root" || sel === "body" || sel === "html") return ESCOPO;
  if (sel.startsWith(":root")) return ESCOPO + sel.slice(5);
  return `${ESCOPO} ${sel}`;
}

function transformar(css) {
  let saida = "";
  for (const { prelude, corpo } of blocos(css)) {
    if (/^@(keyframes|font-face)/.test(prelude)) {
      saida += `${prelude}{${corpo}}\n`;
    } else if (prelude.startsWith("@")) {
      saida += `${prelude}{\n${transformar(corpo)}}\n`;
    } else if (prelude === ':root[data-tema="claro"]') {
      saida += `@media not (prefers-color-scheme: dark){${ESCOPO}{${corpo}}}\n`;
    } else {
      saida += `${dividirSeletores(prelude).map(escopar).join(",")}{${corpo}}\n`;
    }
  }
  return saida;
}

let css = "/* Gerado por scripts/css-editor.mjs a partir do CSS do editor. Não editar. */\n";
for (const f of fontes) {
  css += `/* ${f} */\n` + transformar(semComentarios(readFileSync(join(raiz, f), "utf8")));
}
writeFileSync(join(site, "src", "styles", "editor-card.css"), css);
console.log(`css do editor: ${fontes.length} folhas escopadas em ${ESCOPO}`);
