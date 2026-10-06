#!/usr/bin/env node
// Renderiza toda ajuda publicada no catálogo com o renderizador usado pelo editor
// e falha se marcadores comuns de Markdown continuarem visíveis no resultado.
import { readFileSync } from "node:fs";
import { Markdown } from "../inst/www/markdown.js";
const catalogPath = process.argv[2];
if (!catalogPath) throw new Error("Uso: node tools/verificar-helps.mjs <catalogo.json>");
const catalog = JSON.parse(readFileSync(catalogPath, "utf8"));
const h = (type, props, ...children) => ({ type, props: props || {}, children: children.flat(Infinity) });
function sobrasMarkdown(node, dentroCodigo = false, achados = []) {
  if (node == null || typeof node === "boolean") return achados;
  if (typeof node === "string" || typeof node === "number") {
    if (dentroCodigo) return achados;
    const texto = String(node);
    if (/^\s*\|(?:\s*:?-+:?\s*\|)+\s*$/.test(texto)) achados.push("tabela");
    if (/###/.test(texto)) achados.push("###");
    if (/\]\(/.test(texto)) achados.push("](");
    if (/(^|[\s(])\*[^\s*][^*]*\*(?=[\s.,;:)]|$)/.test(texto)) achados.push("*itálico*");
    if (/\*\*/.test(texto)) achados.push("**");
    if (/&(quot|amp|lt|gt|#39);/.test(texto)) achados.push("entidade");
    if (/^#{1,6}\s/.test(texto)) achados.push("título");
    return achados;
  }
  if (Array.isArray(node)) { for (const filho of node) sobrasMarkdown(filho, dentroCodigo, achados); return achados; }
  if (typeof node !== "object" || !node.type) return achados;
  const codigo = dentroCodigo || node.type === "code";
  for (const filho of node.children) sobrasMarkdown(filho, codigo, achados);
  return achados;
}

const sobras = [];
for (const no of catalog.nodes || []) {
  if (!no.help) continue;
  for (const label of sobrasMarkdown(Markdown({ texto: no.help, h }))) sobras.push(`${no.id}: ${label}`);
}
if (sobras.length) {
  console.error(`Marcadores Markdown crus em ${sobras.length} ajuda(s):\n${sobras.join("\n")}`);
  process.exitCode = 1;
} else {
  console.log(`OK: ${catalog.nodes.filter((n) => n.help).length} ajudas renderizadas sem marcadores crus.`);
}
