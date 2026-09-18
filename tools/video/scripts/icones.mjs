// Extrai só os símbolos que o vídeo usa do sprite lucide, pra virar markup
// inline: `<use href="arquivo.svg#id">` entre documentos é suporte irregular
// num renderizador headless, e 510KB de sprite não precisam viajar por 9 ícones.
import { readFileSync, writeFileSync } from "node:fs";

const QUER = [
  // vídeo da coleção `data`
  "file-spreadsheet", "list-filter", "sigma", "combine", "clipboard-list",
  // vídeo da coleção `models`
  "database", "grid-3x3", "sheet", "chart-column", "git-compare",
  // cromo do card e das cenas
  "chevron-down", "table", "package", "git-branch",
];
const sprite = readFileSync("public/lucide.svg", "utf8");
const saida = {};
for (const id of QUER) {
  const re = new RegExp(`<symbol id="${id}"[^>]*>([\\s\\S]*?)</symbol>`);
  const m = sprite.match(re);
  if (!m) { console.error("não achei:", id); process.exit(1); }
  saida[id] = m[1].trim();
}
writeFileSync("src/trama/icones.ts",
  "// GERADO por tools/video/scripts/icones.mjs — extrai de inst/www/vendor/lucide.svg.\n" +
  "// Não editar à mão; rode `node scripts/icones.mjs` pra regerar.\n\n" +
  "export const ICONES: Record<string, string> = " +
  JSON.stringify(saida, null, 2) + ";\n");
console.log("ok:", Object.keys(saida).join(", "));
