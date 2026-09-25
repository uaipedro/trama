// Copia os templates das coleções (collections/<pkg>/inst/templates/*.json)
// para public/templates/<pkg>/ e grava o índice em src/data/templates.json.
// Roda antes de dev, build e test (predev/prebuild/pretest); os dois
// destinos são gerados e ficam fora do git.

import { readdirSync, readFileSync, mkdirSync, copyFileSync, writeFileSync, rmSync, existsSync } from "node:fs";
import { join, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const site = join(dirname(fileURLToPath(import.meta.url)), "..");
const colecoes = join(site, "..", "collections");
const destino = join(site, "public", "templates");

rmSync(destino, { recursive: true, force: true });
const indice = [];
for (const pkg of readdirSync(colecoes).sort()) {
  const origem = join(colecoes, pkg, "inst", "templates");
  if (!existsSync(origem)) continue;
  for (const arquivo of readdirSync(origem).filter((f) => f.endsWith(".json")).sort()) {
    const texto = readFileSync(join(origem, arquivo), "utf8");
    const tpl = JSON.parse(texto);
    if (tpl.trama !== "template") throw new Error(`${pkg}/${arquivo}: não é um template do trama`);
    mkdirSync(join(destino, pkg), { recursive: true });
    copyFileSync(join(origem, arquivo), join(destino, pkg, arquivo));
    // `texto` vai inteiro no índice: o botão copia da própria página, sem
    // `fetch` — a escrita na área de transferência precisa acontecer dentro
    // do clique, e um `await` antes dela perde o gesto no Safari.
    indice.push({ pkg, arquivo, nome: tpl.nome, descricao: tpl.descricao, doc: tpl.doc, texto });
  }
}
writeFileSync(join(site, "src", "data", "templates.json"), JSON.stringify(indice, null, 2) + "\n");
console.log(`templates: ${indice.length} copiados para public/templates/`);
