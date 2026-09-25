// Mede o sugestor contra os fluxos de exemplo: hit@1/3/5, leave-one-FLOW-out.
//
// Uso (da raiz do repositório):
//   Rscript -e 'library(trama); reg <- tr_registry();
//     for (p in c("trama.data","trama.view","trama.models","trama.sampling",
//                 "trama.multi","trama.series","trama.ml")) tr_use(p, registry = reg);
//     cat(tr_catalog_json(reg))' > cat.json
//   node tools/sugestor/avaliar.mjs cat.json
//
// Para cada aresta a -> b de `exemplos/*/flows/*.json`, tira do catálogo as
// transições que AQUELE fluxo contribuiu (mesma contagem de `minerar.R`) e
// pergunta ao `sugerir` o que vem depois de a. Conta se b está no top-k.

import { readFileSync, readdirSync, existsSync } from "node:fs";
import { join } from "node:path";
import { sugerir, aceitantes, PESOS } from "../../inst/www/sugestor.js";

const cat = JSON.parse(readFileSync(process.argv[2] || "cat.json", "utf8"));
const byId = Object.fromEntries(cat.nodes.map((n) => [n.id, n]));

const fluxos = [];
for (const ex of readdirSync("exemplos")) {
  const dir = join("exemplos", ex, "flows");
  if (!existsSync(dir)) continue;
  for (const f of readdirSync(dir).filter((x) => x.endsWith(".json")))
    fluxos.push({ nome: `${ex}/${f}`, fl: JSON.parse(readFileSync(join(dir, f), "utf8")) });
}

// Mesmo par de `minerar.R`: (tipo de a, tipo de b), sem laço do mesmo tipo.
function pares(fl) {
  const conta = {};
  for (const e of fl.edges || []) {
    const de = fl.nodes[e.from.node]?.type, para = fl.nodes[e.to.node]?.type;
    if (!de || !para || de === para) continue;
    const k = `${de}>${para}`;
    conta[k] = (conta[k] || 0) + 1;
  }
  return conta;
}

function semFluxo(fl) {
  const tira = pares(fl);
  return (cat.transitions || [])
    .map((t) => ({ ...t, n: t.n - (tira[`${t.from}>${t.to}`] || 0) }))
    .filter((t) => t.n > 0);
}

const CONFIGS = [
  { nome: "etapa", pesos: { relacionado: 0, transicao: 0 }, contexto: false },
  { nome: "+relacionado", pesos: { transicao: 0 }, contexto: false },
  { nome: "+transicao", pesos: {}, contexto: false },
  { nome: "+contexto", pesos: {}, contexto: true },
];

function avaliar(cfg) {
  const orig = { ...PESOS };
  Object.assign(PESOS, cfg.pesos);
  let n = 0, fora = 0;
  const hit = { 1: 0, 3: 0, 5: 0 };
  try {
    for (const { fl } of fluxos) {
      const catLoo = { ...cat, transitions: semFluxo(fl) };
      for (const e of fl.edges || []) {
        const de = fl.nodes[e.from.node]?.type, para = fl.nodes[e.to.node]?.type;
        const porta = byId[de]?.outputs?.find((o) => o.name === e.from.port);
        if (!porta) { fora++; continue; }
        if (!aceitantes(cat, porta.type).some((a) => a.id === para)) { fora++; continue; }
        // Os outros nós do fluxo, sem o próprio destino.
        const presentes = cfg.contexto
          ? Object.entries(fl.nodes).filter(([k]) => k !== e.to.node).map(([, x]) => x.type)
          : [];
        const r = sugerir(catLoo, { de, tipo: porta.type, presentes })
          .findIndex((s) => s.id === para);
        n++;
        for (const k of [1, 3, 5]) if (r >= 0 && r < k) hit[k]++;
      }
    }
  } finally {
    Object.assign(PESOS, orig);
  }
  return { n, fora, hit };
}

const pct = (x, n) => `${((100 * x) / n).toFixed(1)}%`.padStart(7);
console.log(`${fluxos.length} fluxos`);
console.log("| camadas       | hit@1  | hit@3  | hit@5  |");
console.log("|---------------|--------|--------|--------|");
let resumo;
for (const cfg of CONFIGS) {
  const { n, fora, hit } = avaliar(cfg);
  resumo = { n, fora };
  console.log(`| ${cfg.nome.padEnd(13)} |${pct(hit[1], n)} |${pct(hit[3], n)} |${pct(hit[5], n)} |`);
}
console.log(`\n${resumo.n} arestas avaliadas; ${resumo.fora} fora (porta ou destino fora de aceitantes).`);
