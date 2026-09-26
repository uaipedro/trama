// Corpus de fluxos reais do repositório para o sugestor de "próximo bloco".
//
// Uso (da raiz do repositório):
//   node --experimental-strip-types --no-warnings tools/sugestor/corpus.mjs [fontes]
// `fontes` é uma lista separada por vírgula (padrão: todas). Imprime JSON:
//   [{ nome, fonte, nodes: { id: { type } }, edges: [{ from: {node, port}, to: {node, port} }] }]
// `minerar.R` e `avaliar.mjs` leem daqui; não há segunda lista de fontes.
//
// Fontes (a ordem importa: na duplicata, fica a primeira):
//   exemplos  — exemplos/*/flows/*.json, os projetos completos.
//   templates — collections/*/inst/templates/*.json (campo `doc`). A cópia em
//               site/src/data/templates.json é idêntica e cai na deduplicação.
//   docs      — blocos ```r com tr_flow()/tr_add() das páginas do site,
//               lidos pelo mesmo parser que desenha o canvas na doc. Não trazem
//               porta: `port` fica null e quem avalia escolhe a porta.
// Fixtures de teste ficaram de fora: nenhuma é um pipeline realista.
// Dois fluxos são o mesmo quando têm os mesmos tipos de nó e as mesmas
// arestas tipo -> tipo (multiconjuntos); ids e parâmetros não contam.

import { readFileSync, readdirSync, existsSync, statSync } from "node:fs";
import { join } from "node:path";
import { pathToFileURL } from "node:url";

const lerJson = (f) => JSON.parse(readFileSync(f, "utf8"));
const jsons = (dir) =>
  existsSync(dir) ? readdirSync(dir).filter((x) => x.endsWith(".json")).map((x) => join(dir, x)) : [];

function deDoc(doc) {
  const nodes = {};
  for (const [id, n] of Object.entries(doc.nodes || {})) nodes[id] = { type: n.type };
  const edges = (doc.edges || []).map((e) => ({
    from: { node: e.from.node, port: e.from.port ?? null },
    to: { node: e.to.node, port: e.to.port ?? null },
  }));
  return { nodes, edges };
}

function arquivosMd(dir) {
  const out = [];
  for (const x of readdirSync(dir)) {
    const p = join(dir, x);
    if (statSync(p).isDirectory()) out.push(...arquivosMd(p));
    else if (/\.mdx?$/.test(x)) out.push(p);
  }
  return out;
}

export const FONTES = {
  exemplos: async () =>
    readdirSync("exemplos").flatMap((ex) =>
      jsons(join("exemplos", ex, "flows")).map((f) => ({ nome: f, ...deDoc(lerJson(f)) })),
    ),
  templates: async () =>
    readdirSync("collections").flatMap((p) =>
      jsons(join("collections", p, "inst", "templates")).map((f) => ({ nome: f, ...deDoc(lerJson(f).doc) })),
    ),
  docs: async () => {
    const { parseFlowExample } = await import(pathToFileURL("site/src/lib/flow-example.ts").href);
    const out = [];
    for (const f of arquivosMd("site/src/content")) {
      const blocos = [...readFileSync(f, "utf8").matchAll(/```r[^\n]*\n([\s\S]*?)```/g)];
      blocos.forEach((b, i) => {
        (parseFlowExample(b[1]) || []).forEach((g, j) => {
          const nodes = Object.fromEntries(g.nodes.map((n) => [n.id, { type: n.type }]));
          const edges = g.edges.map(([de, para, pd, pp]) => ({
            from: { node: de, port: pd ?? null },
            to: { node: para, port: pp ?? null },
          }));
          out.push({ nome: `${f}#${i + 1}.${j + 1}`, nodes, edges });
        });
      });
    }
    return out;
  },
};

export function assinatura(fl) {
  const tipos = Object.values(fl.nodes).map((n) => n.type).sort();
  const arestas = fl.edges.map((e) => `${fl.nodes[e.from.node]?.type}>${fl.nodes[e.to.node]?.type}`).sort();
  return JSON.stringify([tipos, arestas]);
}

export async function carregarCorpus(fontes = Object.keys(FONTES)) {
  const vistos = new Set(), corpus = [];
  for (const fonte of fontes) {
    for (const fl of await FONTES[fonte]()) {
      if (!fl.edges.length) continue;
      const k = assinatura(fl);
      if (vistos.has(k)) continue;
      vistos.add(k);
      corpus.push({ fonte, ...fl });
    }
  }
  return corpus;
}

if (import.meta.url === pathToFileURL(process.argv[1]).href) {
  const fontes = process.argv[2] ? process.argv[2].split(",") : undefined;
  process.stdout.write(JSON.stringify(await carregarCorpus(fontes)));
}
