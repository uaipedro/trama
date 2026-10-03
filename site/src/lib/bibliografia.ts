// Bibliografia do site: junta as referências de todos os blocos
// (src/data/node-docs.json), remove repetidas e agrupa por papel, guardando
// quais blocos citam cada obra. A formatação de cada entrada é a mesma das
// páginas de bloco (`referenciaHast` em node-docs.ts).

import { PAPEIS_REF, resolveText, type NodeDocs, type Referencia } from "./node-docs.ts";

export interface EntradaBib {
  /** Âncora estável na página da bibliografia (sem `#`). */
  id: string;
  /** A referência, sem a `nota` (a nota fala do bloco, não da obra). */
  ref: Referencia;
  /** Ids dos blocos que citam a obra, em ordem alfabética. */
  blocos: string[];
}

export interface GrupoBib {
  papel: Referencia["papel"];
  rotulo: string;
  entradas: EntradaBib[];
}

const norm = (s: string) =>
  s.normalize("NFD").replace(/[̀-ͯ]/g, "").toLowerCase().replace(/[^a-z0-9]+/g, " ").trim();

/** Chave de identidade de uma obra: DOI; senão pacote::função; senão autores+ano+título. */
export function chaveReferencia(r: Referencia): string {
  if (r.doi) return `doi:${r.doi.toLowerCase()}`;
  if (r.papel === "implementacao" && r.pacote) return `pkg:${r.pacote}::${r.funcao ?? ""}`;
  return `obra:${norm((r.autores ?? []).join(" "))}|${r.ano ?? ""}|${norm(resolveText(r.titulo))}`;
}

const slug = (s: string) => norm(s).split(" ").slice(0, 3).join("-");

function idBase(r: Referencia): string {
  if (r.papel === "implementacao" && r.pacote && !r.autores?.length) {
    return `ref-${slug(r.pacote)}${r.funcao ? "-" + slug(r.funcao) : ""}`;
  }
  const autor = (r.autores?.[0] ?? "anonimo").split(",")[0];
  const titulo = norm(resolveText(r.titulo)).split(" ").find((w) => w.length > 3) ?? "";
  return ["ref", slug(autor), r.ano ?? "sd", titulo].filter(Boolean).join("-");
}

const ordemPapel = (p: Referencia["papel"]) => PAPEIS_REF.findIndex(([x]) => x === p);
const ordenacao = (r: Referencia) =>
  norm(r.papel === "implementacao" && r.pacote ? `${r.pacote} ${r.funcao ?? ""}` : `${(r.autores ?? []).join(" ")} ${r.ano ?? ""}`);

/**
 * Agrega as referências de todos os blocos. Uma obra citada com papéis
 * diferentes fica no papel de maior precedência (teoria > livro-texto >
 * implementação > complementar), na ordem de `PAPEIS_REF`.
 */
export function agregarReferencias(docs: Record<string, NodeDocs>): GrupoBib[] {
  const porChave = new Map<string, { ref: Referencia; blocos: Set<string> }>();
  for (const bloco of Object.keys(docs).sort()) {
    for (const r of docs[bloco].referencias) {
      const k = chaveReferencia(r);
      const atual = porChave.get(k);
      const { nota: _nota, ...semNota } = r;
      if (!atual) porChave.set(k, { ref: semNota, blocos: new Set([bloco]) });
      else {
        atual.blocos.add(bloco);
        if (ordemPapel(r.papel) < ordemPapel(atual.ref.papel)) atual.ref = { ...atual.ref, papel: r.papel };
      }
    }
  }
  const usados = new Map<string, number>();
  return PAPEIS_REF.flatMap(([papel, rotulo]) => {
    const entradas = [...porChave.values()]
      .filter((e) => e.ref.papel === papel)
      .sort((a, b) => ordenacao(a.ref).localeCompare(ordenacao(b.ref)))
      .map(({ ref, blocos }) => {
        const base = idBase(ref);
        const n = (usados.get(base) ?? 0) + 1;
        usados.set(base, n);
        return { id: n === 1 ? base : `${base}-${n}`, ref, blocos: [...blocos].sort() };
      });
    return entradas.length ? [{ papel, rotulo, entradas }] : [];
  });
}

/**
 * Encontra uma obra pelo sobrenome do primeiro autor e pelo ano (para o
 * glossário apontar só para referências que já existem nos blocos).
 */
export function encontrarReferencia(grupos: GrupoBib[], autor: string, ano: number): EntradaBib | undefined {
  const alvo = norm(autor);
  return grupos.flatMap((g) => g.entradas)
    .find((e) => e.ref.ano === ano && norm((e.ref.autores?.[0] ?? "").split(",")[0]) === alvo);
}

export interface ResumoBib {
  blocos: number;
  blocosComReferencias: number;
  blocosComPressupostos: number;
  obras: number;
  obrasComDoi: number;
  citacoes: number;
  implementacoes: number;
  pressupostos: number;
  pressupostosComVerificacao: number;
}

/** Números para a página de rigor, calculados no build. */
export function resumoReferencias(docs: Record<string, NodeDocs>): ResumoBib {
  const valores = Object.values(docs);
  const grupos = agregarReferencias(docs);
  const obras = grupos.flatMap((g) => g.entradas).filter((e) => e.ref.papel !== "implementacao");
  return {
    blocos: valores.length,
    blocosComReferencias: valores.filter((d) => d.referencias.length > 0).length,
    blocosComPressupostos: valores.filter((d) => d.pressupostos.length > 0).length,
    obras: obras.length,
    obrasComDoi: obras.filter((e) => e.ref.doi).length,
    citacoes: valores.reduce((s, d) => s + d.referencias.length, 0),
    implementacoes: grupos.flatMap((g) => g.entradas).length - obras.length,
    pressupostos: valores.reduce((s, d) => s + d.pressupostos.length, 0),
    pressupostosComVerificacao: valores.reduce((s, d) => s + d.pressupostos.filter((p) => p.verificar?.length).length, 0)
  };
}
