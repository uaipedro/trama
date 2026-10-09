// Adaptador da captura real do fatorial adubo × irrigação (a mesma sessão do
// segmento F da apresentação) ao vocabulário do renderizador. Os números são
// os do R: `capturas/apresentacao-fatorial.json`, gerado por
// `scripts/capturar.R capturas/apresentacao-fatorial.fluxo.json`.
import capturaJson from "../../capturas/apresentacao-fatorial.json";
import type { CardDeModelo, Quadro, Spec, Tabela } from "./tipos";

const nos: any = (capturaJson as any).nos;
const VISTAS: Record<string, string[]> = {
  "data/table": ["tabela"], "models/fit": ["ajuste", "efeitos"],
  "models/effects": ["quadro", "significância"], "models/emm": ["imagem"],
};
const saida = (id: string): any => Object.values(nos[id].resultado.outputs)[0];

function spec(id: string, rotulo?: string): Spec {
  const no = nos[id], c: any = no.catalogo;
  return {
    id: c.id, rotulo: rotulo ?? c.label, categoria: c.category, icone: c.icon.value,
    entradas: c.inputs.map((p: any) => ({ nome: p.name, obrigatoria: p.required })),
    saidas: c.outputs.map((p: any) => ({ nome: p.name, obrigatoria: p.required })),
    vistas: VISTAS[saida(id).type] ?? ["tabela"],
    params: c.params.map((p: any) => {
      const choices = p.choices ?? [];
      const tipo = p.kind === "enum" ? (choices.length > 8 ? "enum-select" : choices.length > 3 ? "enum-largo" : "enum-inline") :
        p.kind === "number" ? "numero" : "campo";
      const v = no.params[p.name] ?? p.default ?? "";
      return { nome: p.name, rotulo: p.label, tipo, valor: Array.isArray(v) ? v.join(", ") : String(v),
        ...(choices.length ? { opcoes: choices } : {}), ...(p.example ? { exemplo: p.example } : {}) };
    }),
  };
}

const tab = saida("dados").preview.data;
const fit = saida("anova").preview.data;
const ef = saida("quadro").preview.data;

export const FATORIAL = {
  dados: spec("dados", "Dados do experimento"),
  tabela: { colunas: tab.columns, linhas: tab.rows.slice(0, 12).map((r: any) => tab.columns.map((k: string) => r[k])) } as Tabela,
  anova: spec("anova"),
  fit: { rotulo: fit.rotulo, formula: fit.formula, n: fit.n,
    destaques: fit.destaques.map(({ rotulo, valor, barra, pct }: any) => ({ rotulo, valor, barra, pct })),
    global: fit.global ? { rotulo: fit.global.rotulo, p: fit.global.p } : null } as CardDeModelo,
  quadroSpec: spec("quadro"),
  quadro: { titulo: ef.titulo,
    colunas: ef.quadro.colunas.filter((c: any) => c.chave !== "p_valor").map((c: any) => c.rotulo),
    inteiras: [0], rotuloP: "Pr > F",
    linhas: ef.quadro.linhas.map((r: any) => ({ termo: r.termo, valores: [r.gl, r.sq, r.qm, r.F], p: r.p_valor })),
    rodape: ef.rodape } as Quadro,
  tukeySpec: spec("tukey"),
  // O preview de `models/emm` é a imagem que o R desenhou (com as letras).
  tukeyPng: saida("tukey").preview.files.png as string,
};
