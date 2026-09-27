// Adaptador da captura real do editor ao vocabulário simples do renderizador.
import capturaJson from "../../capturas/anova-modos.json";
import type { CardDeModelo, Quadro, Spec, Tabela } from "./tipos";

// JSON capturado é um formato de intercâmbio produzido pelo CLI do trama.
// A adaptação explícita abaixo valida/normaliza os campos que o vídeo consome.
const captura: any = capturaJson;
const nos: any = captura.nos;
function spec(no: any): Spec {
  const c: any = no.catalogo;
  return {
    id: c.id, rotulo: c.label, categoria: c.category,
    icone: c.icon.value,
    entradas: c.inputs.map((p: any) => ({ nome: p.name, obrigatoria: p.required })),
    saidas: c.outputs.map((p: any) => ({ nome: p.name, obrigatoria: p.required })),
    vistas: no.catalogo.id === "models/fit" ? ["ajuste", "efeitos"] :
      no.catalogo.id === "models/effects" ? ["quadro", "significância"] :
      no.catalogo.id === "models/emmeans" ? ["imagem"] : ["tabela"],
    params: c.params.map((p: any) => {
      const choices = p.choices ?? [];
      const tipo = p.kind === "enum" ? (choices.length > 8 ? "enum-select" : choices.length > 3 ? "enum-largo" : "enum-inline") :
        p.kind === "number" ? "numero" : "campo";
      return { nome: p.name, rotulo: p.label, tipo, valor: String(no.params[p.name] ?? p.default ?? ""),
        ...(choices.length ? { opcoes: choices } : {}), ...(p.example ? { exemplo: p.example } : {}) };
    }),
  };
}
function preview(id: string): any { return (Object.values(nos[id].resultado.outputs)[0] as any).preview.data; }

const tabela = preview("dados");
export const CAPTURA_ANOVA = {
  dados: spec(nos.dados),
  tabela: { colunas: tabela.columns, linhas: tabela.rows.map((r: any) => tabela.columns.map((k: string) => r[k])) } as Tabela,
  anova: spec(nos.anova),
  fit: (() => {
    const d = preview("anova");
    return { rotulo: d.rotulo, formula: d.formula, n: d.n,
      destaques: d.destaques.map(({ rotulo, valor, barra, pct }: any) => ({ rotulo, valor, barra, pct })),
      global: { rotulo: d.global.rotulo, p: d.global.p } } as CardDeModelo;
  })(),
  quadroSpec: spec(nos.quadro),
  quadro: (() => {
    const d = preview("quadro");
    const q = d.quadro;
    return { titulo: d.titulo, colunas: q.colunas.map((c: any) => c.rotulo), inteiras: [0], rotuloP: "Pr > F",
      linhas: q.linhas.map((r: any) => ({ termo: r.termo, valores: [r.gl, r.sq, r.qm, r.F], p: r.p_valor })), rodape: d.rodape } as Quadro;
  })(),
  mediasSpec: spec(nos.medias),
  imagem: (Object.values(nos.medias.resultado.outputs)[0] as any).preview.files.png,
};
