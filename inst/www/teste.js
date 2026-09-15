// inst/www/teste.js — as regras do card de teste de hipótese, sem React.
//
// O card é do NÚCLEO porque o teste de hipótese atravessa coleções: a `series`
// tem ADF e Ljung-Box, a `models` tem Shapiro e contrastes, e as duas precisam
// que o mesmo p-valor seja desenhado do mesmo jeito. Cada coleção registra o
// seu id de tipo apontando para `trama/test` — o mesmo reuso explícito que a
// `data` faz com a tabela.
//
// O núcleo continua sem saber o que é um teste: o contrato é só de CAMPOS
// (`p_valor`, `criticos`, `sentido`, `decisao_5`), e o que cada campo significa
// quem escreve é a coleção. Separado do `runtime.js` pelo motivo de sempre: o
// que pode mentir em silêncio — o número arredondado para zero, o pontinho que
// acende na cauda errada — se testa com `node --test` puro.

// A régua vai de p = 1 (vazia) a p = 1e-4 (cheia). Quatro décadas: abaixo de
// 0,0001 a diferença não muda decisão nenhuma, e esticar a escala espremeria a
// região entre 0,1 e 0,01, que é onde as decisões acontecem.
export const DECADAS = 4;
export const MARCAS = [["10%", 0.1], ["5%", 0.05], ["1%", 0.01], ["0,1%", 0.001]];

// Os níveis dos pontinhos, para os testes sem p-valor. Pares [rótulo, alfa] e
// não `alfa * 100` no lugar do rótulo: `0.1 * 100` é 10.000000000000002, e a
// chave do objeto de críticos não bateria nunca.
export const NIVEIS = [["10%", 0.1], ["5%", 0.05], ["1%", 0.01]];

export function posicao(p) {
  if (p == null || Number.isNaN(p)) return 0;
  if (p <= 0) return 1;
  return Math.min(1, Math.max(0, -Math.log10(p) / DECADAS));
}

// As estrelas do `summary()` do R. Com p-valor de verdade não há motivo para
// outra convenção: é a que quem modela já lê sem legenda.
export function estrelas(p) {
  if (p == null || Number.isNaN(p)) return "";
  if (p < 0.001) return "***";
  if (p < 0.01) return "**";
  if (p < 0.05) return "*";
  if (p < 0.1) return ".";
  return "ns";
}

// Faixa pela estrela: é ela que dá a intensidade da cor. Um tom só —
// quem não distingue cor lê pelo comprimento e pela estrela.
export function faixa(est) {
  return { "***": 4, "**": 3, "*": 2, ".": 1 }[est] ?? 0;
}

// Venceu o corte? Com p-valor é comparação direta; sem ele, a estatística
// contra o crítico daquele nível, na cauda que `sentido` diz. É por isso que
// `sentido` é campo do DADO: teste novo não mexe neste arquivo.
export function venceu(d, rotulo, alfa) {
  if (d.p_valor != null) return d.p_valor < alfa;
  const cv = d.criticos && d.criticos[rotulo];
  if (cv == null) return false;
  return d.sentido === "menor" ? d.estatistica < cv : d.estatistica > cv;
}

export function num(v, casas = 3) {
  if (v == null || Number.isNaN(v)) return "—";
  if (typeof v !== "number") return String(v);
  if (!Number.isFinite(v)) return v > 0 ? "∞" : "−∞";
  const abs = Math.abs(v);
  // Abaixo de 1e-3 o `maximumFractionDigits` mente: 0,0002 sairia "0" e 0,0005
  // sairia "0,001", o dobro.
  if (abs !== 0 && (abs < 1e-3 || abs >= 1e6)) return v.toExponential(1).replace(".", ",");
  return v.toLocaleString("pt-BR", { maximumFractionDigits: casas });
}

// O p-valor de manchete: abaixo de 0,001, potência de dez escrita como no
// artigo — "2,5 × 10⁻¹²" —, e não o "2,5e-12" de console, que quem não
// programa lê como erro.
const SOBRESCRITO = { "-": "⁻", 0: "⁰", 1: "¹", 2: "²", 3: "³", 4: "⁴", 5: "⁵", 6: "⁶", 7: "⁷", 8: "⁸", 9: "⁹" };
export function numP(p) {
  if (p == null || Number.isNaN(p)) return "—";
  if (p <= 0) return "< 10⁻³⁰⁰";
  if (p >= 1e-3) return num(p);
  const [m, e] = p.toExponential(1).split("e");
  const exp = String(Number(e)).split("").map((c) => SOBRESCRITO[c] ?? c).join("");
  return `${m.replace(".", ",")} × 10${exp}`;
}

// A geometria do efeito: onde ficam ponto, intervalo e referência numa faixa de
// 0 a 100%. Referência 1 quando o efeito é razão (de chances, de taxas): ali o
// "sem efeito" é 1, e um zero no eixo mentiria.
export function eixoEfeito(e) {
  const razao = /raz[aã]o/i.test(e.rotulo || "");
  const ref = razao ? 1 : 0;
  const temIC = e.li != null && e.ls != null;
  const vals = [e.valor, ref, ...(temIC ? [e.li, e.ls] : [])].filter((v) => v != null && Number.isFinite(v));
  const lo = Math.min(...vals), hi = Math.max(...vals);
  const pad = (hi - lo) * 0.08 || 1;
  const x = (v) => ((v - (lo - pad)) / (hi - lo + 2 * pad)) * 100;
  return { ref, temIC, cruza: temIC && e.li <= ref && e.ls >= ref, x };
}
