// inst/www/params.js — as regras dos widgets de param, sem React.
//
// Separado de `runtime.js` para poder ser testado com `node --test` puro:
// `runtime.js` importa React e o importmap do navegador, e as duas coisas que
// podem dar errado em silêncio aqui — o formato de um enum e deixar passar um
// número inválido — não precisam de DOM para serem verificadas.

// Larguras em caracteres, não em pixels: a fonte do card é fixa (11px) e
// contar caracteres é estável entre máquinas, enquanto medir texto exigiria um
// DOM. Os limites saem dos casos reais: a proporção (16 caracteres em 5
// opções) é o maior que cabe nos ~140px ao lado do rótulo; "média | ingênuo |
// ingênuo sazonal | deriva" (33) é o maior que cabe na largura do card.
const INLINE = { n: 5, chars: 16 };
const WIDE = { n: 6, chars: 36 };

export function layoutEnum(choices) {
  const cs = Array.isArray(choices) ? choices.map(String) : [];
  if (!cs.length) return "select";
  const chars = cs.reduce((s, c) => s + c.length, 0);
  if (cs.length <= INLINE.n && chars <= INLINE.chars) return "inline";
  if (cs.length <= WIDE.n && chars <= WIDE.chars) return "wide";
  return "select";
}

// Vírgula decimal é aceita: a interface fala português e "2,5" é o que se
// digita. `Number("")` dá 0, por isso o vazio é tratado antes.
export function validarNumero(spec, texto) {
  const t = String(texto ?? "").trim().replace(",", ".");
  if (t === "") return { ok: false, erro: "obrigatório" };
  const v = Number(t);
  if (!Number.isFinite(v)) return { ok: false, erro: "precisa ser número" };
  if (spec.kind === "integer" && !Number.isInteger(v)) return { ok: false, erro: "precisa ser inteiro" };
  if (spec.min != null && v < spec.min) return { ok: false, erro: `mínimo ${spec.min}` };
  if (spec.max != null && v > spec.max) return { ok: false, erro: `máximo ${spec.max}` };
  return { ok: true, valor: v };
}
