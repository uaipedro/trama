// Selos que plugins põem no cabeçalho de um card (o "está num documento" do
// `trama.documentos` é o caso).
//
// Puro (sem React, sem "trama"), como o `menu-card.js`: o registro mora no
// `runtime.js` (`registerCardBadge`), o editor só desenha o que `selosDoCard`
// devolve. O plugin recebe o card como `infoDoCard(no)` e devolve um selo ou
// nada; o desenho (ícone do conjunto, discreto, na ponta do cabeçalho) é do
// núcleo, para todo selo de plugin parecer da mesma família.

// Nome de ícone do conjunto (o sprite `vendor/lucide.svg`).
const NOME_ICONE = /^[a-z0-9-]+$/;

// Os selos que valem para este card. `def.selo(card)` devolve
// `{ icone, titulo?, acao? }` ou nada. Erro de um plugin tira só o selo dele.
export function selosDoCard(defs, card) {
  const out = [];
  for (const d of defs || []) {
    try {
      if (typeof d.selo !== "function") continue;
      const s = d.selo(card);
      if (!s || typeof s.icone !== "string" || !NOME_ICONE.test(s.icone)) continue;
      out.push({ id: d.id, icone: s.icone, titulo: typeof s.titulo === "string" ? s.titulo : "",
                 acao: typeof s.acao === "function" ? () => s.acao(card) : null });
    } catch (e) {
      if (typeof console !== "undefined") console.warn(`trama: selo '${d?.id}' falhou`, e);
    }
  }
  return out;
}
