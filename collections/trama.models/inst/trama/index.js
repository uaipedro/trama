// Os renderers da coleção `models`.
//
// O card de TESTE é do núcleo (`trama/test`): a régua logarítmica do p-valor e
// as estrelas atravessam coleções, e a `series` desenha os testes dela com o
// mesmo componente. Aqui só o que é da coleção — o quadro com p-valor por
// linha (ANOVA, coeficientes, comparações) e o card do modelo —, e os dois
// reaproveitam a `Regua` e as `Estrelas` do núcleo em vez de redesenhá-las.

import { h, registerRenderer, getRenderer, Regua, Estrelas, num, numP } from "trama";

registerRenderer("models/test", getRenderer("trama/test"));

// ---- models/effects --------------------------------------------------------------

// Uma linha por termo: nome, régua mini, p e estrela. A linha sem p (o
// resíduo) fica apagada, sem régua: ela está no quadro para dar os gl, não para
// ser lida como efeito.
function LinhasSig({ linhas }) {
  if (!linhas || !linhas.length) return h("div", { className: "tr-empty" }, "sem termos");
  return h("div", { className: "tr-sig" }, linhas.map((l, i) =>
    h("div", { key: i, className: "tr-sig-linha" + (l.p == null ? " tr-sig-apagada" : "") }, [
      h("span", { key: "t", className: "tr-sig-termo", title: l.detalhe ? `${l.termo} · ${l.detalhe}` : l.termo }, l.termo),
      l.p == null ? h("span", { key: "r" }) : h(Regua, { key: "r", p: l.p, estrelas: l.estrelas, mini: true }),
      h("span", { key: "p", className: "tr-sig-p" }, l.p == null ? "—" : num(l.p)),
      h("span", { key: "e", className: "tr-sig-e" }, l.p == null ? "" : h(Estrelas, { estrelas: l.estrelas })),
    ])));
}

function Rodape({ rodape }) {
  if (!rodape) return null;
  return h("div", { className: "tr-sig-rodape" },
    Object.keys(rodape).map((k) => h("span", { key: k }, [h("b", { key: "k" }, k), " ", rodape[k]])));
}

function Significancia({ artifact }) {
  const d = artifact.data || {};
  return h("div", { className: "tr-me" }, [
    h("div", { key: "t", className: "tr-mt-nome" }, d.titulo),
    h(LinhasSig, { key: "l", linhas: d.linhas }),
    h(Rodape, { key: "r", rodape: d.rodape }),
    d.nota ? h("div", { key: "n", className: "tr-sig-nota", title: d.nota }, d.nota) : null,
  ]);
}

// A vista `quadro`: o quadro inteiro, como o estatístico espera ver — FV, GL,
// SQ, QM, Fc e Pr > F —, com a régua e a estrela DENTRO da coluna do p. É a
// vista de abertura, porque é a que se confere; a `significância` é a leitura
// de relance.
function celula(v, chave) {
  if (v == null) return "";
  if (typeof v !== "number") return String(v);
  if (chave === "gl" || chave === "parametros") return Number.isInteger(v) ? String(v) : num(v, 1);
  return num(v, 4);
}

function Quadro({ artifact }) {
  const d = artifact.data || {};
  const q = d.quadro || {};
  const cols = (q.colunas || []).filter((c) => c.chave !== "p_valor");
  const temP = (q.colunas || []).some((c) => c.chave === "p_valor");
  const rotP = ((q.colunas || []).find((c) => c.chave === "p_valor") || {}).rotulo;
  const linhas = q.linhas || [];
  return h("div", { className: "tr-me" }, [
    h("div", { key: "t", className: "tr-mt-nome" }, d.titulo),
    h("div", { key: "q", className: "tr-quadro-wrap" },
      h("table", { className: "tr-quadro" }, [
        h("thead", { key: "h" }, h("tr", null, [
          ...cols.map((c, i) => h("th", { key: c.chave, className: i ? "tr-q-num" : "" }, c.rotulo)),
          temP ? h("th", { key: "p", className: "tr-q-num", colSpan: 3 }, rotP) : null,
        ])),
        h("tbody", { key: "b" }, linhas.map((r, i) => {
          const p = r.p_valor;
          const e = (q.estrelas || [])[i];
          const total = r.termo === "Total";
          const residuo = typeof r.termo === "string" && r.termo.startsWith("Resíduo");
          return h("tr", { key: i, className: (total ? "tr-q-total" : "") + (residuo ? " tr-q-residuo" : "") }, [
            ...cols.map((c, j) => h("td", { key: c.chave, className: j ? "tr-q-num" : "tr-q-fv",
                                             title: j ? undefined : String(r[c.chave] ?? "") },
                                     celula(r[c.chave], c.chave))),
            temP ? h("td", { key: "p", className: "tr-q-num" }, p == null ? "" : num(p)) : null,
            temP ? h("td", { key: "r", className: "tr-q-regua" },
                     p == null ? null : h(Regua, { p, estrelas: e, mini: true })) : null,
            temP ? h("td", { key: "e", className: "tr-q-est" },
                     p == null ? null : h(Estrelas, { estrelas: e })) : null,
          ]);
        })),
      ])),
    h(Rodape, { key: "r", rodape: d.rodape }),
    d.nota ? h("div", { key: "n", className: "tr-sig-nota", title: d.nota }, d.nota) : null,
  ]);
}

registerRenderer("models/effects", {
  views: [
    { id: "quadro", label: "quadro", component: Quadro },
    { id: "significancia", label: "significância", component: Significancia },
  ],
});

// ---- models/fit ------------------------------------------------------------------

function Destaque({ d }) {
  const valor = d.pct ? `${num(d.valor, 1)}%` : num(d.valor);
  return h("div", { className: "tr-mf-dest" }, [
    h("div", { key: "v", className: "tr-mf-dest-valor" }, valor),
    d.barra ? h("div", { key: "b", className: "tr-mf-dest-trilho" },
      h("div", { className: "tr-mf-dest-barra", style: { width: `${Math.max(0, Math.min(1, d.valor)) * 100}%` } })) : null,
    h("div", { key: "r", className: "tr-mt-rotulo" }, d.rotulo),
  ]);
}

function Ajuste({ artifact }) {
  const d = artifact.data || {};
  const g = d.global;
  return h("div", { className: "tr-mf" }, [
    h("div", { key: "topo", className: "tr-mt-topo" }, [
      h("span", { key: "n", className: "tr-mt-nome" }, d.rotulo),
      h("span", { key: "c", className: "tr-mt-rotulo" },
        `n = ${d.n}${d.descartadas ? ` (${d.descartadas} fora)` : ""}`),
    ]),
    h("div", { key: "f", className: "tr-mf-formula", title: d.formula }, d.formula),
    h("div", { key: "d", className: "tr-mf-destaques" },
      (d.destaques || []).slice(0, 3).map((x, i) => h(Destaque, { key: i, d: x }))),
    g ? h("div", { key: "g", className: "tr-mf-global" }, [
      h("div", { key: "l", className: "tr-mt-topo" }, [
        h("span", { key: "r", className: "tr-mt-rotulo" }, `${g.rotulo} · p = ${num(g.p)}`),
        h(Estrelas, { key: "e", estrelas: g.estrelas }),
      ]),
      h(Regua, { key: "r", p: g.p, estrelas: g.estrelas, mini: true }),
    ]) : null,
  ]);
}

function EfeitosDoModelo({ artifact }) {
  const d = artifact.data || {};
  return h("div", { className: "tr-me" }, [
    h("div", { key: "t", className: "tr-mt-nome" }, d.efeitos_titulo || "efeitos"),
    h(LinhasSig, { key: "l", linhas: d.linhas }),
  ]);
}

registerRenderer("models/fit", {
  views: [
    { id: "ajuste", label: "ajuste", component: Ajuste },
    { id: "efeitos", label: "efeitos", component: EfeitosDoModelo },
  ],
});
