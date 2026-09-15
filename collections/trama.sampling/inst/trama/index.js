// Os renderers da coleção `sampling`. Três cards, três perguntas:
//
// - o PLANO responde "por que esse n?": o número grande e a escada dos ajustes,
//   uma barra por degrau, para que o degrau que dobrou o n salte aos olhos;
// - a AMOSTRA responde "o que foi sorteado?": n de N, e a fração de cada
//   estrato numa barra;
// - a ESTIMATIVA responde "quão preciso?": o intervalo e a régua do CV, com as
//   faixas que instituto de estatística usa para decidir se publica.
//
// A simulação não tem renderer aqui: o card dela é um PNG da `view`.

import { h, registerRenderer, getRenderer } from "trama";

// A régua do CV vai de 0 a 40%: acima disso tudo é "imprecisa", e esticar a
// escala espremeria a região entre 5 e 15%, que é onde a decisão acontece.
const CV_MAX = 40;
const FAIXAS = [[5, "ótima"], [15, "boa"], [30, "regular"], [Infinity, "imprecisa"]];

function num(v, casas = 2) {
  if (v == null || Number.isNaN(v)) return "—";
  if (typeof v !== "number") return String(v);
  if (!Number.isFinite(v)) return v > 0 ? "∞" : "−∞";
  const abs = Math.abs(v);
  if (abs !== 0 && abs < 1e-3) return v.toExponential(1).replace(".", ",");
  // Número grande perde as casas: um total de 1.564.926 t com ",37" no fim
  // promete uma precisão que o erro padrão de 55 mil desmente.
  const c = abs >= 1000 ? 0 : abs >= 100 ? 1 : casas;
  return v.toLocaleString("pt-BR", { maximumFractionDigits: c });
}

function pct(v, casas = 1) {
  return v == null ? "—" : `${num(100 * v, casas)}%`;
}

function faixa(cv) {
  if (cv == null) return { i: 0, nome: "" };
  const i = FAIXAS.findIndex(([lim]) => cv <= lim);
  return { i: i + 1, nome: FAIXAS[i][1] };
}

// Pares `dt`/`dd` num array plano: o runtime exporta `h` e não o `React`.
function Campos({ campos }) {
  const filhos = [];
  campos.forEach(([k, v], i) => {
    if (v == null || v === "") return;
    filhos.push(h("dt", { key: `k${i}` }, k));
    filhos.push(h("dd", { key: `v${i}` }, String(v)));
  });
  return h("dl", { className: "tr-kv tr-sa-kv" }, filhos);
}

function TabelaNucleo(props) {
  const base = getRenderer("trama/table");
  return h(base.views[0].component, props);
}

// ---- sampling/plan ------------------------------------------------------------

// A escada: a barra de cada degrau é relativa ao maior degrau DA MESMA
// unidade. No plano de conglomerados, 749 unidades e 29 conglomerados na mesma
// escala fariam o degrau que importa virar um risco.
function Escada({ passos }) {
  const max = {};
  for (const p of passos) max[p.unidade] = Math.max(max[p.unidade] || 0, p.valor);
  return h("div", { className: "tr-sa-escada" }, passos.map((p, i) =>
    h("div", { key: i, className: "tr-sa-degrau" + (i === passos.length - 1 ? " tr-sa-final" : "") }, [
      h("span", { key: "r", className: "tr-sa-degrau-rot", title: p.passo }, p.passo),
      h("span", { key: "t", className: "tr-sa-trilho" },
        h("span", { className: "tr-sa-barra", style: { width: `${(p.valor / (max[p.unidade] || 1)) * 100}%` } })),
      h("span", { key: "v", className: "tr-sa-degrau-val" }, num(p.valor, 1)),
    ])));
}

function Plano({ artifact }) {
  const d = artifact.data || {};
  const margem = d.erro == null ? null : d.percentual ? `± ${num(100 * d.erro, 1)} pontos` : `± ${num(d.erro)}`;
  const sub = d.conglomerados != null
    ? `${num(d.conglomerados)} conglomerados × ${num(d.tamanho_conglomerado)}`
    : margem ? `margem ${margem} · ${pct(d.confianca, 0)}` : `${pct(d.confianca, 0)} de confiança`;
  return h("div", { className: "tr-sa" }, [
    h("div", { key: "t", className: "tr-sa-topo" }, [
      h("span", { key: "n", className: "tr-sa-nome" }, d.rotulo),
      d.erro_alcancado != null ? h("span", { key: "a", className: "tr-sa-rot" }, `margem alcançada ± ${num(d.erro_alcancado)}`) : null,
    ]),
    // No plano de margem a pergunta é o inverso: o número grande é a margem,
    // e o n vai para o rótulo.
    d.tipo === "margem"
      ? h("div", { key: "m", className: "tr-sa-meio" }, [
          h("span", { key: "v", className: "tr-sa-valor" }, `± ${num(100 * d.erro, 1)} pp`),
          h("span", { key: "s", className: "tr-sa-rot" }, `n = ${num(d.n)} · ${pct(d.confianca, 0)}`),
        ])
      : h("div", { key: "m", className: "tr-sa-meio" }, [
          h("span", { key: "v", className: "tr-sa-valor" }, `n = ${num(d.n)}`),
          h("span", { key: "s", className: "tr-sa-rot" }, sub),
        ]),
    h(Escada, { key: "e", passos: d.passos || [] }),
    d.nota ? h("div", { key: "no", className: "tr-sa-nota", title: d.nota }, d.nota) : null,
  ]);
}

// n_h de N_h em barra: a fração amostral de cada estrato. É onde se vê que o
// Neyman foi buscar o Norte.
function Estratos({ linhas }) {
  if (!linhas || !linhas.length) return h("div", { className: "tr-empty" }, "sem estratos");
  return h("div", { className: "tr-sa-estratos" }, linhas.map((l, i) =>
    h("div", { key: i, className: "tr-sa-estrato" }, [
      h("span", { key: "r", className: "tr-sa-degrau-rot", title: l.estrato }, l.estrato),
      h("span", { key: "t", className: "tr-sa-trilho" },
        l.N != null ? h("span", { className: "tr-sa-barra", style: { width: `${Math.min(1, l.n / l.N) * 100}%` } }) : null),
      h("span", { key: "v", className: "tr-sa-degrau-val" }, l.N != null ? `${num(l.n)} / ${num(l.N)}` : num(l.n)),
    ])));
}

function Alocacao({ artifact }) {
  const d = artifact.data || {};
  if (!d.alocacao) {
    return h(Campos, { campos: (d.passos || []).map((p) => [p.passo, num(p.valor, 2)]) });
  }
  return h("div", { className: "tr-sa" }, [
    h("div", { key: "t", className: "tr-sa-nome" }, "n por estrato (de N)"),
    h(Estratos, { key: "e", linhas: d.alocacao }),
  ]);
}

registerRenderer("sampling/plan", {
  views: [
    { id: "plano", label: "plano", component: Plano },
    { id: "alocacao", label: "alocação", component: Alocacao },
  ],
});

// ---- sampling/sample ------------------------------------------------------------

function Desenho({ artifact }) {
  const d = artifact.data || {};
  const fracao = d.N ? ` · ${pct(d.n / d.N)}` : "";
  const pesos = d.peso_min === d.peso_max ? `peso ${num(d.peso_min)}` : `pesos ${num(d.peso_min)} a ${num(d.peso_max)}`;
  return h("div", { className: "tr-sa" }, [
    h("div", { key: "t", className: "tr-sa-topo" }, [
      h("span", { key: "n", className: "tr-sa-nome", title: d.rotulo }, d.rotulo),
    ]),
    h("div", { key: "m", className: "tr-sa-meio" }, [
      h("span", { key: "v", className: "tr-sa-valor" }, `n = ${num(d.n)}`),
      h("span", { key: "s", className: "tr-sa-rot" }, d.N ? `de N = ${num(d.N)}${fracao}` : ""),
    ]),
    h("div", { key: "u", className: "tr-sa-rot" },
      `${d.upas != null ? `${num(d.upas)} ${d.unidade_primaria} · ` : ""}${pesos}`),
    h(Estratos, { key: "e", linhas: d.estratos }),
    d.mais_estratos ? h("div", { key: "x", className: "tr-sa-rot" }, `+ ${d.mais_estratos} estratos`) : null,
    d.nota ? h("div", { key: "no", className: "tr-sa-nota", title: d.nota }, d.nota) : null,
  ]);
}

registerRenderer("sampling/sample", {
  views: [
    { id: "desenho", label: "desenho", component: Desenho },
    { id: "tabela", label: "tabela", component: TabelaNucleo },
  ],
});

// ---- sampling/estimate ------------------------------------------------------------

function ReguaCV({ cv, mini }) {
  const f = faixa(cv);
  const pos = cv == null ? 0 : Math.min(1, cv / CV_MAX);
  return h("div", { className: "tr-sa-cv" + (mini ? " tr-sa-cv-mini" : "") + ` tr-sa-f${f.i}`,
                    title: cv == null ? "" : `CV ${num(cv, 1)}% · ${f.nome}` }, [
    h("div", { key: "t", className: "tr-sa-cv-trilho" }, [
      h("div", { key: "b", className: "tr-sa-cv-barra", style: { width: `${pos * 100}%` } }),
      ...[5, 15, 30].map((m) => h("span", { key: m, className: "tr-sa-cv-marca", style: { left: `${(m / CV_MAX) * 100}%` } })),
    ]),
    mini ? null : h("div", { key: "r", className: "tr-sa-cv-rotulos" },
      [5, 15, 30].map((m) => h("span", { key: m, style: { left: `${(m / CV_MAX) * 100}%` } }, `${m}%`))),
  ]);
}

// O intervalo de um domínio numa escala comum a todos: posição da estimativa e
// largura do intervalo comparáveis entre linhas.
function Intervalo({ l, lo, hi }) {
  const span = hi - lo || 1;
  const x = (v) => `${((v - lo) / span) * 100}%`;
  return h("span", { className: "tr-sa-ic" }, [
    h("span", { key: "b", className: "tr-sa-ic-barra", style: { left: x(l.li), width: `${((l.ls - l.li) / span) * 100}%` } }),
    h("span", { key: "p", className: "tr-sa-ic-ponto", style: { left: x(l.estimativa) } }),
  ]);
}

function Estimativa({ artifact }) {
  const d = artifact.data || {};
  const linhas = d.linhas || [];
  const fmt = (v) => (d.percentual ? pct(v) : num(v));
  const conf = pct(d.confianca, 0);
  if (linhas.length === 1) {
    const l = linhas[0];
    const f = faixa(l.cv);
    return h("div", { className: "tr-sa" }, [
      h("div", { key: "t", className: "tr-sa-topo" }, [
        h("span", { key: "n", className: "tr-sa-nome", title: d.desenho }, d.titulo),
        h("span", { key: "f", className: `tr-sa-selo tr-sa-f${f.i}` }, f.nome),
      ]),
      h("div", { key: "m", className: "tr-sa-meio" }, [
        h("span", { key: "v", className: "tr-sa-valor" }, fmt(l.estimativa)),
        h("span", { key: "s", className: "tr-sa-rot" }, `± ${fmt(l.margem)} (${conf})`),
      ]),
      h("div", { key: "i", className: "tr-sa-rot" },
        `IC [${fmt(l.li)}; ${fmt(l.ls)}] · CV ${num(l.cv, 1)}%${l.deff != null ? ` · deff ${num(l.deff)}` : ""} · n ${num(l.n)}`),
      h(ReguaCV, { key: "r", cv: l.cv }),
      d.nota ? h("div", { key: "no", className: "tr-sa-nota", title: d.nota }, d.nota) : null,
    ]);
  }
  const lo = Math.min(...linhas.map((l) => l.li));
  const hi = Math.max(...linhas.map((l) => l.ls));
  return h("div", { className: "tr-sa" }, [
    h("div", { key: "t", className: "tr-sa-topo" }, [
      h("span", { key: "n", className: "tr-sa-nome", title: d.desenho }, d.titulo),
      h("span", { key: "c", className: "tr-sa-rot" }, `IC ${conf}`),
    ]),
    h("div", { key: "l", className: "tr-sa-dominios" }, linhas.map((l, i) =>
      h("div", { key: i, className: "tr-sa-dominio", title: `${l.rotulo}: ${fmt(l.estimativa)} [${fmt(l.li)}; ${fmt(l.ls)}] · CV ${num(l.cv, 1)}%` }, [
        h("span", { key: "r", className: "tr-sa-degrau-rot" }, l.rotulo),
        h(Intervalo, { key: "i", l, lo, hi }),
        h("span", { key: "v", className: "tr-sa-degrau-val" }, fmt(l.estimativa)),
        h(ReguaCV, { key: "c", cv: l.cv, mini: true }),
      ]))),
    d.nota ? h("div", { key: "no", className: "tr-sa-nota", title: d.nota }, d.nota) : null,
  ]);
}

registerRenderer("sampling/estimate", {
  views: [
    { id: "estimativa", label: "estimativa", component: Estimativa },
    { id: "tabela", label: "tabela", component: TabelaNucleo },
  ],
});
