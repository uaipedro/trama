// Os renderers da coleção `spatial`. Dois cards de dados, duas perguntas:
//
// - os PONTOS respondem "o que há neste conjunto?": a variável e a unidade, o
//   n e os quartis, o postplot (cada ponto no seu lugar, símbolo e cor por
//   quartil) e as primeiras linhas da tabela;
// - o VARIOGRAMA responde "há dependência espacial?": a semivariância contra a
//   distância, com a ÁREA de cada ponto proporcional ao número de pares. Uma
//   classe apoiada em poucos pares tem que parecer menos confiável, porque é.
//
// Modelo e superfície não têm renderer aqui: o card deles é um PNG da `view`.

import { h, registerRenderer, getRenderer } from "trama";

function num(v, casas = 2) {
  if (v == null || Number.isNaN(v)) return "—";
  if (typeof v !== "number") return String(v);
  if (!Number.isFinite(v)) return v > 0 ? "∞" : "−∞";
  const abs = Math.abs(v);
  if (abs !== 0 && abs < 1e-3) return v.toExponential(1).replace(".", ",");
  // Número grande perde as casas: uma coordenada UTM de 7.500.000,37 promete
  // uma precisão que nenhum dado do card tem.
  const c = abs >= 1000 ? 0 : abs >= 100 ? 1 : casas;
  return v.toLocaleString("pt-BR", { maximumFractionDigits: c });
}

function TabelaNucleo(props) {
  const base = getRenderer("trama/table");
  return h(base.views[0].component, props);
}

// ---- spatial/points ----------------------------------------------------------

// Quatro classes, da mais baixa à mais alta. O raio cresce com a classe E a
// cor também (opacidade do destaque do editor): quem não distingue o tom lê
// pelo tamanho, e quem não distingue o tamanho lê pelo tom. Raio em fração da
// maior extensão, porque o postplot vive em coordenadas do mundo.
const RAIO = [0.007, 0.0115, 0.017, 0.024];
const OPAC = [0.18, 0.42, 0.7, 1];

// Classe por quartil, fechada à direita como o `cut()` do R faz no gráfico do
// editor: z <= Q1 é a classe 1. Valores empatados no quartil ficam na classe de
// baixo, igual nos dois lugares.
function classe(z, q) {
  if (z <= q[1]) return 0;
  if (z <= q[2]) return 1;
  if (z <= q[3]) return 2;
  return 3;
}

function extensao(pontos, borda) {
  let x0 = Infinity, x1 = -Infinity, y0 = Infinity, y1 = -Infinity;
  for (const p of pontos.concat(borda || [])) {
    if (p.x < x0) x0 = p.x;
    if (p.x > x1) x1 = p.x;
    if (p.y < y0) y0 = p.y;
    if (p.y > y1) y1 = p.y;
  }
  return { x0, x1, y0, y1 };
}

// O postplot. O `viewBox` sai da extensão VERDADEIRA em x e em y, e o
// `preserveAspectRatio` mantém a razão: um postplot esticado mente sobre a
// forma da área. O eixo y do SVG cresce para baixo, então y vira `y1 - y`.
function Postplot({ pontos, borda, q }) {
  const e = extensao(pontos, borda);
  const w = e.x1 - e.x0 || 1, hh = e.y1 - e.y0 || 1;
  const lado = Math.max(w, hh);
  const pad = lado * 0.04;
  const vb = `${e.x0 - pad} ${-pad} ${w + 2 * pad} ${hh + 2 * pad}`;
  const Y = (y) => e.y1 - y;
  const caminho = borda && borda.length
    ? "M" + borda.map((p) => `${p.x} ${Y(p.y)}`).join("L") + "Z"
    : null;
  // Os maiores primeiro: o ponto grande não esconde o pequeno por cima dele.
  const ordem = pontos.map((p, i) => [classe(p.z, q), i]).sort((a, b) => b[0] - a[0]);
  return h("svg", {
    className: "tr-sp-svg", viewBox: vb, preserveAspectRatio: "xMidYMid meet",
    role: "img", "aria-label": "postplot dos pontos",
  }, [
    caminho ? h("path", { key: "b", className: "tr-sp-borda", d: caminho, vectorEffect: "non-scaling-stroke" }) : null,
    ...ordem.map(([k, i]) => h("circle", {
      key: i, className: "tr-sp-pt", cx: pontos[i].x, cy: Y(pontos[i].y), r: RAIO[k] * lado,
      style: { fillOpacity: OPAC[k] }, vectorEffect: "non-scaling-stroke",
    })),
  ]);
}

// Os quartis como legenda do postplot: o mesmo símbolo do mapa, com a faixa de
// valores que ele representa.
function Quartis({ q }) {
  const rot = (k) => (k === 0 ? `≤ ${num(q[1])}` : k === 3 ? `> ${num(q[3])}` : `${num(q[k])} a ${num(q[k + 1])}`);
  return h("div", { className: "tr-sp-quartis" }, [0, 1, 2, 3].map((k) =>
    h("span", { key: k, className: "tr-sp-q", title: `classe ${k + 1}` }, [
      h("svg", { key: "s", className: "tr-sp-q-sim", viewBox: "-6 -6 12 12" },
        h("circle", { className: "tr-sp-pt", r: 1.6 + 1.3 * k, style: { fillOpacity: OPAC[k] } })),
      h("span", { key: "t" }, rot(k)),
    ])));
}

// Identificador não é grandeza: o código do IBGE 2800100 não é "2.800.100".
// Cobre as grafias em português e em inglês: id, cod, codigo, code, cd, e o
// mesmo como sufixo (municipio_id, geocodigo).
const ID_COL = /^(id|cod|codigo|code|cd)(_|\d|$)|(_id|_cod|_codigo|_code|_cd)$|^geocod(igo)?$|^codmun/i;

// Coordenada não leva separador de milhar: "570.298" lê-se como decimal, e este
// é o card cujo assunto é a distância. Mas as casas ficam: zero casas só de
// 1000 para cima (UTM em metros); em quilômetros ou graus, 12,5 continua 12,5.
function coordenada(v) {
  const abs = Math.abs(v);
  return v.toLocaleString("pt-BR", { useGrouping: false, maximumFractionDigits: abs >= 1000 ? 0 : 6 });
}

function celula(v, c, coord) {
  if (ID_COL.test(c)) return v == null ? "—" : String(v);
  if (coord.has(c) && typeof v === "number") return coordenada(v);
  return num(v);
}

// A coluna da variável vai primeiro: é a que o card existe para mostrar, e num
// card estreito a sexta coluna fica fora da vista.
function MiniTabela({ columns, rows, variavel, coordCols }) {
  if (!columns || !columns.length) return null;
  const cols = [variavel, ...columns.filter((c) => c !== variavel)].filter((c) => columns.includes(c)).slice(0, 6);
  const lin = (rows || []).slice(0, 4);
  const coord = new Set(coordCols || []);
  return h("div", { className: "tr-sp-tab", title: `${columns.length} colunas` },
    h("table", null, [
      h("thead", { key: "h" }, h("tr", null, cols.map((c, i) => h("th", { key: i }, c)))),
      h("tbody", { key: "b" }, lin.map((r, i) =>
        h("tr", { key: i }, cols.map((c, j) => h("td", { key: j }, celula(r[c], c, coord)))))),
    ]));
}

function Pontos({ artifact }) {
  const d = artifact.data || {};
  const q = d.quartis || [];
  const tem = (d.pontos || []).length && q.length === 5;
  return h("div", { className: "tr-sp" }, [
    // A pergunta do card é "o que há aqui?": a variável e a unidade vêm antes
    // de qualquer desenho.
    h("div", { key: "t", className: "tr-sp-topo" }, [
      h("span", { key: "v", className: "tr-sp-var", title: d.rotulo || d.variavel }, d.variavel),
      h("span", { key: "n", className: "tr-sp-n" }, `n = ${num(d.n)}`),
    ]),
    // Números de verdade a qualquer altura: o intervalo e a mediana da variável.
    // As linhas da tabela só cabem no card esticado.
    tem ? h("div", { key: "r", className: "tr-sp-resumo", title: "mínimo a máximo · mediana" },
      `${num(q[0])} a ${num(q[4])} · mediana ${num(q[2])}`) : null,
    h("div", { key: "u", className: "tr-sp-rot" },
      // `unidade` é a da DISTÂNCIA (a das coordenadas), não a da variável: o
      // objeto não guarda unidade da variável, e dizer "em m" ao lado do nome
      // dela seria afirmar o que ninguém declarou.
      [d.unidade ? `distâncias em ${d.unidade}` : "unidade da distância não declarada",
       d.crs ? `EPSG:${String(d.crs).replace(/^EPSG:/i, "")}` : null,
       d.tem_borda ? "com borda" : "sem borda"].filter(Boolean).join(" · ")),
    tem ? h(Postplot, { key: "p", pontos: d.pontos, borda: d.borda, q }) : h("div", { key: "p", className: "tr-empty" }, "sem pontos"),
    tem ? h(Quartis, { key: "q", q }) : null,
    h(MiniTabela, { key: "m", columns: d.columns, rows: d.rows, variavel: d.variavel, coordCols: d.coord_cols }),
    d.nota ? h("div", { key: "no", className: "tr-sp-nota", title: d.nota }, d.nota) : null,
  ]);
}

registerRenderer("spatial/points", {
  views: [
    { id: "postplot", label: "postplot", component: Pontos },
    { id: "tabela", label: "tabela", component: TabelaNucleo },
  ],
});

// ---- spatial/variogram -------------------------------------------------------

const VW = 300, VH = 180, MG = 10;
const R_MAX = 9;

function direcaoTexto(d) {
  return d.direcao == null ? "omnidirecional" : `${num(d.direcao, 0)}° ± ${num(d.tolerancia, 1)}°`;
}

function GraficoVariograma({ classes }) {
  const u1 = Math.max(...classes.map((c) => c.u)) || 1;
  const g1 = Math.max(...classes.map((c) => c.gamma)) || 1;
  const npMax = Math.max(...classes.map((c) => c.np)) || 1;
  // A semivariância parte de ZERO: cortar o eixo no menor valor exageraria a
  // subida e venderia dependência onde ela é pequena.
  const X = (u) => MG + (u / u1) * (VW - 2 * MG - R_MAX);
  const Y = (g) => VH - MG - (g / (g1 * 1.05)) * (VH - 2 * MG);
  const linha = "M" + classes.map((c) => `${X(c.u)} ${Y(c.gamma)}`).join("L");
  return h("svg", {
    className: "tr-sp-svg tr-sp-vario", viewBox: `0 0 ${VW} ${VH}`, preserveAspectRatio: "xMidYMid meet",
    role: "img", "aria-label": "variograma empírico",
  }, [
    h("line", { key: "x", className: "tr-sp-eixo", x1: MG, x2: VW - MG, y1: VH - MG, y2: VH - MG, vectorEffect: "non-scaling-stroke" }),
    h("line", { key: "y", className: "tr-sp-eixo", x1: MG, x2: MG, y1: MG, y2: VH - MG, vectorEffect: "non-scaling-stroke" }),
    h("path", { key: "l", className: "tr-sp-liga", d: linha, vectorEffect: "non-scaling-stroke" }),
    ...classes.map((c, i) => {
      const f = Math.sqrt(c.np / npMax);  // raio ∝ √pares, logo área ∝ pares
      return h("circle", {
        key: i, className: "tr-sp-pt", cx: X(c.u), cy: Y(c.gamma), r: Math.max(1.6, R_MAX * f),
        style: { fillOpacity: 0.2 + 0.75 * f }, vectorEffect: "non-scaling-stroke",
      }, h("title", null, `distância ${num(c.u)} · γ ${num(c.gamma)} · ${num(c.np)} pares`));
    }),
  ]);
}

function Variograma({ artifact }) {
  const d = artifact.data || {};
  const classes = (d.classes || []).filter((c) => c.u != null && c.gamma != null);
  if (!classes.length) return h("div", { className: "tr-empty" }, "sem classes");
  const nps = classes.map((c) => c.np);
  const g1 = Math.max(...classes.map((c) => c.gamma));
  const u1 = Math.max(...classes.map((c) => c.u));
  return h("div", { className: "tr-sp" }, [
    h("div", { key: "t", className: "tr-sp-topo" }, [
      h("span", { key: "v", className: "tr-sp-var", title: d.variavel }, d.variavel),
      h("span", { key: "n", className: "tr-sp-n" }, `${classes.length} classes`),
    ]),
    h(GraficoVariograma, { key: "g", classes }),
    // As pontas dos eixos em HTML, não em texto do SVG: num card estreito o
    // SVG encolhe e o texto dentro dele ficaria ilegível; aqui o corpo é fixo.
    h("div", { key: "e", className: "tr-sp-pontas" }, [
      h("span", { key: "a" }, "distância 0"),
      h("span", { key: "b" }, `até ${num(u1)}${d.unidade ? ` ${d.unidade}` : ""}`),
    ]),
    h("div", { key: "g1", className: "tr-sp-rot" },
      // Sem unidade em γ: a `unidade` do variograma é a da distância, e a de γ
      // seria a da variável ao quadrado, que o objeto não conhece.
      `γ de 0 a ${num(g1)} · área ∝ pares (${num(Math.min(...nps))} a ${num(Math.max(...nps))})`),
    h("div", { key: "f", className: "tr-sp-rodape" }, [
      h("span", { key: "1" }, `estimador ${d.estimador}`),
      h("span", { key: "2" }, `tendência ${d.tendencia}`),
      h("span", { key: "3" }, direcaoTexto(d)),
    ]),
    d.nota ? h("div", { key: "no", className: "tr-sp-nota", title: d.nota }, d.nota) : null,
  ]);
}

registerRenderer("spatial/variogram", {
  views: [
    { id: "variograma", label: "variograma", component: Variograma },
    { id: "tabela", label: "tabela", component: TabelaNucleo },
  ],
});
