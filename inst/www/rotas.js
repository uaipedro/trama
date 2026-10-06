// inst/www/rotas.js — rota dos fios entre cards. Puro: sem React, sem xyflow.
//
// O fio sai da porta na horizontal, entra no destino na horizontal e, no
// meio, só anda em ângulo reto (H/V). O que o smoothstep do xyflow não faz, e
// este módulo faz: enxergar TODOS os cards (nenhum fio entra num card que não
// é a sua origem nem o seu destino) e separar em faixas paralelas os trechos
// de fios diferentes que cairiam um em cima do outro.
//
//   rotear({ obstaculos, frames, arestas }) -> Map(id -> [{x,y}, ...] | null)
//
// `obstaculos`: [{ id, x1, y1, x2, y2 }] (cards e notas, todos).
// `frames`:     [{ id, x1, y1, x2, y2 }] (só custo, nunca barreira).
// `arestas`:    [{ id, source, target, s: { x, y, pos }, t: { x, y, pos } }],
//               com `s`/`t` os pontos das portas e `pos` "left" | "right".
// `null` = sem rota viável: quem chama cai no smoothstep.

export const FOLGA = 10;        // distância mínima do fio ao card
export const SAIDA = 16;        // trecho reto que sai da porta antes de virar
export const PASSO_FAIXA = 6;   // distância entre fios paralelos
const FOLGA_FAIXA = 4;          // menor distância que uma faixa deslocada guarda do card
const TRECHO_MIN = 3;           // trecho que o deslocamento não pode encurtar além disso
const CURVA = 30;               // custo de uma dobra, em px de caminho
const JANELA = 240;             // quanto além da caixa origem-destino a busca olha
const FRAME_CABECALHO = 44;     // faixa do título do frame (geometria.js: FRAME_HEAD)
const EPS = 1e-6;

// ---------------------------------------------------------------- utilidades

function unicos(v) {
  v.sort((a, b) => a - b);
  const o = [];
  for (const x of v) if (!o.length || x - o[o.length - 1] > 0.25) o.push(x);
  return o;
}
// Primeiro índice com v[i] >= x - eps / último com v[i] <= x + eps.
function primeiroMaior(v, x) { let a = 0, b = v.length; while (a < b) { const m = (a + b) >> 1; if (v[m] > x) b = m; else a = m + 1; } return a; }
function primeiroMaiorIgual(v, x) { let a = 0, b = v.length; while (a < b) { const m = (a + b) >> 1; if (v[m] >= x) b = m; else a = m + 1; } return a; }
function indiceDe(v, x) {
  const i = primeiroMaiorIgual(v, x - 0.3);
  return i < v.length && Math.abs(v[i] - x) <= 0.3 ? i : -1;
}

// Heap mínimo por prioridade, desempate pela ordem de entrada (determinismo).
class Fila {
  constructor() { this.a = []; this.n = 0; }
  get tam() { return this.a.length; }
  push(f, idx) {
    const a = this.a, no = { f, s: this.n++, idx };
    let i = a.push(no) - 1;
    while (i > 0) {
      const p = (i - 1) >> 1;
      if (a[p].f < no.f || (a[p].f === no.f && a[p].s < no.s)) break;
      a[i] = a[p]; i = p;
    }
    a[i] = no;
  }
  pop() {
    const a = this.a, topo = a[0], fim = a.pop();
    if (a.length) {
      let i = 0;
      for (;;) {
        let c = 2 * i + 1;
        if (c >= a.length) break;
        if (c + 1 < a.length && (a[c + 1].f < a[c].f || (a[c + 1].f === a[c].f && a[c + 1].s < a[c].s))) c++;
        if (fim.f < a[c].f || (fim.f === a[c].f && fim.s < a[c].s)) break;
        a[i] = a[c]; i = c;
      }
      a[i] = fim;
    }
    return topo;
  }
}

// ------------------------------------------------------- rota de uma aresta

// Trecho reto que sai/entra pela porta. Cards de frente um pro outro e perto
// dividem o vão ao meio em vez de se atropelarem.
function saidas(a) {
  const { s, t } = a;
  const dir = (p) => (p.pos === "right" ? 1 : p.pos === "left" ? -1 : 0);
  const ds = dir(s), dt = dir(t);
  let ls = SAIDA, lt = SAIDA;
  if (ds && dt && ds === -dt) {
    const vao = (t.x - s.x) * ds;     // >0: de frente um pro outro, com vão
    if (vao >= 0 && vao < 2 * SAIDA) ls = lt = Math.max(vao / 2, 1);
  }
  return { sf: { x: s.x + ds * ls, y: s.y }, tf: { x: t.x + dt * lt, y: t.y }, ls, lt };
}

// Retângulo inflado de um card. Nos cards da própria aresta, o lado da porta
// só infla até o fim do trecho reto, pra o ponto de saída ficar na borda.
function inflar(r, m, ladoPorta, folgaPorta) {
  const o = { x1: r.x1 - m, y1: r.y1 - m, x2: r.x2 + m, y2: r.y2 + m };
  if (ladoPorta === "right") o.x2 = r.x2 + folgaPorta;
  if (ladoPorta === "left") o.x1 = r.x1 - folgaPorta;
  return o;
}

function buscar(a, obst, frames, m, limite) {
  const { sf, tf, ls, lt } = saidas(a);
  const caixa = {
    x1: Math.min(sf.x, tf.x) - (limite ? JANELA : 1e9), y1: Math.min(sf.y, tf.y) - (limite ? JANELA : 1e9),
    x2: Math.max(sf.x, tf.x) + (limite ? JANELA : 1e9), y2: Math.max(sf.y, tf.y) + (limite ? JANELA : 1e9),
  };
  const rs = [];
  for (const o of obst) {
    const proprio = o.id === a.source ? "s" : o.id === a.target ? "t" : null;
    const r = proprio === "s" ? inflar(o, m, a.s.pos, ls)
      : proprio === "t" ? inflar(o, m, a.t.pos, lt) : inflar(o, m, null, 0);
    if (r.x1 < caixa.x2 && r.x2 > caixa.x1 && r.y1 < caixa.y2 && r.y2 > caixa.y1) rs.push(r);
  }

  // Linhas candidatas: bordas inflaradas, as das saídas, e o meio de cada vão
  // livre (os corredores entre colunas/linhas de cards).
  const noJanela = (v, lo, hi) => v >= lo && v <= hi;
  const xs0 = [sf.x, tf.x], ys0 = [sf.y, tf.y];
  for (const r of rs) {
    if (noJanela(r.x1, caixa.x1, caixa.x2)) xs0.push(r.x1);
    if (noJanela(r.x2, caixa.x1, caixa.x2)) xs0.push(r.x2);
    if (noJanela(r.y1, caixa.y1, caixa.y2)) ys0.push(r.y1);
    if (noJanela(r.y2, caixa.y1, caixa.y2)) ys0.push(r.y2);
  }
  const base = (v) => unicos(v);
  const meios = (v) => {
    const mid = new Set();
    for (let i = 0; i + 1 < v.length; i++) if (v[i + 1] - v[i] >= 2 * FOLGA + 4) mid.add((v[i] + v[i + 1]) / 2);
    return mid;
  };
  const bx = base(xs0), by = base(ys0);
  const midX = meios(bx), midY = meios(by);
  const xs = unicos([...bx, ...midX]), ys = unicos([...by, ...midY]);
  const nx = xs.length, ny = ys.length;
  if (nx * ny > 250000) return null;

  const ix0 = indiceDe(xs, sf.x), iy0 = indiceDe(ys, sf.y);
  const ix1 = indiceDe(xs, tf.x), iy1 = indiceDe(ys, tf.y);
  if (ix0 < 0 || iy0 < 0 || ix1 < 0 || iy1 < 0) return null;

  // Segmentos bloqueados: o que passa pelo interior aberto de algum card.
  const hB = new Uint8Array(Math.max(nx - 1, 0) * ny);   // (i,j) -> (i+1,j)
  const vB = new Uint8Array(nx * Math.max(ny - 1, 0));   // (i,j) -> (i,j+1)
  for (const r of rs) {
    const iA = primeiroMaiorIgual(xs, r.x1 - 0.3), iB = primeiroMaior(xs, r.x2 + 0.3) - 1;   // xs em [x1,x2]
    const jA = primeiroMaiorIgual(ys, r.y1 - 0.3), jB = primeiroMaior(ys, r.y2 + 0.3) - 1;
    // linhas estritamente dentro
    const iIn0 = primeiroMaior(xs, r.x1 + 0.3), iIn1 = primeiroMaiorIgual(xs, r.x2 - 0.3) - 1;
    const jIn0 = primeiroMaior(ys, r.y1 + 0.3), jIn1 = primeiroMaiorIgual(ys, r.y2 - 0.3) - 1;
    // H: linha j estritamente dentro, trecho i..i+1 dentro de [x1,x2]
    for (let j = jIn0; j <= jIn1; j++) for (let i = Math.max(iA, 0); i < Math.min(iB, nx - 1); i++) hB[j * (nx - 1) + i] = 1;
    // V: coluna i estritamente dentro, trecho j..j+1 dentro de [y1,y2]
    for (let i = iIn0; i <= iIn1; i++) for (let j = Math.max(jA, 0); j < Math.min(jB, ny - 1); j++) vB[i * (ny - 1) + j] = 1;
  }
  // Janela cortando um card: o trecho de borda fora da janela não existe, mas
  // um card que engole a janela inteira também bloqueia; o caso é coberto pelos
  // índices clampados acima.

  // Custo extra de andar dentro de frame alheio (o que não contém origem nem destino).
  const alvoDentro = (f, p) => p.x >= f.x1 && p.x <= f.x2 && p.y >= f.y1 && p.y <= f.y2;
  const alheios = frames.filter((f) => !alvoDentro(f, a.s) && !alvoDentro(f, a.t));
  const custoFrame = (x, y, comp) => {
    let c = 0;
    for (const f of alheios) if (x > f.x1 && x < f.x2 && y > f.y1 && y < f.y2) c += (y < f.y1 + FRAME_CABECALHO ? 3 : 1) * comp;
    return c;
  };

  const N = nx * ny * 2;
  const g = new Float64Array(N).fill(Infinity);
  const pai = new Int32Array(N).fill(-1);
  const est = (i, j, d) => (j * nx + i) * 2 + d;
  const h = (i, j) => Math.abs(xs[i] - xs[ix1]) + Math.abs(ys[j] - ys[iy1]);
  const fila = new Fila();
  const ini = est(ix0, iy0, 0);
  g[ini] = 0;
  fila.push(h(ix0, iy0), ini);
  let melhor = Infinity, melhorEst = -1;
  while (fila.tam) {
    const { f, idx } = fila.pop();
    if (f >= melhor) break;
    const d = idx & 1, no = idx >> 1, i = no % nx, j = (no / nx) | 0;
    const gi = g[idx];
    if (gi + h(i, j) > f + 1e-9) continue;
    if (i === ix1 && j === iy1) {
      const total = gi + (d === 1 ? CURVA : 0);
      if (total < melhor) { melhor = total; melhorEst = idx; }
      continue;
    }
    // quatro vizinhos
    for (let k = 0; k < 4; k++) {
      let ni = i, nj = j, bloq, len, nd, cx, cy;
      if (k === 0) { if (i === 0) continue; ni = i - 1; bloq = hB[j * (nx - 1) + ni]; len = xs[i] - xs[ni]; nd = 0; }
      else if (k === 1) { if (i === nx - 1) continue; ni = i + 1; bloq = hB[j * (nx - 1) + i]; len = xs[ni] - xs[i]; nd = 0; }
      else if (k === 2) { if (j === 0) continue; nj = j - 1; bloq = vB[i * (ny - 1) + nj]; len = ys[j] - ys[nj]; nd = 1; }
      else { if (j === ny - 1) continue; nj = j + 1; bloq = vB[i * (ny - 1) + j]; len = ys[nj] - ys[j]; nd = 1; }
      if (bloq) continue;
      cx = (xs[i] + xs[ni]) / 2; cy = (ys[j] + ys[nj]) / 2;
      let c = len + (nd !== d ? CURVA : 0);
      // ligeiro favor aos corredores centrais (desempata sem criar desvio)
      const central = nd === 0 ? midY.has(ys[j]) : midX.has(xs[i]);
      if (!central) c += 0.02 * len;
      if (alheios.length) c += custoFrame(cx, cy, len);
      const ne = est(ni, nj, nd), ng = gi + c;
      if (ng < g[ne] - 1e-9) {
        g[ne] = ng; pai[ne] = idx;
        fila.push(ng + h(ni, nj), ne);
      }
    }
  }
  if (melhorEst < 0) return null;
  const pts = [];
  for (let e = melhorEst; e !== -1; e = pai[e]) {
    const no = e >> 1;
    pts.push({ x: xs[no % nx], y: ys[(no / nx) | 0] });
  }
  pts.reverse();
  return [{ x: a.s.x, y: a.s.y }, ...pts, { x: a.t.x, y: a.t.y }];
}

// Tira pontos repetidos e os do meio de trechos retos.
export function simplificar(p) {
  const o = [];
  for (const q of p) {
    const u = o[o.length - 1];
    if (u && Math.abs(u.x - q.x) < EPS && Math.abs(u.y - q.y) < EPS) continue;
    o.push({ x: q.x, y: q.y });
  }
  for (let i = o.length - 2; i >= 1; i--) {
    const a = o[i - 1], b = o[i], c = o[i + 1];
    const mesmoX = Math.abs(a.x - b.x) < EPS && Math.abs(b.x - c.x) < EPS;
    const mesmoY = Math.abs(a.y - b.y) < EPS && Math.abs(b.y - c.y) < EPS;
    if (mesmoX || mesmoY) o.splice(i, 1);
  }
  return o;
}

function rotaDeUma(a, obst, frames) {
  if ((a.s.pos !== "left" && a.s.pos !== "right") || (a.t.pos !== "left" && a.t.pos !== "right")) return { rota: null, completa: true };
  // Folga cheia primeiro; se encaixotado, folga mínima; só então desiste.
  for (const [m, limite] of [[FOLGA, true], [FOLGA, false], [3, false]]) {
    const r = buscar(a, obst, frames, m, limite);
    if (r) return { rota: simplificar(r), completa: !limite };
  }
  return { rota: null, completa: true };
}

// ------------------------------------------------------------- separação

// Limites por onde um trecho pode andar sem entrar em card nem inverter os
// trechos vizinhos.
function limites(p, k, vertical, obst) {
  const c = vertical ? p[k].x : p[k].y;
  const a = vertical ? Math.min(p[k].y, p[k + 1].y) : Math.min(p[k].x, p[k + 1].x);
  const b = vertical ? Math.max(p[k].y, p[k + 1].y) : Math.max(p[k].x, p[k + 1].x);
  let lo = -1e9, hi = 1e9;
  for (const o of obst) {
    const [o1, o2, s1, s2] = vertical ? [o.x1, o.x2, o.y1, o.y2] : [o.y1, o.y2, o.x1, o.x2];
    if (!(s1 < b && s2 > a)) continue;       // não passa pela extensão do trecho
    if (o2 <= c + EPS) lo = Math.max(lo, o2 + FOLGA_FAIXA);
    else if (o1 >= c - EPS) hi = Math.min(hi, o1 - FOLGA_FAIXA);
  }
  // vizinhos: o trecho anterior vai de p[k-1] a p[k], o seguinte de p[k+1] a p[k+2]
  const eixo = (q) => (vertical ? q.x : q.y);
  for (const [fixo, meu] of [[p[k - 1], p[k]], [p[k + 2], p[k + 1]]]) {
    const sinal = Math.sign(eixo(meu) - eixo(fixo)) || 1;
    if (sinal > 0) lo = Math.max(lo, eixo(fixo) + TRECHO_MIN); else hi = Math.min(hi, eixo(fixo) - TRECHO_MIN);
  }
  if (lo > c) lo = c;
  if (hi < c) hi = c;
  return { lo, hi, a, b, c };
}

function separar(rotas, obst) {
  const segs = [];
  for (const [id, p] of rotas) {
    if (!p) continue;
    for (let k = 1; k <= p.length - 3; k++) {      // sem o primeiro e o último (presos às portas)
      const vertical = Math.abs(p[k].x - p[k + 1].x) < EPS;
      segs.push({ id, p, k, vertical, c: vertical ? p[k].x : p[k].y,
                  a: vertical ? Math.min(p[k].y, p[k + 1].y) : Math.min(p[k].x, p[k + 1].x),
                  b: vertical ? Math.max(p[k].y, p[k + 1].y) : Math.max(p[k].x, p[k + 1].x) });
    }
  }
  for (const vertical of [true, false]) {
    const doEixo = segs.filter((s) => s.vertical === vertical);
    // O outro eixo pode ter deslocado as pontas destes trechos: remede antes de agrupar.
    for (const s of doEixo) {
      const u = s.p[s.k], v = s.p[s.k + 1];
      s.c = vertical ? u.x : u.y;
      s.a = vertical ? Math.min(u.y, v.y) : Math.min(u.x, v.x);
      s.b = vertical ? Math.max(u.y, v.y) : Math.max(u.x, v.x);
    }
    doEixo.sort((x, y) => x.c - y.c || x.a - y.a);
    // mesma linha (coordenada igual), depois encadeia os que se sobrepõem
    const grupos = [];
    for (let i = 0; i < doEixo.length;) {
      let j = i;
      while (j + 1 < doEixo.length && doEixo[j + 1].c - doEixo[i].c < 1.5) j++;
      const linha = doEixo.slice(i, j + 1).sort((x, y) => x.a - y.a);
      let atual = [linha[0]], fim = linha[0].b;
      for (let q = 1; q < linha.length; q++) {
        if (linha[q].a < fim - 1) { atual.push(linha[q]); fim = Math.max(fim, linha[q].b); }
        else { grupos.push(atual); atual = [linha[q]]; fim = linha[q].b; }
      }
      grupos.push(atual);
      i = j + 1;
    }
    for (const g of grupos) {
      if (g.length < 2) continue;
      const lim = g.map((s) => limites(s.p, s.k, vertical, obst));
      let lo = Math.max(...lim.map((l) => l.lo)), hi = Math.min(...lim.map((l) => l.hi));
      const c0 = g.reduce((t, s) => t + s.c, 0) / g.length;
      if (lo > hi) { const m = (lo + hi) / 2; lo = hi = m; }
      const n = g.length;
      let passo = PASSO_FAIXA;
      if (hi - lo < (n - 1) * passo) passo = (hi - lo) / (n - 1);
      if (passo < 2) continue;                     // sem espaço: deixa sobrepor
      const larg = (n - 1) * passo;
      const centro = Math.min(Math.max(c0, lo + larg / 2), hi - larg / 2);
      // Quem dobra pra menos (esquerda/cima) nas duas pontas fica na faixa de menos.
      const eixo = (q) => (vertical ? q.x : q.y);
      const chave = (s) => {
        const ini = Math.sign(eixo(s.p[s.k - 1]) - eixo(s.p[s.k])), fim = Math.sign(eixo(s.p[s.k + 2]) - eixo(s.p[s.k + 1]));
        return [ini + fim, ini, String(s.id)];
      };
      const ordem = g.map((s) => ({ s, c: chave(s) })).sort((x, y) =>
        x.c[0] - y.c[0] || x.c[1] - y.c[1] || (x.c[2] < y.c[2] ? -1 : x.c[2] > y.c[2] ? 1 : 0));
      ordem.forEach(({ s }, q) => {
        const novo = centro - larg / 2 + q * passo;
        if (vertical) { s.p[s.k].x = novo; s.p[s.k + 1].x = novo; }
        else { s.p[s.k].y = novo; s.p[s.k + 1].y = novo; }
      });
    }
  }
}

// ------------------------------------------------------------------ API

const arr = (r) => [Math.round(r.x1), Math.round(r.y1), Math.round(r.x2), Math.round(r.y2)].join(",");

// Roteador com memória: a rota de uma aresta só é refeita se mudou algo ao
// alcance dela (as portas ou um card na janela de busca). Arrastar um card
// refaz só as arestas que ele toca ou vizinhas.
export function criarRoteador() {
  let cache = new Map();
  return function rotear({ obstaculos, frames = [], arestas }) {
    const novo = new Map();
    const brutas = new Map();
    for (const a of arestas) {
      const x1 = Math.min(a.s.x, a.t.x) - JANELA - 2 * SAIDA, x2 = Math.max(a.s.x, a.t.x) + JANELA + 2 * SAIDA;
      const y1 = Math.min(a.s.y, a.t.y) - JANELA - 2 * SAIDA, y2 = Math.max(a.s.y, a.t.y) + JANELA + 2 * SAIDA;
          const perto = (r) => r.x1 < x2 && r.x2 > x1 && r.y1 < y2 && r.y2 > y1;
      const sig = [a.source, a.target, a.s.pos, a.t.pos, Math.round(a.s.x), Math.round(a.s.y), Math.round(a.t.x), Math.round(a.t.y),
        obstaculos.filter(perto).map((o) => o.id + ":" + arr(o)).join(";"),
        frames.filter(perto).map((f) => arr(f)).join(";")].join("|");
      // Rota achada sem janela depende de tudo no canvas: nunca é reaproveitada.
      const ant = cache.get(a.id);
      let res;
      if (ant && ant.sig === sig && !ant.completa) res = ant;
      else {
        const r = rotaDeUma(a, obstaculos, frames);
        res = { sig, rota: r.rota, completa: r.completa };
      }
      novo.set(a.id, res);
      brutas.set(a.id, res.rota ? res.rota.map((q) => ({ x: q.x, y: q.y })) : null);
    }
    cache = novo;
    separar(brutas, obstaculos);
    return brutas;
  };
}

export function rotear(entrada) { return criarRoteador()(entrada); }

// ------------------------------------------------- desenho da polilinha

// Cantos arredondados: cada vértice interno vira um arco curto (`Q`) em vez de bico.
export function caminhoComCantos(pontos, raio) {
  let d = `M${pontos[0].x},${pontos[0].y}`;
  for (let i = 1; i < pontos.length - 1; i++) {
    const p0 = pontos[i - 1], p1 = pontos[i], p2 = pontos[i + 1];
    const l1x = p0.x - p1.x, l1y = p0.y - p1.y, len1 = Math.hypot(l1x, l1y);
    const l2x = p2.x - p1.x, l2y = p2.y - p1.y, len2 = Math.hypot(l2x, l2y);
    const r = Math.min(raio, len1 / 2, len2 / 2);
    if (r <= 0 || len1 === 0 || len2 === 0) { d += `L${p1.x},${p1.y}`; continue; }
    const a = { x: p1.x + (l1x / len1) * r, y: p1.y + (l1y / len1) * r };
    const b = { x: p1.x + (l2x / len2) * r, y: p1.y + (l2y / len2) * r };
    d += `L${a.x},${a.y}Q${p1.x},${p1.y} ${b.x},${b.y}`;
  }
  const fim = pontos[pontos.length - 1];
  return d + `L${fim.x},${fim.y}`;
}

// Ponto a meio comprimento de uma poligonal: onde o "+" do meio fica.
export function meioDaLinha(pontos) {
  const seg = pontos.slice(1).map((p, i) => Math.hypot(p.x - pontos[i].x, p.y - pontos[i].y));
  let resta = seg.reduce((a, b) => a + b, 0) / 2;
  for (let i = 0; i < seg.length; i++) {
    if (resta <= seg[i] && seg[i] > 0) {
      const a = pontos[i], b = pontos[i + 1], t = resta / seg[i];
      return [a.x + (b.x - a.x) * t, a.y + (b.y - a.y) * t];
    }
    resta -= seg[i];
  }
  return [pontos[0].x, pontos[0].y];
}
