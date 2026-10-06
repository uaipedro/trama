// inst/www/percurso.js — andar pelos blocos do grafo (setas) e o Desenrolar.
//
// Puro, sem React: o `editor.js` só desenha. `nos` é `[{id, x, y}]` (só blocos
// de verdade, sem frame nem nota) e `arestas` é `[{source, target}]`. Aresta
// com ponta fora de `nos` é ignorada. A navegação segue a ESTRUTURA do grafo,
// não a geometria: → filho, ← pai, ↑/↓ irmãos (blocos com o mesmo pai).

const porId = (a, b) => (a.id < b.id ? -1 : a.id > b.id ? 1 : 0);

// Ordem de leitura de um grafo que corre da esquerda para a direita: por
// COLUNA primeiro (o mais perto vem antes), e dentro da coluna de cima para
// baixo. Blocos cujo x difere menos que `FOLGA_COLUNA` são a mesma coluna
// (um arrasto de poucos pixels não troca a ordem). Só y faria o filho distante
// que está um pouco mais alto passar à frente do vizinho logo à direita.
const FOLGA_COLUNA = 120;
function ordenar(lista) {
  const porX = [...lista].sort((a, b) => a.x - b.x || porId(a, b));
  const coluna = new Map();
  let c = 0;
  porX.forEach((n, i) => {
    if (i > 0 && n.x - porX[i - 1].x > FOLGA_COLUNA) c++;
    coluna.set(n.id, c);
  });
  return [...lista].sort((a, b) =>
    coluna.get(a.id) - coluna.get(b.id) || a.y - b.y || a.x - b.x || porId(a, b));
}

function estrutura(nos, arestas) {
  const por = new Map(nos.map((n) => [n.id, n]));
  const filhos = new Map(nos.map((n) => [n.id, []]));
  const pais = new Map(nos.map((n) => [n.id, []]));
  const visto = new Set();
  for (const a of arestas) {
    if (!por.has(a.source) || !por.has(a.target) || a.source === a.target) continue;
    const k = a.source + "\n" + a.target;
    if (visto.has(k)) continue;
    visto.add(k);
    filhos.get(a.source).push(por.get(a.target));
    pais.get(a.target).push(por.get(a.source));
  }
  for (const m of [filhos, pais]) for (const [k, l] of m) m.set(k, ordenar(l));
  return { por, filhos, pais };
}

// Blocos sem pai, de cima para baixo.
export function fontes(nos, arestas) {
  const { pais } = estrutura(nos, arestas);
  return ordenar(nos.filter((n) => pais.get(n.id).length === 0)).map((n) => n.id);
}

// Bloco mais perto de `centro` (`{x, y}`, no espaço do fluxo); null se não há.
export function maisPerto(nos, centro) {
  let melhor = null, dist = Infinity;
  for (const n of nos) {
    const d = Math.hypot(n.x - centro.x, n.y - centro.y);
    if (d < dist) { dist = d; melhor = n.id; }
  }
  return melhor;
}

// Para onde vai a seta `tecla` ("right"|"left"|"up"|"down") a partir de `id`.
// `pai` é o pai por onde se chegou (se ainda for pai de `id`): ← volta a ele, e
// ↑/↓ andam entre os filhos dele. Devolve `{id, pai}` (o pai a lembrar do
// destino) ou null se a seta não leva a lugar nenhum. Sem pai de verdade, ↑/↓
// andam entre as fontes.
export function vizinho(nos, arestas, id, tecla, pai = null) {
  const { por, filhos, pais } = estrutura(nos, arestas);
  if (!por.has(id)) return null;
  const meusPais = pais.get(id);
  const ref = meusPais.find((p) => p.id === pai) || meusPais[0] || null;
  if (tecla === "right") {
    const f = filhos.get(id)[0];
    if (f) return { id: f.id, pai: id };
    // Folha: segue para o próximo irmão; sem irmão adiante, sobe até o
    // ancestral que tenha um, para a seta nunca parar no meio do fluxo.
    const raizes = ordenar(nos.filter((n) => pais.get(n.id).length === 0));
    let cur = id, p = pai;
    for (let guarda = nos.length; guarda > 0 && cur; guarda--) {
      const ps = pais.get(cur);
      const r = ps.find((x) => x.id === p) || ps[0] || null;
      const grupo = r ? filhos.get(r.id) : raizes;
      const prox = grupo[grupo.findIndex((n) => n.id === cur) + 1];
      if (prox) return { id: prox.id, pai: r ? r.id : null };
      cur = r ? r.id : null;
      p = cur ? pais.get(cur)[0]?.id ?? null : null;
    }
    return null;
  }
  if (tecla === "left") return ref ? { id: ref.id, pai: pais.get(ref.id)[0]?.id ?? null } : null;
  if (tecla === "up" || tecla === "down") {
    const grupo = ref ? filhos.get(ref.id)
      : ordenar(nos.filter((n) => pais.get(n.id).length === 0));
    const i = grupo.findIndex((n) => n.id === id) + (tecla === "down" ? 1 : -1);
    if (i < 0 || i >= grupo.length) return null;
    return { id: grupo[i].id, pai: ref ? ref.id : null };
  }
  return null;
}
