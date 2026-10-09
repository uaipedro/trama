// Fios dos canvas da home (hero e seções). Cada fio é um `<g data-fio="a>b"
// data-forma="h|v|desce">` com três paths (trilho, fio, pulso). Ponta vazia
// liga à borda do canvas: ">b" vem do meio do topo, "a>" sai pela base — é por
// aí que o fluxo continua de uma seção para a outra.

type Ponto = { x: number; y: number };

const ACAO = "cubic-bezier(.5,0,.3,1)";

// Medido pelo layout (offset*), não pela caixa na tela: a seção pode estar
// no meio do zoom de rolagem, e a câmera do hero é um transform.
function centro(porta: HTMLElement): Ponto {
  const card = porta.offsetParent as HTMLElement;
  return {
    x: card.offsetLeft + porta.offsetLeft + porta.offsetWidth / 2,
    y: card.offsetTop + porta.offsetTop + porta.offsetHeight / 2,
  };
}

// Degrau com cantos arredondados quando o destino está à frente (o fio do
// editor); curva em S quando fica atrás.
function horizontal(s: Ponto, t: Ponto, curva?: number): string {
  if (t.x - s.x >= 40) {
    const mx = curva != null ? s.x + curva : (s.x + t.x) / 2, dy = Math.sign(t.y - s.y);
    const r = Math.min(12, Math.abs(t.y - s.y) / 2, (t.x - s.x) / 2);
    if (r < 1) return `M${s.x},${s.y}L${t.x},${t.y}`;
    return `M${s.x},${s.y}L${mx - r},${s.y}Q${mx},${s.y} ${mx},${s.y + dy * r}L${mx},${t.y - dy * r}Q${mx},${t.y} ${mx + r},${t.y}L${t.x},${t.y}`;
  }
  const my = (s.y + t.y) / 2;
  return `M${s.x},${s.y}C${s.x + 60},${s.y} ${s.x + 60},${my} ${(s.x + t.x) / 2},${my}S${t.x - 60},${t.y} ${t.x},${t.y}`;
}

// Polilinha ortogonal com cantos arredondados (o desenho da TrAresta).
function poli(pts: Ponto[], raio = 14): string {
  let d = `M${pts[0].x},${pts[0].y}`;
  for (let i = 1; i < pts.length - 1; i++) {
    const a = pts[i - 1], p = pts[i], b = pts[i + 1];
    const l1 = Math.hypot(a.x - p.x, a.y - p.y), l2 = Math.hypot(b.x - p.x, b.y - p.y);
    const r = Math.min(raio, l1 / 2, l2 / 2);
    if (r < 1) { d += `L${p.x},${p.y}`; continue; }
    d += `L${p.x + ((a.x - p.x) / l1) * r},${p.y + ((a.y - p.y) / l1) * r}Q${p.x},${p.y} ${p.x + ((b.x - p.x) / l2) * r},${p.y + ((b.y - p.y) / l2) * r}`;
  }
  const f = pts[pts.length - 1];
  return d + `L${f.x},${f.y}`;
}

function vertical(s: Ponto, t: Ponto): string {
  const k = Math.max(30, Math.abs(t.y - s.y) / 2);
  return `M${s.x},${s.y}C${s.x},${s.y + k} ${t.x},${t.y - k} ${t.x},${t.y}`;
}

export function ligarFios(svg: SVGSVGElement, cards: Map<string, HTMLElement>, base: HTMLElement) {
  const gs = [...svg.querySelectorAll<SVGGElement>("[data-fio]")];
  for (const g of gs) {
    const [a, b] = g.dataset.fio!.split(">");
    const h = g.dataset.forma === "h" || g.dataset.forma === "entra";
    cards.get(a)?.classList.add(h ? "tem-out" : "tem-base");
    cards.get(b)?.classList.add(h ? "tem-in" : "tem-topo");
  }

  // As pontas vazias vão até a borda da seção, no meio da largura, para o fio
  // de uma seção encontrar o da outra.
  const desenhar = () => {
    const secao = base.closest("section") ?? base;
    let x0 = 0, y0 = 0;
    for (let el: HTMLElement | null = base; el && el !== secao; el = el.offsetParent as HTMLElement | null) { x0 += el.offsetLeft; y0 += el.offsetTop; }
    const meio = secao.clientWidth / 2 - x0;
    const topo = -y0, fundo = secao.clientHeight - y0;
    for (const g of gs) {
      const [a, b] = g.dataset.fio!.split(">");
      const ca = cards.get(a), cb = cards.get(b);
      if ((a && !ca) || (b && !cb)) continue;
      let d: string;
      if (g.dataset.forma === "entra") {
        // vem do meio do topo da seção, contorna pela esquerda (ao lado do
        // título) e entra no bloco pela porta de entrada
        const t = centro(cb!.querySelector(".hc-porta--in")!);
        const gx = -base.offsetLeft - 28, y1 = topo + 36;
        d = poli([{ x: meio, y: topo }, { x: meio, y: y1 }, { x: gx, y: y1 }, { x: gx, y: t.y }, { x: t.x - 2, y: t.y }]);
      } else if (g.dataset.forma === "h") {
        // `data-saida`: qual porta de saída (card com várias); `data-curva`:
        // onde o fio dobra, em px a partir da origem, para fios irmãos não
        // se sobreporem.
        const saidas = ca!.querySelectorAll<HTMLElement>(".hc-porta--out");
        const s = centro(saidas[Number(g.dataset.saida ?? 0)] ?? saidas[0]), t = centro(cb!.querySelector(".hc-porta--in")!);
        d = horizontal({ x: s.x + 5, y: s.y }, { x: t.x - 2, y: t.y }, g.dataset.curva ? Number(g.dataset.curva) : undefined);
      } else {
        const s = ca ? centro(ca.querySelector(".hc-porta--base")!) : null;
        const t = cb ? centro(cb.querySelector(".hc-porta--topo")!) : null;
        const ss = s ? { x: s.x, y: s.y + 6 } : { x: meio, y: topo };
        const tt = t ? { x: t.x, y: t.y - 4 } : { x: meio, y: fundo };
        d = vertical(ss, tt);
      }
      for (const p of g.children) p.setAttribute("d", d);
    }
  };

  const ligarTodos = () => gs.forEach((g) => { if (!("espera" in g.dataset)) g.classList.add("is-ligado"); });
  return { desenhar, ligarTodos };
}

/** Faz os blocos "rodarem" na ordem: cada um entra, fica âmbar e fica verde.
 *  Um fio aparece (desenhando-se da origem) só quando as duas pontas entraram. */
export function rodar(ids: string[], cards: Map<string, HTMLElement>, svg: SVGSVGElement, passo: number, atraso = 0) {
  const entrou = (id: string) => !id || cards.get(id)?.classList.contains("is-entrou");
  ids.forEach((id, i) => {
    setTimeout(() => {
      const c = cards.get(id);
      if (!c) return;
      c.classList.add("is-entrou", "is-rodando");
      for (const g of svg.querySelectorAll<SVGGElement>("[data-fio]")) {
        const [a, b] = g.dataset.fio!.split(">");
        if (a !== id && b !== id) continue;
        if ("espera" in g.dataset) continue; // fio que só aparece por pedido (ver `revelar`)
        if (!entrou(a) || !entrou(b)) continue;
        g.classList.add("is-ligado");
        const [, fio, pulso] = g.children as unknown as SVGPathElement[];
        const L = fio.getTotalLength();
        fio.animate([{ strokeDasharray: `${L}`, strokeDashoffset: L }, { strokeDasharray: `${L}`, strokeDashoffset: 0 }], { duration: 420, easing: ACAO });
        const cor = cards.get(b || a)!.style.getPropertyValue("--cat");
        pulso.style.setProperty("--cat", cor);
        pulso.animate([{ strokeDashoffset: 14, opacity: 1 }, { strokeDashoffset: -L, opacity: 1 }], { duration: 700, delay: 200, easing: ACAO });
      }
      setTimeout(() => { c.classList.remove("is-rodando"); c.classList.add("is-pronto"); }, 650);
    }, atraso + i * passo);
  });
}

/** Mostra um fio que estava esperando (`data-espera`), desenhando-o da origem. */
export function revelar(g: SVGGElement) {
  if (g.classList.contains("is-ligado")) return;
  delete g.dataset.espera;
  g.classList.add("is-ligado");
  const [, fio, pulso] = g.children as unknown as SVGPathElement[];
  const L = fio.getTotalLength();
  if (matchMedia("(prefers-reduced-motion: reduce)").matches) return;
  fio.animate([{ strokeDasharray: `${L}`, strokeDashoffset: L }, { strokeDasharray: `${L}`, strokeDashoffset: 0 }], { duration: 600, easing: ACAO });
  pulso.animate([{ strokeDashoffset: 14, opacity: 1 }, { strokeDashoffset: -L, opacity: 1 }], { duration: 900, delay: 250, easing: ACAO });
}
