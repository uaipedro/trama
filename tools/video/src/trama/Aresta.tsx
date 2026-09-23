// src/trama/Aresta.tsx — a ligação entre duas portas.
//
// A rota é a MESMA da `TrAresta` de `inst/www/editor.js`: ângulo reto com cantos
// de raio 12 (o smoothstep do xyflow), e — quando um card está empilhado sobre
// o outro — a volta por FORA dos dois retângulos, atravessando só o vão livre
// entre eles. As funções abaixo são transcrição direta das de lá; mudou lá,
// muda aqui.
import React from "react";
import { useCurrentFrame } from "remotion";
import { tema } from "../theme";
import { mover } from "../lib/movimento";
import { geometriaEm } from "./estado";
import type { NoFluxo } from "./tipos";

type Ponto = { x: number; y: number };
type Ret = { x1: number; y1: number; x2: number; y2: number };

const MARGEM_ARESTA = 16;
// `offset` padrão do `getSmoothStepPath` do xyflow.
const OFFSET = 20;

// Portas sempre Right (saída) → Left (entrada), como no editor.
function caminhoContornando(s: Ponto, t: Ponto, origem: Ret, destino: Ret): Ponto[] | null {
  let vaoTopo: number, vaoBase: number;
  if (destino.y1 - origem.y2 >= MARGEM_ARESTA) { vaoTopo = origem.y2; vaoBase = destino.y1; }
  else if (origem.y1 - destino.y2 >= MARGEM_ARESTA) { vaoTopo = destino.y2; vaoBase = origem.y1; }
  else return null;
  const meioY = (vaoTopo + vaoBase) / 2;
  const sx = s.x + MARGEM_ARESTA, tx = t.x - MARGEM_ARESTA;
  return [s, { x: sx, y: s.y }, { x: sx, y: meioY }, { x: tx, y: meioY }, { x: tx, y: t.y }, t];
}

// O smoothstep padrão, sem obstáculo: degrau no meio quando o destino está à
// frente; volta em Z quando está atrás.
function smoothstep(s: Ponto, t: Ponto): Ponto[] {
  if (t.x - s.x >= 2 * OFFSET) {
    const mx = (s.x + t.x) / 2;
    return [s, { x: mx, y: s.y }, { x: mx, y: t.y }, t];
  }
  const my = (s.y + t.y) / 2;
  return [
    s, { x: s.x + OFFSET, y: s.y }, { x: s.x + OFFSET, y: my },
    { x: t.x - OFFSET, y: my }, { x: t.x - OFFSET, y: t.y }, t,
  ];
}

function caminhoComCantos(pontos: Ponto[], raio: number): string {
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

// Ponto a uma fração `f` do comprimento da polilinha — pro pulso andar sem
// medir o `<path>` no DOM.
function aoLongo(pontos: Ponto[], f: number): Ponto {
  const seg = pontos.slice(1).map((p, i) => Math.hypot(p.x - pontos[i].x, p.y - pontos[i].y));
  let resta = f * seg.reduce((a, b) => a + b, 0);
  for (let i = 0; i < seg.length; i++) {
    if (resta <= seg[i] || i === seg.length - 1) {
      const k = seg[i] ? Math.min(1, resta / seg[i]) : 0;
      return {
        x: pontos[i].x + (pontos[i + 1].x - pontos[i].x) * k,
        y: pontos[i].y + (pontos[i + 1].y - pontos[i].y) * k,
      };
    }
    resta -= seg[i];
  }
  return pontos[pontos.length - 1];
}

export const Aresta: React.FC<{
  de: NoFluxo;
  para: NoFluxo;
  portaDestino: number;
  desenha: number;
}> = ({ de, para, portaDestino, desenha }) => {
  const quadro = useCurrentFrame();
  const gd = geometriaEm(de, quadro);
  const gp = geometriaEm(para, quadro);
  const s = { x: de.x + gd.w, y: de.y + gd.portas("out", 0) };
  const t = { x: para.x, y: para.y + gp.portas("in", portaDestino) };
  const ret = (n: NoFluxo, g: { w: number; h: number }) => ({ x1: n.x, y1: n.y, x2: n.x + g.w, y2: n.y + g.h });
  const pontos = caminhoContornando(s, t, ret(de, gd), ret(para, gp)) ?? smoothstep(s, t);
  const caminho = caminhoComCantos(pontos, 12);

  // `pathLength={1}` normaliza o comprimento: o traço se desenha com
  // dashoffset de 1 a 0 sem ninguém precisar medir a curva.
  const traco = mover(quadro, [desenha, desenha + 16], [1, 0]);
  if (quadro < desenha) return null;

  // O pulso é o dado ANDANDO: sai da porta de saída e chega na de entrada
  // logo depois do traço fechar — a ligação leva o resultado adiante.
  const tp = mover(quadro, [desenha + 10, desenha + 30], [0, 1], tema.ease.ambos);
  const pulso = aoLongo(pontos, tp);
  const pulsando = quadro >= desenha + 10 && quadro <= desenha + 32;

  return (
    <>
      <path
        className="react-flow__edge-path"
        d={caminho}
        fill="none"
        stroke={tema.cor.aresta}
        strokeWidth={2}
        pathLength={1}
        strokeDasharray={1}
        strokeDashoffset={traco}
      />
      {pulsando ? (
        <circle cx={pulso.x} cy={pulso.y} r={3.4} fill={tema.cor.destaque}
          opacity={Math.sin(tp * Math.PI)} />
      ) : null}
    </>
  );
};
