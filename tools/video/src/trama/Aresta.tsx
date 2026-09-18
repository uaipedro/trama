// src/trama/Aresta.tsx — a ligação entre duas portas.
//
// A curva é a MESMA do xyflow (`getBezierPath` com curvatura 0,25), porque é
// ela que dá ao grafo do trama o desenho que se reconhece: um S horizontal que
// sai reto da porta e chega reto na outra. Uma reta, ou uma bezier com controle
// arbitrário, deixaria o canvas parecido com qualquer editor de nós.
import React from "react";
import { useCurrentFrame } from "remotion";
import { tema } from "../theme";
import { larguraCard, yPorta } from "./metricas";
import type { NoFluxo } from "./tipos";
import { mover } from "../lib/movimento";

// Deslocamento do ponto de controle, transcrito do xyflow: com o destino à
// frente da origem o controle fica na metade do caminho; ATRÁS dela, cresce com
// a raiz da distância, que é o que faz a aresta que volta contornar em vez de
// dobrar sobre si mesma.
function deslocamento(distancia: number, curvatura = 0.25): number {
  if (distancia >= 0) return 0.5 * distancia;
  return curvatura * 25 * Math.sqrt(-distancia);
}

function pontos(de: NoFluxo, para: NoFluxo, portaDestino: number) {
  const sx = de.x + larguraCard(de.tamanho);
  const sy = de.y + yPorta(de.spec, 0, de.tamanho);
  const tx = para.x;
  const ty = para.y + yPorta(para.spec, portaDestino, para.tamanho);
  const d = deslocamento(tx - sx);
  return { sx, sy, tx, ty, c1x: sx + d, c2x: tx - d };
}

function cubica(t: number, a: number, b: number, c: number, d: number): number {
  const u = 1 - t;
  return u * u * u * a + 3 * u * u * t * b + 3 * u * t * t * c + t * t * t * d;
}

export const Aresta: React.FC<{
  de: NoFluxo;
  para: NoFluxo;
  portaDestino: number;
  desenha: number;
}> = ({ de, para, portaDestino, desenha }) => {
  const quadro = useCurrentFrame();
  const { sx, sy, tx, ty, c1x, c2x } = pontos(de, para, portaDestino);
  const caminho = `M ${sx},${sy} C ${c1x},${sy} ${c2x},${ty} ${tx},${ty}`;

  // `pathLength={1}` normaliza o comprimento: o traço se desenha com
  // dashoffset de 1 a 0 sem ninguém precisar medir a curva. Medir com
  // `getTotalLength` exigiria um ref e um quadro de atraso — e um quadro de
  // atraso numa aresta que nasce é uma aresta que pisca.
  const traco = mover(quadro, [desenha, desenha + 16], [1, 0]);
  if (quadro < desenha) return null;

  // O pulso é o dado ANDANDO: sai da porta de saída e chega na de entrada logo
  // depois do traço fechar. É o que diz, sem legenda, que a ligação não é
  // enfeite — ela leva o resultado adiante.
  const tp = mover(quadro, [desenha + 10, desenha + 30], [0, 1], tema.ease.ambos);
  const px = cubica(tp, sx, c1x, c2x, tx);
  const py = cubica(tp, sy, sy, ty, ty);
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
        <circle
          cx={px}
          cy={py}
          r={3.4}
          fill={tema.cor.destaque}
          opacity={Math.sin(tp * Math.PI)}
        />
      ) : null}
    </>
  );
};
