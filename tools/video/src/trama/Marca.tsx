// src/trama/Marca.tsx — a marca do trama, desenhada peça por peça.
//
// É o `inst/www/marca.svg` reescrito como componente em vez de carregado com
// `<Img>`: o hexágono, as ligações e os nós existem separados no arquivo
// original, e animá-los na ordem em que o desenho se explica — contorno, depois
// os fios, depois os nós cobrindo as pontas dos fios — vale muito mais que
// escalar um PNG. As coordenadas e as cores são as do arquivo, sem arredondar:
// a marca é assinatura e é a MESMA nos dois temas.
import React from "react";
import { interpolate, spring, useCurrentFrame, useVideoConfig } from "remotion";
import { tema } from "../theme";
import { mover } from "../lib/movimento";

const LIGACOES = [
  { d: "M34.5 75C34.5 95 86.5 92 86.5 111", cor: "#2f80ed" },
  { d: "M86.5 75 86.5 111", cor: "#27ae60" },
  { d: "M138.5 75C138.5 95 86.5 92 86.5 111", cor: "#9b51e0" },
  { d: "M86.5 137 86.5 153", cor: "#27ae60" },
];

const NOS = [
  { x: 21.5, y: 49, cor: "#2f80ed" },
  { x: 73.5, y: 49, cor: "#27ae60" },
  { x: 125.5, y: 49, cor: "#9b51e0" },
  { x: 73.5, y: 111, cor: "#27ae60" },
  { x: 73.5, y: 153, cor: "#9b51e0" },
];

export const Marca: React.FC<{
  altura: number;
  atraso?: number;
  brilho?: boolean;
  // `false` = a marca só ASSENTA, já inteira. O desenho peça por peça é um
  // número de abertura: repetido no fecho, ele gasta os 28 quadros em que a
  // marca ainda não é marca nenhuma — e num fecho de 98 quadros isso é um
  // segundo de quadro quase vazio, que foi exatamente o que a folha de contato
  // mostrou em 27,28s.
  desenhar?: boolean;
}> = ({ altura, atraso = 0, brilho = true, desenhar = true }) => {
  const quadro = useCurrentFrame();
  const { fps } = useVideoConfig();
  const f = quadro - atraso;

  // O contorno se desenha; o conjunto entra com mola elástica. As duas coisas
  // ao mesmo tempo, porque contorno desenhando sobre um grupo parado lê como
  // animação de carregamento, não como marca.
  const contorno = desenhar ? mover(f, [0, 26], [1, 0], tema.ease.saida) : 0;
  const entrada = spring({ frame: f, fps, config: tema.mola.elastica });
  const gira = interpolate(entrada, [0, 1], [-14, 0]);
  // Respiro: a marca fica em tela mais de dois segundos na abertura.
  const respira = 1 + Math.sin(f / 36) * 0.008;

  return (
    <svg
      viewBox="0 0 173 200"
      style={{
        height: altura,
        width: (altura * 173) / 200,
        overflow: "visible",
        transform:
          `scale(${interpolate(entrada, [0, 1], [0.72, 1]) * respira})` +
          ` rotate(${gira}deg)`,
        filter: brilho ? `drop-shadow(0 0 ${altura * 0.13}px ${tema.cor.brilho})` : undefined,
      }}
    >
      <path
        d="M86.5 3 170.5 51.5 170.5 148.5 86.5 197 2.5 148.5 2.5 51.5Z"
        fill="none"
        stroke="#6e7681"
        strokeWidth={6}
        strokeLinejoin="round"
        pathLength={1}
        strokeDasharray={1}
        strokeDashoffset={contorno}
      />
      <g strokeWidth={7} strokeLinecap="round" fill="none">
        {LIGACOES.map((l, i) => (
          <path
            key={l.d}
            d={l.d}
            stroke={l.cor}
            pathLength={1}
            strokeDasharray={1}
            // Os fios saem escalonados em 3 quadros, depois do contorno fechar:
            // o desenho conta a mesma história do editor — primeiro o quadro,
            // depois as ligações, por último os blocos.
            strokeDashoffset={desenhar ? mover(f, [16 + i * 3, 34 + i * 3], [1, 0]) : 0}
          />
        ))}
      </g>
      <g>
        {NOS.map((n, i) => {
          // Sem desenho, os nós entram junto com o conjunto (escalonados em 2)
          // em vez de esperar o contorno e os fios que já estão prontos.
          const p = spring({
            frame: f - (desenhar ? 28 + i * 3 : i * 2),
            fps,
            config: tema.mola.seca,
          });
          return (
            <rect
              key={`${n.x}-${n.y}`}
              x={n.x}
              y={n.y}
              width={26}
              height={26}
              rx={7}
              fill={n.cor}
              opacity={p}
              style={{
                transformOrigin: `${n.x + 13}px ${n.y + 13}px`,
                transform: `scale(${interpolate(p, [0, 1], [0.3, 1])})`,
              }}
            />
          );
        })}
      </g>
    </svg>
  );
};
