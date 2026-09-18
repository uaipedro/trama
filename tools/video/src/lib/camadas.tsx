// src/lib/camadas.tsx — a pilha de cinco camadas que toda cena empilha, de
// baixo pra cima: malha de fundo → conteúdo → grade de cor → grão → vinheta.
//
// Fundo chapado é a assinatura do vídeo genérico. Aqui o fundo é o MESMO
// `--tr-bg` do editor, com dois halos que se movem devagar: o canvas do trama é
// escuro e quase vazio, então o que dá profundidade tem que vir de trás.
import React from "react";
import { AbsoluteFill, useCurrentFrame } from "remotion";
import { tema } from "../theme";

export const Malha: React.FC = () => {
  const quadro = useCurrentFrame();
  // Senos de período longo e incomensurável: em 900 quadros o par nunca repete
  // a mesma posição, então o fundo não "batuca".
  const dx = Math.sin(quadro / 97) * 60;
  const dy = Math.cos(quadro / 131) * 44;
  return (
    <AbsoluteFill style={{ background: tema.cor.fundo }}>
      <div
        style={{
          position: "absolute",
          width: 1500,
          height: 1500,
          borderRadius: "50%",
          top: -560 + dy,
          left: -360 + dx,
          filter: "blur(60px)",
          background: `radial-gradient(circle, ${tema.cor.destaque}2b, transparent 63%)`,
        }}
      />
      <div
        style={{
          position: "absolute",
          width: 1150,
          height: 1150,
          borderRadius: "50%",
          bottom: -470 - dy,
          right: -300 - dx,
          filter: "blur(80px)",
          background: `radial-gradient(circle, ${tema.categoria.aggregate}22, transparent 66%)`,
        }}
      />
    </AbsoluteFill>
  );
};

// Pontinhos do canvas do xyflow, na cor `--tr-dots`. É o que faz o plano de
// fundo ser reconhecidamente o editor, e não uma tela preta qualquer.
export const Pontos: React.FC<{ escala?: number; dx?: number; dy?: number }> = ({
  escala = 1,
  dx = 0,
  dy = 0,
}) => (
  <AbsoluteFill
    style={{
      backgroundImage: `radial-gradient(${tema.cor.pontos} 1px, transparent 1px)`,
      backgroundSize: `${20 * escala}px ${20 * escala}px`,
      backgroundPosition: `${dx}px ${dy}px`,
      opacity: 0.22,
    }}
  />
);

// Unifica o que vem de fontes diferentes (cards desenhados, painéis de código,
// tipografia) num só banho de cor. Sem ela, cada bloco parece colado de um
// lugar diferente.
export const Grade: React.FC = () => (
  <AbsoluteFill style={{ pointerEvents: "none" }}>
    <AbsoluteFill
      style={{
        backgroundColor: tema.cor.destaque,
        mixBlendMode: "soft-light",
        opacity: 0.16,
      }}
    />
    <AbsoluteFill
      style={{
        background:
          "linear-gradient(180deg, rgba(0,0,0,0.16), transparent 26%," +
          " transparent 74%, rgba(0,0,0,0.24))",
      }}
    />
  </AbsoluteFill>
);

// Grão procedural: nenhum arquivo, e o deslocamento por quadro dá a cintilação
// de filme. Sem isso, um gradiente grande em tela escura mostra as faixas de
// banding do 8-bit.
export const Grao: React.FC = () => {
  const quadro = useCurrentFrame();
  const textura =
    `url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg'` +
    ` width='220' height='220'%3E%3Cfilter id='n'%3E%3CfeTurbulence` +
    ` type='fractalNoise' baseFrequency='0.9' numOctaves='2'/%3E%3C/filter%3E` +
    `%3Crect width='220' height='220' filter='url(%23n)' opacity='0.5'/%3E%3C/svg%3E")`;
  return (
    <AbsoluteFill
      style={{
        pointerEvents: "none",
        backgroundImage: textura,
        backgroundSize: "220px",
        // Primos diferentes nos dois eixos: a textura nunca volta pro mesmo
        // lugar, que é o que evita o grão parecer uma máscara parada.
        backgroundPosition: `${(quadro * 7) % 220}px ${(quadro * 13) % 220}px`,
        opacity: 0.055,
        mixBlendMode: "overlay",
      }}
    />
  );
};

export const Vinheta: React.FC = () => (
  <AbsoluteFill
    style={{
      pointerEvents: "none",
      background:
        "radial-gradient(ellipse at center, transparent 54%, rgba(0,0,0,0.30) 100%)",
    }}
  />
);

// As duas camadas de acabamento sempre juntas e sempre por último: separá-las
// é como a ordem se perde numa cena nova.
export const Acabamento: React.FC = () => (
  <>
    <Grade />
    <Grao />
    <Vinheta />
  </>
);
