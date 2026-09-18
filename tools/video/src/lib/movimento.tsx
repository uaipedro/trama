// src/lib/movimento.tsx — as primitivas de animação. Nada neste projeto
// interpola sem curva, e nada entra mexendo uma só propriedade: as duas regras
// moram aqui pra não precisarem ser lembradas em cada cena.
import React from "react";
import { interpolate, spring, useCurrentFrame, useVideoConfig } from "remotion";
import { tema } from "../theme";

type Mola = { damping: number; stiffness: number; mass: number };

// Entrada padrão: opacidade + subida + escala, as três na mesma mola. Um fade
// sozinho é a marca do movimento genérico — ele não diz de onde a coisa veio.
export const Entrada: React.FC<{
  atraso?: number;
  mola?: Mola;
  sobe?: number;
  de?: number;
  estilo?: React.CSSProperties;
  children: React.ReactNode;
}> = ({ atraso = 0, mola = tema.mola.suave, sobe = 40, de = 0.94, estilo, children }) => {
  const quadro = useCurrentFrame();
  const { fps } = useVideoConfig();
  const p = spring({ frame: quadro - atraso, fps, config: mola });
  return (
    <div
      style={{
        opacity: p,
        transform:
          `translateY(${interpolate(p, [0, 1], [sobe, 0])}px)` +
          ` scale(${interpolate(p, [0, 1], [de, 1])})`,
        ...estilo,
      }}
    >
      {children}
    </div>
  );
};

// Revelação palavra por palavra. `porPalavra` em 3 quadros: menos que isso lê
// como bloco, mais que isso arrasta a frase.
export const Palavras: React.FC<{
  texto: string;
  atraso?: number;
  porPalavra?: number;
  destacar?: string[];
  estilo?: React.CSSProperties;
}> = ({ texto, atraso = 0, porPalavra = 3, destacar = [], estilo }) => {
  const quadro = useCurrentFrame();
  const { fps } = useVideoConfig();
  return (
    <div
      style={{
        display: "flex",
        flexWrap: "wrap",
        // Gap em PIXEL, não em em: `em` resolve contra a fonte do PAI (16px de
        // praxe), e ao lado de tipo de 90px isso vira espaço nenhum entre as
        // palavras. Foi a falha que mais custou em texto grande.
        columnGap: 16,
        rowGap: 6,
        ...estilo,
      }}
    >
      {texto.split(" ").map((palavra, i) => {
        const p = spring({
          frame: quadro - atraso - i * porPalavra,
          fps,
          config: tema.mola.seca,
        });
        const herói = destacar.includes(palavra.replace(/[.,:]/g, ""));
        return (
          <span
            key={i}
            style={{
              display: "inline-block",
              opacity: p,
              transform: `translateY(${interpolate(p, [0, 1], [34, 0])}px)`,
              color: herói ? tema.cor.destaque : undefined,
              textShadow: herói ? `0 0 42px ${tema.cor.brilho}` : undefined,
            }}
          >
            {palavra}
          </span>
        );
      })}
    </div>
  );
};

// O envelope da cena inteira: aparece em `entra` quadros, sai em `sai`.
//
// A ENTRADA existe por causa de um defeito que só aparece amostrando quadros
// em intervalos regulares: com as cenas encostadas uma na outra, a que sai já
// estava em opacidade zero e a que entra ainda não tinha começado, e o corte
// virava um piscar de tela vazia. Com as cenas SOBREPOSTAS (ver
// `SOBREPOSICAO` em `Video.tsx`) e esta entrada, a saída de uma acontece
// enquanto a outra chega, e o corte é uma dissolução de verdade.
//
// A saída é mais curta que a entrada dos elementos (~10 quadros contra ~20):
// entrada e saída no mesmo tempo dão a sensação de vídeo que não sabe pra onde
// vai.
export function estiloDeCena(
  quadro: number,
  duracao: number,
  { entra = 8, sai = 10 }: { entra?: number; sai?: number } = {},
) {
  const comum = { extrapolateLeft: "clamp", extrapolateRight: "clamp" } as const;
  const inicio = duracao - sai;
  const chegando = interpolate(quadro, [0, entra], [0, 1], {
    ...comum,
    easing: tema.ease.saida,
  });
  const saindo = interpolate(quadro, [inicio, duracao - 2], [1, 0], {
    ...comum,
    easing: tema.ease.entrada,
  });
  return {
    opacity: chegando * saindo,
    transform: `translateY(${interpolate(quadro, [inicio, duracao - 2], [0, -38], {
      ...comum,
      easing: tema.ease.entrada,
    })}px) scale(${interpolate(quadro, [inicio, duracao - 2], [1, 0.97], {
      ...comum,
      easing: tema.ease.entrada,
    })})`,
  };
}

// Respiro: o que fica em tela mais de dois segundos não pode ficar PARADO, ou a
// cena lê como imagem estática com legenda em cima.
export function respiro(quadro: number, fase = 0) {
  return {
    escala: 1 + Math.sin(quadro / 34 + fase) * 0.006,
    flutua: Math.sin(quadro / 41 + fase) * 2.4,
  };
}

// Deslocamento com curva, com os dois `clamp` que sempre faltam — sem eles o
// elemento aparece antes da hora e continua andando depois do fim.
export function mover(
  quadro: number,
  [q0, q1]: [number, number],
  [v0, v1]: [number, number],
  easing = tema.ease.saida,
) {
  return interpolate(quadro, [q0, q1], [v0, v1], {
    easing,
    extrapolateLeft: "clamp",
    extrapolateRight: "clamp",
  });
}
