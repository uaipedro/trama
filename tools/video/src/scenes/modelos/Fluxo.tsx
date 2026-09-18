// src/scenes/modelos/Fluxo.tsx — o corpo do vídeo de modelos.
//
// Mesma divisão de responsabilidade do outro vídeo: aqui só o ENQUADRAMENTO e
// as legendas; quem entra quando está em `catalogo-modelos.ts`, e como cada
// card se comporta está em `NoCard.tsx`.
//
// Esta cena tem duas marcas de câmera que os outros vídeos não têm: depois de
// montar o fluxo, ela VOLTA e fecha em cima de dois cards — o quadro da ANOVA e
// o gráfico das médias. É o que diferencia uma demo de ferramenta de uma
// explicação: montar o grafo mostra o produto, fechar no p-valor e nas letras
// mostra o resultado que alguém foi ali buscar.
import React from "react";
import { AbsoluteFill, useCurrentFrame, useVideoConfig } from "remotion";
import { tema } from "../../theme";
import { estiloDeCena, mover, respiro } from "../../lib/movimento";
import { Legenda } from "../../lib/Legenda";
import { Canvas, type Camera } from "../../trama/Canvas";
import { ARESTAS, NOS } from "../../trama/catalogo-modelos";

const DESLOCA = 26;

const MARCAS: { q: number; cam: Camera }[] = [
  { q: 0, cam: { cx: 120, cy: 163, s: 2.1 } }, // os dados
  { q: 56, cam: { cx: 440, cy: 170, s: 1.85 } }, // o modelo ajustado
  { q: 146, cam: { cx: 835, cy: -17, s: 1.5 } }, // o quadro da ANOVA
  { q: 246, cam: { cx: 850, cy: 440, s: 1.25 } }, // as médias
  { q: 356, cam: { cx: 1350, cy: 143, s: 1.35 } }, // as comparações
  // A volta: fecha no quadro e depois nas letras.
  { q: 450, cam: { cx: 835, cy: -17, s: 2.3 } },
  // O fechado nas letras enquadra o CARD INTEIRO, não só o gráfico: centrado no
  // meio do card (439) e em 1,6, os 479px dele caem em 766 e sobram 157 de
  // respiro em cima e embaixo — o bastante para a legenda pousar sem cobrir os
  // parâmetros. Em 1,9 e centrado no gráfico, o card saía cortado no meio de uma
  // linha de param, que lê como render inacabado.
  { q: 545, cam: { cx: 850, cy: 439, s: 1.6 } },
  // E o grafo inteiro, com o centro deslocado para baixo do centro geométrico
  // para sobrar faixa livre no rodapé.
  { q: 645, cam: { cx: 775, cy: 300, s: 0.88 } },
];

function cameraEm(f: number): Camera {
  if (f <= MARCAS[0].q) return MARCAS[0].cam;
  for (let i = 1; i < MARCAS.length; i++) {
    const anterior = MARCAS[i - 1];
    const atual = MARCAS[i];
    if (f >= atual.q) continue;
    const inicio = Math.max(anterior.q, atual.q - DESLOCA);
    if (f <= inicio) return anterior.cam;
    const t = mover(f, [inicio, atual.q], [0, 1], tema.ease.ambos);
    return {
      cx: anterior.cam.cx + (atual.cam.cx - anterior.cam.cx) * t,
      cy: anterior.cam.cy + (atual.cam.cy - anterior.cam.cy) * t,
      s: anterior.cam.s + (atual.cam.s - anterior.cam.s) * t,
    };
  }
  return MARCAS[MARCAS.length - 1].cam;
}

export const FluxoModelos: React.FC = () => {
  const quadro = useCurrentFrame();
  const { durationInFrames, width, height } = useVideoConfig();
  const camera = cameraEm(quadro);
  const r = respiro(quadro);

  return (
    <AbsoluteFill style={estiloDeCena(quadro, durationInFrames)}>
      <Canvas
        camera={{ cx: camera.cx + r.flutua, cy: camera.cy - r.flutua, s: camera.s * r.escala }}
        largura={width}
        altura={height}
        quadro={quadro}
        nos={NOS}
        arestas={ARESTAS}
      />
      {/* As três legendas dizem o que a ferramenta NÃO diz sozinha, e as duas
          últimas saem da ajuda dos próprios blocos: o papel do bloco e a regra
          das letras. */}
      <Legenda
        texto="o bloco tira do resíduo a variação do terreno"
        de={124}
        ate={216}
        destacar={["bloco"]}
      />
      <Legenda
        texto="a régua mostra onde o p-valor caiu"
        de={458}
        ate={548}
        destacar={["régua"]}
      />
      <Legenda
        texto="médias com a mesma letra não diferem"
        de={562}
        ate={648}
        destacar={["letra"]}
        // 62, e não os 96 de praxe: neste plano o card das médias desce até
        // 924px, e a pílula na altura padrão encostava nos 28px finais dele.
        base={62}
      />
      <Legenda
        texto="um modelo, lido de três maneiras"
        de={656}
        ate={durationInFrames - 8}
        destacar={["três"]}
        base={38}
      />
    </AbsoluteFill>
  );
};
