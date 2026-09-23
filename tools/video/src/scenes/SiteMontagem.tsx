// src/scenes/SiteMontagem.tsx — o loop "montagem real" da home do site.
//
// Diferente de `Fluxo.tsx` (a cena homônima do `TramaDemo`), esta composição é
// o VÍDEO INTEIRO, não uma cena dentro de uma linha do tempo maior: sem corte
// de entrada/saída de cena, sem trilha — só o canvas e o acabamento. E precisa
// FECHAR EM LOOP, porque é assim que a tag `<video loop>` da home a usa.
//
// A câmera fica PARADA a cena inteira: os três cards cabem no quadro com folga
// (a margem de segurança evita que o `object-fit: cover` do CSS corte um
// canto), então não há por que passear por eles um de cada vez como o
// `TramaDemo` faz com o grafo de seis nós.
//
// O laço fecha desvanecendo o fluxo (nós + arestas, não o fundo) de volta ao
// canvas vazio: o primeiro quadro já É vazio (a mola de todo card vale 0 antes
// de `entra`), então o último quadro só precisa alcançar essa mesma opacidade
// para o corte de loop não piscar.
import React from "react";
import { AbsoluteFill, interpolate, useCurrentFrame, useVideoConfig } from "remotion";
import { tema } from "../theme";
import { Acabamento } from "../lib/camadas";
import { Canvas } from "../trama/Canvas";
import { ARESTAS_MONTAGEM, NOS_MONTAGEM } from "../trama/catalogo-site";

// O quase-preto do editor (`tema.cor.fundo`) briga com o papel claro da home
// (`--paper`, oklch(97% .008 200)). Este tom é o `--ink` do site (#16303d)
// levado pro oklch, escurecido e dessaturado — a MESMA família de cor da moldura
// `.flow-reel` (`#10262f`) que envolve o vídeo na página, não um tom novo.
const FUNDO = "oklch(24% .03 220)";
// Pontos mais claros que o fundo, mas discretos: a grade do canvas precisa se
// reconhecer sem competir com os cards.
const COR_PONTOS = "#6f96a1";

const CAMERA = { cx: 450, cy: 285, s: 1.6 };

// Início e fim do desvanecimento do fluxo. Sobra folga depois do último
// resultado (`agrupar.resulta = 220`) antes de começar a apagar — o card
// precisa ser VISTO com o resultado, não apagado em cima dele.
const FADE_DE = 300;
const FADE_ATE = 330;

export const DURACAO_TOTAL = 360;

// Respiro periódico: em vez do `respiro()` de `movimento.tsx` (que usa o
// quadro absoluto e por isso não fecha em loop), as fases aqui são múltiplos
// inteiros de uma volta completa em `DURACAO_TOTAL` — o que garante
// `respiro(0) === respiro(DURACAO_TOTAL)` exatamente, sem precisar medir nada.
function respiroDoLaco(quadro: number) {
  const t = (2 * Math.PI * quadro) / DURACAO_TOTAL;
  return { escala: 1 + Math.sin(t * 5) * 0.005, flutua: Math.sin(t * 7) * 1.6 };
}

export const SiteMontagem: React.FC = () => {
  const quadro = useCurrentFrame();
  const { durationInFrames, width, height } = useVideoConfig();
  if (durationInFrames !== DURACAO_TOTAL) {
    console.error(
      `[trama-video] SiteMontagem com ${durationInFrames} quadros, esperava ${DURACAO_TOTAL}.`,
    );
  }
  const r = respiroDoLaco(quadro);
  const opacidadeConteudo = interpolate(quadro, [FADE_DE, FADE_ATE], [1, 0], {
    easing: tema.ease.entrada,
    extrapolateLeft: "clamp",
    extrapolateRight: "clamp",
  });

  return (
    <AbsoluteFill>
      <Canvas
        camera={{ cx: CAMERA.cx + r.flutua, cy: CAMERA.cy - r.flutua, s: CAMERA.s * r.escala }}
        largura={width}
        altura={height}
        quadro={quadro}
        nos={NOS_MONTAGEM}
        arestas={ARESTAS_MONTAGEM}
        fundo={FUNDO}
        corPontos={COR_PONTOS}
        opacidadeConteudo={opacidadeConteudo}
      />
      <Acabamento />
    </AbsoluteFill>
  );
};
