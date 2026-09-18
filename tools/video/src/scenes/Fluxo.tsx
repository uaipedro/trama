// src/scenes/Fluxo.tsx — o corpo do vídeo: o fluxo sendo montado.
//
// A cena só decide o ENQUADRAMENTO. Quem entra quando está em `catalogo.ts`, e
// como cada card se comporta está em `NoCard.tsx`. Aqui moram as marcas de
// câmera e as legendas.
//
// Ritmo: a câmera CHEGA um pouco antes de a ação começar, e fica PARADA
// enquanto ela acontece. Câmera em movimento contínuo é o que faz uma demo
// parecer filmada na mão; o contraste entre um deslocamento rápido e uma
// imobilidade completa é o que lê como caro.
import React from "react";
import { AbsoluteFill, useCurrentFrame, useVideoConfig } from "remotion";
import { tema } from "../theme";
import { estiloDeCena, mover, respiro } from "../lib/movimento";
import { Legenda } from "../lib/Legenda";
import { Canvas, type Camera } from "../trama/Canvas";
import { ARESTAS, NOS } from "../trama/catalogo";

// `q` é o quadro em que a câmera CHEGOU. O deslocamento ocupa os `DESLOCA`
// quadros anteriores, então basta olhar o `entra` do bloco em `catalogo.ts` e
// pedir a chegada uns quadros antes.
const DESLOCA = 26;

const MARCAS: { q: number; cam: Camera }[] = [
  { q: 0, cam: { cx: 120, cy: 220, s: 2.3 } }, // o Ler CSV, de perto
  { q: 58, cam: { cx: 300, cy: 95, s: 1.72 } }, // sobe pro Resumo
  { q: 134, cam: { cx: 336, cy: 320, s: 1.72 } }, // desce pro Filtrar
  { q: 218, cam: { cx: 648, cy: 240, s: 1.62 } }, // o primeiro Agrupar
  { q: 292, cam: { cx: 660, cy: 480, s: 1.62 } }, // o segundo Agrupar
  { q: 340, cam: { cx: 900, cy: 400, s: 1.5 } }, // o Juntar
  // E o grafo inteiro. A escala é 0,9 e o centro está ABAIXO do centro
  // geométrico do grafo (320 contra 283): em escala cheia e centrado, o card de
  // baixo encostava na borda inferior e a legenda pousava em cima dele. Com o
  // centro deslocado, o grafo sobe e sobra uma faixa livre no rodapé.
  { q: 398, cam: { cx: 615, cy: 320, s: 0.9 } },
];

function cameraEm(f: number): Camera {
  if (f <= MARCAS[0].q) return MARCAS[0].cam;
  for (let i = 1; i < MARCAS.length; i++) {
    const anterior = MARCAS[i - 1];
    const atual = MARCAS[i];
    if (f >= atual.q) continue;
    const inicio = Math.max(anterior.q, atual.q - DESLOCA);
    // Antes do início do deslocamento a câmera está PARADA na marca anterior.
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

export const Fluxo: React.FC = () => {
  const quadro = useCurrentFrame();
  const { durationInFrames, width, height } = useVideoConfig();
  const camera = cameraEm(quadro);
  // Mesmo parada, a câmera respira: um canvas absolutamente imóvel por 60
  // quadros lê como imagem congelada, e não como tela sendo observada.
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
      <Legenda
        texto="cada bloco mostra o próprio resultado"
        de={52}
        ate={132}
        destacar={["próprio", "resultado"]}
      />
      <Legenda
        // Não descreve o que o quadro acabou de mostrar — diz o que aquilo É, e
        // entrega a deixa para a cena seguinte, que mostra justamente o
        // `fluxo.R`. Uma legenda que narra o próprio plano é legenda perdida.
        texto="conecte blocos pra construir seu script"
        de={400}
        ate={durationInFrames - 8}
        destacar={["script"]}
        base={38}
      />
    </AbsoluteFill>
  );
};
