// src/scenes/SiteFluxoAmplo.tsx — o loop "fluxo conectado" da home do site.
//
// O fluxo é o mesmo grafo de seis nós do `catalogo.ts` (ler, conhecer, filtrar,
// os dois agregados, juntar) — só que aqui ele já está PRONTO: nada digita,
// nada "computando", porque quem se move é a câmera, passeando pelo grafo. Na
// versão anterior o grafo inteiro cabia no quadro de uma vez só, pequeno
// demais pra ler; aqui a câmera abre no todo, PASSEIA de perto por cada bloco
// na ordem em que a legenda da página os descreve, e volta pro todo — que é
// também o quadro em que o laço fecha.
import React from "react";
import { AbsoluteFill, useCurrentFrame, useVideoConfig } from "remotion";
import { tema } from "../theme";
import { mover } from "../lib/movimento";
import { Acabamento } from "../lib/camadas";
import { Canvas, type Camera } from "../trama/Canvas";
import { ARESTAS_AMPLO, NOS_AMPLO } from "../trama/catalogo-site";

const FUNDO = "oklch(24% .03 220)";
const COR_PONTOS = "#6f96a1";

export const DURACAO_TOTAL = 300;

// A marca de abertura e a de fechamento são A MESMA câmera: é isso que fecha o
// laço sem corte. Entre elas, a câmera visita os seis blocos na ordem que a
// legenda da página promete — ler, conhecer, filtrar, agregar (as duas
// contagens) e juntar — a um zoom que cabe 2-3 cards por quadro.
const CHEIO: Camera = { cx: 615, cy: 282.5, s: 0.95 };
const PERTO = 1.5;

// `q` é o quadro em que a câmera CHEGA em cada marca (mesma convenção de
// `scenes/Fluxo.tsx`). `DESLOCA` é quanto do intervalo anterior é usado pelo
// deslocamento; o resto é câmera PARADA — o contraste entre deslocamento e
// imobilidade é o que lê como câmera, e não tremedeira.
const DESLOCA = 20;

const MARCAS: { q: number; cam: Camera }[] = [
  { q: 0, cam: CHEIO },
  { q: 50, cam: { cx: 120, cy: 220, s: PERTO } }, // ler
  { q: 95, cam: { cx: 450, cy: -72, s: 1.45 } }, // resumo (conhecer)
  { q: 140, cam: { cx: 450, cy: 335, s: PERTO } }, // filtrar
  { q: 185, cam: { cx: 780, cy: 218, s: PERTO } }, // contar (agregar #1)
  { q: 225, cam: { cx: 780, cy: 588, s: PERTO } }, // somar (agregar #2)
  { q: 260, cam: { cx: 1110, cy: 403, s: PERTO } }, // juntar
  // O último quadro válido (DURACAO_TOTAL - 1): a câmera some aqui EXATAMENTE
  // na marca de abertura, então `cameraEm(0) === cameraEm(DURACAO_TOTAL - 1)`
  // e o corte de loop não pisca.
  { q: DURACAO_TOTAL - 1, cam: CHEIO },
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

// Mesma ideia de `SiteMontagem`: fases em múltiplos inteiros de uma volta
// completa em `DURACAO_TOTAL`, pra `respiro(0)` bater com `respiro(fim)` sem
// precisar medir nada — sem isso o corte de loop teria um microssalto na
// respiração, mesmo com a câmera idêntica nos dois lados.
function respiroDoLaco(quadro: number) {
  const t = (2 * Math.PI * quadro) / DURACAO_TOTAL;
  return { escala: 1 + Math.sin(t * 5) * 0.006, flutua: Math.sin(t * 7) * 2.2 };
}

export const SiteFluxoAmplo: React.FC = () => {
  const quadro = useCurrentFrame();
  const { durationInFrames, width, height } = useVideoConfig();
  if (durationInFrames !== DURACAO_TOTAL) {
    console.error(
      `[trama-video] SiteFluxoAmplo com ${durationInFrames} quadros, esperava ${DURACAO_TOTAL}.`,
    );
  }
  const camera = cameraEm(quadro);
  const r = respiroDoLaco(quadro);

  return (
    <AbsoluteFill>
      <Canvas
        camera={{ cx: camera.cx + r.flutua, cy: camera.cy - r.flutua, s: camera.s * r.escala }}
        largura={width}
        altura={height}
        quadro={quadro}
        nos={NOS_AMPLO}
        arestas={ARESTAS_AMPLO}
        fundo={FUNDO}
        corPontos={COR_PONTOS}
      />
      <Acabamento />
    </AbsoluteFill>
  );
};
