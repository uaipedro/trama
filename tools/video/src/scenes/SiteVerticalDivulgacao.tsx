// src/scenes/SiteVerticalDivulgacao.tsx — a versão pra compartilhar do loop
// vertical: os mesmos 15s de `SiteVertical`, mais ~2s de fecho com a marca e o
// endereço, pro clipe fazer sentido sozinho num story ou num grupo de
// WhatsApp — que não tem `<video loop>` nem legenda da página em volta.
//
// Composição SEPARADA de `SiteVertical` (e não uma prop nela) porque o loop da
// home precisa terminar exatamente onde começa; um fecho de marca no meio
// disso quebraria a costura. Aqui não há costura pra guardar.
import React from "react";
import { AbsoluteFill, Sequence, useCurrentFrame, useVideoConfig } from "remotion";
import { tema } from "../theme";
import { Entrada, mover, respiro } from "../lib/movimento";
import { Pontos, Acabamento } from "../lib/camadas";
import { Marca } from "../trama/Marca";
import { NucleoVertical, DURACAO_TOTAL as DURACAO_LOOP } from "./SiteVertical";

const FUNDO = "oklch(24% .03 220)";
const COR_PONTOS = "#6f96a1";

const DURACAO_FECHO = 60;
export const DURACAO_TOTAL = DURACAO_LOOP + DURACAO_FECHO;

// Mesma sobreposição de `Video.tsx`: o fecho já está chegando enquanto o loop
// ainda está saindo, pra o corte não virar um pisca de tela vazia.
const SOBREPOSICAO = 10;

// O fecho: fundo do MESMO canvas (não a malha clara do `Outro.tsx` do vídeo
// de demonstração) — este clipe não tem cena de fundo claro em lugar nenhum,
// e trocar de tom no último segundo leria como emenda de dois vídeos.
//
// SÓ entrada, sem `estiloDeCena`: é a ÚLTIMA cena do vídeo, então não há saída
// nenhuma pra desenhar — a marca precisa ficar em tela, inteira, até o quadro
// final, e não desvanecer de novo nos últimos quadros.
const FechoVertical: React.FC = () => {
  const quadro = useCurrentFrame();
  const r = respiro(quadro);
  const opacidade = mover(quadro, [0, 10], [0, 1]);

  return (
    <AbsoluteFill style={{ opacity: opacidade }}>
      <AbsoluteFill style={{ background: FUNDO }}>
        <Pontos escala={1.3} cor={COR_PONTOS} />
      </AbsoluteFill>
      <AbsoluteFill
        style={{
          alignItems: "center",
          justifyContent: "center",
          flexDirection: "column",
          gap: 34,
        }}
      >
        <div style={{ transform: `translateY(${r.flutua}px)` }}>
          <Marca altura={210} desenhar={false} />
        </div>
        <Entrada atraso={12} mola={tema.mola.seca} sobe={22}>
          <div
            style={{
              fontFamily: tema.fonte.display,
              fontWeight: 800,
              fontSize: 46,
              letterSpacing: "-0.02em",
              color: tema.cor.destaque,
              textAlign: "center",
              textShadow: `0 0 48px ${tema.cor.brilho}`,
            }}
          >
            uaipedro.github.io/trama
          </div>
        </Entrada>
      </AbsoluteFill>
    </AbsoluteFill>
  );
};

export const SiteVerticalDivulgacao: React.FC = () => {
  const { durationInFrames } = useVideoConfig();
  if (durationInFrames !== DURACAO_TOTAL) {
    console.error(
      `[trama-video] SiteVerticalDivulgacao com ${durationInFrames} quadros, esperava ${DURACAO_TOTAL}.`,
    );
  }
  return (
    <AbsoluteFill>
      <Sequence from={0} durationInFrames={DURACAO_LOOP}>
        <NucleoVertical />
      </Sequence>
      <Sequence from={DURACAO_LOOP - SOBREPOSICAO} durationInFrames={DURACAO_FECHO + SOBREPOSICAO}>
        <FechoVertical />
      </Sequence>
      {/* Acabamento por cima das duas cenas, não dentro de cada uma — do
          contrário os dez quadros de sobreposição dobrariam grão e vinheta. */}
      <Acabamento />
    </AbsoluteFill>
  );
};
