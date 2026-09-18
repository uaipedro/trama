// src/scenes/modelos/Abertura.tsx — o gancho do vídeo de modelos.
//
// Anuncia o CASO, não o produto: quem procura "ANOVA em blocos" precisa saber
// nos dois primeiros segundos que é disto que o vídeo trata. A marca fica
// pequena em cima, porque aqui ela é assinatura e não assunto.
import React from "react";
import { AbsoluteFill, useCurrentFrame, useVideoConfig } from "remotion";
import { tema } from "../../theme";
import { Malha, Pontos } from "../../lib/camadas";
import { Entrada, Palavras, estiloDeCena, respiro } from "../../lib/movimento";
import { Marca } from "../../trama/Marca";

export const AberturaModelos: React.FC = () => {
  const quadro = useCurrentFrame();
  const { durationInFrames } = useVideoConfig();
  const r = respiro(quadro);

  return (
    <AbsoluteFill style={estiloDeCena(quadro, durationInFrames)}>
      <Malha />
      <Pontos escala={1.6} />
      <AbsoluteFill
        style={{
          alignItems: "center",
          justifyContent: "center",
          flexDirection: "column",
          gap: 26,
        }}
      >
        <div style={{ transform: `translateY(${r.flutua}px)` }}>
          {/* `desenhar={false}`: o número de desenhar a marca peça por peça é
              da abertura do vídeo do núcleo. Aqui ela só assenta. */}
          <Marca altura={96} desenhar={false} brilho={false} />
        </div>
        <Entrada atraso={10} mola={tema.mola.seca} sobe={30}>
          <div
            style={{
              fontFamily: tema.fonte.display,
              fontWeight: 800,
              fontSize: 132,
              lineHeight: 1.0,
              letterSpacing: "-0.04em",
              color: tema.cor.frente,
            }}
          >
            ANOVA · DBC
          </div>
        </Entrada>
        <Palavras
          texto="do dado às letras, num fluxo"
          atraso={26}
          porPalavra={2}
          destacar={["letras"]}
          estilo={{
            fontFamily: tema.fonte.corpo,
            fontWeight: 500,
            fontSize: 40,
            letterSpacing: "-0.01em",
            color: tema.cor.fraco,
          }}
        />
      </AbsoluteFill>
    </AbsoluteFill>
  );
};
