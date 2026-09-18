// src/scenes/Abertura.tsx — o gancho. A marca se desenha, o nome assenta, a
// linha de apoio entra palavra por palavra.
//
// A marca à esquerda e o texto à direita, e não empilhados no centro: em
// 16:9 o centro vertical é caro, e a leitura em L é o que dá ao quadro uma
// diagonal em vez de uma pilha simétrica e inerte.
import React from "react";
import { AbsoluteFill, useCurrentFrame, useVideoConfig } from "remotion";
import { tema } from "../theme";
import { Malha, Pontos } from "../lib/camadas";
import { Entrada, Palavras, estiloDeCena, respiro } from "../lib/movimento";
import { Marca } from "../trama/Marca";

const LINHA_APOIO: React.CSSProperties = {
  fontFamily: tema.fonte.corpo,
  fontWeight: 500,
  fontSize: 40,
  letterSpacing: "-0.01em",
  color: tema.cor.fraco,
};

export const Abertura: React.FC = () => {
  const quadro = useCurrentFrame();
  const { durationInFrames } = useVideoConfig();
  const r = respiro(quadro);

  return (
    <AbsoluteFill style={estiloDeCena(quadro, durationInFrames)}>
      <Malha />
      <Pontos escala={1.6} />
      <AbsoluteFill>
        <AbsoluteFill
          style={{
            alignItems: "center",
            justifyContent: "center",
            flexDirection: "row",
            // Gap em pixel: ao lado de tipo de 150px um gap em `em` resolveria
            // contra os 16px do pai e viraria zero.
            gap: 76,
          }}
        >
          <div style={{ transform: `translateY(${r.flutua}px)` }}>
            <Marca altura={252} />
          </div>
          <div style={{ display: "flex", flexDirection: "column", gap: 18 }}>
            <Entrada atraso={14} mola={tema.mola.seca} sobe={30}>
              <div
                style={{
                  fontFamily: tema.fonte.display,
                  fontWeight: 800,
                  fontSize: 168,
                  lineHeight: 1.0,
                  letterSpacing: "-0.045em",
                  color: tema.cor.frente,
                }}
              >
                trama
              </div>
            </Entrada>
            {/* Duas linhas em dois blocos, e não uma frase longa com
                `maxWidth`: deixar o flex quebrar sozinho fazia a segunda linha
                quebrar TAMBÉM, e a frase saía em três pedaços de comprimento
                aleatório. Aqui a quebra é onde o sentido quebra, e a segunda
                linha entra depois da primeira. */}
            <Palavras
              texto="R em blocos"
              atraso={32}
              porPalavra={2}
              estilo={LINHA_APOIO}
            />
            <Palavras
              texto="diagramas que rodam"
              atraso={44}
              porPalavra={2}
              destacar={["rodam"]}
              estilo={LINHA_APOIO}
            />
          </div>
        </AbsoluteFill>
      </AbsoluteFill>
    </AbsoluteFill>
  );
};
