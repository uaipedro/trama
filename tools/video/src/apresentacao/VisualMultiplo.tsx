// src/apresentacao/VisualMultiplo.tsx — segmento A da apresentação.
//
// Primeiro o fatorial montado passo a passo (roteiro `apresentacao-a`, dados do
// R). Depois o "não é só isso": quatro fluxos de outros domínios, fotografados
// no editor de verdade a partir dos templates das coleções, entram num mosaico.
import React from "react";
import {
  AbsoluteFill, Audio, Img, interpolate, Sequence, spring, staticFile, useCurrentFrame, useVideoConfig,
} from "remotion";
import { compilar } from "../motor/compilar";
import { Filme } from "../motor/Filme";
import { apresentacaoA } from "../roteiros/apresentacao-a";
import { tema } from "../theme";

const filme = compilar(apresentacaoA);
const MOSAICO = 6 * 30;
export const DURACAO_A = filme.duracao + MOSAICO;

const DOMINIOS = [
  { arq: "decomposicao-de-serie", rotulo: "Séries temporais" },
  { arq: "pca-com-biplot", rotulo: "Multivariada" },
  { arq: "krigagem-com-mapa-do-predito-e-do-erro-padrao", rotulo: "Geoestatística" },
  { arq: "arvore-de-decisao", rotulo: "Aprendizado de máquina" },
];

const Mosaico: React.FC = () => {
  const q = useCurrentFrame();
  const { fps } = useVideoConfig();
  const sai = interpolate(q, [MOSAICO - 12, MOSAICO - 1], [1, 0], { extrapolateLeft: "clamp" });
  return (
    <AbsoluteFill style={{ background: tema.cor.fundo, opacity: sai }}>
      <div style={{ position: "absolute", inset: "64px 64px 150px", display: "grid",
        gridTemplateColumns: "1fr 1fr", gridTemplateRows: "1fr 1fr", gap: 22 }}>
        {DOMINIOS.map((d, i) => {
          const p = spring({ frame: q - 6 - i * 7, fps, config: { damping: 200 } });
          return (
            <div key={d.arq} style={{ position: "relative", borderRadius: 12, overflow: "hidden",
              border: `1px solid ${tema.cor.linha}`, opacity: p,
              transform: `translateY(${(1 - p) * 30}px) scale(${0.96 + 0.04 * p})` }}>
              <Img src={staticFile(`apresentacao/mosaico-${d.arq}.png`)}
                style={{ width: "100%", height: "100%", objectFit: "cover", objectPosition: "center" }} />
              <div style={{ position: "absolute", left: 14, bottom: 12, padding: "6px 12px", borderRadius: 8,
                background: "rgba(15,17,21,.86)", color: tema.cor.frente, fontFamily: tema.fonte.corpo,
                fontWeight: 600, fontSize: 24 }}>{d.rotulo}</div>
            </div>
          );
        })}
      </div>
      <div style={{ position: "absolute", left: 0, right: 0, bottom: 56, textAlign: "center",
        fontFamily: tema.fonte.display, fontWeight: 800, fontSize: 46, color: tema.cor.frente,
        opacity: spring({ frame: q - 40, fps, config: { damping: 200 } }) }}>
        qualquer <span style={{ color: tema.cor.destaque }}>linha de raciocínio</span>
      </div>
    </AbsoluteFill>
  );
};

export const VisualMultiplo: React.FC = () => (
  <AbsoluteFill style={{ background: tema.cor.fundo }}>
    <Sequence durationInFrames={filme.duracao}>
      <Filme filme={filme} />
    </Sequence>
    <Sequence from={filme.duracao}>
      <Mosaico />
    </Sequence>
    <Sequence from={15}>
      <Audio src={staticFile("apresentacao/narracao-A.wav")} />
    </Sequence>
  </AbsoluteFill>
);
