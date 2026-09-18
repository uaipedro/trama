// src/VideoModelos.tsx — a montagem do vídeo de modelos.
//
// Três cenas, e não cinco: o fluxo é UMA cena longa porque a câmera volta e
// fecha nos cards já montados. Remontar o canvas numa cena nova obrigaria a
// trazer o relógio dos cards por `Sequence` negativa só para mostrar de perto o
// que já está lá.
import React from "react";
import { AbsoluteFill, Audio, Sequence, staticFile, useVideoConfig } from "remotion";
import { Acabamento } from "./lib/camadas";
import { AberturaModelos } from "./scenes/modelos/Abertura";
import { FluxoModelos } from "./scenes/modelos/Fluxo";
import { Outro } from "./scenes/Outro";
import { NOS } from "./trama/catalogo-modelos";

export const CENAS = {
  abertura: { de: 0, dur: 84 },
  fluxo: { de: 84, dur: 710 },
  outro: { de: 794, dur: 106 },
} as const;

export const DURACAO_TOTAL = CENAS.outro.de + CENAS.outro.dur;

// Cada cena vive 10 quadros além do seu corte, e a seguinte começa no corte
// mesmo — ver `estiloDeCena`. Sem isso a que sai já está em opacidade zero e a
// que entra ainda não começou, e o corte vira um piscar de tela vazia.
const SOBREPOSICAO = 10;

function duracaoDaCena(cena: { de: number; dur: number }) {
  const ultima = cena.de + cena.dur >= DURACAO_TOTAL;
  return ultima ? cena.dur : cena.dur + SOBREPOSICAO;
}

// Os quadros em que a câmera começa a se deslocar — espelham `MARCAS`/`DESLOCA`
// de `scenes/modelos/Fluxo.tsx`.
const DESLOCAMENTOS = [56, 146, 246, 356, 450, 545, 645].map(
  (q) => CENAS.fluxo.de + q - 26,
);

const Efeito: React.FC<{ arquivo: string; em: number; volume: number }> = ({
  arquivo,
  em,
  volume,
}) => (
  // O efeito entra 3 quadros ANTES do visual assentar: adiantado o som lê como
  // sincronizado, atrasado lê como defeito.
  <Sequence from={Math.max(0, Math.round(em) - 3)} layout="none">
    <Audio src={staticFile(`sfx/${arquivo}.wav`)} volume={volume} />
  </Sequence>
);

const Trilha: React.FC = () => {
  const cortes = [CENAS.fluxo.de, CENAS.outro.de];
  return (
    <>
      <Audio loop src={staticFile("sfx/pad.wav")} volume={0.46} />
      {cortes.map((q) => (
        <React.Fragment key={q}>
          <Efeito arquivo="riser" em={q - 22} volume={0.36} />
          <Efeito arquivo="thump" em={q} volume={0.58} />
        </React.Fragment>
      ))}
      {NOS.map((no) => (
        <React.Fragment key={no.id}>
          <Efeito arquivo="pop" em={CENAS.fluxo.de + no.entra} volume={0.46} />
          {no.digita === undefined ? null : (
            <Efeito arquivo="tick" em={CENAS.fluxo.de + no.digita + 2} volume={0.3} />
          )}
          {/* O `pop-alto` marca a CHEGADA do resultado, que neste vídeo é o
              momento que interessa: é quando a régua corre e as letras
              aparecem. */}
          <Efeito arquivo="pop-alto" em={CENAS.fluxo.de + no.resulta} volume={0.3} />
        </React.Fragment>
      ))}
      {DESLOCAMENTOS.map((q) => (
        <Efeito key={q} arquivo="whoosh" em={q} volume={0.33} />
      ))}
      <Efeito arquivo="whoosh" em={4} volume={0.34} />
      <Efeito arquivo="pop-alto" em={CENAS.outro.de + 22} volume={0.36} />
    </>
  );
};

export const TramaModelos: React.FC = () => {
  const { durationInFrames } = useVideoConfig();
  if (durationInFrames !== DURACAO_TOTAL) {
    console.error(
      `[trama-video] composição com ${durationInFrames} quadros, cenas somam ${DURACAO_TOTAL}.`,
    );
  }
  return (
    <AbsoluteFill>
      <Trilha />
      <Sequence from={CENAS.abertura.de} durationInFrames={duracaoDaCena(CENAS.abertura)}>
        <AberturaModelos />
      </Sequence>
      <Sequence from={CENAS.fluxo.de} durationInFrames={duracaoDaCena(CENAS.fluxo)}>
        <FluxoModelos />
      </Sequence>
      <Sequence from={CENAS.outro.de} durationInFrames={duracaoDaCena(CENAS.outro)}>
        <Outro pacote="uaipedro/trama/collections/trama.models" />
      </Sequence>
      {/* Acabamento é do QUADRO, não da cena: uma pilha só, do primeiro ao
          último. */}
      <Acabamento />
    </AbsoluteFill>
  );
};
