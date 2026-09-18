// src/Video.tsx — a montagem do vídeo da coleção `data`: as cinco cenas na
// linha de tempo e o som. As folhas de estilo entram em `src/estilos.ts`, que o
// Root importa — são as mesmas para os dois vídeos.
import React from "react";
import { AbsoluteFill, Audio, Sequence, staticFile, useVideoConfig } from "remotion";
import { Acabamento } from "./lib/camadas";
import { Abertura } from "./scenes/Abertura";
import { Colecoes } from "./scenes/Colecoes";
import { Fluxo } from "./scenes/Fluxo";
import { Codigo } from "./scenes/Codigo";
import { Outro } from "./scenes/Outro";
import { NOS } from "./trama/catalogo";

// A linha de tempo, num lugar só. Somar as durações de cabeça em cinco
// arquivos é como um vídeo ganha dois segundos de silêncio no meio.
export const CENAS = {
  abertura: { de: 0, dur: 90 },
  colecoes: { de: 90, dur: 108 },
  fluxo: { de: 198, dur: 450 },
  codigo: { de: 648, dur: 154 },
  outro: { de: 802, dur: 98 },
} as const;

export const DURACAO_TOTAL = CENAS.outro.de + CENAS.outro.dur;

// Cada cena vive 10 quadros ALÉM do seu corte, e a seguinte começa no corte
// mesmo. Isso faz a saída de uma acontecer enquanto a outra chega — sem isso a
// que sai já estava em opacidade zero e a que entra ainda não tinha começado, e
// o corte virava um piscar de tela vazia (defeito que só apareceu amostrando
// quadros em intervalos regulares, em 6,58s e 27,28s da primeira versão).
// A última cena não sobrepõe nada: depois dela é o fim do vídeo.
const SOBREPOSICAO = 10;

function duracaoDaCena(cena: { de: number; dur: number }) {
  const ultima = cena.de + cena.dur >= DURACAO_TOTAL;
  return ultima ? cena.dur : cena.dur + SOBREPOSICAO;
}

// Os quadros em que a câmera COMEÇA a se deslocar dentro da cena do fluxo —
// espelham `MARCAS`/`DESLOCA` de `scenes/Fluxo.tsx`, que é o único lugar onde
// se muda o enquadramento.
const DESLOCAMENTOS = [58, 134, 218, 292, 340, 398].map((q) => CENAS.fluxo.de + q - 26);

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
  const cortes = [CENAS.colecoes.de, CENAS.fluxo.de, CENAS.codigo.de, CENAS.outro.de];
  return (
    <>
      {/* A cama, em laço: 10s de pad para 30s de vídeo.
          O ganho da cama é o que governa a LOUDNESS do master, e não o dos
          efeitos. Com o pad em 0,2 o vídeo fechava em −21,9 LUFS integrado,
          uns 7 dB abaixo do que se entrega para web — e subir tudo junto os
          7 dB estouraria os transientes, que já estavam com pico em
          −4,4 dBFS. Então a cama sobe mais que os golpes: a loudness sobe,
          a faixa dinâmica encolhe e o pico continua com folga. */}
      <Audio loop src={staticFile("sfx/pad.wav")} volume={0.46} />

      {cortes.map((q) => (
        <React.Fragment key={q}>
          {/* Sobe pro corte e bate NO corte: a dupla é o que faz uma troca de
              cena soar como decisão, e não como emenda. */}
          <Efeito arquivo="riser" em={q - 22} volume={0.36} />
          <Efeito arquivo="thump" em={q} volume={0.58} />
        </React.Fragment>
      ))}

      {/* Um pop por card que assenta, e um clique quando o parâmetro é
          digitado. Derivados do catálogo: mexer no `entra` de um bloco move o
          som junto, sem ninguém precisar lembrar. */}
      {NOS.map((no) => (
        <React.Fragment key={no.id}>
          <Efeito arquivo="pop" em={CENAS.fluxo.de + no.entra} volume={0.46} />
          {no.digita === undefined ? null : (
            <Efeito arquivo="tick" em={CENAS.fluxo.de + no.digita + 2} volume={0.3} />
          )}
          <Efeito arquivo="pop-alto" em={CENAS.fluxo.de + no.resulta} volume={0.3} />
        </React.Fragment>
      ))}

      {DESLOCAMENTOS.map((q) => (
        <Efeito key={q} arquivo="whoosh" em={q} volume={0.33} />
      ))}

      {/* A marca da abertura e o endereço do fecho: os dois ganham corpo. */}
      <Efeito arquivo="whoosh" em={4} volume={0.4} />
      <Efeito arquivo="pop-alto" em={CENAS.outro.de + 22} volume={0.36} />
    </>
  );
};

export const TramaDemo: React.FC = () => {
  const { durationInFrames } = useVideoConfig();
  if (durationInFrames !== DURACAO_TOTAL) {
    // Falha alta, e não silenciosa: `durationInFrames` menor que a soma das
    // cenas corta o fim, maior deixa ar morto — os dois passam batido numa
    // inspeção por amostragem de quadros.
    console.error(
      `[trama-video] composição com ${durationInFrames} quadros, cenas somam ${DURACAO_TOTAL}.`,
    );
  }
  return (
    <AbsoluteFill>
      <Trilha />
      {/* A ordem importa: a cena seguinte é irmã POSTERIOR, então ela desenha
          por cima da anterior durante a sobreposição. */}
      <Sequence from={CENAS.abertura.de} durationInFrames={duracaoDaCena(CENAS.abertura)}>
        <Abertura />
      </Sequence>
      <Sequence from={CENAS.colecoes.de} durationInFrames={duracaoDaCena(CENAS.colecoes)}>
        <Colecoes />
      </Sequence>
      <Sequence from={CENAS.fluxo.de} durationInFrames={duracaoDaCena(CENAS.fluxo)}>
        <Fluxo />
      </Sequence>
      <Sequence from={CENAS.codigo.de} durationInFrames={duracaoDaCena(CENAS.codigo)}>
        <Codigo />
      </Sequence>
      <Sequence from={CENAS.outro.de} durationInFrames={duracaoDaCena(CENAS.outro)}>
        <Outro />
      </Sequence>
      {/* Grade de cor, grão e vinheta são acabamento do QUADRO, e não de cada
          cena: com as cenas sobrepostas, duas delas desenhando a própria
          vinheta e o próprio grão dobrariam a densidade justamente nos dez
          quadros do corte. Aqui a pilha é uma só, do primeiro quadro ao
          último. */}
      <Acabamento />
    </AbsoluteFill>
  );
};
