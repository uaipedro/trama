// src/motor/Composicoes.tsx — cada roteiro vira três coisas no Studio:
//
//   <id>            4:3, 1440×1080, com trilha
//   <id>-vertical   9:16, 1080×1920: o MESMO desenho, ampliado e recortado no
//                   meio (é por isso que o motor mantém o foco na zona segura)
//   <id>-<nome>     um still por plano `still`, em 4:3 e em vertical. O
//                   instante vem de uma `Sequence` com `from` negativo: um
//                   `<Freeze>` num `<Still>` de um quadro saía em branco.
//
// Nada aqui decide conteúdo: só enquadra o `Filme`.
import React from "react";
import { AbsoluteFill, Composition, Sequence, Still } from "remotion";
import { compilar, QUADRO, type Filme as FilmeT } from "./compilar";
import { Filme } from "./Filme";
import type { Roteiro } from "./roteiro";

const VERT = { w: 1080, h: 1920 };
const K = VERT.h / QUADRO.h; // 16/9: a altura do 4:3 vira a altura do 9:16

const Vertical: React.FC<{ filme: FilmeT; guias?: boolean; som?: boolean }> = (p) => (
  <AbsoluteFill style={{ background: "#0f1115", overflow: "hidden" }}>
    <div style={{
      position: "absolute", width: QUADRO.w, height: QUADRO.h,
      left: (VERT.w - QUADRO.w * K) / 2, top: 0,
      transform: `scale(${K})`, transformOrigin: "0 0",
    }}>
      <Filme {...p} vertical />
    </div>
  </AbsoluteFill>
);

type Props = { guias: boolean };

export const Composicoes: React.FC<{ roteiros: Roteiro[] }> = ({ roteiros }) => (
  <>
    {roteiros.map((r) => {
      const filme = compilar(r);
      return (
        <React.Fragment key={r.id}>
          <Composition<Props, any>
            id={r.id}
            component={({ guias }: Props) => <Filme filme={filme} guias={guias} />}
            durationInFrames={filme.duracao}
            fps={QUADRO.fps}
            width={QUADRO.w}
            height={QUADRO.h}
            defaultProps={{ guias: false }}
          />
          <Composition<Props, any>
            id={`${r.id}-vertical`}
            component={({ guias }: Props) => <Vertical filme={filme} guias={guias} />}
            durationInFrames={filme.duracao}
            fps={QUADRO.fps}
            width={VERT.w}
            height={VERT.h}
            defaultProps={{ guias: false }}
          />
          {filme.stills.map((s) => (
            <React.Fragment key={s.nome}>
              <Still
                id={`${r.id}-${s.nome}`}
                component={() => (
                  <Sequence from={-s.q}><Filme filme={filme} som={false} /></Sequence>
                )}
                width={QUADRO.w}
                height={QUADRO.h}
              />
              <Still
                id={`${r.id}-${s.nome}-vertical`}
                component={() => (
                  <Sequence from={-s.q}><Vertical filme={filme} som={false} /></Sequence>
                )}
                width={VERT.w}
                height={VERT.h}
              />
            </React.Fragment>
          ))}
        </React.Fragment>
      );
    })}
  </>
);
