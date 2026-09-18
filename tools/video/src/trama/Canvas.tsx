// src/trama/Canvas.tsx — o canvas do editor com uma câmera por cima.
//
// A cena não posiciona cards: ela move a CÂMERA. As posições dos blocos são
// coordenadas de canvas fixas (as do `ui.positions` de um `main.json`), e o
// enquadramento é `(centro, escala)` interpolado entre marcas. É o que permite
// cards legíveis durante a montagem — a 240px num quadro de 1920 o texto de
// 10,5px não se lê — e o grafo inteiro no fim, sem mover nada de lugar.
//
// Recebe o fluxo por PROPS: os dois vídeos desenham catálogos diferentes (a
// coleção `data` num, a `models` no outro) com este mesmo canvas.
import React from "react";
import { AbsoluteFill } from "remotion";
import { tema } from "../theme";
import { Pontos } from "../lib/camadas";
import { Aresta } from "./Aresta";
import { Handles, NoCard } from "./NoCard";
import type { ArestaFluxo, NoFluxo } from "./tipos";

export type Camera = { cx: number; cy: number; s: number };

// O card ATIVO é aquele que a cena está construindo agora: da entrada até um
// pouco depois do resultado chegar. É o único que recebe borda de destaque e
// brilho, e é assim que a regra de "no máximo um elemento-herói por quadro"
// sobrevive a um canvas com seis cards.
function idAtivo(nos: NoFluxo[], quadro: number): string | null {
  let ativo: string | null = null;
  for (const no of nos) {
    if (quadro >= no.entra && quadro < no.resulta + 26) ativo = no.id;
  }
  return ativo;
}

export const Canvas: React.FC<{
  camera: Camera;
  largura: number;
  altura: number;
  quadro: number;
  nos: NoFluxo[];
  arestas: ArestaFluxo[];
}> = ({ camera, largura, altura, quadro, nos, arestas }) => {
  const { cx, cy, s } = camera;
  const ox = largura / 2 - cx * s;
  const oy = altura / 2 - cy * s;
  const ativo = idAtivo(nos, quadro);
  const passo = 20 * s;
  const porId = (id: string): NoFluxo => {
    const no = nos.find((n) => n.id === id);
    if (!no) throw new Error(`nó desconhecido no fluxo: ${id}`);
    return no;
  };

  return (
    <AbsoluteFill style={{ background: tema.cor.fundo, overflow: "hidden" }}>
      {/* Os pontos acompanham a câmera: sem isso o fundo fica parado enquanto
          os cards deslizam, e o movimento lê como cards voando em vez de
          câmera passeando. O módulo mantém a grade alinhada sem desenhar uma
          textura do tamanho do canvas. */}
      <Pontos
        escala={s}
        dx={((ox % passo) + passo) % passo}
        dy={((oy % passo) + passo) % passo}
      />
      <div
        style={{
          position: "absolute",
          left: 0,
          top: 0,
          transform: `translate(${ox}px, ${oy}px) scale(${s})`,
          transformOrigin: "0 0",
        }}
      >
        {/* As arestas ficam ATRÁS dos cards, como no editor: a curva encosta na
            borda do card e desaparece por baixo dele. `overflow:visible` porque
            o fluxo tem y negativo (há card acima da origem). */}
        <svg
          style={{ position: "absolute", left: 0, top: 0, overflow: "visible" }}
          width={1}
          height={1}
        >
          {arestas.map((a) => (
            <Aresta
              key={`${a.de}->${a.paraNo}:${a.paraPorta}`}
              de={porId(a.de)}
              para={porId(a.paraNo)}
              portaDestino={a.paraPorta}
              desenha={a.desenha}
            />
          ))}
        </svg>

        {nos.map((no) =>
          quadro >= no.entra ? (
            <div key={no.id} style={{ position: "absolute", left: no.x, top: no.y }}>
              <NoCard no={no} ativo={no.id === ativo} />
            </div>
          ) : null,
        )}

        {/* Os handles por último e fora dos cards: o `.tr-node` é
            `overflow:hidden` e cortaria a metade do ponto que fica sobre a
            borda — justamente a metade que a aresta toca. */}
        {nos.map((no) =>
          quadro >= no.entra ? <Handles key={no.id} no={no} visivel={no.entra + 3} /> : null,
        )}
      </div>
    </AbsoluteFill>
  );
};
