// src/Root.tsx — o registro das composições.
import React from "react";
import { Composition } from "remotion";
import "./estilos";
import { carregarFontes } from "./fontes";
import { DURACAO_TOTAL, TramaDemo } from "./Video";
import { DURACAO_TOTAL as DURACAO_MODELOS, TramaModelos } from "./VideoModelos";

carregarFontes();

export const Root: React.FC = () => (
  <>
    <Composition
      id="TramaDemo"
      component={TramaDemo}
      durationInFrames={DURACAO_TOTAL}
      // 30 e não 60: os dois vídeos são de interface, com deslocamentos curtos
      // e muitas paradas. 60 dobraria o tempo de render sem nada que peça.
      fps={30}
      width={1920}
      height={1080}
    />
    <Composition
      id="TramaModelos"
      component={TramaModelos}
      durationInFrames={DURACAO_MODELOS}
      fps={30}
      width={1920}
      height={1080}
    />
  </>
);
