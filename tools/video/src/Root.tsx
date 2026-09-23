// src/Root.tsx — o registro das composições.
import React from "react";
import { Composition } from "remotion";
import "./estilos";
import { carregarFontes } from "./fontes";
import { DURACAO_TOTAL, TramaDemo } from "./Video";
import { DURACAO_TOTAL as DURACAO_MODELOS, TramaModelos } from "./VideoModelos";
import { DURACAO_TOTAL as DURACAO_VINHETA, TramaVinheta } from "./VideoVinheta";
import { DURACAO_TOTAL as DURACAO_SITE_MONTAGEM, SiteMontagem } from "./scenes/SiteMontagem";
import { DURACAO_TOTAL as DURACAO_SITE_AMPLO, SiteFluxoAmplo } from "./scenes/SiteFluxoAmplo";
import { DURACAO_TOTAL as DURACAO_SITE_VERTICAL, SiteVertical } from "./scenes/SiteVertical";
import {
  DURACAO_TOTAL as DURACAO_SITE_VERTICAL_DIVULGACAO,
  SiteVerticalDivulgacao,
} from "./scenes/SiteVerticalDivulgacao";

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
    <Composition
      id="TramaVinheta"
      component={TramaVinheta}
      durationInFrames={DURACAO_VINHETA}
      fps={30}
      width={1920}
      height={1080}
    />
    {/* As duas a seguir são os loops da home do site (`site/public/montagens`),
        não cenas do vídeo de demonstração: duração curta, sem trilha, e
        pensadas pra fechar em loop — ver `scenes/SiteMontagem.tsx` e
        `scenes/SiteFluxoAmplo.tsx`. */}
    <Composition
      id="SiteMontagem"
      component={SiteMontagem}
      durationInFrames={DURACAO_SITE_MONTAGEM}
      fps={30}
      width={1920}
      height={1080}
    />
    <Composition
      id="SiteFluxoAmplo"
      component={SiteFluxoAmplo}
      durationInFrames={DURACAO_SITE_AMPLO}
      fps={30}
      width={1920}
      height={1080}
    />
    {/* Vertical (1080×1920): a coluna direita do hero em loop mudo, e a base
        do vídeo pra compartilhar — ver `SiteVertical.tsx` e
        `SiteVerticalDivulgacao.tsx`. */}
    <Composition
      id="SiteVertical"
      component={SiteVertical}
      durationInFrames={DURACAO_SITE_VERTICAL}
      fps={30}
      width={1080}
      height={1920}
    />
    <Composition
      id="SiteVerticalDivulgacao"
      component={SiteVerticalDivulgacao}
      durationInFrames={DURACAO_SITE_VERTICAL_DIVULGACAO}
      fps={30}
      width={1080}
      height={1920}
    />
  </>
);
