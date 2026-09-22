// src/VideoVinheta.tsx — a abertura sozinha, pra quando só a vinheta (e não o
// demo inteiro) precisa ser renderizada. Mesma cena de `Video.tsx`, mas sem
// as outras quatro e sem som.
import React from "react";
import { AbsoluteFill } from "remotion";
import { Acabamento } from "./lib/camadas";
import { Abertura } from "./scenes/Abertura";
import { CENAS } from "./Video";

export const DURACAO_TOTAL = CENAS.abertura.dur;

export const TramaVinheta: React.FC = () => (
  <AbsoluteFill>
    <Abertura />
    <Acabamento />
  </AbsoluteFill>
);
