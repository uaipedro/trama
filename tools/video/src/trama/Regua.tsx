// src/trama/Regua.tsx — a régua logarítmica do p-valor e as estrelas.
//
// As regras são transcritas de `inst/www/teste.js`, que é o arquivo onde elas
// moram justamente porque "o que pode mentir em silêncio se testa com
// `node --test` puro". Aqui não se reinventa nenhuma: a régua vai de p = 1
// (vazia) a p = 1e-4 (cheia), as estrelas são as do `summary()` do R, e a
// intensidade da cor sai da estrela — um tom só, para quem não distingue cor
// ler pelo comprimento e pela estrela.
import React from "react";
import { interpolate, useCurrentFrame } from "remotion";
import { tema } from "../theme";

const DECADAS = 4;
const MARCAS: [string, number][] = [
  ["10%", 0.1],
  ["5%", 0.05],
  ["1%", 0.01],
  ["0,1%", 0.001],
];

export function posicao(p: number | null | undefined): number {
  if (p == null || Number.isNaN(p)) return 0;
  if (p <= 0) return 1;
  return Math.min(1, Math.max(0, -Math.log10(p) / DECADAS));
}

export function estrelas(p: number | null | undefined): string {
  if (p == null || Number.isNaN(p)) return "";
  if (p < 0.001) return "***";
  if (p < 0.01) return "**";
  if (p < 0.05) return "*";
  if (p < 0.1) return ".";
  return "ns";
}

export function faixa(est: string): number {
  return ({ "***": 4, "**": 3, "*": 2, ".": 1 } as Record<string, number>)[est] ?? 0;
}

// `num` do teste.js: pt-BR, e potência de dez abaixo de 1e-3 porque
// `maximumFractionDigits` MENTE ali — 0,0002 sairia "0" e 0,0005 sairia
// "0,001", o dobro.
export function num(v: number | null | undefined, casas = 3): string {
  if (v == null || Number.isNaN(v)) return "—";
  if (!Number.isFinite(v)) return v > 0 ? "∞" : "−∞";
  const abs = Math.abs(v);
  if (abs !== 0 && (abs < 1e-3 || abs >= 1e6)) return v.toExponential(1).replace(".", ",");
  return v.toLocaleString("pt-BR", { maximumFractionDigits: casas });
}

export const Estrelas: React.FC<{ est: string }> = ({ est }) => {
  if (!est) return null;
  return <span className={`tr-estrelas tr-faixa-${faixa(est)}`}>{est}</span>;
};

export const Regua: React.FC<{
  p: number | null | undefined;
  mini?: boolean;
  // Quadro em que a barra começa a crescer. A régua CRESCE em cena em vez de
  // aparecer cheia: é o único elemento do card que carrega a conclusão do
  // teste, e vê-la correr até a marca dos 5% é o que explica a figura sem
  // legenda nenhuma.
  desde?: number;
}> = ({ p, mini, desde }) => {
  const quadro = useCurrentFrame();
  const est = estrelas(p);
  const alvo = posicao(p);
  const pos =
    desde === undefined
      ? alvo
      : interpolate(quadro, [desde, desde + 22], [0, alvo], {
          easing: tema.ease.saida,
          extrapolateLeft: "clamp",
          extrapolateRight: "clamp",
        });
  return (
    <div className={`tr-regua${mini ? " tr-regua-mini" : ""} tr-faixa-${faixa(est)}`}>
      <div className="tr-regua-trilho">
        <div className="tr-regua-barra" style={{ width: `${pos * 100}%` }} />
        {MARCAS.map(([rot, alfa]) => (
          <span
            key={rot}
            className={"tr-regua-marca" + (alfa === 0.05 ? " tr-regua-marca-5" : "")}
            style={{ left: `${posicao(alfa) * 100}%` }}
          />
        ))}
        {p == null ? null : (
          <span className="tr-regua-ponta" style={{ left: `${pos * 100}%` }} />
        )}
      </div>
    </div>
  );
};
