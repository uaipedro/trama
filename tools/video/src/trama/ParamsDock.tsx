// src/trama/ParamsDock.tsx — o painel de parâmetros à esquerda, com o markup do
// `ParamsDock` de `inst/www/modos-ui.js`. Aparece quando o card selecionado
// está num modo que não mostra parâmetros (`mini`, `preview`).
//
// No app ele é `position:absolute` na borda do canvas; aqui quem posiciona é
// o `Filme`, então o posicionamento do CSS é desarmado e sobra só a aparência.
import React from "react";
import { tema } from "../theme";
import { LinhaParam, ModoIcone } from "./NoCard";
import { modoVigente, rotuloDe } from "./estado";
import { useCurrentFrame } from "remotion";
import type { NoFluxo } from "./tipos";

export const ParamsDock: React.FC<{ no: NoFluxo; escala?: number; origem?: string }> = ({
  no,
  escala = 1,
  origem = "0 50%",
}) => {
  const q = useCurrentFrame();
  const cor = tema.categoria[no.spec.categoria];
  return (
    <div style={{ width: 280, transform: `scale(${escala})`, transformOrigin: origem,
      fontFamily: tema.fonte.card }}>
      <aside className="tr-dock" style={{ position: "static", transform: "none", maxHeight: "none" }}>
        <div className="tr-dock-head" style={{ borderColor: cor }}>
          <ModoIcone modo={modoVigente(no, q)} />
          <strong>{rotuloDe(no)}</strong>
          <button className="tr-dock-fechar">‹</button>
        </div>
        {no.spec.params.length ? (
          <div className="tr-params">
            {no.spec.params.map((p) => <LinhaParam key={p.nome} no={no} param={p} solto />)}
          </div>
        ) : (
          <div className="tr-empty">este bloco não tem parâmetros</div>
        )}
      </aside>
    </div>
  );
};
