// src/lib/Legenda.tsx — a legenda que diz o que está acontecendo na tela.
//
// A pílula entra junto com a primeira palavra, e não antes: com a caixa em
// opacidade cheia e as palavras ainda por chegar, o quadro mostra uma lápide
// cinza vazia por dez quadros — defeito que só aparece extraindo justamente um
// quadro do começo da legenda.
//
// `caixa` existe porque o lugar certo depende da cena: sobre o canvas ela fica
// no terço inferior centrado, mas na cena de código o rodapé é dos painéis, e
// legenda por cima de código é legenda que apaga o que ela mesma está
// apontando.
import React from "react";
import { useCurrentFrame } from "remotion";
import { tema } from "../theme";
import { Palavras, mover } from "./movimento";

export const Legenda: React.FC<{
  texto: string;
  de: number;
  ate: number;
  destacar?: string[];
  // Altura a partir da base do quadro, quando `caixa` não diz outra coisa.
  base?: number;
  caixa?: React.CSSProperties;
}> = ({ texto, de, ate, destacar = [], base = 96, caixa }) => {
  const quadro = useCurrentFrame();
  if (quadro < de || quadro > ate) return null;

  const entra = mover(quadro, [de, de + 10], [0, 1]);
  const sai = mover(quadro, [ate - 8, ate], [1, 0], tema.ease.entrada);
  const sobe = mover(quadro, [ate - 8, ate], [0, 16], tema.ease.entrada);

  return (
    <div
      style={{
        position: "absolute",
        left: 0,
        right: 0,
        bottom: base,
        display: "flex",
        justifyContent: "center",
        opacity: entra * sai,
        transform: `translateY(${sobe}px)`,
        ...caixa,
      }}
    >
      <div
        style={{
          display: "flex",
          alignItems: "center",
          gap: 18,
          padding: "14px 30px",
          borderRadius: 14,
          // Vidro sobre o canvas, com a linha do editor na borda: a legenda
          // pertence à mesma interface, não é adesivo colado em cima.
          background: "rgba(13,16,22,.74)",
          border: `1px solid ${tema.cor.linha}`,
          backdropFilter: "blur(14px)",
          boxShadow: "0 18px 50px rgba(0,0,0,.45)",
          transform: `scale(${mover(quadro, [de, de + 12], [0.97, 1])})`,
        }}
      >
        <span
          style={{
            width: 5,
            alignSelf: "stretch",
            borderRadius: 4,
            background: tema.cor.destaque,
          }}
        />
        <Palavras
          texto={texto}
          atraso={de}
          porPalavra={2}
          destacar={destacar}
          estilo={{
            fontFamily: tema.fonte.display,
            fontWeight: 600,
            fontSize: 38,
            letterSpacing: "-0.02em",
            color: tema.cor.frente,
          }}
        />
      </div>
    </div>
  );
};
