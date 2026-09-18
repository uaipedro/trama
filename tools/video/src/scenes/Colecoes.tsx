// src/scenes/Colecoes.tsx — de onde vêm os blocos.
//
// Cada ficha é um pacote R instalável, com a cor de uma das categorias da
// paleta. As fichas entram escalonadas em 4 quadros: seis coisas aparecendo ao
// mesmo tempo lêem como uma imagem, e o que interessa aqui é justamente que são
// SEIS peças que se somam.
import React from "react";
import { AbsoluteFill, useCurrentFrame, useVideoConfig } from "remotion";
import { tema } from "../theme";
import { Malha, Pontos } from "../lib/camadas";
import { Entrada, Palavras, estiloDeCena, respiro } from "../lib/movimento";
import { COLECOES } from "../trama/catalogo";
import { ICONES } from "../trama/icones";

const Ficha: React.FC<{ nome: string; o_que: string; cor: string; i: number }> = ({
  nome,
  o_que,
  cor,
  i,
}) => {
  const quadro = useCurrentFrame();
  const r = respiro(quadro, i * 1.1);
  return (
    <Entrada atraso={16 + i * 4} mola={tema.mola.seca} sobe={34}>
      <div
        style={{
          display: "flex",
          alignItems: "center",
          gap: 18,
          width: 470,
          padding: "20px 24px",
          borderRadius: 14,
          background: tema.cor.painel,
          border: `1px solid ${tema.cor.linha}`,
          boxShadow: "0 10px 30px rgba(0,0,0,.35)",
          transform: `translateY(${r.flutua}px)`,
        }}
      >
        {/* A pastilha repete a faixa colorida do cabeçalho do card: é o mesmo
            código visual de "que tipo de bloco é este" usado no canvas. */}
        <span
          style={{
            width: 42,
            height: 42,
            borderRadius: 11,
            background: tema.categoria[cor],
            display: "flex",
            alignItems: "center",
            justifyContent: "center",
            flex: "none",
            color: tema.cor.tinta,
          }}
        >
          <svg
            viewBox="0 0 24 24"
            style={{
              width: 22,
              height: 22,
              fill: "none",
              stroke: "currentColor",
              strokeWidth: 2,
              strokeLinecap: "round",
              strokeLinejoin: "round",
            }}
            dangerouslySetInnerHTML={{ __html: ICONES.package }}
          />
        </span>
        <div style={{ display: "flex", flexDirection: "column", gap: 2, minWidth: 0 }}>
          <span
            style={{
              fontFamily: tema.fonte.mono,
              fontWeight: 500,
              fontSize: 27,
              color: tema.cor.frente,
            }}
          >
            {nome}
          </span>
          <span
            style={{
              fontFamily: tema.fonte.corpo,
              fontWeight: 500,
              fontSize: 21,
              color: tema.cor.fraco,
            }}
          >
            {o_que}
          </span>
        </div>
      </div>
    </Entrada>
  );
};

export const Colecoes: React.FC = () => {
  const quadro = useCurrentFrame();
  const { durationInFrames } = useVideoConfig();

  return (
    <AbsoluteFill style={estiloDeCena(quadro, durationInFrames)}>
      <Malha />
      <Pontos escala={1.6} />
      <AbsoluteFill>
        <AbsoluteFill
          style={{
            alignItems: "center",
            justifyContent: "center",
            flexDirection: "column",
            gap: 46,
          }}
        >
          <Palavras
            texto="baixe os blocos em coleções"
            atraso={2}
            porPalavra={3}
            destacar={["coleções"]}
            estilo={{
              fontFamily: tema.fonte.display,
              fontWeight: 800,
              fontSize: 74,
              letterSpacing: "-0.035em",
              color: tema.cor.frente,
            }}
          />
          <div
            style={{
              display: "grid",
              gridTemplateColumns: "repeat(2, auto)",
              gap: 18,
            }}
          >
            {COLECOES.map((c, i) => (
              <Ficha key={c.nome} nome={c.nome} o_que={c.o_que} cor={c.cor} i={i} />
            ))}
          </div>
          {/* 34 e não 58: a cena tem 108 quadros e sai nos últimos 10, então
              entrar em 58 deixava o comando legível por pouco mais de um
              segundo — tempo de ver que há uma linha ali, não de ler. */}
          <Entrada atraso={34} mola={tema.mola.suave} sobe={22}>
            <div
              style={{
                fontFamily: tema.fonte.mono,
                fontSize: 26,
                color: tema.cor.fraco,
                background: tema.cor.campo,
                border: `1px solid ${tema.cor.linha}`,
                borderRadius: 10,
                padding: "13px 22px",
              }}
            >
              <span style={{ color: tema.cor.fraco }}>pak::pak(</span>
              <span style={{ color: tema.cor.frente }}>"uaipedro/trama"</span>
              <span style={{ color: tema.cor.fraco }}>)</span>
            </div>
          </Entrada>
        </AbsoluteFill>
      </AbsoluteFill>
    </AbsoluteFill>
  );
};
