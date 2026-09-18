// src/scenes/Outro.tsx — o fechamento: uma ação só.
//
// Duas chamadas ("instale" e "veja a documentação") dividem a atenção e
// resultam em nenhuma. Aqui fica o endereço, com o comando de instalação logo
// abaixo porque é literalmente o próximo passo de quem acabou de ver o fluxo
// ser montado.
import React from "react";
import { AbsoluteFill, useCurrentFrame, useVideoConfig } from "remotion";
import { tema } from "../theme";
import { Malha, Pontos } from "../lib/camadas";
import { Entrada, estiloDeCena, respiro } from "../lib/movimento";
import { Marca } from "../trama/Marca";

export const Outro: React.FC<{
  // O vídeo da coleção `models` fecha mandando instalar a `models`, não o
  // núcleo: quem acabou de ver uma ANOVA quer os blocos da ANOVA.
  pacote?: string;
}> = ({ pacote = "uaipedro/trama" }) => {
  const quadro = useCurrentFrame();
  const { durationInFrames } = useVideoConfig();
  const r = respiro(quadro);

  return (
    // A saída daqui é a do VÍDEO: 12 quadros escurecendo, em vez de corte seco
    // no preto — que num laço de rede social lê como travamento. A entrada de 8
    // cobre o corte vindo da cena de código.
    <AbsoluteFill style={estiloDeCena(quadro, durationInFrames, { entra: 8, sai: 12 })}>
      <Malha />
      <Pontos escala={1.6} />
      <AbsoluteFill
        style={{
          alignItems: "center",
          justifyContent: "center",
          flexDirection: "column",
          gap: 30,
        }}
      >
        <div style={{ transform: `translateY(${r.flutua}px)` }}>
          <Marca altura={168} desenhar={false} />
        </div>
        <Entrada atraso={10} mola={tema.mola.seca} sobe={26}>
          <div
            style={{
              fontFamily: tema.fonte.display,
              fontWeight: 800,
              fontSize: 62,
              letterSpacing: "-0.03em",
              color: tema.cor.destaque,
              // O brilho é do endereço, e é o único do quadro: é a ação.
              textShadow: `0 0 54px ${tema.cor.brilho}`,
            }}
          >
            github.com/uaipedro/trama
          </div>
        </Entrada>
        <Entrada atraso={26} mola={tema.mola.suave} sobe={20}>
          <div
            style={{
              fontFamily: tema.fonte.mono,
              fontSize: 27,
              background: tema.cor.campo,
              border: `1px solid ${tema.cor.linha}`,
              borderRadius: 10,
              padding: "14px 24px",
            }}
          >
            <span style={{ color: tema.cor.fraco }}>pak::pak(</span>
            <span style={{ color: tema.cor.frente }}>{`"${pacote}"`}</span>
            <span style={{ color: tema.cor.fraco }}>)</span>
          </div>
        </Entrada>
      </AbsoluteFill>
    </AbsoluteFill>
  );
};
