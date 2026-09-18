// src/scenes/Codigo.tsx — o mesmo fluxo em JSON e em R.
//
// O grafo continua na tela, pequeno à esquerda: sem ele os dois painéis seriam
// código sobre fundo escuro, e a afirmação ("é ISTO que está no disco") perde o
// referente. O canvas aqui não é remontado — é o estado final da cena anterior,
// trazido por um `Sequence` de deslocamento negativo, que adianta o relógio dos
// cards para depois de todos terem assentado.
import React from "react";
import { AbsoluteFill, Sequence, useCurrentFrame, useVideoConfig } from "remotion";
import { tema } from "../theme";
import { Malha } from "../lib/camadas";
import { Entrada, estiloDeCena, mover, respiro } from "../lib/movimento";
import { Legenda } from "../lib/Legenda";
import { Canvas } from "../trama/Canvas";
import { ARESTAS, NOS } from "../trama/catalogo";

// Quadro do fluxo, em tempo da cena anterior, em que tudo já assentou: o último
// resultado chega em 380 e a última linha de tabela em ~390.
const FLUXO_ASSENTADO = 430;

type Cor = "chave" | "texto" | "num" | "pont" | "fn" | "coment";

const CORES: Record<Cor, string> = {
  chave: tema.cor.frente,
  texto: tema.categoria.inspect,
  num: tema.categoria.sink,
  pont: tema.cor.fraco,
  // Azul-claro, e NÃO a cor-herói: o destaque é da legenda, e código colorido
  // com a cor-herói espalharia o foco por trinta tokens.
  fn: tema.categoria.transform,
  coment: tema.cor.fraco,
};

type Token = [string, Cor?];

const JSON_FLUXO: Token[][] = [
  [["{", "pont"]],
  [['  "format"', "chave"], [": ", "pont"], ["1", "num"], [",", "pont"]],
  [['  "collections"', "chave"], [": { ", "pont"], ['"data"', "chave"], [": ", "pont"], ['"0.1.0"', "texto"], [" },", "pont"]],
  [['  "nodes"', "chave"], [": {", "pont"]],
  [['    "ler"', "chave"], [": { ", "pont"], ['"type"', "chave"], [": ", "pont"], ['"data/read_csv"', "texto"], [",", "pont"]],
  [["             ", "pont"], ['"params"', "chave"], [": { ", "pont"], ['"path"', "chave"], [": ", "pont"], ['"vendas.csv"', "texto"], [" } },", "pont"]],
  [['    "cafe"', "chave"], [": { ", "pont"], ['"type"', "chave"], [": ", "pont"], ['"data/filter"', "texto"], [",", "pont"]],
  [["              ", "pont"], ['"params"', "chave"], [": { ", "pont"], ['"expr"', "chave"], [": ", "pont"], ['"produto == \\"cafe\\""', "texto"], [" } }", "pont"]],
  [["  },", "pont"]],
  [['  "edges"', "chave"], [": [{ ", "pont"], ['"from"', "chave"], [": {", "pont"], ['"node"', "chave"], [":", "pont"], ['"ler"', "texto"], [",", "pont"], ['"port"', "chave"], [":", "pont"], ['"out"', "texto"], ["},", "pont"]],
  [["             ", "pont"], ['"to"', "chave"], [": {", "pont"], ['"node"', "chave"], [":", "pont"], ['"cafe"', "texto"], [",", "pont"], ['"port"', "chave"], [":", "pont"], ['"data"', "texto"], ["} }]", "pont"]],
  [["}", "pont"]],
];

const R_FLUXO: Token[][] = [
  [["library", "fn"], ["(trama)", "pont"]],
  [[""]],
  [["tr_flow", "fn"], ["(reg) ", "pont"], ["|>", "fn"]],
  [["  tr_add", "fn"], ["(", "pont"], ['"ler"', "texto"], [", ", "pont"], ['"data/read_csv"', "texto"], [",", "pont"]],
  [["         path = ", "chave"], ['"vendas.csv"', "texto"], [") ", "pont"], ["|>", "fn"]],
  [["  tr_add", "fn"], ["(", "pont"], ['"cafe"', "texto"], [", ", "pont"], ['"data/filter"', "texto"], [", from = ", "chave"], ['"ler"', "texto"], [",", "pont"]],
  [["         expr = ", "chave"], ["'produto == \"cafe\"'", "texto"], [") ", "pont"], ["|>", "fn"]],
  [["  tr_add", "fn"], ["(", "pont"], ['"contar"', "texto"], [", ", "pont"], ['"data/group_summarise"', "texto"], [",", "pont"]],
  [["         from = ", "chave"], ['"cafe"', "texto"], [", by = ", "chave"], ['"regiao"', "texto"], [",", "pont"]],
  [["         name = ", "chave"], ['"n"', "texto"], [", expr = ", "chave"], ['"dplyr::n()"', "texto"], [")", "pont"]],
];

const Painel: React.FC<{
  titulo: string;
  linhas: Token[][];
  atraso: number;
  corpo: number;
}> = ({ titulo, linhas, atraso, corpo }) => {
  const quadro = useCurrentFrame();
  return (
    <Entrada atraso={atraso} mola={tema.mola.suave} sobe={34}>
      <div
        style={{
          width: 940,
          borderRadius: 14,
          overflow: "hidden",
          background: tema.cor.painel,
          border: `1px solid ${tema.cor.linha}`,
          boxShadow: "0 22px 60px rgba(0,0,0,.45)",
        }}
      >
        {/* O cabeçalho é o do card do editor em corpo maior: nome do arquivo à
            esquerda, nada à direita. */}
        <div
          style={{
            display: "flex",
            alignItems: "center",
            gap: 10,
            padding: "11px 18px",
            background: tema.cor.campo,
            borderBottom: `1px solid ${tema.cor.linha}`,
            fontFamily: tema.fonte.mono,
            fontSize: 20,
            color: tema.cor.fraco,
          }}
        >
          {titulo}
        </div>
        <div style={{ padding: "14px 18px", fontFamily: tema.fonte.mono, fontSize: corpo }}>
          {linhas.map((linha, i) => {
            // Uma linha por quadro: o código aparece de cima pra baixo, como se
            // estivesse sendo escrito, sem gastar a cena letra a letra. A cena
            // tem 154 quadros para dois painéis — 1,4 quadro por linha deixava
            // o segundo painel ainda vazio na metade dela.
            const o = mover(quadro, [atraso + 6 + i, atraso + 14 + i], [0, 1]);
            const dx = mover(quadro, [atraso + 6 + i, atraso + 14 + i], [-10, 0]);
            return (
              <div
                key={i}
                style={{
                  opacity: o,
                  transform: `translateX(${dx}px)`,
                  lineHeight: 1.62,
                  whiteSpace: "pre",
                }}
              >
                {linha.map(([txt, cor], j) => (
                  <span key={j} style={{ color: CORES[cor ?? "pont"] }}>
                    {txt}
                  </span>
                ))}
                {/* Linha vazia continua ocupando altura: sem isto o respiro
                    entre `library()` e o pipe desaparece. O escape, e não um
                    espaço literal, para o próximo leitor não "limpar" o que
                    parece sujeira e derrubar a linha em branco. */}
                {linha.length === 1 && linha[0][0] === "" ? "\u00a0" : null}
              </div>
            );
          })}
        </div>
      </div>
    </Entrada>
  );
};

export const Codigo: React.FC = () => {
  const quadro = useCurrentFrame();
  const { durationInFrames, height } = useVideoConfig();
  const r = respiro(quadro);

  return (
    <AbsoluteFill style={estiloDeCena(quadro, durationInFrames)}>
      <Malha />
      {/* O grafo, pequeno e à esquerda. A câmera continua centrando no meio do
          fluxo, mas o quadro dela agora é esta caixa, não a composição — por
          isso `largura`/`altura` são os da caixa.
          Escala 0,45: em 0,62 o grafo mede 763px e a caixa 760, então ele saía
          cortado nas duas pontas e lia como recorte, não como mapa. */}
      <div
        style={{
          position: "absolute",
          left: 0,
          top: 0,
          width: 820,
          height: 800,
          overflow: "hidden",
          // Esmaece à direita para o grafo não terminar numa borda reta colada
          // nos painéis.
          maskImage: "linear-gradient(90deg, #000 84%, transparent 100%)",
          WebkitMaskImage: "linear-gradient(90deg, #000 84%, transparent 100%)",
          opacity: mover(quadro, [0, 18], [0, 0.78]),
        }}
      >
        <Sequence from={-FLUXO_ASSENTADO} layout="none">
          <Canvas
            camera={{ cx: 615 + r.flutua * 2, cy: 283, s: 0.45 }}
            largura={820}
            altura={800}
            quadro={FLUXO_ASSENTADO + quadro}
            nos={NOS}
            arestas={ARESTAS}
          />
        </Sequence>
      </div>

      <div
        style={{
          position: "absolute",
          right: 70,
          top: 0,
          height,
          display: "flex",
          flexDirection: "column",
          justifyContent: "center",
          gap: 26,
        }}
      >
        <Painel titulo="meu-projeto/flows/main.json" linhas={JSON_FLUXO} atraso={2} corpo={20} />
        <Painel titulo="fluxo.R" linhas={R_FLUXO} atraso={24} corpo={21} />
      </div>

      {/* A legenda vai para a coluna da ESQUERDA, sob o grafo: o rodapé desta
          cena é dos painéis, e legenda por cima de código apaga justamente o
          que ela está apontando. */}
      <Legenda
        texto="o mesmo fluxo, em JSON e em R"
        de={48}
        ate={durationInFrames - 8}
        destacar={["JSON"]}
        caixa={{ left: 64, right: "auto", bottom: 170, justifyContent: "flex-start" }}
      />
    </AbsoluteFill>
  );
};
