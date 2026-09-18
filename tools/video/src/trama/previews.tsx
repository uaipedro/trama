// src/trama/previews.tsx — o que cada card desenha dentro da faixa de preview.
//
// Um por renderer do app: a tabela do núcleo (`trama/table`), o card do modelo
// e o quadro de efeitos da coleção `models` (`models/fit`, `models/effects`,
// vistas `ajuste`, `quadro` e `significância`) e o gráfico de médias.
//
// As classes são as dos renderers de verdade (`inst/www/runtime.js` e
// `collections/trama.models/inst/trama/index.js`), e a aparência vem de
// `trama.css` + `models.css` copiados. A ÚNICA coisa desenhada de novo aqui é o
// gráfico de médias: no app ele é um PNG que o ggplot2 gera pelo `trama.view`, e
// um renderizador de vídeo não roda R — então ele é redesenhado em SVG com a
// paleta do tema `claro` (`R/theme.R`), o que é uma representação fiel do
// desenho, não o arquivo que o R produz.
import React from "react";
import { interpolate, spring, useCurrentFrame, useVideoConfig } from "remotion";
import { tema } from "../theme";
import { Estrelas, Regua, estrelas, num } from "./Regua";
import type { CardDeModelo, Medias, Quadro, Resultado, Tabela } from "./tipos";

// A mesma formatação de célula do runtime (`fmt`): inteiro sai inteiro, o resto
// vai a três casas. É o detalhe que faz a tabela do vídeo ser lida como a
// tabela do app, e não como números escritos à mão.
function celulaTabela(v: string | number): string {
  if (typeof v === "number") return Number.isInteger(v) ? String(v) : v.toFixed(3);
  return String(v);
}

const TabelaPreview: React.FC<{ tabela: Tabela; desde: number }> = ({ tabela, desde }) => {
  const quadro = useCurrentFrame();
  const { fps } = useVideoConfig();
  return (
    <div className="tr-table-wrap">
      <table className="tr-table">
        <thead>
          <tr>
            {tabela.colunas.map((c) => (
              <th key={c}>{c}</th>
            ))}
          </tr>
        </thead>
        <tbody>
          {tabela.linhas.map((linha, i) => {
            // As linhas chegam escalonadas em 1,6 quadro. No app elas aparecem
            // de uma vez; aqui o escalonamento é o que dá a leitura de "o
            // resultado está PREENCHENDO" em vez de "a imagem trocou".
            const p = spring({ frame: quadro - desde - i * 1.6, fps, config: tema.mola.seca });
            return (
              <tr
                key={i}
                style={{
                  opacity: p,
                  transform: `translateX(${interpolate(p, [0, 1], [-7, 0])}px)`,
                }}
              >
                {tabela.colunas.map((c, j) => (
                  <td key={c}>{celulaTabela(linha[j])}</td>
                ))}
              </tr>
            );
          })}
        </tbody>
      </table>
    </div>
  );
};

// ---- models/fit, vista `ajuste` -------------------------------------------

const Destaque: React.FC<{
  rotulo: string;
  valor: number;
  barra?: boolean;
  pct?: boolean;
  desde: number;
}> = ({ rotulo, valor, barra, pct, desde }) => {
  const quadro = useCurrentFrame();
  const largura = interpolate(quadro, [desde, desde + 20], [0, Math.max(0, Math.min(1, valor))], {
    easing: tema.ease.saida,
    extrapolateLeft: "clamp",
    extrapolateRight: "clamp",
  });
  return (
    <div className="tr-mf-dest">
      <div className="tr-mf-dest-valor">{pct ? `${num(valor, 1)}%` : num(valor)}</div>
      {barra ? (
        <div className="tr-mf-dest-trilho">
          <div className="tr-mf-dest-barra" style={{ width: `${largura * 100}%` }} />
        </div>
      ) : null}
      <div className="tr-mt-rotulo">{rotulo}</div>
    </div>
  );
};

const CardModelo: React.FC<{ modelo: CardDeModelo; desde: number }> = ({ modelo, desde }) => (
  <div className="tr-mf">
    <div className="tr-mt-topo">
      <span className="tr-mt-nome">{modelo.rotulo}</span>
      <span className="tr-mt-rotulo">{`n = ${modelo.n}`}</span>
    </div>
    <div className="tr-mf-formula">{modelo.formula}</div>
    {/* `.slice(0, 3)`: é o que o renderer faz. O card do modelo mostra três
        destaques, e o quarto (o AIC) fica para a vista `efeitos`. */}
    <div className="tr-mf-destaques">
      {modelo.destaques.slice(0, 3).map((d, i) => (
        <Destaque key={d.rotulo} {...d} desde={desde + 4 + i * 3} />
      ))}
    </div>
    <div className="tr-mf-global">
      <div className="tr-mt-topo">
        <span className="tr-mt-rotulo">{`${modelo.global.rotulo} · p = ${num(modelo.global.p)}`}</span>
        <Estrelas est={estrelas(modelo.global.p)} />
      </div>
      <Regua p={modelo.global.p} mini desde={desde + 8} />
    </div>
  </div>
);

// ---- models/effects -------------------------------------------------------

const Rodape: React.FC<{ rodape?: Record<string, string> }> = ({ rodape }) => {
  if (!rodape) return null;
  return (
    <div className="tr-sig-rodape">
      {Object.keys(rodape).map((k) => (
        <span key={k}>
          <b>{k}</b> {rodape[k]}
        </span>
      ))}
    </div>
  );
};

// Vista `quadro`: FV, GL, SQ, QM, Fc e Pr > F, com a régua e a estrela DENTRO
// da coluna do p. É a vista de abertura porque é a que se confere.
const QuadroEfeitos: React.FC<{ quadro: Quadro; desde: number }> = ({ quadro: q, desde }) => {
  const quadro = useCurrentFrame();
  const { fps } = useVideoConfig();
  return (
    <div className="tr-me">
      <div className="tr-mt-nome">{q.titulo}</div>
      <div className="tr-quadro-wrap">
        <table className="tr-quadro">
          <thead>
            <tr>
              {q.colunas.map((c, i) => (
                <th key={c} className={i ? "tr-q-num" : ""}>
                  {c}
                </th>
              ))}
              <th className="tr-q-num" colSpan={3}>
                {q.rotuloP}
              </th>
            </tr>
          </thead>
          <tbody>
            {q.linhas.map((linha, i) => {
              const p = spring({ frame: quadro - desde - i * 1.6, fps, config: tema.mola.seca });
              const total = linha.termo === "Total";
              const residuo = linha.termo.startsWith("Resíduo");
              const est = estrelas(linha.p);
              return (
                <tr
                  key={linha.termo}
                  className={(total ? "tr-q-total" : "") + (residuo ? " tr-q-residuo" : "")}
                  style={{
                    opacity: p,
                    transform: `translateX(${interpolate(p, [0, 1], [-7, 0])}px)`,
                  }}
                >
                  <td className="tr-q-fv">{linha.termo}</td>
                  {linha.valores.map((v, j) => (
                    <td key={j} className="tr-q-num">
                      {v == null ? "" : (q.inteiras ?? []).includes(j) ? String(v) : num(v, 4)}
                    </td>
                  ))}
                  <td className="tr-q-num">{linha.p == null ? "" : num(linha.p)}</td>
                  <td className="tr-q-regua">
                    {linha.p == null ? null : (
                      <Regua p={linha.p} mini desde={desde + 6 + i * 3} />
                    )}
                  </td>
                  <td className="tr-q-est">
                    {linha.p == null ? null : <Estrelas est={est} />}
                  </td>
                </tr>
              );
            })}
          </tbody>
        </table>
      </div>
      <Rodape rodape={q.rodape} />
    </div>
  );
};

// Vista `significância`: uma linha por termo — nome, régua, p e estrela. É a
// leitura de relance, e a única que caberia legível num quadro de dez
// comparações.
const Significancia: React.FC<{ quadro: Quadro; desde: number }> = ({ quadro: q, desde }) => {
  const quadro = useCurrentFrame();
  const { fps } = useVideoConfig();
  return (
    <div className="tr-me">
      <div className="tr-mt-nome">{q.titulo}</div>
      <div className="tr-sig">
        {q.linhas.map((linha, i) => {
          const p = spring({ frame: quadro - desde - i * 1.4, fps, config: tema.mola.seca });
          return (
            <div
              key={linha.termo}
              className={"tr-sig-linha" + (linha.p == null ? " tr-sig-apagada" : "")}
              style={{ opacity: p }}
            >
              <span className="tr-sig-termo">{linha.termo}</span>
              {linha.p == null ? (
                <span />
              ) : (
                <Regua p={linha.p} mini desde={desde + 4 + i * 2} />
              )}
              <span className="tr-sig-p">{linha.p == null ? "—" : num(linha.p)}</span>
              <span className="tr-sig-e">
                {linha.p == null ? "" : <Estrelas est={estrelas(linha.p)} />}
              </span>
            </div>
          );
        })}
      </div>
      <Rodape rodape={q.rodape} />
    </div>
  );
};

// ---- o gráfico de médias --------------------------------------------------

// O tema `claro` embutido (`R/theme.R`, `.tr_temas_embutidos`). É este que o
// vídeo usa: o gráfico é a figura que vai para o artigo, e figura de artigo é
// clara — o que também a separa do canvas escuro em volta.
const CLARO = {
  fundo: "#ffffff",
  texto: "#1f2937",
  eixos: "#4b5563",
  grade: "#e5e7eb",
};
// `.TR_MODELS_COR` (collections/trama.models/R/comum.R): a cor de série única
// do `tr_models_plot_means`.
const COR_SERIE = "#14b8a6";

const GraficoMedias: React.FC<{ medias: Medias; desde: number }> = ({ medias, desde }) => {
  const quadro = useCurrentFrame();
  const { fps } = useVideoConfig();
  const L = 1600;
  const A = 900;
  const margem = { esq: 165, dir: 40, topo: 34, base: 120 };
  const px0 = margem.esq;
  const px1 = L - margem.dir;
  const py0 = margem.topo;
  const py1 = A - margem.base;

  const n = medias.pontos.length;
  // Escala discreta do ggplot: expansão aditiva de 0,6 em cada ponta.
  const xDe = 0.4;
  const xAte = n + 0.6;
  const px = (i: number) => px0 + ((i + 1 - xDe) / (xAte - xDe)) * (px1 - px0);

  // Escala contínua com `expansion(mult = c(.05, .12))` — a folga de cima é
  // maior porque as LETRAS são desenhadas acima do limite superior do IC.
  const lo = Math.min(...medias.pontos.map((p) => p.li));
  const hi = Math.max(...medias.pontos.map((p) => p.ls));
  const amp = hi - lo;
  const yDe = lo - amp * 0.05;
  const yAte = hi + amp * 0.12;
  const py = (v: number) => py1 - ((v - yDe) / (yAte - yDe)) * (py1 - py0);

  // Cortes "bonitos" de meio em meio, que é o que o `scales` escolhe nesta
  // amplitude.
  const cortes: number[] = [];
  for (let v = Math.ceil(yDe * 2) / 2; v <= yAte; v += 0.5) cortes.push(Number(v.toFixed(1)));

  return (
    <div className="tr-img-wrap">
      <svg viewBox={`0 0 ${L} ${A}`} className="tr-img" style={{ background: CLARO.fundo }}>
        {/* `theme_minimal`: sem moldura, só a grade. */}
        {cortes.map((v) => (
          <line
            key={v}
            x1={px0}
            x2={px1}
            y1={py(v)}
            y2={py(v)}
            stroke={CLARO.grade}
            strokeWidth={2}
          />
        ))}
        {medias.pontos.map((p, i) => (
          <line
            key={p.nivel}
            x1={px(i)}
            x2={px(i)}
            y1={py0}
            y2={py1}
            stroke={CLARO.grade}
            strokeWidth={2}
          />
        ))}

        {cortes.map((v) => (
          <text
            key={v}
            x={px0 - 16}
            y={py(v) + 10}
            textAnchor="end"
            fontSize={28}
            fill={CLARO.eixos}
            fontFamily="sans-serif"
          >
            {/* Ponto decimal: o rótulo do eixo é do `scales` do ggplot, que não
                segue a localidade do resto da interface. */}
            {v.toFixed(1)}
          </text>
        ))}

        {medias.pontos.map((p, i) => {
          // Cada tratamento entra escalonado: barra de erro, ponto e letra, na
          // ordem em que as camadas do ggplot são somadas.
          const e = spring({ frame: quadro - desde - i * 4, fps, config: tema.mola.seca });
          const cap = ((px1 - px0) / (xAte - xDe)) * 0.15 * 0.5;
          const yMedia = py(p.media);
          const yLi = py(p.li);
          const yLs = py(p.ls);
          return (
            <g key={p.nivel} opacity={e}>
              <line
                x1={px(i)}
                x2={px(i)}
                y1={yLi}
                y2={yLs}
                stroke={COR_SERIE}
                strokeWidth={4}
              />
              <line
                x1={px(i) - cap}
                x2={px(i) + cap}
                y1={yLs}
                y2={yLs}
                stroke={COR_SERIE}
                strokeWidth={4}
              />
              <line
                x1={px(i) - cap}
                x2={px(i) + cap}
                y1={yLi}
                y2={yLi}
                stroke={COR_SERIE}
                strokeWidth={4}
              />
              <circle
                cx={px(i)}
                cy={yMedia}
                r={interpolate(e, [0, 1], [3, 11])}
                fill={COR_SERIE}
              />
              <text
                x={px(i)}
                y={yLs - 18}
                textAnchor="middle"
                fontSize={34}
                fontWeight={600}
                fill={CLARO.texto}
                fontFamily="sans-serif"
              >
                {p.grupo}
              </text>
              <text
                x={px(i)}
                y={py1 + 40}
                textAnchor="middle"
                fontSize={28}
                fill={CLARO.eixos}
                fontFamily="sans-serif"
              >
                {p.nivel}
              </text>
            </g>
          );
        })}

        <text
          x={(px0 + px1) / 2}
          y={A - 24}
          textAnchor="middle"
          fontSize={30}
          fill={CLARO.texto}
          fontFamily="sans-serif"
        >
          {medias.fator}
        </text>
        <text
          x={34}
          y={(py0 + py1) / 2}
          textAnchor="middle"
          fontSize={30}
          fill={CLARO.texto}
          fontFamily="sans-serif"
          transform={`rotate(-90 34 ${(py0 + py1) / 2})`}
        >
          {medias.rotuloY}
        </text>
      </svg>
    </div>
  );
};

// O despachante. `vista` é o índice da aba corrente: o quadro de efeitos tem
// duas vistas, e qual delas está aberta é escolha do documento.
export const Preview: React.FC<{ resultado: Resultado; desde: number; vista: number }> = ({
  resultado,
  desde,
  vista,
}) => {
  switch (resultado.tipo) {
    case "tabela":
      return <TabelaPreview tabela={resultado.tabela} desde={desde} />;
    case "modelo":
      return <CardModelo modelo={resultado.modelo} desde={desde} />;
    case "quadro":
      return vista === 1 ? (
        <Significancia quadro={resultado.quadro} desde={desde} />
      ) : (
        <QuadroEfeitos quadro={resultado.quadro} desde={desde} />
      );
    case "grafico":
      return <GraficoMedias medias={resultado.medias} desde={desde} />;
  }
};
