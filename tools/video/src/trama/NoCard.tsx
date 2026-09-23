// src/trama/NoCard.tsx — o card de um bloco, com o markup do editor.
//
// As classes são as de `inst/www/editor.js` (`NdNode`) e a aparência vem de
// `trama.css` + `models.css`, importados sem alteração. O que este arquivo
// acrescenta é só TEMPO: quando o card assenta, quando o param é digitado,
// quando o resultado substitui a barra de progresso. Nada de cor ou espaçamento
// é decidido aqui — se o editor mudar de cara, `scripts/sincronizar.sh` traz a
// mudança.
//
// Serve aos dois vídeos: o que muda entre eles é o catálogo, não o card.
import React from "react";
import { interpolate, spring, useCurrentFrame, useVideoConfig } from "remotion";
import { tema } from "../theme";
import { ICONES } from "./icones";
import { M, alturaLinhaParam, alturaPreview, rotuloLongo } from "./metricas";
import { anel, destaqueEm, execucaoEm, geometriaEm, modoEm, rotuloDe, valorEm } from "./estado";
import { MODOS, mostraParams, mostraPreview, type Modo } from "./modos-app.js";
import { Preview } from "./previews";
import type { NoFluxo, Param } from "./tipos";

const Icone: React.FC<{ nome: string; principal?: boolean }> = ({ nome, principal }) => (
  <svg
    className={"tr-node-icon" + (principal ? " tr-node-icon-main" : "")}
    viewBox="0 0 24 24"
    dangerouslySetInnerHTML={{ __html: ICONES[nome] ?? "" }}
  />
);

// Uma linha de parâmetro, no card ou no painel (`solto`: sem altura
// declarada, porque no painel o rótulo vai sempre por cima e quebra linha).
export const LinhaParam: React.FC<{
  no: NoFluxo;
  param: Param;
  atraso?: number;
  solto?: boolean;
}> = ({ no, param, atraso, solto }) => {
  const quadro = useCurrentFrame();
  const { valor: texto, digitando } = valorEm(no, param, quadro, atraso);
  const alvo = texto;
  const altura = solto ? undefined : alturaLinhaParam(param.tipo, param);
  const cls = "tr-param" + (rotuloLongo(param) ? " tr-param-longo" : "");
  const destaque = anel(destaqueEm(no, "param:" + param.nome, quadro));
  const linha = (estilo: React.CSSProperties, filhos: React.ReactNode) => (
    <div className={cls} style={{ height: altura, ...estilo, ...destaque }}>
      <span>{param.rotulo}</span>
      {filhos}
    </div>
  );

  if (param.tipo === "enum-inline" || param.tipo === "enum-largo") {
    const largo = param.tipo === "enum-largo";
    return linha(
      largo ? { gridTemplateColumns: "1fr", alignItems: "start" } : {},
      <div className={"tr-seg" + (largo ? " tr-seg-wide" : "")} role="radiogroup">
        {(param.opcoes ?? []).map((o) => (
          <button key={o} type="button" className={o === alvo ? "tr-seg-on" : undefined}>
            {o}
          </button>
        ))}
      </div>,
    );
  }

  // `enum-select` é o formato que `layoutEnum` escolhe quando há opção demais
  // para caber — o caso do "Conjunto" da coleção `models`, com onze exemplos.
  if (param.tipo === "enum-select") {
    return linha(
      {},
      <select value={alvo} disabled>
        {(param.opcoes ?? [alvo]).map((o) => (
          <option key={o} value={o}>
            {o}
          </option>
        ))}
      </select>,
    );
  }

  // Borda acesa só enquanto digita: depois, todo card terminado pareceria um
  // campo em edição.
  const borda = digitando ? tema.cor.destaque : undefined;
  if (param.tipo === "expr") {
    return linha(
      { alignItems: "start" },
      <textarea
        className="tr-expr"
        rows={2}
        readOnly
        value={texto}
        placeholder={param.exemplo ?? param.rotulo}
        style={{ height: solto ? 38 : M.linha.expr, resize: "none", borderColor: borda }}
      />,
    );
  }

  return linha(
    {},
    <input type="text" readOnly value={texto} placeholder={param.exemplo ?? ""}
      style={{ borderColor: borda }} />,
  );
};

// A faixa de abas é SEMPRE reservada, inclusive com uma vista só — é o que
// impede a altura do card mudar quando o resultado chega. Com uma vista ela
// exibe o nome dela; com duas ou mais, botões, e a corrente sublinhada.
const Abas: React.FC<{ vistas: string[]; atual: number }> = ({ vistas, atual }) => (
  <div className="tr-tabs" style={{ height: M.abas - 1 }}>
    {vistas.length === 0 ? (
      <span className="tr-tab-idle">—</span>
    ) : vistas.length === 1 ? (
      <span className="tr-tab-only">{vistas[0]}</span>
    ) : (
      vistas.map((v, i) => (
        <button key={v} className={"tr-tab" + (i === atual ? " tr-tab-on" : "")}>
          {v}
        </button>
      ))
    )}
  </div>
);

// O ícone do modo, igual ao `ModoIcone` de modos-ui.js: dois retângulos
// empilhados, cheio = parte visível; o mini é um quadradinho só.
export const ModoIcone: React.FC<{ modo: Modo }> = ({ modo }) => {
  const r = (y: number, cheio: boolean) => (
    <rect key={y} x={2} y={y} width={12} height={5} rx={1}
      fill={cheio ? "currentColor" : "none"} stroke="currentColor" strokeWidth={1.4} />
  );
  return (
    <svg viewBox="0 0 16 16" width={14} height={14} aria-hidden>
      {modo === "mini" ? (
        <rect x={5} y={5} width={6} height={6} rx={1} fill="currentColor" />
      ) : (
        [r(2, modo !== "params"), r(9, modo !== "preview")]
      )}
    </svg>
  );
};

export const ModoPicker: React.FC<{ valor: Modo; destaque?: number; estilo?: React.CSSProperties }> = ({
  valor,
  destaque = 0,
  estilo,
}) => (
  <div className="tr-modo-picker" role="group" style={{ ...estilo, ...anel(destaque) }}>
    {MODOS.map((m) => (
      <button key={m} type="button" className={"tr-modo-btn" + (valor === m ? " tr-on" : "")}>
        <ModoIcone modo={m} />
      </button>
    ))}
  </div>
);

// Um card inteiro num modo. Durante a troca de modo o `NoCard` empilha dois
// destes (o de saída e o de chegada) e cruza a opacidade: os dois leiautes são
// diferentes demais (o mini é coluna, os outros são linha) pra interpolar CSS.
const Corpo: React.FC<{
  no: NoFluxo;
  modo: Modo;
  w: number;
  h: number;
  ativo: boolean;
  opacidade: number;
}> = ({ no, modo, w, h, ativo, opacidade }) => {
  const quadro = useCurrentFrame();
  const spec = no.spec;
  const cor = tema.categoria[spec.categoria];
  const vista = no.vista ?? 0;
  const mini = modo === "mini";
  const semPreview = !mostraPreview(modo);
  const exec = execucaoEm(no, quadro);
  const fracao = !exec.rodando ? 1 : interpolate(quadro, [exec.de + 2, exec.ate], [0.05, 0.97], {
    easing: tema.ease.saida,
    extrapolateLeft: "clamp",
    extrapolateRight: "clamp",
  });
  const estado = exec.rodando ? "running" : "done";

  return (
    <div
      className={[
        "tr-node",
        ativo ? "tr-node-sel" : "",
        exec.rodando ? "tr-state-running" : "",
        mini ? "tr-node-mini" : "",
      ].filter(Boolean).join(" ")}
      style={{
        position: "absolute",
        left: 0,
        top: 0,
        width: w,
        minWidth: 0,
        height: h,
        fontFamily: tema.fonte.card,
        opacity: opacidade,
        // O brilho é da cor-herói e só do card ATIVO: dois cards brilhando ao
        // mesmo tempo e o olho não sabe mais onde a cena quer que ele esteja.
        boxShadow: ativo ? `0 6px 20px rgba(0,0,0,.35), 0 0 46px ${tema.cor.brilho}` : undefined,
      }}
    >
      <div className="tr-node-head" style={{ background: cor, height: mini ? undefined : M.cabeca }}>
        <Icone nome={spec.icone} principal />
        <span className="tr-node-title">{rotuloDe(no)}</span>
        {mini ? <span className={`tr-mini-dot tr-${estado}`} /> : null}
        {/* A duração só existe depois que o bloco rodou. */}
        {exec.rodando || mini ? null : <span className="tr-dur">{no.duracao}</span>}
        {!mini && semPreview && exec.rodando ? (
          <div className="tr-head-progress">
            <div style={{ width: `${Math.round(fracao * 100)}%` }} />
          </div>
        ) : null}
        {mini ? null : <ModoPicker valor={modo} destaque={destaqueEm(no, "modo", quadro)} />}
      </div>

      {semPreview ? null : (
        <>
          <div
            className={"tr-preview" + (exec.rodando ? " tr-busy" : "")}
            style={{ height: alturaPreview(no.tamanho), overflow: "hidden" }}
          >
            {exec.rodando ? (
              <div className="tr-progress">
                <div className="tr-progress-fill" style={{ width: `${Math.round(fracao * 100)}%` }} />
                <span className="tr-progress-msg">computando…</span>
              </div>
            ) : (
              <Preview resultado={exec.resultado} desde={exec.desde} vista={vista} />
            )}
          </div>
          <Abas vistas={spec.vistas} atual={vista} />
        </>
      )}

      {mostraParams(modo) && spec.params.length ? (
        <div className="tr-params" style={{ padding: `${M.paramsPad}px 9px` }}>
          {spec.params.map((param, i) => (
            <LinhaParam
              key={param.nome}
              no={no}
              param={param}
              // Só o primeiro campo é digitado na entrada; os outros nascem
              // preenchidos (ou mudam por `trocas`).
              atraso={no.digita !== undefined && i === 0 ? no.digita : undefined}
            />
          ))}
        </div>
      ) : null}

      <div className="tr-ports">
        <div className="tr-in">
          {spec.entradas.map((porta) => (
            <div key={porta.nome} className="tr-port">
              {mini ? null : <span>{porta.nome}</span>}
            </div>
          ))}
        </div>
        <div className="tr-out">
          {spec.saidas.map((porta) => (
            <div key={porta.nome} className="tr-port tr-port-out">
              {mini ? null : <span>{porta.nome}</span>}
            </div>
          ))}
        </div>
      </div>
      {/* A alça: no card sem preview ela só ajusta a largura. O mini tem
          largura automática e fica sem. */}
      {mini ? null : <div className={"tr-grip" + (semPreview ? " tr-grip-w" : "")} />}
    </div>
  );
};

export const NoCard: React.FC<{ no: NoFluxo; ativo: boolean }> = ({ no, ativo }) => {
  const quadro = useCurrentFrame();
  const { fps } = useVideoConfig();
  const { de, para, t } = modoEm(no, quadro);
  const g = geometriaEm(no, quadro);
  const destaqueModo = destaqueEm(no, "modo", quadro);
  const mini = (t < 0.5 ? de : para) === "mini";

  // Entrada em três propriedades ao mesmo tempo: opacidade, escala e uma
  // subida curta. O card cai no lugar; não desbota dentro dele.
  const p = spring({ frame: quadro - no.entra, fps, config: tema.mola.seca });

  return (
    <div
      style={{
        position: "relative",
        width: g.w,
        height: g.h,
        opacity: p,
        transform:
          `translateY(${interpolate(p, [0, 1], [26, 0])}px)` +
          ` scale(${interpolate(p, [0, 1], [0.9, 1])})`,
      }}
    >
      {t < 1 ? (
        <Corpo no={no} modo={de} w={g.w} h={g.h} ativo={ativo} opacidade={1 - t} />
      ) : null}
      <Corpo no={no} modo={para} w={g.w} h={g.h} ativo={ativo} opacidade={t < 1 ? t : 1} />
      {/* No mini o seletor mora ACIMA do card (`.tr-node-mini .tr-modo-picker`)
          e só aparece com o card selecionado. Fica fora do `.tr-node`, que é
          `overflow:hidden` e o cortaria. */}
      {mini && (ativo || destaqueModo > 0) ? (
        <ModoPicker
          valor={t < 0.5 ? de : para}
          destaque={destaqueModo}
          estilo={{
            position: "absolute",
            top: -26,
            left: "50%",
            transform: "translateX(-50%)",
            background: tema.cor.painel,
            color: tema.cor.frente,
            border: `1px solid ${tema.cor.linha}`,
          }}
        />
      ) : null}
    </div>
  );
};

// Os handles ficam FORA do card, desenhados pelo canvas: dentro dele o
// `overflow:hidden` do `.tr-node` cortaria a metade que fica por cima da borda,
// que é justamente a metade onde a aresta encosta.
export const Handles: React.FC<{ no: NoFluxo; visivel: number }> = ({ no, visivel }) => {
  const quadro = useCurrentFrame();
  const { fps } = useVideoConfig();
  const p = spring({ frame: quadro - visivel, fps, config: tema.mola.seca });
  const cor = tema.categoria[no.spec.categoria];
  const pontos: React.ReactNode[] = [];
  const bolinha = (chave: string, x: number, y: number) => (
    <div
      key={chave}
      className="react-flow__handle"
      style={{
        position: "absolute",
        left: x - M.handle / 2,
        top: y - M.handle / 2,
        width: M.handle,
        height: M.handle,
        borderRadius: "50%",
        background: cor,
        borderStyle: "solid",
        borderWidth: 2,
        borderColor: tema.cor.painel,
        opacity: p,
        transform: `scale(${interpolate(p, [0, 1], [0.4, 1])})`,
      }}
    />
  );
  const g = geometriaEm(no, quadro);
  no.spec.entradas.forEach((porta, i) =>
    pontos.push(bolinha("in" + porta.nome, no.x, no.y + g.portas("in", i))),
  );
  no.spec.saidas.forEach((porta, i) =>
    pontos.push(bolinha("out" + porta.nome, no.x + g.w, no.y + g.portas("out", i))),
  );
  return <>{pontos}</>;
};
