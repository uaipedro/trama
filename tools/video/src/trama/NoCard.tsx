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
import {
  M,
  alturaCard,
  alturaLinhaParam,
  alturaPreview,
  larguraCard,
  yPorta,
} from "./metricas";
import { Preview } from "./previews";
import { valorParam, type NoFluxo, type Param } from "./tipos";

const Icone: React.FC<{ nome: string }> = ({ nome }) => (
  <svg
    className="tr-node-icon"
    viewBox="0 0 24 24"
    dangerouslySetInnerHTML={{ __html: ICONES[nome] ?? "" }}
  />
);

// Barra de progresso do estado `running`, igual à do `Preview` do editor. A
// fração é função do quadro: uma animação de CSS não avançaria num render
// quadro a quadro, e a barra sairia congelada no mesmo lugar em todos eles.
const Computando: React.FC<{ entra: number; resulta: number }> = ({ entra, resulta }) => {
  const quadro = useCurrentFrame();
  const fracao = interpolate(quadro, [entra + 2, resulta], [0.05, 0.97], {
    easing: tema.ease.saida,
    extrapolateLeft: "clamp",
    extrapolateRight: "clamp",
  });
  return (
    <div className="tr-progress">
      <div className="tr-progress-fill" style={{ width: `${Math.round(fracao * 100)}%` }} />
      <span className="tr-progress-msg">computando…</span>
    </div>
  );
};

// Valor digitado caractere a caractere. Antes de `digita` o campo mostra o
// placeholder — que no trama é o `example` do catálogo, não o rótulo repetido.
function digitado(valor: string, quadro: number, digita?: number): string {
  if (digita === undefined) return valor;
  if (quadro < digita) return "";
  const porChar = 1.15;
  const n = Math.floor((quadro - digita) / porChar);
  return valor.slice(0, Math.min(n, valor.length));
}

const LinhaParam: React.FC<{ no: NoFluxo; param: Param; atraso?: number }> = ({
  no,
  param,
  atraso,
}) => {
  const quadro = useCurrentFrame();
  const alvo = valorParam(no, param);
  const altura = alturaLinhaParam(param.tipo);

  if (param.tipo === "enum-inline" || param.tipo === "enum-largo") {
    const largo = param.tipo === "enum-largo";
    return (
      <div
        className="tr-param"
        style={
          largo
            ? { height: altura, gridTemplateColumns: "1fr", alignItems: "start" }
            : { height: altura }
        }
      >
        <span>{param.rotulo}</span>
        <div className={"tr-seg" + (largo ? " tr-seg-wide" : "")} role="radiogroup">
          {(param.opcoes ?? []).map((o) => (
            <button key={o} type="button" className={o === alvo ? "tr-seg-on" : undefined}>
              {o}
            </button>
          ))}
        </div>
      </div>
    );
  }

  // `enum-select` é o formato que `layoutEnum` escolhe quando há opção demais
  // para caber — o caso do "Conjunto" da coleção `models`, com onze exemplos.
  if (param.tipo === "enum-select") {
    return (
      <div className="tr-param" style={{ height: altura }}>
        <span>{param.rotulo}</span>
        <select value={alvo} disabled>
          {(param.opcoes ?? [alvo]).map((o) => (
            <option key={o} value={o}>
              {o}
            </option>
          ))}
        </select>
      </div>
    );
  }

  const texto = digitado(alvo, quadro, atraso);
  // Cursor só enquanto digita: deixá-lo aceso depois faria todo card terminado
  // parecer um campo em edição.
  const digitando = atraso !== undefined && quadro >= atraso && texto.length < alvo.length;

  if (param.tipo === "expr") {
    return (
      <div className="tr-param" style={{ height: altura, alignItems: "start" }}>
        <span>{param.rotulo}</span>
        <textarea
          className="tr-expr"
          rows={2}
          readOnly
          value={texto}
          placeholder={param.exemplo ?? param.rotulo}
          style={{
            height: altura,
            resize: "none",
            borderColor: digitando ? tema.cor.destaque : undefined,
          }}
        />
      </div>
    );
  }

  return (
    <div className="tr-param" style={{ height: altura }}>
      <span>{param.rotulo}</span>
      <input
        type="text"
        readOnly
        value={texto}
        placeholder={param.exemplo ?? ""}
        style={{ borderColor: digitando ? tema.cor.destaque : undefined }}
      />
    </div>
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

export const NoCard: React.FC<{ no: NoFluxo; ativo: boolean }> = ({ no, ativo }) => {
  const quadro = useCurrentFrame();
  const { fps } = useVideoConfig();
  const spec = no.spec;
  const cor = tema.categoria[spec.categoria];
  const vista = no.vista ?? 0;

  // Entrada em três propriedades ao mesmo tempo, como todo o resto do projeto:
  // opacidade, escala e uma subida curta. O card cai no lugar; não desbota
  // dentro dele.
  const p = spring({ frame: quadro - no.entra, fps, config: tema.mola.seca });
  const rodando = quadro < no.resulta;

  return (
    <div
      className={
        "tr-node" + (ativo ? " tr-node-sel" : "") + (rodando ? " tr-state-running" : "")
      }
      style={{
        width: larguraCard(no.tamanho),
        height: alturaCard(spec, no.tamanho),
        fontFamily: tema.fonte.card,
        opacity: p,
        transform:
          `translateY(${interpolate(p, [0, 1], [26, 0])}px)` +
          ` scale(${interpolate(p, [0, 1], [0.9, 1])})`,
        // O brilho é da cor-herói e só do card ATIVO: dois cards brilhando ao
        // mesmo tempo e o olho não sabe mais onde a cena quer que ele esteja.
        boxShadow: ativo
          ? `0 6px 20px rgba(0,0,0,.35), 0 0 46px ${tema.cor.brilho}`
          : undefined,
      }}
    >
      <div className="tr-node-head" style={{ background: cor, height: M.cabeca }}>
        <Icone nome={spec.icone} />
        <span className="tr-node-title">{no.rotulo ?? spec.rotulo}</span>
        <span className="tr-fold-btn">
          <Icone nome="chevron-down" />
        </span>
        <span className="tr-help-btn">?</span>
        {/* A duração só existe depois que o bloco rodou: mostrá-la antes seria
            o card afirmando um tempo que ele ainda não mediu. */}
        {rodando ? null : <span className="tr-dur">{no.duracao}</span>}
      </div>

      <div
        className={"tr-preview" + (rodando ? " tr-busy" : "")}
        style={{ height: alturaPreview(no.tamanho), overflow: "hidden" }}
      >
        {rodando ? (
          <Computando entra={no.entra} resulta={no.resulta} />
        ) : (
          <Preview resultado={no.resultado} desde={no.resulta} vista={vista} />
        )}
      </div>

      <Abas vistas={spec.vistas} atual={vista} />

      {spec.params.length ? (
        <div className="tr-params" style={{ padding: `${M.paramsPad}px 9px` }}>
          {spec.params.map((param, i) => (
            <LinhaParam
              key={param.nome}
              no={no}
              param={param}
              // Só o primeiro campo de TEXTO é digitado em cena; os outros já
              // nascem preenchidos. Digitar três campos por card gastaria a
              // cena inteira em datilografia.
              atraso={no.digita !== undefined && i === 0 ? no.digita : undefined}
            />
          ))}
        </div>
      ) : null}

      <div className="tr-ports">
        <div className="tr-in">
          {spec.entradas.map((porta) => (
            <div key={porta.nome} className="tr-port">
              <span>{porta.nome}</span>
            </div>
          ))}
        </div>
        <div className="tr-out">
          {spec.saidas.map((porta) => (
            <div key={porta.nome} className="tr-port tr-port-out">
              <span>{porta.nome}</span>
            </div>
          ))}
        </div>
      </div>
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
  no.spec.entradas.forEach((porta, i) =>
    pontos.push(bolinha("in" + porta.nome, no.x, no.y + yPorta(no.spec, i, no.tamanho))),
  );
  no.spec.saidas.forEach((porta, i) =>
    pontos.push(
      bolinha(
        "out" + porta.nome,
        no.x + larguraCard(no.tamanho),
        no.y + yPorta(no.spec, i, no.tamanho),
      ),
    ),
  );
  return <>{pontos}</>;
};
