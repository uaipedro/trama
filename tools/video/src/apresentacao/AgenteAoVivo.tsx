// src/apresentacao/AgenteAoVivo.tsx — segmento F da apresentação: o agente
// conversa à esquerda e o editor, gravado de verdade, monta a análise à direita.
//
// A tela é uma gravação do trama (Chrome headless) dirigida por um servidor MCP
// real repetindo as chamadas de uma sessão de agente gravada. O chat mostra a
// mesma sessão: pedido, chamadas e a resposta final, nos instantes em que cada
// chamada aconteceu na gravação. Nada aqui é número desenhado à mão.
import React from "react";
import {
  AbsoluteFill, Audio, Easing, interpolate, OffthreadVideo, Sequence, spring,
  staticFile, useCurrentFrame, useVideoConfig,
} from "remotion";
import { tema } from "../theme";
import dados from "./agente-ao-vivo.json";

const FPS = 30;
const CHAT = 480;
const fonte = "Inter, sans-serif";
const mono = "'Noto Sans Mono', monospace";

type Item =
  | { tipo: "pedido"; t: number; texto: string }
  | { tipo: "tool"; t: number; nome: string; alvo: string; fim?: number }
  | { tipo: "resposta"; t: number; texto: string };

export const DURACAO_AGENTE = Math.ceil(dados.duracao * FPS);

const Entra: React.FC<{ t: number; children: React.ReactNode }> = ({ t, children }) => {
  const frame = useCurrentFrame();
  const { fps } = useVideoConfig();
  const p = spring({ frame: frame - t * fps, fps, config: { damping: 200 } });
  if (frame < t * fps) return null;
  return <div style={{ opacity: p, transform: `translateY(${(1 - p) * 14}px)` }}>{children}</div>;
};

const Tool: React.FC<{ item: Extract<Item, { tipo: "tool" }> }> = ({ item }) => {
  const frame = useCurrentFrame();
  const rodando = item.fim !== undefined && frame < item.fim * FPS;
  return (
    <div style={{
      display: "flex", alignItems: "center", gap: 10, padding: "9px 12px", borderRadius: 8,
      background: tema.cor.painel, border: `1px solid ${tema.cor.linha}`, fontFamily: mono, fontSize: 17,
    }}>
      <span style={{ width: 9, height: 9, borderRadius: 9, flex: "none",
        background: rodando ? tema.cor.destaque : tema.cor.ok,
        opacity: rodando ? 0.5 + 0.5 * Math.sin(frame / 4) : 1 }} />
      <span style={{ color: tema.cor.fraco, flex: "none" }}>trama ·</span>
      <span style={{ color: tema.cor.frente, flex: "none" }}>{item.nome}</span>
      <span style={{ color: tema.cor.fraco, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap", minWidth: 0 }}>
        {item.alvo}
      </span>
    </div>
  );
};

const Chat: React.FC = () => {
  const frame = useCurrentFrame();
  const itens = dados.chat as Item[];
  // O chat rola para manter o último item à vista, como um terminal.
  const visiveis = itens.filter((i) => frame >= i.t * FPS);
  return (
    <div style={{
      position: "absolute", inset: 0, width: CHAT, padding: "56px 28px 40px",
      display: "flex", flexDirection: "column", justifyContent: "flex-end", gap: 12,
      background: tema.cor.fundo, borderRight: `1px solid ${tema.cor.linha}`, overflow: "hidden",
    }}>
      <div style={{ position: "absolute", top: 22, left: 28, fontFamily: fonte, fontWeight: 600,
        fontSize: 16, letterSpacing: 1.2, textTransform: "uppercase", color: tema.cor.fraco }}>
        Agente <span style={{ color: tema.cor.destaque }}>+ MCP do trama</span>
      </div>
      {visiveis.map((item, k) => (
        <Entra key={k} t={item.t}>
          {item.tipo === "pedido" && (
            <div style={{ alignSelf: "flex-end", background: tema.cor.destaque, color: "#fff",
              borderRadius: "14px 14px 4px 14px", padding: "14px 16px", fontFamily: fonte, fontSize: 19,
              lineHeight: 1.4, fontWeight: 500 }}>{item.texto}</div>
          )}
          {item.tipo === "tool" && <Tool item={item} />}
          {item.tipo === "resposta" && (
            <div style={{ color: tema.cor.frente, fontFamily: fonte, fontSize: 19, lineHeight: 1.45,
              fontWeight: 500 }}>
              {item.texto.slice(0, Math.max(0, Math.floor((frame - item.t * FPS) * 3.2)))}
            </div>
          )}
        </Entra>
      ))}
    </div>
  );
};

const Tela: React.FC = () => {
  const frame = useCurrentFrame();
  const { fps } = useVideoConfig();
  // Close-up no card final: o resultado que a resposta comenta.
  const alvo = dados.fecha;
  const p = spring({ frame: frame - dados.fechaEm * fps, fps, config: { damping: 200, mass: 1.4 } });
  const escala = interpolate(p, [0, 1], [1, alvo.escala]);
  const cx = alvo.x + alvo.w / 2, cy = alvo.y + alvo.h / 2;
  const tx = interpolate(p, [0, 1], [0, 480 - cx], { easing: Easing.out(Easing.quad) });
  const ty = interpolate(p, [0, 1], [0, 540 - cy], { easing: Easing.out(Easing.quad) });
  return (
    <div style={{ position: "absolute", left: CHAT, top: 0, width: 960, height: 1080, overflow: "hidden" }}>
      <div style={{ width: 960, height: 1080, transformOrigin: "480px 540px",
        transform: `scale(${escala}) translate(${tx}px, ${ty}px)` }}>
        <OffthreadVideo src={staticFile("apresentacao/agente-tela.mp4")} muted
          style={{ width: 960, height: 1080 }} />
      </div>
    </div>
  );
};

export const AgenteAoVivo: React.FC = () => (
  <AbsoluteFill style={{ background: tema.cor.fundo }}>
    <Tela />
    <Chat />
    <Sequence from={Math.round(dados.narracaoEm * FPS)}>
      <Audio src={staticFile("apresentacao/narracao-F.wav")} />
    </Sequence>
  </AbsoluteFill>
);
