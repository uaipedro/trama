// src/motor/Filme.tsx — desenha um roteiro compilado.
//
// Sempre em 1440×1080 (4:3), qualquer que seja a composição: a vertical e os
// stills só enquadram ESTE desenho (ver `Composicoes.tsx`). Por isso a largura
// e a altura vêm de `QUADRO`, não de `useVideoConfig`.
import React from "react";
import { AbsoluteFill, Audio, Sequence, staticFile, useCurrentFrame } from "remotion";
import { tema } from "../theme";
import { Acabamento } from "../lib/camadas";
import { Legenda } from "../lib/Legenda";
import { mover, respiro } from "../lib/movimento";
import { Canvas } from "../trama/Canvas";
import { ParamsDock } from "../trama/ParamsDock";
import { modoVigente } from "../trama/estado";
import { precisaPainel } from "../trama/modos-app.js";
import { DESLOCA, QUADRO, SEGURA, type Camera, type Filme as FilmeT } from "./compilar";

function cameraEm(marcas: FilmeT["cameras"], f: number): Camera {
  let cam = marcas[0].cam;
  for (let i = 1; i < marcas.length; i++) {
    const m = marcas[i];
    if (f < m.q) break;
    const t = mover(f, [m.q, m.q + DESLOCA], [0, 1], tema.ease.ambos);
    cam = {
      cx: cam.cx + (m.cam.cx - cam.cx) * t,
      cy: cam.cy + (m.cam.cy - cam.cy) * t,
      s: cam.s + (m.cam.s - cam.s) * t,
    };
  }
  return cam;
}

function selecionadoEm(filme: FilmeT, q: number): string | null {
  let id: string | null = null;
  for (const s of filme.selecao) if (q >= s.q) id = s.id;
  return id;
}

// O painel abre quando o card selecionado está num modo sem parâmetros — a
// regra do editor (`precisaPainel`). Entra e sai deslizando, em ~8 quadros.
function painelEm(filme: FilmeT, q: number): string | null {
  const id = selecionadoEm(filme, q);
  if (!id) return null;
  const no = filme.nos.find((n) => n.id === id)!;
  return q >= no.entra && precisaPainel(modoVigente(no, q)) ? id : null;
}

const Painel: React.FC<{ filme: FilmeT; vertical: boolean }> = ({ filme, vertical }) => {
  const q = useCurrentFrame();
  // Quantos dos últimos 8 quadros tiveram o MESMO painel aberto, e quantos dos
  // próximos 6 terão: dá entrada e saída sem guardar estado.
  const id = painelEm(filme, q) ?? painelEm(filme, q - 6);
  if (!id) return null;
  let antes = 0, depois = 0;
  for (let k = 0; k < 8; k++) if (painelEm(filme, q - k) === id) antes++;
  for (let k = 0; k < 6; k++) if (painelEm(filme, q + k) === id) depois++;
  const p = Math.min(antes / 8, depois / 6);
  const no = filme.nos.find((n) => n.id === id)!;
  const e = tema.ease.saida(p);
  // No vertical a calha esquerda é cortada: o painel vira folha na base da
  // zona segura, acima da legenda — como um app de celular abriria.
  if (vertical) {
    const largura = SEGURA.w - 120;
    return (
      <div style={{ position: "absolute", left: SEGURA.x + 60, bottom: 250, width: largura,
        transform: `translateY(${(1 - e) * 40}px)`, opacity: e }}>
        <ParamsDock no={no} escala={largura / 280} origem="0 100%" />
      </div>
    );
  }
  return (
    <div
      style={{
        position: "absolute",
        // Na calha esquerda, FORA da zona segura: é interface do app, não o
        // assunto. A mesma borda da tela onde o editor o põe.
        left: 22,
        top: "50%",
        width: SEGURA.x - 44,
        transform: `translate(${(1 - e) * -40}px, -50%)`,
        opacity: e,
      }}
    >
      <ParamsDock no={no} escala={(SEGURA.x - 44) / 280} />
    </div>
  );
};

const Trilha: React.FC<{ filme: FilmeT }> = ({ filme }) => (
  <>
    <Audio loop src={staticFile("sfx/pad.wav")} volume={0.4} />
    {filme.sons.map((s, i) => (
      // O som entra 3 quadros ANTES do visual: adiantado lê como sincronizado,
      // atrasado lê como defeito.
      <Sequence key={i} from={Math.max(0, s.em - 3)} layout="none">
        <Audio src={staticFile(`sfx/${s.arquivo}.wav`)} volume={s.volume} />
      </Sequence>
    ))}
  </>
);

// As guias da zona segura, pra conferir enquadramento no studio.
const Guias: React.FC = () => (
  <AbsoluteFill style={{ pointerEvents: "none" }}>
    {[SEGURA.x, SEGURA.x + SEGURA.w].map((x) => (
      <div key={x} style={{ position: "absolute", left: x, top: 0, bottom: 0,
        borderLeft: "2px dashed rgba(255,80,80,.8)" }} />
    ))}
  </AbsoluteFill>
);

export const Filme: React.FC<{
  filme: FilmeT;
  guias?: boolean;
  som?: boolean;
  // O `Filme` é o mesmo desenho nos dois formatos; só o painel muda de lugar.
  vertical?: boolean;
}> = ({ filme, guias = false, som = true, vertical = false }) => {
  const q = useCurrentFrame();
  const cam = cameraEm(filme.cameras, q);
  const r = respiro(q);
  const entra = mover(q, [0, 10], [0, 1]);
  const sai = mover(q, [filme.duracao - 12, filme.duracao - 1], [1, 0], tema.ease.entrada);

  return (
    <AbsoluteFill style={{ width: QUADRO.w, height: QUADRO.h, background: tema.cor.fundo }}>
      {som ? <Trilha filme={filme} /> : null}
      <AbsoluteFill style={{ opacity: entra * sai }}>
        <Canvas
          camera={{ cx: cam.cx + r.flutua, cy: cam.cy - r.flutua, s: cam.s * r.escala }}
          largura={QUADRO.w}
          altura={QUADRO.h}
          quadro={q}
          nos={filme.nos}
          arestas={filme.arestas}
          ativo={selecionadoEm(filme, q)}
        />
        <Painel filme={filme} vertical={vertical} />
        {filme.legendas.map((l) => (
          <Legenda
            key={l.de}
            texto={l.texto}
            de={l.de}
            ate={l.ate}
            destacar={l.destacar}
            base={70}
            // Dentro da zona segura: a legenda sobrevive ao recorte vertical.
            caixa={{ left: SEGURA.x + 16, right: SEGURA.x + 16 }}
          />
        ))}
      </AbsoluteFill>
      <Acabamento />
      {guias ? <Guias /> : null}
    </AbsoluteFill>
  );
};
