// src/scenes/SiteVertical.tsx — o loop vertical (1080×1920) da home e do
// compartilhamento: os mesmos quatro blocos entrando, digitando e resultando
// de `SiteMontagem`, só que em pé — a câmera desce a coluna em vez de ficar
// parada, porque quatro cards empilhados não cabem legíveis de uma vez num
// quadro de telefone.
//
// Fecha em loop do mesmo jeito que `SiteMontagem`: o quadro 0 já é vazio (a
// mola de todo card vale 0 antes de `entra`) e o último quadro apaga o fluxo
// de volta a essa mesma câmera — ver `FADE_DE`/`FADE_ATE` e a marca final de
// `MARCAS` abaixo.
import React from "react";
import { AbsoluteFill, interpolate, useCurrentFrame, useVideoConfig } from "remotion";
import { tema } from "../theme";
import { mover } from "../lib/movimento";
import { Acabamento } from "../lib/camadas";
import { Canvas, type Camera } from "../trama/Canvas";
import { alturaCard, larguraCard } from "../trama/metricas";
import { ARESTAS_VERTICAL, NOS_VERTICAL } from "../trama/catalogo-site";

const FUNDO = "oklch(24% .03 220)";
const COR_PONTOS = "#6f96a1";

export const DURACAO_TOTAL = 450;

// Escala em que um card de 240px de largura ocupa ~80% de um quadro de
// 1080px — a leitura confortável num telefone, com folga pras margens de
// segurança de cada lado.
const ESCALA_CARD = 3.6;

// Centro de cada card, em coordenadas de canvas — o meio da caixa que
// `alturaCard`/`larguraCard` descrevem, não um ponto arbitrário.
function centroDoCard(no: (typeof NOS_VERTICAL)[number]): Camera {
  const largura = larguraCard(no.tamanho);
  const altura = alturaCard(no.spec, no.tamanho);
  return { cx: no.x + largura / 2, cy: no.y + altura / 2, s: ESCALA_CARD };
}

const [LER, FILTRAR, AGRUPAR, RESUMO] = NOS_VERTICAL.map(centroDoCard);

// O plano geral: cabe o retângulo dos quatro cards MAIS a curva que "volta"
// nas ligações que ziguezagueiam (`deslocamento()` de `Aresta.tsx` empurra o
// controle bem além da borda do card quando o alvo fica atrás da origem) —
// por isso a margem generosa, medida a olho na folha de contato, e não só o
// bounding box dos cards.
const CHEIO: Camera = { cx: 270, cy: 799, s: 1.05 };

// Só o canvas, sem acabamento — `SiteVerticalDivulgacao` reaproveita isto e
// põe UMA pilha de acabamento por cima do clipe inteiro (loop + fecho de
// marca); duplicar `Acabamento` aqui dobraria grão e vinheta nos quadros em
// que as duas cenas se sobrepõem.
export const NucleoVertical: React.FC = () => {
  const quadro = useCurrentFrame();
  const { width, height } = useVideoConfig();

  const camera = cameraEm(quadro);
  const r = respiroDoLaco(quadro);
  const opacidadeConteudo = interpolate(quadro, [FADE_DE, FADE_ATE], [1, 0], {
    easing: tema.ease.entrada,
    extrapolateLeft: "clamp",
    extrapolateRight: "clamp",
  });

  return (
    <Canvas
      camera={{ cx: camera.cx + r.flutua, cy: camera.cy, s: camera.s * r.escala }}
      largura={width}
      altura={height}
      quadro={quadro}
      nos={NOS_VERTICAL}
      arestas={ARESTAS_VERTICAL}
      fundo={FUNDO}
      corPontos={COR_PONTOS}
      opacidadeConteudo={opacidadeConteudo}
    />
  );
};

export const SiteVertical: React.FC = () => {
  const { durationInFrames } = useVideoConfig();
  if (durationInFrames !== DURACAO_TOTAL) {
    console.error(
      `[trama-video] SiteVertical com ${durationInFrames} quadros, esperava ${DURACAO_TOTAL}.`,
    );
  }
  return (
    <AbsoluteFill>
      <NucleoVertical />
      <Acabamento />
    </AbsoluteFill>
  );
};

// Início e fim do desvanecimento do fluxo, com folga depois do último
// resultado (`resumo.resulta = 290`) e do plano geral (chega em 380) — o card
// final e o flow inteiro precisam ser VISTOS antes de a tela apagar. E folga
// TAMBÉM depois do fim do desvanecimento (`FADE_ATE` antes do último quadro
// válido, 449): sem ela o corte de loop compara um quadro ainda a 3% de
// opacidade com o quadro 0 (mola em zero, opacidade cheia mas nenhum card
// montado) — a mesma folga que `SiteMontagem` guarda entre `FADE_ATE` e
// `DURACAO_TOTAL`.
const FADE_DE = 410;
const FADE_ATE = 435;

// `q` é o quadro em que a câmera CHEGA em cada marca (convenção de
// `SiteFluxoAmplo`/`scenes/Fluxo.tsx`). Cada card recebe a câmera ~10 quadros
// depois de entrar — o bastante pra ela já estar parada quando o parâmetro
// começa a ser digitado — e a segura até o próximo card puxar o olhar.
const DESLOCA = 20;

const MARCAS: { q: number; cam: Camera }[] = [
  { q: 0, cam: CHEIO },
  { q: 16, cam: LER },
  { q: 86, cam: FILTRAR },
  { q: 170, cam: AGRUPAR },
  { q: 258, cam: RESUMO },
  // Por volta dos 12s (380/450 quadros): a câmera abre pro fluxo inteiro,
  // com o resultado do último card ainda fresco na tela.
  { q: 380, cam: CHEIO },
  // Mesma marca no último quadro válido: fecha o laço sem corte.
  { q: DURACAO_TOTAL - 1, cam: CHEIO },
];

function cameraEm(f: number): Camera {
  if (f <= MARCAS[0].q) return MARCAS[0].cam;
  for (let i = 1; i < MARCAS.length; i++) {
    const anterior = MARCAS[i - 1];
    const atual = MARCAS[i];
    if (f >= atual.q) continue;
    const inicio = Math.max(anterior.q, atual.q - DESLOCA);
    if (f <= inicio) return anterior.cam;
    const t = mover(f, [inicio, atual.q], [0, 1], tema.ease.ambos);
    return {
      cx: anterior.cam.cx + (atual.cam.cx - anterior.cam.cx) * t,
      cy: anterior.cam.cy + (atual.cam.cy - anterior.cam.cy) * t,
      s: anterior.cam.s + (atual.cam.s - anterior.cam.s) * t,
    };
  }
  return MARCAS[MARCAS.length - 1].cam;
}

// Só a vertical (`flutua`) respira: horizontal ficaria estranho encostado no
// zig-zag dos cards. Fases em múltiplos inteiros de uma volta completa em
// `DURACAO_TOTAL`, pra `respiro(0)` bater com `respiro(fim)` sem medir nada.
function respiroDoLaco(quadro: number) {
  const t = (2 * Math.PI * quadro) / DURACAO_TOTAL;
  return { escala: 1 + Math.sin(t * 5) * 0.005, flutua: Math.sin(t * 7) * 1.6 };
}
