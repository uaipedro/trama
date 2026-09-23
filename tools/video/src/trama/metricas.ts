// src/trama/metricas.ts — a geometria do card, em números.
//
// POR QUE NÃO MEDIR O DOM: as arestas precisam saber onde cada porta está para
// nascer ancoradas, e medir com `getBoundingClientRect` dentro de um
// renderizador quadro a quadro é justamente o laço "medir → mudar estado →
// remedir" que o trama.css passa o arquivo inteiro evitando. Aqui a altura é
// DECLARADA: os números abaixo saem das mesmas regras de `trama.css`
// (`.tr-preview`, `.tr-tabs`, `.tr-params`, `.tr-ports`), e o card recebe essas
// alturas como estilo inline. Assim o desenho e a âncora não podem divergir —
// os dois leem daqui.
import type { Param, Spec } from "./tipos";
import { mostraParams, mostraPreview, type Modo } from "./modos-app.js";

export const M = {
  largura: 240,
  // `.tr-node-head`: padding 6px vertical + linha de 12px/1.45. Pinado em 29
  // porque 17,4 + 12 não é inteiro, e meio pixel de cabeçalho desalinha a
  // âncora de TODAS as portas abaixo dela.
  cabeca: 29,
  // `.tr-preview` 132px + a borda de baixo de 1px.
  preview: 132,
  // `.tr-tabs` 18px + borda de 1px.
  abas: 19,
  // `.tr-params`: padding 5px em cima e embaixo, gap 4px entre linhas.
  paramsPad: 5,
  paramsGap: 4,
  // Altura de linha por tipo de widget, conforme o widget que a coleção
  // registra: input de uma linha, textarea `rows=2` (o `expr` da coleção
  // `data`), e o segmentado "wide", que empilha rótulo e opções — o formato
  // que `layoutEnum` escolhe quando as opções não cabem ao lado do rótulo.
  linha: { campo: 22, expr: 38, largo: 46 } as Record<string, number>,
  // `.tr-ports`: padding 5px em cima, 14px embaixo (o respiro que mantém a alça
  // de redimensionar fora do alvo do handle), portas de 15px com gap de 3px.
  portasTopo: 5,
  portasBase: 14,
  porta: 15,
  portaGap: 3,
  handle: 9,
} as const;

// O que o documento guarda em `ui.sizes`: o usuário arrastou a alça do card.
// `undefined` em qualquer um dos dois cai no padrão do CSS.
export type Tamanho = { largura?: number; preview?: number };

export function larguraCard(t?: Tamanho): number {
  return t?.largura ?? M.largura;
}

export function alturaPreview(t?: Tamanho): number {
  return t?.preview ?? M.preview;
}

// Rótulo com mais de 16 caracteres vai pra cima do campo (`.tr-param-longo`,
// decidido por contagem em `ParamsList`, modos-ui.js) — a linha cresce o rótulo
// mais o gap de 2px.
export const rotuloLongo = (p: Param) => p.rotulo.length > 16;

export function alturaLinhaParam(tipo: string, p?: Param): number {
  if (p && rotuloLongo(p) && tipo !== "enum-largo") return alturaLinhaParam(tipo) + 17;
  if (tipo === "expr") return M.linha.expr;
  if (tipo === "enum-largo") return M.linha.largo;
  return M.linha.campo;
}

export function alturaParams(spec: Spec): number {
  const n = spec.params.length;
  if (!n) return 0;
  const linhas = spec.params.reduce((s, p) => s + alturaLinhaParam(p.tipo, p), 0);
  return M.paramsPad * 2 + linhas + M.paramsGap * (n - 1);
}

export function alturaPortas(spec: Spec): number {
  const n = Math.max(spec.entradas.length, spec.saidas.length);
  return M.portasTopo + n * M.porta + (n - 1) * M.portaGap + M.portasBase;
}

export function alturaCard(spec: Spec, t?: Tamanho): number {
  // +1 em cada um: a borda de baixo do preview e a da faixa de abas.
  return (
    M.cabeca + alturaPreview(t) + 1 + M.abas + alturaParams(spec) + alturaPortas(spec)
  );
}

// Distância do topo do card até o CENTRO da porta de índice `i`. Entradas e
// saídas são duas grades irmãs dentro de `.tr-ports`, as duas começando no
// mesmo topo — então a mesma conta serve pras duas, e é por isso que ela não
// recebe o lado.
export function yPorta(spec: Spec, i: number, t?: Tamanho): number {
  return (
    M.cabeca +
    alturaPreview(t) +
    1 +
    M.abas +
    alturaParams(spec) +
    M.portasTopo +
    i * (M.porta + M.portaGap) +
    M.porta / 2
  );
}

// --- Modos do card -----------------------------------------------------------
//
// O editor tem quatro modos (`inst/www/modos.js`): `completo` (preview +
// params), `preview` (sem params), `params` (sem preview nem abas) e `mini`
// (ícone grande e rótulo, portas coladas na borda). A geometria de cada um é
// declarada aqui, pelas mesmas regras de `trama.css`, pra aresta e câmera
// saberem onde o card está sem medir o DOM.

// `.tr-node-mini .tr-node-title`: 12px/1.45, no máximo 84px por linha. A
// largura do texto é ESTIMADA (7,4px por caractere a 12px semibold na pilha
// de sistema) — o card recebe a largura inline, então o erro vira folga, não
// desalinhamento.
const PX_CHAR = 7.4;
function linhasMini(rotulo: string): { linhas: number; maior: number } {
  const palavras = rotulo.split(" ");
  let linhas = 1, atual = 0, maior = 0;
  for (const p of palavras) {
    const w = p.length * PX_CHAR;
    const com = atual ? atual + PX_CHAR + w : w;
    if (com > 84 && atual) { maior = Math.max(maior, atual); linhas++; atual = w; }
    else atual = com;
  }
  return { linhas, maior: Math.min(84, Math.max(maior, atual)) };
}

export function larguraMini(rotulo: string): number {
  // padding 8px dos dois lados + borda de 1px.
  return Math.max(76, Math.ceil(linhasMini(rotulo).maior) + 18);
}

export function alturaMini(rotulo: string): number {
  // padding 10 + ícone 28 + gap 4 + linhas + padding 8 + bordas.
  return 10 + 28 + 4 + Math.ceil(linhasMini(rotulo).linhas * 17.4) + 8 + 2;
}

export type Geometria = { w: number; h: number; portas: (lado: "in" | "out", i: number) => number };

export function geometria(spec: Spec, modo: Modo, rotulo: string, t?: Tamanho): Geometria {
  if (modo === "mini") {
    const w = larguraMini(rotulo), h = alturaMini(rotulo);
    // Portas centradas na altura toda do card (`.tr-node-mini .tr-ports`).
    const n = (lado: "in" | "out") => (lado === "in" ? spec.entradas : spec.saidas).length;
    return {
      w, h,
      portas: (lado, i) => {
        const bloco = n(lado) * M.porta + (n(lado) - 1) * M.portaGap;
        return h / 2 - bloco / 2 + i * (M.porta + M.portaGap) + M.porta / 2;
      },
    };
  }
  const corpo =
    (mostraPreview(modo) ? alturaPreview(t) + 1 + M.abas : 0) +
    (mostraParams(modo) ? alturaParams(spec) : 0);
  const topoPortas = M.cabeca + corpo + M.portasTopo;
  return {
    w: larguraCard(t),
    h: M.cabeca + corpo + alturaPortas(spec),
    portas: (_lado, i) => topoPortas + i * (M.porta + M.portaGap) + M.porta / 2,
  };
}
