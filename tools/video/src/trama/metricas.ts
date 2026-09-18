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
import type { Spec } from "./tipos";

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

export function alturaLinhaParam(tipo: string): number {
  if (tipo === "expr") return M.linha.expr;
  if (tipo === "enum-largo") return M.linha.largo;
  return M.linha.campo;
}

export function alturaParams(spec: Spec): number {
  const n = spec.params.length;
  if (!n) return 0;
  const linhas = spec.params.reduce((s, p) => s + alturaLinhaParam(p.tipo), 0);
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
