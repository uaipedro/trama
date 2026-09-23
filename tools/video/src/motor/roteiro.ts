// src/motor/roteiro.ts — o formato do ROTEIRO, a única coisa que se escreve
// pra fazer um vídeo novo.
//
// Um roteiro é DADO: os blocos que existem no canvas (onde ficam, o que
// mostram) e a lista de planos, em ordem. O motor (`compilar.ts`) decide o
// resto — quando cada coisa acontece, pra onde a câmera vai, quando o painel
// de parâmetros abre, que som toca. Quem escreve roteiro não escreve quadro,
// nem coordenada de câmera, nem JSX.
//
// Guia completo, com receitas: `.claude/skills/trama-video/SKILL.md`.
import type { Modo } from "../trama/modos-app.js";
import type { Tamanho } from "../trama/metricas";
import type { Resultado, Spec } from "../trama/tipos";

export type Bloco = {
  spec: Spec;
  // Canto superior-esquerdo em coordenadas de canvas (as do `ui.positions`).
  x: number;
  y: number;
  resultado: Resultado;
  // A duração que o cabeçalho mostra depois de rodar ("31ms", "0,4s").
  duracao: string;
  rotulo?: string;
  tamanho?: Tamanho;
  vista?: number;
  params?: Record<string, string>;
  // Modo com que o card entra. Padrão: `completo`.
  modo?: Modo;
};

// O que TODO plano aceita.
type Comum = {
  // Legenda na base do quadro, durante o plano. Curta: 3 a 8 palavras.
  legenda?: string;
  // Palavras da legenda na cor-herói (sem pontuação).
  destacar?: string[];
  // Começa junto com o plano anterior, em vez de depois dele.
  junto?: boolean;
  // Duração em quadros (30 = 1s). Quase nunca precisa: cada tipo tem a sua.
  dur?: number;
};

export type Plano = Comum &
  (
    // O card cai no canvas e roda (como no trama: ligou, rodou).
    | { faz: "entra"; bloco: string }
    // Aresta da saída de `de` pra entrada `porta` de `para` (0 = primeira).
    | { faz: "liga"; de: string; para: string; porta?: number }
    // Muda um parâmetro: anel no campo (no card, ou no painel à esquerda se o
    // card não mostra parâmetros), valor digitado, e o bloco roda de novo.
    | { faz: "param"; bloco: string; param: string; valor: string; resultado?: Resultado }
    // Troca o modo do card: anel no seletor, e o card muda de forma.
    | { faz: "modo"; bloco: string; modo: Modo }
    // Câmera em um bloco, ou num grupo.
    | { faz: "foco"; bloco?: string; blocos?: string[] }
    // Câmera no fluxo inteiro.
    | { faz: "geral" }
    // Nada acontece (a câmera respira). Use pra dar tempo de ler.
    | { faz: "espera" }
    // Marca este instante como imagem estática (`<id>-<nome>`).
    | { faz: "still"; nome: string }
  );

export type Roteiro = {
  // Vira o nome das composições: `<id>`, `<id>-vertical`, `<id>-<still>`.
  id: string;
  blocos: Record<string, Bloco>;
  planos: Plano[];
};
