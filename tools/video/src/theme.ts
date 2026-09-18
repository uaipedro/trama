// src/theme.ts — a fonte única de cor, easing e mola do vídeo.
//
// Os tokens de cor NÃO são escolha do vídeo: são os do editor
// (`inst/www/trama.css`, bloco `:root`) e as categorias da coleção `data`
// (`collections/trama.data/R/collection.R`). Um vídeo que mostra o trama com
// outra paleta mostra outra ferramenta — e, pior, envelhece sozinho quando o
// tema do app muda e ninguém lembra que havia uma cópia aqui.
import { Easing } from "remotion";

export const tema = {
  cor: {
    fundo: "#0f1115",
    painel: "#171a21",
    linha: "#262b36",
    frente: "#e6e9ef",
    fraco: "#8b93a7",
    campo: "#0d1016",
    aresta: "#3b4252",
    pontos: "#91919a",
    // `destaque` é a cor-herói: no máximo UM elemento por quadro a usa, e é
    // ela que diz onde olhar. Por isso o brilho também é só dela.
    destaque: "#5b8def",
    brilho: "rgba(91,141,239,.45)",
    ok: "#22c55e",
    tinta: "#0b0e13",
  },
  // As sete categorias da coleção `data`, na ordem em que a paleta as mostra.
  // São cor de DADO (que bloco é aquele), não decoração — daí poderem aparecer
  // várias por quadro sem brigar com a regra da cor-herói.
  categoria: {
    source: "#6366f1",
    inspect: "#f59e0b",
    clean: "#14b8a6",
    transform: "#0ea5e9",
    reshape: "#ec4899",
    aggregate: "#a855f7",
    sink: "#22c55e",
  } as Record<string, string>,
  fonte: {
    display: '"Inter Display", ui-sans-serif, system-ui, sans-serif',
    corpo: 'Inter, ui-sans-serif, system-ui, sans-serif',
    mono: '"Noto Sans Mono", ui-monospace, monospace',
    // O card herda a MESMA pilha que `body` declara no trama.css. Trocar por
    // Inter aqui deixaria o bloco do vídeo diferente do bloco do editor em
    // largura de texto — o tipo de detalhe que faz a demo parecer maquete.
    card: 'ui-sans-serif, system-ui, -apple-system, "Segoe UI", sans-serif',
  },
  // Nenhuma interpolação linear no projeto: toda entrada é mola, todo
  // deslocamento é bezier.
  ease: {
    saida: Easing.bezier(0.16, 1, 0.3, 1),     // easeOutExpo — entradas
    ambos: Easing.bezier(0.83, 0, 0.17, 1),    // easeInOutQuint — câmera
    entrada: Easing.bezier(0.7, 0, 0.84, 0),   // só para saídas de cena
  },
  mola: {
    seca: { damping: 14, stiffness: 160, mass: 0.6 },   // cards, palavras
    suave: { damping: 20, stiffness: 90, mass: 1 },     // blocos grandes
    elastica: { damping: 11, stiffness: 170, mass: 0.7 }, // a marca
  },
} as const;
