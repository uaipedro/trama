// Modos de exibição do card e a tabela de atalhos do editor. Módulo puro (sem
// React), pelo mesmo motivo de `geometria.js`: `editor.js` não carrega sob
// `node --test`, e o que tem ramo precisa de teste.

// Do menor pro maior. `completo` é o padrão, e é o que a ausência de modo no
// documento significa (`.tr_modes` em R/document.R).
export const MODOS = ["mini", "params", "preview", "completo"];
export const MODO_PADRAO = "completo";

export const modoDe = (data) => (MODOS.includes(data?.modo) ? data.modo : MODO_PADRAO);
export const mostraPreview = (m) => m === "preview" || m === "completo";
export const mostraParams = (m) => m === "params" || m === "completo";
// O painel à esquerda só existe pra quem não mostra os parâmetros no card.
export const precisaPainel = (m) => !mostraParams(m);

// Nome da tecla na forma da tabela: `mod+` (Ctrl ou Cmd), `shift+`, e `e.key`
// em minúsculas. `<` e `>` são o Shift de `,` e `.` no teclado ABNT, e o
// Shift de outras teclas em outros layouts: os quatro viram `,`/`.` sem
// `shift+`, pra navegar entre frames funcionar com ou sem Shift.
export function nomeDaTecla(e) {
  const k = e.key.toLowerCase();
  if (k === "<" || k === ",") return (e.ctrlKey || e.metaKey ? "mod+" : "") + ",";
  if (k === ">" || k === ".") return (e.ctrlKey || e.metaKey ? "mod+" : "") + ".";
  // "+" pede Shift no teclado principal e não no numérico: vira sempre "+".
  if (k === "+") return (e.ctrlKey || e.metaKey ? "mod+" : "") + "+";
  return (e.ctrlKey || e.metaKey ? "mod+" : "") + (e.shiftKey ? "shift+" : "") + k;
}

// `frames` é `framesOrd` do editor: `{id, x, y, w, h}` na ordem de slide.
export function frameMaisPerto(frames, centro) {
  let melhor = -1, dist = Infinity;
  frames.forEach((f, i) => {
    const d = Math.hypot(f.x + f.w / 2 - centro.x, f.y + f.h / 2 - centro.y);
    if (d < dist) { dist = d; melhor = i; }
  });
  return melhor;
}

// Índice do frame `delta` passos depois do atual. Sem atual (nunca navegou,
// ou o frame foi apagado), o ponto de partida é o que está mais perto do
// centro da tela: é o que o usuário está olhando.
export function frameVizinho(frames, atualId, delta, centro) {
  if (frames.length === 0) return -1;
  let i = frames.findIndex((f) => f.id === atualId);
  if (i < 0) i = frameMaisPerto(frames, centro);
  return Math.max(0, Math.min(frames.length - 1, i + delta));
}

// Fonte única dos atalhos: o painel do H e as dicas dos botões leem daqui, pra
// nunca discordarem. `teclas` é o que se MOSTRA; o mapa de teclas de verdade
// continua em `editor.js`, perto das ações.
export const ATALHOS = [
  { id: "modo-mini", grupo: "Card", teclas: ["A"], rotulo: "Mini" },
  { id: "modo-params", grupo: "Card", teclas: ["S"], rotulo: "Só parâmetros" },
  { id: "modo-preview", grupo: "Card", teclas: ["W"], rotulo: "Só preview" },
  { id: "modo-completo", grupo: "Card", teclas: ["D"], rotulo: "Completo" },
  { id: "vista", grupo: "Card", teclas: ["V"], rotulo: "Ver em tela cheia" },
  { id: "tamanho", grupo: "Card", teclas: ["Shift+R"], rotulo: "Restaurar tamanho" },
  { id: "proximo", grupo: "Card", teclas: ["+"], rotulo: "Próximo bloco" },
  { id: "ajuda", grupo: "Geral", teclas: ["H"], rotulo: "Ajuda do bloco / atalhos" },
  { id: "desfazer", grupo: "Geral", teclas: ["Ctrl+Z"], rotulo: "Desfazer" },
  { id: "tudo", grupo: "Geral", teclas: ["Ctrl+A"], rotulo: "Selecionar tudo" },
  { id: "copiar-template", grupo: "Geral", teclas: ["Ctrl+Shift+C"], rotulo: "Copiar como template" },
  { id: "apresentar", grupo: "Frames", teclas: ["F"], rotulo: "Apresentar / sair" },
  { id: "frame", grupo: "Frames", teclas: ["Shift+F"], rotulo: "Desenhar frame" },
  { id: "frame-sel", grupo: "Frames", teclas: ["Ctrl+G"], rotulo: "Frame da seleção" },
  { id: "frame-n", grupo: "Frames", teclas: ["1…9", "0"], rotulo: "Ir ao frame 1…10" },
  { id: "frame-passo", grupo: "Frames", teclas: [",", "."], rotulo: "Frame anterior / próximo" },
  { id: "markdown", grupo: "Notas", teclas: ["M"], rotulo: "Nota markdown" },
  { id: "imagem", grupo: "Notas", teclas: ["I"], rotulo: "Nota imagem" },
];

export function dica(id) {
  const a = ATALHOS.find((x) => x.id === id);
  return a ? `${a.rotulo} (${a.teclas.join(" / ")})` : "";
}
