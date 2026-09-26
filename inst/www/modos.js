// Modos de exibição do card e a tabela de atalhos do editor. Módulo puro (sem
// React), pelo mesmo motivo de `geometria.js`: `editor.js` não carrega sob
// `node --test`, e o que tem ramo precisa de teste.

// Dois modos: o card ABERTO (`completo`, o padrão, que é o que a ausência de
// modo no documento significa — `.tr_modes` em R/document.R) e a MINIATURA.
// `solto` é miniatura com o preview destacado do card, flutuando no canvas
// como uma imagem (`PreviewSolto`, modos-ui.js), com posição e tamanho em
// `ui.soltos`. Parâmetros não são mais modo: dobram embaixo do card.
export const MODOS = ["mini", "completo"];
export const MODO_PADRAO = "completo";

// `params` e `preview` são os modos de antes, que documentos antigos ainda
// trazem: os dois abrem o card, e `preview` (que escondia os parâmetros)
// começa com eles dobrados — `paramsDobradosDe`.
export const modoDe = (data) => {
  const m = data?.modo;
  return m === "mini" || m === "solto" ? m : MODO_PADRAO;
};
export const ehMini = (m) => m === "mini" || m === "solto";
export const paramsDobradosDe = (data) => data?.modo === "preview";

// Quantos parâmetros cabem embaixo do card. O resto fica na engrenagem, que
// abre o formulário inteiro (`ParamsModal`). A ordem é a da declaração no
// bloco: quem declara primeiro é o mais importante.
export const LIMITE_PARAMS_CARD = 5;

// `when` (`tr_when`, R/param.R): `{param: [valores]}`, e todas as condições
// têm de valer. Valor comparado como texto: o JSON traz `true`/`1`/`"a"` e o
// widget pode devolver o mesmo valor noutro tipo.
export function paramVisivel(p, valores) {
  const w = p?.when;
  if (!w) return true;
  return Object.entries(w).every(([nome, aceitos]) => {
    const v = valores?.[nome];
    return (Array.isArray(aceitos) ? aceitos : [aceitos]).some((a) => String(a) === String(v));
  });
}

// Valores efetivos: o que o nó guarda por cima dos `default` do spec — um
// parâmetro nunca tocado ainda decide a visibilidade dos outros.
export function valoresEfetivos(spec, params) {
  const out = {};
  (spec?.params || []).forEach((p) => { out[p.name] = p.default; });
  return { ...out, ...(params || {}) };
}

export function paramsVisiveis(spec, params) {
  const vals = valoresEfetivos(spec, params);
  return (spec?.params || []).filter((p) => paramVisivel(p, vals));
}

// Tamanho que o preview pede pra não ficar encavalado: o que ele rola a mais
// do que mostra, somado ao tamanho atual, com teto. Só cresce — encolher o
// que o usuário alargou à mão seria desfazer a arrumação dele. `null` quando
// já cabe (folga de `tol` px pra borda sub-pixel não disparar nada).
export const TETO_AUTO = { w: 720, h: 520 };
export function tamanhoPedido({ cardW, prevH, clientW, clientH, scrollW, scrollH },
                              teto = TETO_AUTO, tol = 6) {
  const faltaW = scrollW - clientW, faltaH = scrollH - clientH;
  if (faltaW <= tol && faltaH <= tol) return null;
  const w = faltaW > tol ? Math.min(teto.w, Math.max(cardW, Math.ceil((cardW + faltaW) / 16) * 16)) : cardW;
  const hh = faltaH > tol ? Math.min(teto.h, Math.max(prevH, Math.ceil((prevH + faltaH) / 16) * 16)) : prevH;
  if (w === cardW && hh === prevH) return null;
  return { w, h: hh };
}

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
  { id: "modo-mini", grupo: "Card", teclas: ["A"], rotulo: "Miniatura" },
  { id: "modo-completo", grupo: "Card", teclas: ["D"], rotulo: "Aberto" },
  { id: "modo-alternar", grupo: "Card", teclas: ["S"], rotulo: "Alternar miniatura / aberto" },
  { id: "params", grupo: "Card", teclas: ["W"], rotulo: "Mostrar / dobrar parâmetros" },
  { id: "params-todos", grupo: "Card", teclas: ["P"], rotulo: "Todos os parâmetros" },
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
