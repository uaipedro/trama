// inst/www/geometria.js — a geometria dos frames, sem React nem xyflow.
//
// Módulo puro de propósito: é o que o `node --test` consegue importar (o
// `frames.js` puxa React, xyflow e dagre pelo importmap, que só existe no
// navegador). Retângulos, contenção, a grade da prancheta e o dono de cada
// card no Organizar moram aqui; quem desenha e quem emite op mora nos outros.

// `params.js` também é puro: a regra de número da prancheta é a mesma do card.
import { validarNumero } from "./params.js";

// Espelha `.tr_aspects` (R/document.R). `null` é proporção livre. A4 é
// retrato: 210/297 = 1/√2.
export const ASPECTS = { "livre": null, "16:9": 16 / 9, "4:3": 4 / 3, "1:1": 1, "A4": 1 / Math.SQRT2 };
export const ratioOf = (aspect) => ASPECTS[aspect] ?? null;

// Nomes, e não hex: o documento guarda o nome, e o tom mora no CSS
// (`.tr-frame-<cor>`), onde pode ser reajustado sem migrar documento nenhum.
export const FRAME_COLORS = ["azul", "verde", "amarelo", "laranja", "rosa", "roxo", "cinza"];

// Tamanho de nascença de cada kind de nota (espelha `.tr_note_tamanho`,
// R/document.R) e o piso abaixo do qual o bloco fica ilegível: o markdown
// precisa de umas duas linhas de texto na escala "nota", a imagem de área
// pra se reconhecer o que é. Antes o piso era um só (80×40) e o clique seco
// criava a nota exatamente nele — era por isso que nasciam espremidas.
export const NOTA_TAMANHO = { markdown: { w: 280, h: 160 }, imagem: { w: 320, h: 220 } };
export const NOTA_MIN = { markdown: { w: 160, h: 80 }, imagem: { w: 160, h: 120 } };
const kindNota = (kind) => (kind === "imagem" ? "imagem" : "markdown");
export const tamanhoNota = (kind) => ({ ...NOTA_TAMANHO[kindNota(kind)] });
export const minimoNota = (kind) => ({ ...NOTA_MIN[kindNota(kind)] });
// Leva w/h ao piso do kind. Ausente ou zero conta como "sem tamanho" e vira o
// de nascença, não o mínimo: é o caso de documento escrito à mão.
export function pisoNota(kind, w, hh) {
  const m = minimoNota(kind), t = tamanhoNota(kind);
  return { w: w > 0 ? Math.max(m.w, w) : t.w, h: hh > 0 ? Math.max(m.h, hh) : t.h };
}

// Padrões do card enquanto ele não foi medido. Largura e altura repetem
// `NODE_W`/`NODE_H` do editor.js, e o preview o `.tr-preview` do trama.css;
// copiados, e não importados, porque o editor importa esta geometria (via
// frames.js) e a volta seria um ciclo por três números.
const CARD_W = 240, CARD_H = 190, PREVIEW_H = 132;

// Retângulo de um nó em coordenadas do fluxo. Frame usa o tamanho que declara
// (o documento manda nele); card usa o que o React Flow MEDIU, porque a altura
// do card depende de quantos params e portas o bloco tem. Card ainda sem
// medida (recém-chegado, ou numa aba escondida, onde ninguém mede) vira uma
// estimativa pelo tamanho declarado, e não 0x0: um ponto cabe em qualquer
// frame, e Ctrl+G enquadraria só o canto dos cards.
export function rectOf(n) {
  const [sw, sh] = n.data?.size || [];
  const w = n.width ?? n.measured?.width ?? Math.max(CARD_W, sw ?? 0);
  // A altura declarada é a do PREVIEW; o resto do card cresce junto com ela.
  const hh = n.height ?? n.measured?.height ?? CARD_H + Math.max(0, (sh ?? PREVIEW_H) - PREVIEW_H);
  return { x: n.position.x, y: n.position.y, w, h: hh };
}

export const inside = (a, b) =>
  a.x >= b.x && a.y >= b.y && a.x + a.w <= b.x + b.w && a.y + a.h <= b.y + b.h;

// Cards INTEIRAMENTE dentro do frame. Card pela metade fica onde está: levar
// junto o vizinho que só encosta seria o acidente que o pertencimento
// geométrico promete não causar.
export function containedCards(frame, nodes) {
  const r = rectOf(frame);
  return nodes.filter((n) => n.type === "ndNode" && inside(rectOf(n), r)).map((n) => n.id);
}

// Notas INTEIRAMENTE dentro do frame, mesma régua de `containedCards`. Duas
// funções, e não um parâmetro de tipo, porque os dois chamadores querem
// coisas diferentes: o arrasto quer as duas listas juntas (cards e notas), e
// o "Organizar" (Phase 5) quer só os cards — o dagre não teria onde pôr uma
// nota, que não tem ligação nenhuma.
export function containedNotes(frame, nodes) {
  const r = rectOf(frame);
  return nodes.filter((n) => n.type === "trNota" && inside(rectOf(n), r)).map((n) => n.id);
}

// Folga em volta de uma bbox; depois a dimensão curta cresce até a proporção
// pedida, mantendo o centro. Só cresce, nunca encolhe: o frame da seleção
// nunca corta um card da seleção.
export function fitAspect(b, aspect, pad = 48) {
  let w = b.width + 2 * pad, hh = b.height + 2 * pad;
  const r = ratioOf(aspect);
  if (r) { if (w / hh < r) w = hh * r; else hh = w / r; }
  const cx = b.x + b.width / 2, cy = b.y + b.height / 2;
  return { x: Math.round(cx - w / 2), y: Math.round(cy - hh / 2), w: Math.round(w), h: Math.round(hh) };
}

// Faixa do cabeçalho que o conteúdo não pode cobrir. `.tr-frame-head` (CSS)
// mede 8 + 20 (o badge de ordem, mais alto que a linha de 15px) + 8 = 36px, e
// começa depois da borda de 2px do frame: 38px. Mais uma folga pra o título
// não encostar no card de cima. É constante, e não medida do DOM, porque o
// layout é função pura de nós e arestas.
export const FRAME_HEAD = 44;
// Margem entre o conteúdo e a borda do frame, nos quatro lados (a de cima
// conta a partir do cabeçalho).
export const FRAME_PAD = 32;

// Frames INTEIRAMENTE dentro de outro, sem ele mesmo. Mesma régua dos cards:
// arrastar o externo de uma prancheta leva as células; frame que só encosta
// fica. A contenção é transitiva por geometria (o que está dentro da célula
// está dentro do externo), então quem chama não precisa recursar.
export function containedFrames(frame, nodes) {
  const r = rectOf(frame);
  return nodes.filter((n) => n.type === "trFrame" && n.id !== frame.id && inside(rectOf(n), r))
    .map((n) => n.id);
}

// Um frame em volta de uma bbox: folga nos quatro lados, mais o cabeçalho em
// cima (senão o título do externo cobre o das células da primeira linha), e a
// proporção pela regra do `fitAspect`. É o externo que a prancheta cria.
export function envolver(b, aspect, pad = 48) {
  return fitAspect({ x: b.x, y: b.y - FRAME_HEAD, width: b.width, height: b.height + FRAME_HEAD },
                   aspect, pad);
}

export const PRANCHETA_PADRAO = { linhas: 3, colunas: 3, aspect: "16:9", largura: 1600,
                                  altura: 900, espaco: 200, externo: true, cor: "rodízio" };

// Campos que viraram texto no popover voltam a número aqui. Erro por campo,
// com a mensagem do card: o popover marca o campo e desliga o Criar.
const REGRAS = { linhas: { kind: "integer", min: 1, max: 10 }, colunas: { kind: "integer", min: 1, max: 10 },
                 largura: { min: 200 }, altura: { min: 200 }, espaco: { min: 0 } };
export function validarPrancheta(o) {
  const erros = {};
  Object.entries(REGRAS).forEach(([k, spec]) => {
    // Com proporção fixa a altura sai da largura; o campo nem aparece.
    if (k === "altura" && ratioOf(o.aspect) != null) return;
    const r = validarNumero(spec, o[k]);
    if (!r.ok) erros[k] = r.erro;
  });
  return erros;
}

// A grade da prancheta: a lista de `add_frame` na ordem de slide (externo
// primeiro, depois as células linha a linha). `centro` em coordenadas do
// fluxo; `existentes` é quantos frames o documento já tem, pra numeração
// seguir a do `tituloNovo`. É também o que a miniatura desenha: a prévia e o
// resultado saem da mesma conta. Largura e altura arredondam antes das
// posições, pra células vizinhas não diferirem de 1px.
export function gradeDeFrames(o, centro, existentes) {
  const L = Number(o.linhas), C = Number(o.colunas), g = Math.round(Number(o.espaco));
  const ra = ratioOf(o.aspect);
  const w = Math.round(Number(o.largura));
  const hh = Math.round(ra ? Number(o.largura) / ra : Number(o.altura));
  const W = C * w + (C - 1) * g, H = L * hh + (L - 1) * g;
  const ext = o.externo ? envolver({ x: 0, y: 0, width: W, height: H }, o.aspect, Math.max(g / 2, 48)) : null;
  // O deslocamento sai do retângulo JÁ arredondado pelo `fitAspect`, e é
  // inteiro: somado a posições inteiras, nada sai fracionário.
  const caixa = ext || { x: 0, y: 0, w: W, h: H };
  const dx = Math.round(centro.x - (caixa.x + caixa.w / 2));
  const dy = Math.round(centro.y - (caixa.y + caixa.h / 2));
  const aspect = Object.hasOwn(ASPECTS, o.aspect) ? o.aspect : "livre";
  // Cinza é do externo: numa célula ele confundiria célula com prancheta.
  const rodizio = FRAME_COLORS.filter((c) => c !== "cinza");
  const out = [];
  if (ext) out.push({ x: ext.x + dx, y: ext.y + dy, w: ext.w, h: ext.h,
                      title: "Prancheta", aspect, color: "cinza" });
  for (let i = 0; i < L; i++) for (let j = 0; j < C; j++) {
    const k = i * C + j;
    // O título é a posição de slide que o frame vai ocupar (a regra de
    // `tituloNovo`): o externo vem antes e conta, senão o próximo frame da
    // toolbar pularia um número.
    out.push({ x: j * (w + g) + dx, y: i * (hh + g) + dy, w, h: hh, aspect,
               title: `Frame ${existentes + (o.externo ? 1 : 0) + k + 1}`,
               color: o.cor === "rodízio" || !FRAME_COLORS.includes(o.cor) ? rodizio[k % rodizio.length] : o.cor });
  }
  return out;
}

// Quem é dono de cada card no Organizar, e quais frames são "externos".
//
// Externo é o frame que contém outro frame inteiro e menor (a prancheta): ele
// não vira bloco, senão, sendo o primeiro na ordem, ficaria com todos os
// cards e desmontaria a grade. Entre os que sobram, o card fica com o MENOR
// frame que o contém inteiro; empate de área (frames iguais sobrepostos)
// decide pela ordem de slide, que era a regra única antes do aninhamento.
export function donos(nodes) {
  const frames = nodes.filter((n) => n.type === "trFrame");
  const area = (n) => { const r = rectOf(n); return r.w * r.h; };
  const externos = frames.filter((f) => frames.some((g) => g.id !== f.id && area(g) < area(f)
                                                       && inside(rectOf(g), rectOf(f))))
    .map((f) => f.id);
  const candidatos = frames.filter((f) => !externos.includes(f.id))
    .sort((a, b) => area(a) - area(b) || (a.data?.order ?? 0) - (b.data?.order ?? 0));
  const dono = {};
  candidatos.forEach((f) => containedCards(f, nodes).forEach((id) => { dono[id] ??= f.id; }));
  return { externos, dono };
}

// As unidades rígidas do Organizar: uma por externo DE CIMA (externo que não
// está dentro de outro externo). Tudo que está inteiro dentro dele no começo
// — células, externos aninhados e cards — é MEMBRO, e anda com ele como uma
// peça só: uma grade de slides tem que sobreviver ao Organizar, e o dagre de
// fora, sem arestas entre as células, as empilharia numa coluna.
//
// Os externos vão do maior pro menor (empate pela ordem de slide), e quem já
// é membro de uma unidade não abre outra: assim o aninhado cai dentro do de
// fora, e dois externos idênticos sobrepostos viram uma unidade só.
//
// Card com dono (ver `donos`) segue o dono: é membro só se o dono for. Card
// no pedaço de um frame de fora que invade o externo sairia com o frame dele
// no layout, e não pode ter duas posições.
//
// Volta `{unidades: [{id, membros: {frames, cards}}], unidadeDe: {id: externo}}`,
// com `unidadeDe` só pros membros (o externo de cima não é membro de si).
export function unidades(nodes) {
  const { externos, dono } = donos(nodes);
  const frames = nodes.filter((n) => n.type === "trFrame");
  const area = (n) => { const r = rectOf(n); return r.w * r.h; };
  const ext = frames.filter((f) => externos.includes(f.id))
    .sort((a, b) => area(b) - area(a) || (a.data?.order ?? 0) - (b.data?.order ?? 0));
  const unidadeDe = {};
  const lista = [];
  ext.forEach((e) => {
    if (e.id in unidadeDe) return;
    const r = rectOf(e);
    const membros = { frames: [], cards: [] };
    frames.forEach((f) => {
      if (f.id === e.id || f.id in unidadeDe || lista.some((u) => u.id === f.id)) return;
      if (inside(rectOf(f), r)) { unidadeDe[f.id] = e.id; membros.frames.push(f.id); }
    });
    lista.push({ id: e.id, membros });
  });
  // Cards depois dos frames: o de dono precisa saber se o dono entrou.
  nodes.filter((n) => n.type === "ndNode" && !(n.id in unidadeDe)).forEach((c) => {
    let u;
    if (c.id in dono) u = unidadeDe[dono[c.id]];
    else u = lista.find((x) => inside(rectOf(c), rectOf(frames.find((f) => f.id === x.id))))?.id;
    if (u) { unidadeDe[c.id] = u; lista.find((x) => x.id === u).membros.cards.push(c.id); }
  });
  return { unidades: lista, unidadeDe };
}

// Quanto cada membro de uma prancheta anda quando células crescem. A célula
// que cresce não pode só esticar pra direita e pra baixo: invadiria a vizinha,
// o card da vizinha passaria a ter outro dono e o Organizar seguinte faria
// outra coisa. Então a grade ABRE ESPAÇO: quem está inteiro à direita de um
// membro anda o que ele andou mais o que ele cresceu, e o mesmo pra baixo.
// Numa grade, a coluna k inteira anda a soma do maior crescimento de cada
// coluna antes dela (linhas e colunas continuam alinhadas); fora de grade, se
// nada se sobrepunha antes, nada se sobrepõe depois, porque o deslocamento de
// um membro é pelo menos o deslocamento mais o crescimento de tudo à esquerda.
// Ninguém anda pra cima nem pra esquerda.
//
// `membros` é `[{id, x, y, w, h, nw, nh}]`: retângulo ANTES do layout e o
// tamanho novo (card e célula que não cresceu têm nw = w). Volta `{id: {dx, dy}}`.
// Programação dinâmica na ordem da borda de antes: quem termina antes de `m`
// começar tem x menor (largura positiva), então já foi calculado.
export function abrirEspaco(membros) {
  const out = Object.fromEntries(membros.map((m) => [m.id, { dx: 0, dy: 0 }]));
  const eixo = (ini, tam, novo, d) => {
    const ord = [...membros].sort((a, b) => a[ini] - b[ini]);
    ord.forEach((m, i) => {
      let v = 0;
      for (let j = 0; j < i; j++) {
        const c = ord[j];
        if (c[ini] + c[tam] <= m[ini]) v = Math.max(v, out[c.id][d] + c[novo] - c[tam]);
      }
      out[m.id][d] = v;
    });
  };
  eixo("x", "w", "nw", "dx");
  eixo("y", "h", "nh", "dy");
  return out;
}

// O retângulo de um externo depois que suas células se arrumaram. Mesma
// doutrina do frame: o tamanho que o usuário escolheu é piso. Se tudo ainda
// cabe, o externo fica EXATAMENTE como estava (e o segundo Organizar não tem
// o que mudar); se algo vazou, ele cresce pra direita e pra baixo até cobrir
// os membros com a folga de frame, e trava a proporção esticando o lado curto
// (também pra direita ou pra baixo). O canto de cima-esquerdo NÃO anda: com
// `abrirEspaco` nenhum membro vai pra cima nem pra esquerda, então o canto
// continua válido, e reenvolver com cabeçalho e folga nos quatro lados
// empurraria o externo pra cima e pra esquerda a cada vazamento só à direita.
export function crescerExterno(atual, membros, aspect) {
  if (membros.every((m) => inside(m, atual))) return { x: atual.x, y: atual.y, w: atual.w, h: atual.h };
  const x1 = Math.max(...membros.map((r) => r.x + r.w)), y1 = Math.max(...membros.map((r) => r.y + r.h));
  let w = Math.max(atual.w, x1 - atual.x + FRAME_PAD), hh = Math.max(atual.h, y1 - atual.y + FRAME_PAD);
  // `ceil` pelo lado que manda, o outro saindo dele: a regra dos blocos do
  // Organizar, pra não devolver o meio pixel que o conteúdo precisava.
  const ra = ratioOf(aspect);
  if (ra) {
    if (w / hh < ra) { hh = Math.ceil(hh); w = Math.ceil(hh * ra); }
    else { w = Math.ceil(w); hh = Math.ceil(w / ra); }
  } else { w = Math.ceil(w); hh = Math.ceil(hh); }
  return { x: atual.x, y: atual.y, w, h: hh };
}

// A nota anda com o bloco que a contém, mas NUNCA é arranjada pelo dagre (ela
// não tem ligação, e o dagre a jogaria num canto, destruindo a composição
// feita à mão). Do MENOR frame que a contém inteira (mesma régua de `donos`:
// empate de área não importa aqui, uma nota não decide dono de card), ela
// herda o delta que ESSE frame andou: `framesNovos[container].{x,y} -
// rect[container].{x,y}`, usando o retângulo ORIGINAL (`rect`, o snapshot de
// antes do Organizar) e a posição NOVA (`framesNovos`, o `out.frames` do
// Organizar) — ela anda o MESMO tanto, não escala com o crescimento do frame.
// Sem frame contêiner, ou contêiner que não está em `framesNovos` (não fez
// parte do dagre, então não se moveu), a nota fica onde está: não entra no
// resultado.
//
// Pura: não sabe nada de dagre, card ou unidade. É por isso que mora AQUI, e
// não dentro de `organizar` (`inst/www/frames.js`): `organizar` chama
// `dagrePos`, que importa o pacote `@dagrejs/dagre` — indisponível sob
// `node --test` NESTE repositório (confirmado por experimento: nem
// `frames.js` nem um import isolado de `@dagrejs/dagre` resolvem fora do
// navegador, sem `node_modules` na raiz; só o importmap do navegador os acha).
// Isolando o cálculo da nota aqui, ele continua com teste direto sob Node; o
// resto do Organizar (dagre) segue sem teste sob Node, como já era antes
// desta task — nenhum teste existente importa `frames.js`.
export function notasComFrame(nodes, rect, framesNovos) {
  const frames = nodes.filter((n) => n.type === "trFrame");
  const area = (id) => rect[id].w * rect[id].h;
  const out = {};
  nodes.filter((n) => n.type === "trNota").forEach((n) => {
    const container = frames.filter((f) => inside(rect[n.id], rect[f.id]))
      .sort((a, b) => area(a.id) - area(b.id))[0];
    if (!container || !framesNovos[container.id]) return;
    const dx = framesNovos[container.id].x - rect[container.id].x;
    const dy = framesNovos[container.id].y - rect[container.id].y;
    out[n.id] = { x: n.position.x + dx, y: n.position.y + dy };
  });
  return out;
}

// Onde o hexágono da marca entra num PNG exportado. Fica aqui, e não no
// desenho, porque é conta com limites: em imagem pequena a marca não pode
// comer o conteúdo, e em imagem grande não pode virar cartaz.
//
// O piso e o teto estão em unidades de CSS — as do FRAME, não as do PNG. Por
// isso a escala da captura é PARÂMETRO daqui, e não uma multiplicação feita
// depois por quem desenha: com ela do lado de fora, nada impede alguém de
// passar os pixels do PNG (que sai em 2x) como se fossem os do frame, e aí a
// altura dobra mas o teto não — a marca fica com METADE da fração pretendida
// da imagem e o hexágono vira um ponto no canto de um frame grande. Foi o bug
// de 0a4dc3a, e ele passava pelos testes justamente porque a escala morava no
// `frames.js`, que o `node --test` não roda. Aqui dentro, o teste de
// invariância cobre isso.
//
// `w`/`hh`/`margem` entram em unidades do frame; o retângulo devolvido já sai
// em pixels de DESTINO (multiplicado pela escala), pronto pro `drawImage`.

// `173/200` é o `viewBox` de `inst/www/marca.svg`: é dele que vem a proporção
// do hexágono. Mudar o desenho (outro `viewBox`) sem mudar este número estica
// ou achata a marca no PNG, e em silêncio — nada aqui lê o SVG pra conferir.
export const MARCA_RAZAO = 173 / 200;
export function marcaDaAgua(w, hh, margem = 16, escala = 1) {
  const h = Math.min(48, Math.max(14, hh * 0.032));
  const larg = h * MARCA_RAZAO;
  return { w: larg * escala, h: h * escala, margem: margem * escala,
           x: (w - margem - larg) * escala, y: (hh - margem - h) * escala };
}
