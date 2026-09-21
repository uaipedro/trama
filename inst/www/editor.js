// inst/www/editor.js — o editor. Dirigido por catálogo: não conhece nenhum
// tipo de nó, nenhum tipo de dado, nenhuma categoria. Tudo — portas, cores,
// widgets de param, renderers de preview — vem do catálogo e dos registros do
// runtime. É a propriedade do insumo que mais se provou, e a única herdada
// sem repensar.
//
// Sem bundler, sem JSX: `React.createElement` direto. O custo é a verbosidade;
// o ganho é que uma coleção nova é um `.js` solto, sem toolchain.

import React, { useCallback, useEffect, useMemo, useRef, useState } from "react";
import { createRoot } from "react-dom/client";
import {
  ReactFlow, Background, BackgroundVariant, MiniMap, Controls,
  Handle, Position, applyNodeChanges, applyEdgeChanges, SelectionMode,
  useReactFlow, ReactFlowProvider,
} from "@xyflow/react";
import { h, getRenderer, getWidget, getViews, Segmented, setThemes } from "trama";
import { FrameNode, FrameDraw, ASPECTS, FRAME_COLORS, ratioOf, rectOf, inside,
         containedCards, containedFrames, containedNotes, fitAspect, FramePanel, exportFramePng,
         dagrePos, organizar, PranchetaPopover, gradeDeFrames, PRANCHETA_PADRAO,
         MARCA } from "./frames.js";
import { NotaNode, NotaDraw } from "./notas.js";
import { SettingsPanel } from "./settings.js";
import { contagemDoPasso } from "./params.js";

const NODE_W = 240, NODE_H = 190;

// --- Ponte com o Shiny -----------------------------------------------------
// UM canal pra cima (input `tr_op`), UM pra baixo (`tr_event`). `seq` existe
// porque `input$x` do Shiny descarta valores idênticos consecutivos — duas ops
// iguais em sequência sumiriam sem um campo que muda sempre.

let seqCounter = 0;

// `window.Shiny` existe assim que shiny.js carrega, mas `setInputValue` só
// depois que a sessão CONECTA. O editor é um módulo ESM que resolve imports de
// CDN, então a ordem entre as duas coisas não é garantida — chamar cedo demais
// lança "setInputValue is not a function" de dentro de um efeito do React,
// derruba a árvore e deixa a página em branco. Foi exatamente esta a
// armadilha que o insumo documentava ao esperar `shiny:connected`.
function shinyReady() {
  return !!(window.Shiny && typeof window.Shiny.setInputValue === "function");
}
// Polling, e não `addEventListener("shiny:connected")`: o Shiny dispara esse
// evento pelo jQuery, e um listener nativo pode simplesmente nunca ser
// chamado — a página fica em "carregando…" para sempre, sem erro nenhum.
// Poderíamos depender de `window.$`, mas amarrar o editor ao jQuery só pra
// isso é pior que um laço de 50ms que morre no primeiro acerto.
function onShinyReady(fn, tries = 200) {
  if (shinyReady()) return fn();
  if (tries <= 0) return console.error("[trama] Shiny não ficou pronto.");
  setTimeout(() => onShinyReady(fn, tries - 1), 50);
}

function sendOp(op, baseRev) {
  if (!shinyReady()) return null;
  const seq = ++seqCounter;
  window.Shiny.setInputValue("tr_op", { seq, base_rev: baseRev, op },
                             { priority: "event" });
  return seq;
}
function sendInput(name, value) {
  if (shinyReady()) window.Shiny.setInputValue(name, value, { priority: "event" });
}

const assetUrl = (rel) => `trama-store/${rel}`;

// --- Layout ----------------------------------------------------------------
// Posições são OPCIONAIS no documento (o schema diz isso de propósito: é o que
// permite escrever um grafo à mão ou com um LLM sem inventar coordenadas).
// Quem não tem posição recebe uma do dagre.

// O botão "Organizar" não passa por aqui: ele trata frame como bloco
// (`organizar`, em frames.js). Os dois dividem o grafo do dagre (`dagrePos`).
function autoLayout(nodes, edges) {
  const pos = dagrePos(nodes.map((n) => ({
    id: n.id, w: n.measured?.width || NODE_W, h: n.measured?.height || NODE_H })),
    edges.map((e) => [e.source, e.target]));
  return nodes.map((n) => ({ ...n, position: pos[n.id] }));
}

// --- Documento -> React Flow ----------------------------------------------

function docToFlow(doc, catalog) {
  const byId = catalog ? Object.fromEntries(catalog.nodes.map((n) => [n.id, n])) : {};
  let missing = 0;
  const nodes = Object.entries(doc.nodes || {}).map(([id, n]) => {
    const pos = (doc.ui && doc.ui.positions && doc.ui.positions[id]) || null;
    if (!pos) missing++;
    return {
      id, type: "ndNode",
      position: pos ? { x: pos[0], y: pos[1] } : { x: 0, y: 0 },
      // `seed` vem junto porque o card de nó `stochastic` a mostra e a troca.
      // É semântica (entra na chave de cache), não `ui.*`: mora no nó, não no
      // bloco de apresentação do documento.
      data: { nodeType: n.type, label: n.label, params: n.params || {}, spec: byId[n.type],
              seed: n.seed,
              view: (doc.ui && doc.ui.views && doc.ui.views[id]) || null,
              size: (doc.ui && doc.ui.sizes && doc.ui.sizes[id]) || null,
              fold: (doc.ui && doc.ui.folds && doc.ui.folds[id]) || null },
    };
  });
  // Aresta de FLUXO: a porta de SAÍDA de onde ela sai, OU a porta de ENTRADA
  // aonde ela chega, declara `stream` no catálogo. O front decide isso
  // sozinho, sem perguntar ao servidor — é a propriedade que `tr_catalog()`
  // existe pra preservar (`R/catalog.R`: "adapters viaja... pro front poder
  // responder... sem round-trip").
  //
  // As DUAS pontas, e não só a origem: `.tr_stream_detect()`
  // (`R/stream-region.R`) só faz a fonte e o nó com memória DECLARAREM —
  // um nó elevado (a maioria dos membros de uma região: um `data/filter`
  // comum, usado dentro e fora dela) não declara NADA, e mesmo assim toda
  // saída dele propaga fluxo enquanto estiver dentro da região. Sem checar
  // a entrada também, a aresta que sai de um elevado e entra no COLAPSO
  // (`data/from_stream`, cuja porta de entrada É declarada) ficava sem
  // traço — exatamente a aresta que fecha a região, que é a mais
  // importante de mostrar como fluxo. O que este critério NÃO alcança: uma
  // aresta entre DOIS elevados em sequência dentro da mesma região (nem
  // origem nem destino declara) — só a região inteira (via mensagem
  // `regions`) resolve isso, porque só ela sabe que os dois nós são membros.
  // Essa mensagem chega depois do `document`, de forma assíncrona — o traço
  // dessa aresta específica aparece um instante depois do resto do grafo — e
  // é o handler de `regions` (mais abaixo, junto de `regionNodesRef`) quem
  // completa: recomputa `edges` marcando `animated: true` em toda aresta cujas
  // DUAS pontas são membros da mesma região. Preferimos essa lacuna de um
  // instante a deixar a aresta cinza pra sempre: um traço sólido bem no meio
  // de um contorno tracejado contradiz a própria moldura, e "diz a verdade um
  // pouco atrasado" é menos ruim que "mente sobre a forma do grafo".
  const portaFluxo = (nodeId, port, lado) => {
    const n = doc.nodes?.[nodeId];
    const spec = n && byId[n.type];
    const portas = spec?.[lado]?.find((p) => p.name === port);
    return !!portas?.stream;
  };
  const ehFluxo = (e) =>
    portaFluxo(e.from.node, e.from.port, "outputs") || portaFluxo(e.to.node, e.to.port, "inputs");
  const edges = (doc.edges || []).map((e) => ({
    id: edgeId(e), source: e.from.node, sourceHandle: e.from.port,
    target: e.to.node, targetHandle: e.to.port, data: { index: e.index },
    // `animated` é o traço tracejado do próprio React Flow — nenhum CSS novo
    // pra manter em sincronia com a versão da biblioteca.
    animated: ehFluxo(e),
  }));
  // Frames vêm primeiro no array e com `zIndex: -1`: ficam atrás de cards e
  // ligações. A ordem de exibição (`index`) sai do `order`, e proporção que o
  // editor não conhece (documento escrito à mão) vira `livre`, mesma
  // doutrina da vista ausente.
  const frames = Object.entries((doc.ui && doc.ui.frames) || {})
    .sort(([, a], [, b]) => (a.order ?? 0) - (b.order ?? 0))
    .map(([id, f], i) => ({
      id, type: "trFrame", position: { x: f.x, y: f.y }, width: f.w, height: f.h,
      zIndex: -1, dragHandle: ".tr-frame-head",
      data: { title: f.title ?? "", aspect: Object.hasOwn(ASPECTS, f.aspect) ? f.aspect : "livre",
              color: f.color, order: f.order, index: i + 1 },
    }));
  const notes = Object.entries((doc.ui && doc.ui.notes) || {})
    .map(([id, n]) => ({
      id, type: "trNota", position: { x: n.x, y: n.y }, width: n.w, height: n.h,
      data: { kind: n.kind, text: n.text, src: n.src, fit: n.fit,
              escala: n.escala, fundo: n.fundo, color: n.color },
    }));
  return { nodes: [...frames, ...notes, ...(missing ? autoLayout(nodes, edges) : nodes)],
           edges, needsLayout: missing > 0 };
}

const edgeId = (e) =>
  `${e.from.node}:${e.from.port}->${e.to.node}:${e.to.port}#${e.index ?? 1}`;

// Id de cópia/colagem: gerado no cliente (o servidor aceita `id` explícito em
// `add_node`/`add_frame`/`add_note`, ver `.tr_op_add_node`), pra poder montar
// as ligações internas da cópia no MESMO batch — esperar o eco do servidor
// pra saber os ids novos deixaria connect correndo atrás de nó que ainda não
// existe do lado de cá. Alfabeto restrito a `[A-Za-z0-9_.-]` (`.tr_check_node_id`).
const novoId = () => `c${Date.now().toString(36)}${Math.random().toString(36).slice(2, 10)}`;

// --- Preview ---------------------------------------------------------------

// A vista escolhida chega de fora (`view`): quem resolve a lista é o `NdNode`,
// que precisa dela pra montar a faixa de abas. Aqui só se repete a mesma
// escolha, pra que aba acesa e desenho não possam divergir.
function pickView(handle, view) {
  const views = getViews(handle.preview.renderer, handle);
  // Vista salva que não existe mais (coleção atualizada, renderer trocado) cai
  // na primeira — nunca em card vazio, mesma doutrina do renderer ausente.
  return views.find((x) => x.id === view) || views[0] || null;
}

function Preview({ state, handle, error, progress, partial, view }) {
  if (error) {
    return h("div", { className: "tr-preview tr-preview-error", title: error.traceback || "" },
      [h("div", { key: "m", className: "tr-err-msg" }, error.message)]);
  }
  if (state === "running") {
    const bar = h("div", { key: "bar", className: "tr-progress" }, [
      h("div", { key: "f", className: "tr-progress-fill",
                 style: { width: `${Math.round(((progress && progress.fraction) || 0) * 100)}%` } }),
      h("span", { key: "m", className: "tr-progress-msg" },
        (progress && progress.message) || "computando…"),
    ]);
    if (partial && handle && handle.preview) {
      // Prévia parcial respeita a MESMA vista do resultado final: trocar de aba
      // no meio da execução e ver a vista antiga voltar sozinha quando o
      // parcial chega seria a aba mentindo sobre o que está na tela.
      const v = pickView(handle, view);
      return h("div", { className: "tr-preview tr-partial" },
        [bar, v ? h(v.component, { key: "p", artifact: handle.preview, handle, assetUrl }) : null]);
    }
    return h("div", { className: "tr-preview tr-busy" }, bar);
  }
  if (state === "pending") return h("div", { className: "tr-preview tr-busy" }, "na fila");
  if (state === "blocked") return h("div", { className: "tr-preview tr-blocked" }, "bloqueado");
  if (state === "invalid") return h("div", { className: "tr-preview tr-blocked" }, "incompleto");
  if (!handle || !handle.preview) return h("div", { className: "tr-preview tr-empty" }, "sem preview");

  const art = handle.preview;
  if (!getRenderer(art.renderer)) {
    // Renderer não carregado é INFORMAÇÃO, não erro: mostra o id pra o autor
    // da coleção saber exatamente o que registrar. A pergunta é feita ao
    // `getRenderer`, e NÃO a `getViews(...).length`: um handle com `summary`
    // ganha a vista `resumo` mesmo sem renderer, e o diagnóstico sumiria
    // atrás de um card que parece funcionar.
    return h("div", { className: "tr-preview tr-empty", title: art.renderer },
      `renderer ausente: ${art.renderer}`);
  }
  const v = pickView(handle, view);
  return h("div", { className: "tr-preview" },
    h(v.component, { artifact: art, handle, assetUrl }));
}

// --- Markdown --------------------------------------------------------------
// Só o subconjunto que as páginas de ajuda usam: `## título`, parágrafo, lista
// com `-` (com continuação indentada), bloco de código com crases triplas,
// `**negrito**` e `código`. Escrito à mão, e não vendorizado: uma biblioteca de
// markdown seria mais um pacote pra manter em sincronia dentro de `vendor/`,
// pelo mesmo resultado. Monta nós React em vez de `innerHTML` — o texto vem do
// catálogo, que é do próprio projeto, mas montar nó a nó é mais barato que
// sanitizar e não abre superfície nova.

function mdInline(t) {
  const parts = []; const re = /\*\*([^*]+)\*\*|`([^`]+)`/g;
  let last = 0, m, k = 0;
  while ((m = re.exec(t))) {
    if (m.index > last) parts.push(t.slice(last, m.index));
    parts.push(m[1] ? h("strong", { key: k++ }, m[1]) : h("code", { key: k++ }, m[2]));
    last = re.lastIndex;
  }
  if (last < t.length) parts.push(t.slice(last));
  return parts;
}

function md(text) {
  const lines = (text || "").split("\n"); const out = [];
  let i = 0, k = 0;
  while (i < lines.length) {
    const l = lines[i];
    if (l.startsWith("```")) {
      const buf = []; i++;
      while (i < lines.length && !lines[i].startsWith("```")) buf.push(lines[i++]);
      i++;
      out.push(h("pre", { key: k++ }, h("code", null, buf.join("\n"))));
    } else if (l.startsWith("## ")) {
      out.push(h("h4", { key: k++ }, l.slice(3))); i++;
    } else if (l.startsWith("- ")) {
      const items = [];
      while (i < lines.length && lines[i].startsWith("- ")) {
        let t = lines[i++].slice(2);
        // Item de lista quebrado em várias linhas: a continuação vem indentada
        // e pertence ao item anterior, não a um parágrafo novo.
        while (i < lines.length && /^\s+\S/.test(lines[i])) t += " " + lines[i++].trim();
        items.push(t);
      }
      out.push(h("ul", { key: k++ }, items.map((t, j) => h("li", { key: j }, mdInline(t)))));
    } else if (!l.trim()) {
      i++;
    } else {
      const buf = [];
      while (i < lines.length && lines[i].trim() && !/^(## |- |```)/.test(lines[i])) buf.push(lines[i++]);
      out.push(h("p", { key: k++ }, mdInline(buf.join(" "))));
    }
  }
  return out;
}

// --- Nó genérico -----------------------------------------------------------

// Piso e grade do redimensionamento. `GRID` é o mesmo `gap` do `<Background/>`:
// não alinha o card aos pontos do fundo (a posição não é snapada, e a altura
// total do card não é múltipla de 16), mas quantiza o tamanho na mesma unidade
// do fundo — mata o tremor sub-pixel e faz dois cards arrastados "no olho"
// darem exatamente a mesma largura. O piso repete o que o CSS já trava em
// `min-width`/`min-height`, pra alça não deixar arrastar abaixo do que o
// estilo aceitaria.
const GRID = 16;
const MIN_W = 240, MIN_H = 132;
const snap = (v) => Math.round(v / GRID) * GRID;

// Alça própria em vez do `NodeResizer`/`NodeResizeControl` do xyflow: eles
// escrevem largura E altura no nó do store, e aqui a altura que cresce é a do
// PREVIEW, não a do card — abaixo dele vêm params e portas, em quantidade que
// varia por tipo de bloco.
//
// Durante o arrasto as variáveis são escritas DIRETO no DOM, sem estado React:
// cada quadro passaria por setNodes, remedição e redecoração de todos os cards
// — o laço que trama.css documenta. O estado (e a op) só entram no `pointerup`,
// mesma disciplina do `move`, que só emite com `dragging === false`.
function Grip({ nodeId, onResize }) {
  // Desmontar no meio do arrasto é alcançável: Delete com o nó selecionado e o
  // ponteiro pressionado. Sem isto o `up` ainda rodaria e emitiria `resize` pra
  // um nó que não existe mais, voltando como `op_rejected` — barulho por nada.
  const fim = useRef(null);
  useEffect(() => () => { if (fim.current) fim.current(false); }, []);
  // O ponteiro anda em pixels de TELA; a largura do card é em pixels de
  // layout, e entre os dois está o zoom do canvas. Sem dividir por ele, o
  // `fitView` da abertura (que abre abaixo de 1) faz a alça descolar do
  // cursor: o card cresce o dobro do que a mão pediu. `getZoom()` lê sob
  // demanda e não assina o viewport, então não redesenha card nenhum.
  const rf = useReactFlow();

  const onDown = (ev) => {
    ev.preventDefault(); ev.stopPropagation();
    const card = ev.currentTarget.closest(".tr-node");
    const pv = card && card.querySelector(".tr-preview");
    if (!pv) return;
    // Captura de ponteiro: sem ela, soltar o botão FORA da janela nunca entrega
    // o `pointerup` e os listeners ficariam pendurados, arrastando o card
    // sozinho no próximo movimento do mouse.
    try { ev.currentTarget.setPointerCapture(ev.pointerId); } catch (_) {}
    const x0 = ev.clientX, y0 = ev.clientY;
    const w0 = card.offsetWidth, h0 = pv.offsetHeight;
    // Lido uma vez, no começo: o zoom não muda no meio de um arrasto, e reler
    // por quadro só daria a chance de o card pular se mudasse.
    const z = rf.getZoom() || 1;
    let w = w0, hgt = h0;
    const move = (e) => {
      w = Math.max(MIN_W, snap(w0 + (e.clientX - x0) / z));
      hgt = Math.max(MIN_H, snap(h0 + (e.clientY - y0) / z));
      card.style.setProperty("--tr-w", `${w}px`);
      card.style.setProperty("--tr-h", `${hgt}px`);
    };
    // `commit = false` é saída sem confirmar: o arrasto foi cancelado (ponteiro
    // perdido, nó desmontado) e o card volta ao tamanho em que começou.
    const encerrar = (commit) => {
      window.removeEventListener("pointermove", move);
      window.removeEventListener("pointerup", up);
      window.removeEventListener("pointercancel", cancelar);
      fim.current = null;
      if (!commit) {
        card.style.setProperty("--tr-w", `${w0}px`);
        card.style.setProperty("--tr-h", `${h0}px`);
        return;
      }
      // Clique seco na alça não é redimensionamento: emitir a op mesmo assim
      // sujaria o documento e gastaria um passo do desfazer sem mudar nada.
      if (w !== w0 || hgt !== h0) onResize(nodeId, w, hgt);
    };
    const up = () => encerrar(true);
    const cancelar = () => encerrar(false);
    fim.current = encerrar;
    window.addEventListener("pointermove", move);
    window.addEventListener("pointerup", up);
    window.addEventListener("pointercancel", cancelar);
  };
  return h("div", { className: "tr-grip nodrag", title: "redimensionar",
                    onPointerDown: onDown });
}

// --- Controles de fluxo (Tarefa 8.2) ---------------------------------------
//
// Só no card da FONTE de uma região (Decisão 5: `data/to_stream` é o âncora).
// `tr_stream_cmd` é COMANDO, não op — `data.onStreamCmd` (montado em App) só
// chama `sendInput`, nunca `pushOp`: não entra no log de desfazer, não mexe
// em `rev`, e a velocidade não é param (Decisão 9) — arrastar o slider não
// pode mudar a CHAVE da região e reexecutar dez mil pontos.
//
// Sob o executor SEQUENCIAL os botões ficam mortos por construção — o processo
// R está preso dentro do laço e não há quem leia `control.json` até a região
// terminar (`R/stream-control.R` documenta) — e não há como o front SABER
// qual executor está em uso. Em vez de fingir que funciona ou escondê-los,
// eles ficam sempre visíveis enquanto a região roda, com uma legenda que diz
// a condição; o clique nunca é ignorado em silêncio no navegador — na pior
// hipótese (sequencial) ele só chega tarde demais pro servidor honrar.
function StreamControls({ region, ctl, onCmd, progress }) {
  const rodando = ctl?.estado !== "paused";
  const tempoRemoto = ctl?.tempo ?? 0;
  // Eco LOCAL do slider, separado de `ctl.tempo` (o eco otimista do último
  // COMANDO mandado — `onStreamCmd`, em App). Sem isto o `onInput` de baixo
  // não teria onde escrever sem já mandar `tr_stream_cmd`: `value` do input
  // tem que vir de algum estado pra a alça se mover sob o dedo, e não podia
  // ser `tempoRemoto` porque esse só muda quando o comando É mandado — exatamente
  // o que estamos adiando até soltar.
  const [tempoLocal, setTempoLocal] = useState(tempoRemoto);
  // Ressincroniza quando o eco do comando muda por FORA de um arrasto em
  // andamento (outra sessão, ou o clique de play/pause que também escreve
  // `ctl.tempo` como está — não altera). Guardado por um ref, e não por
  // comparar valores, porque `tempoLocal` já É o valor mais recente durante o
  // arrasto: um `useEffect` sem a guarda faria o slider "voltar" pro valor
  // remoto no meio do gesto, brigando com o dedo do usuário.
  const arrastandoRef = useRef(false);
  useEffect(() => { if (!arrastandoRef.current) setTempoLocal(tempoRemoto); }, [tempoRemoto]);

  // ANTES: um `onChange` só, que o React entrega como o `input` nativo do
  // DOM — dispara a CADA pixel do arrasto, não só ao soltar (é a semântica
  // que o React usa pra tudo que chama `onChange`, diferente do HTML puro
  // onde `change` só dispara na soltura). Um arrasto sintético de 8 passos
  // media 7 comandos (`seq` 2→8); um arrasto de mouse de verdade é uma ordem
  // de grandeza pior. Cada um é um read-modify-write INTEIRO de
  // `control.json` (`R/stream-control.R` documenta: já é a escrita não
  // atômica que a corrida entre vários escritores não fecha) — mandar um por
  // pixel multiplica a janela da corrida por nada.
  //
  // AGORA: `onInput` só atualiza o eco local (a alça se move, o número ao
  // lado acompanha), sem tocar no barramento. O COMANDO só sai na soltura do
  // gesto (`onPointerUp`) ou, pra quem mexe pelo teclado (setas, sem
  // ponteiro nenhum), no `onKeyUp` — as duas únicas formas de "arrasto
  // terminou" que um `<input type=range>` tem.
  const mandar = (e) => onCmd(region.id, "tempo", Number(e.target.value));
  return h("div", { key: "stream", className: "tr-stream nodrag" }, [
    h("button", { key: "pp", className: "tr-stream-btn",
                 title: rodando ? "pausar" : "retomar",
                 onClick: (e) => { e.stopPropagation(); onCmd(region.id, rodando ? "pause" : "play"); } },
      rodando ? "⏸" : "▶"),
    h("button", { key: "st", className: "tr-stream-btn", title: "um passo",
                 onClick: (e) => { e.stopPropagation(); onCmd(region.id, "step"); } }, "⏭"),
    h("input", { key: "tp", className: "tr-stream-tempo nodrag", type: "range",
                min: 0, max: 5, step: 0.1, value: tempoLocal, title: `${tempoLocal.toFixed(1)}s por passo`,
                onClick: (e) => e.stopPropagation(),
                onPointerDown: () => { arrastandoRef.current = true; },
                onInput: (e) => setTempoLocal(Number(e.target.value)),
                onPointerUp: (e) => { arrastandoRef.current = false; mandar(e); },
                onKeyUp: (e) => { if (!arrastandoRef.current) mandar(e); } }),
    h("span", { key: "ct", className: "tr-stream-count" },
      // `passo` (variável) NÃO existe aqui — é local a `App()`, lá embaixo.
      // Em módulo ES (strict mode) referenciar um nome inexistente é
      // ReferenceError na hora, não `undefined`: todo card com região RODANDO
      // quebrava o render assim que `StreamControls` entrava em cena.
      // `contagemDoPasso` já vem importado (`./params.js`) pra extrair
      // "passo N / M" da MESMA mensagem que a barra de progresso usa
      // (`progress.message`, formato `{fraction, message}` — ver `Preview`
      // logo acima, que lê os dois campos do mesmo jeito).
      contagemDoPasso(progress?.message) || "aguardando…"),
    h("span", { key: "hint", className: "tr-stream-hint",
               title: "play/pause/passo só respondem com execução em pool; " +
                      "no executor sequencial o processo fica ocupado com a região inteira" }, "ⓘ"),
  ]);
}

function NdNode({ id, data, selected }) {
  const spec = data.spec;
  if (!spec) {
    // Tipo desconhecido vira nó ÓRFÃO visível, nunca documento que não abre.
    return h("div", { className: "tr-node tr-node-orphan" },
      `coleção ausente para: ${data.nodeType}`);
  }
  const cat = data.categories?.[spec.category];
  const fold = data.fold || {};
  const semPreview = fold.preview === false, semParams = fold.params === false;
  const nParams = (spec.params || []).length;
  const falhou = data.state === "failed" || data.state === "invalid";
  const frac = data.progress?.fraction;
  // Contorno sutil pra todo card que é MEMBRO de uma região de fluxo —
  // fonte, elevado ou colapso, os três (`data.emRegiao` vem do índice reverso
  // montado no handler de `regions`, em App). Não é um retângulo por cima de
  // vários cards (isso seguiria posição e arrasto de cada um, como o Frame já
  // faz por outro motivo, e duplicaria aquela maquinaria pra um sinal que só
  // precisa dizer "isto roda dentro de um laço"); é uma marca POR CARD, que
  // continua certa se o autor arrastar os nós pra qualquer lugar do canvas.
  const cls = ["tr-node", selected ? "tr-node-sel" : "", `tr-state-${data.state || "idle"}`,
              data.emRegiao ? "tr-node-region" : ""]
    .filter(Boolean).join(" ");

  // A faixa é SEMPRE reservada, inclusive com uma vista só. As vistas dependem do
  // renderer, que só se sabe quando o preview chega; faixa que aparecesse junto
  // com o resultado mudaria a altura do card no meio do caminho — o laço de
  // remedição que trama.css documenta. Com uma vista só ela exibe o nome dela,
  // que é o indicador de tipo de saída que o card não tinha.
  const views = data.handle && data.handle.preview
    ? getViews(data.handle.preview.renderer, data.handle) : [];
  const cur = views.find((v) => v.id === data.view) || views[0] || null;

  // Só o preview cresce em altura: abaixo dele vêm params e portas, em número
  // que varia por bloco, então altura total explícita cortaria os cards
  // cheios. `--tr-w` dimensiona o card, `--tr-h` só a faixa de preview.
  // Ausente, cada variável some do `style` e o padrão do CSS vale.
  const [w, hgt] = data.size || [];
  return h("div", { className: cls, style: {
    "--tr-w": w ? `${w}px` : undefined, "--tr-h": hgt ? `${hgt}px` : undefined } }, [
    h("div", { key: "hd", className: "tr-node-head",
               style: { background: cat?.color || "#64748b" } }, [
      // Sem `color`: aqui o ícone herda a cor de FRENTE da faixa, porque a
      // faixa já É a cor da categoria (o background logo acima). Mesmo
      // componente, contexto invertido.
      spec.icon ? h(Icon, { key: "i", icon: spec.icon, className: "tr-node-icon" }) : null,
      h("span", { key: "l", className: "tr-node-title" }, data.label || spec.label),
      // Com o preview recolhido, o estado de execução muda pro cabeçalho: um
      // preview escondido não pode esconder uma falha. O preview NÃO reabre
      // sozinho, porque isso desfaria a arrumação que o usuário escolheu.
      semPreview && falhou
        ? h("span", { key: "al", className: "tr-head-alert",
                      title: data.error?.message || "falhou" },
            h(Icon, { icon: { kind: "set", value: "triangle-alert" }, className: "tr-node-icon" }))
        : null,
      h("button", { key: "fv", className: "tr-fold-btn nodrag",
                    title: semPreview ? "mostrar preview (P)" : "recolher preview (P)",
                    onClick: (e) => { e.stopPropagation(); data.onFold(id, { preview: semPreview }); } },
        h(Icon, { icon: { kind: "set", value: semPreview ? "chevron-right" : "chevron-down" },
                  className: "tr-node-icon" })),
      // Só para nó declarado `stochastic`: é a ÚNICA forma de re-sortear pela
      // tela, porque a semente não é param e portanto não tem campo no corpo
      // do card. Sem ele, um gerador aberto no editor ficaria preso à amostra
      // com que nasceu — reproduzível, o que é o ponto, mas irreversivelmente.
      spec.stochastic
        ? h("button", { key: "sd", className: "tr-fold-btn nodrag",
                        title: `re-sortear (semente ${data.seed ?? "?"})`,
                        onClick: (e) => { e.stopPropagation(); data.onReseed(id); } },
            h(Icon, { icon: { kind: "set", value: "dices" }, className: "tr-node-icon" }))
        : null,
      h("button", { key: "?", className: "tr-help-btn", title: "ajuda deste bloco",
                    onClick: (e) => { e.stopPropagation(); data.onHelp(data.nodeType); } }, "?"),
      data.duration != null
        ? h("span", { key: "d", className: "tr-dur", title: "última execução real" },
            fmtDur(data.duration)) : null,
      data.state === "cached" ? h("span", { key: "c", className: "tr-dot", title: "cache" }) : null,
      // Absoluta, por cima da borda de baixo do cabeçalho: uma barra que
      // entrasse no fluxo mudaria a altura do card quando a execução começa,
      // e altura em função do que chegou é o laço que trama.css evita. Sem
      // fração, a barra fica indeterminada em vez de parada em 0%: é o único
      // sinal de vida do card recolhido. `== null`, não falsy: 0 explícito
      // é progresso de verdade e mostra 0%.
      semPreview && data.state === "running"
        ? h("div", { key: "hp", className: "tr-head-progress" + (frac == null ? " tr-indet" : "") },
            h("div", { style: frac == null ? undefined : { width: `${Math.round(frac * 100)}%` } }))
        : null,
    ]),
    // Visível enquanto a região RODA (dobrado ou não — esconder com o preview
    // recolhido tiraria justamente o botão de pausa de quem recolheu o card
    // pra caber mais fluxo na tela). ANTES disto era incondicional em
    // `data.streamSource`: uma região que ainda não rodou nenhuma vez (idle)
    // e uma que já terminou (done/cached/failed/...) mostravam ⏸
    // "aguardando…" do mesmo jeito que uma rodando de verdade — um botão de
    // pausa pra um laço que não está rodando é o convite errado (clicar não
    // faz nada visível, porque não há nada em voo pra pausar). `data.state`
    // do card da FONTE espelha o da região inteira (`regionMemberPatch`
    // propaga "running" pro papel source/lifted como está, e o estado
    // TERMINAL também) — é o mesmo sinal que já pinta a borda do card, então
    // gatear por ele não pede estado novo nenhum.
    data.streamSource && data.state === "running"
      ? h(StreamControls, { key: "sc", region: data.streamSource,
                            ctl: (data.streamCtl || {})[data.streamSource.id],
                            onCmd: data.onStreamCmd, progress: data.progress })
      : null,
    semPreview ? null : h(Preview, { key: "pv", state: data.state, handle: data.handle, error: data.error,
                 progress: data.progress, partial: data.partial, view: cur?.id }),
    semPreview ? null : h("div", { key: "tabs", className: "tr-tabs" },
      views.length === 0
        ? h("span", { key: "-", className: "tr-tab-idle" }, "—")
        : views.length === 1
          ? h("span", { key: "1", className: "tr-tab-only" }, views[0].label)
          : views.map((v) => h("button", {
              key: v.id,
              className: "tr-tab nodrag" + (cur && v.id === cur.id ? " tr-tab-on" : ""),
              title: v.label,
              onClick: (e) => { e.stopPropagation(); data.onView(id, v.id); },
            }, v.label))),
    // Recolhidos, os parâmetros viram uma linha que diz QUANTOS são: o card
    // nunca esconde que tem configuração.
    semParams
      ? (nParams ? h("button", { key: "pm", className: "tr-params-fold nodrag",
                                 title: "mostrar parâmetros (O)",
                                 onClick: (e) => { e.stopPropagation(); data.onFold(id, { params: true }); } },
                     `${nParams} parâmetro${nParams > 1 ? "s" : ""} ▸`) : null)
      : h("div", { key: "pm", className: "tr-params" }, (spec.params || []).map((p) => {
          const W = getWidget(p.kind);
          // `div`, e não `label`: o `<label>` repassa o clique ao primeiro
          // controle rotulável de dentro, e com botões ali (segmentado, chave)
          // clicar no texto "Tipo" escolhia a primeira opção. Sem `htmlFor`
          // de propósito: os widgets não recebem `id`, e manter a API
          // `(spec, value, onChange)` das coleções vale mais que focar o
          // campo clicando no rótulo.
          return h("div", { key: p.name, className: "tr-param" }, [
            h("span", { key: "n", title: p.name }, p.label || p.name),
            // O widget entra num Fragment com `key` porque vai num array ao lado
            // do rótulo, e o elemento que a coleção devolve não tem chave.
            W ? h(React.Fragment, { key: "w" }, W(p, data.params[p.name], (v) => data.onParam(id, p.name, v)))
              : h("input", { key: "w", className: "nodrag", type: "text",
                             defaultValue: JSON.stringify(data.params[p.name] ?? p.default),
                             onBlur: (e) => { try { data.onParam(id, p.name, JSON.parse(e.target.value)); }
                                              catch (_) {} } }),
          ]);
        })),
    h("div", { key: "po", className: "tr-ports" }, [
      h("div", { key: "in", className: "tr-in" }, (spec.inputs || []).map((p) =>
        h("div", { key: p.name, className: "tr-port" }, [
          h(Handle, { key: "h", type: "target", position: Position.Left, id: p.name,
                      style: { background: data.typeColors?.[p.type] || "#64748b" } }),
          h("span", { key: "n", title: p.type },
            p.name + (p.multiple ? " (N)" : "") + (p.required ? "" : "?")),
        ]))),
      h("div", { key: "out", className: "tr-out" }, (spec.outputs || []).map((p) =>
        h("div", { key: p.name, className: "tr-port tr-port-out" }, [
          h("span", { key: "n", title: p.type }, p.name),
          h(Handle, { key: "h", type: "source", position: Position.Right, id: p.name,
                      style: { background: data.typeColors?.[p.type] || "#64748b" } }),
        ]))),
    ]),
    semPreview ? null : h(Grip, { key: "gr", nodeId: id, onResize: data.onResize }),
  ]);
}

const nodeTypes = { ndNode: NdNode, trFrame: FrameNode, trNota: NotaNode };

function fmtDur(s) {
  if (s < 1) return `${Math.round(s * 1000)}ms`;
  if (s < 60) return `${s.toFixed(1)}s`;
  return `${Math.floor(s / 60)}min`;
}

// --- Ícones ---------------------------------------------------------------
// O sprite é referenciado por fragmento, nunca embutido: o navegador busca o
// arquivo uma vez e só os ícones que a tela usa entram no DOM — não os 1834.
// `import.meta.url` já carrega o prefixo versionado que o htmlDependency serve
// (`trama-0.0.0.9000/`), o mesmo problema que R/app.R:35-37 documenta ter tido
// com o importmap, e que aqui se resolve sozinho.
const SPRITE = new URL("vendor/lucide.svg", import.meta.url).href;

// Os `kind` que este front sabe desenhar. É constante porque DUAS decisões a
// consultam — `Icon` para desenhar, e a calha da paleta para escolher entre
// ícone e bolinha — e elas discordarem deixa a linha sem nenhum dos dois,
// desalinhando a coluna de rótulos. Ambas leem DAQUI: acrescentar um `kind`
// aqui sem ensinar o `Icon` a desenhá-lo devolve `null`, que a calha já trata.
const ICON_KINDS = new Set(["set", "svg"]);

function Icon({ icon, className, color }) {
  // `kind` que este front não conhece é um catálogo mais novo que o editor.
  // Sai aqui, antes de qualquer ramo: cair no de innerHTML faria um `kind`
  // futuro injetar como markup um valor que ninguém prometeu que fosse markup.
  // E é a MESMA pergunta que a calha da paleta faz, por construção.
  if (!icon || !ICON_KINDS.has(icon.kind)) return null;
  const props = { className, viewBox: "0 0 24 24", "aria-hidden": "true",
                  style: color ? { color } : undefined };
  if (icon.kind === "set") {
    return h("svg", props, h("use", { href: `${SPRITE}#${icon.value}` }));
  }
  if (icon.kind === "svg") {
    // A coleção entrega GEOMETRIA, não documento: o <svg> e o viewBox são
    // nossos. Não é barreira de confiança — uma coleção é um pacote R
    // instalado e já roda JS próprio (R/app.R:131-142) — é o que faz o ícone
    // de terceiro herdar cor e tamanho como os do conjunto.
    return h("svg", { ...props, dangerouslySetInnerHTML: { __html: icon.value } });
  }
  return null;
}

// --- Paleta ----------------------------------------------------------------

// Dois modos. NAVEGAR: uma aba por coleção, e dentro dela as seções de
// categoria — a mesma renderização de sempre, recortada. ACHAR (busca digitada
// ou arrasto de porta): a aba não manda, porque quem procura quer "o que
// serve", não "de que coleção veio" — as abas somem e a lista vira global, com
// o selo dizendo de onde cada bloco vem.
function Palette({ catalog, filterType, onPick }) {
  const [q, setQ] = useState("");
  const [tab, setTab] = useState(null);
  const cols = catalog.collections || [];
  // Derivar em vez de guardar: se a coleção da aba ativa sumir entre duas
  // cargas de catálogo, cai na primeira em vez de deixar a paleta vazia.
  const active = cols.some((c) => c.id === tab) ? tab : cols[0]?.id;
  const achando = q.trim() !== "" || !!filterType;

  const hits = useMemo(() => {
    const term = q.trim().toLowerCase();
    return (catalog.nodes || []).filter((n) => {
      if (term && !`${n.label} ${n.description || ""} ${n.id}`.toLowerCase().includes(term)) return false;
      // Filtro por compatibilidade: arrastando de uma porta, só o que conecta.
      // O catálogo carrega os adaptadores justamente pra o front responder
      // isso sozinho, sem round-trip — o servidor continua validando.
      if (filterType && !(n.inputs || []).some((p) => compatible(catalog, filterType, p.type))) return false;
      return true;
    });
  }, [catalog, q, filterType]);

  // A coleção sai do prefixo do id, que o R garante estar sob o namespace da
  // coleção (R/collection.R:19-22). Repetir o campo em cada nó só engordaria o
  // catálogo com um dado que já viaja.
  const groups = useMemo(() => {
    const m = {};
    hits.forEach((n) => {
      if (n.id.split("/")[0] !== active) return;
      (m[n.category || "outros"] ||= []).push(n);
    });
    return m;
  }, [hits, active]);

  const achados = useMemo(
    () => [...hits].sort((a, b) => (a.label || a.id).localeCompare(b.label || b.id, "pt")),
    [hits]);

  const rotulo = (id) => cols.find((c) => c.id === id)?.label || id;

  const catColor = (n) =>
    (catalog.categories || []).find((c) => c.id === n.category)?.color || "#64748b";

  const item = (n, selo) => h("button", {
    key: n.id, className: "tr-palette-item", title: n.description || n.id,
    draggable: true,
    onDragStart: (e) => {
      e.dataTransfer.setData("application/trama-type", n.id);
      e.dataTransfer.effectAllowed = "copy";
    },
    onClick: () => onPick(n.id),
  }, [
    // Calha de largura fixa: ícone quando o bloco declara um, bolinha da cor
    // da categoria quando não. Nunca vazia, senão a lista desalinha entre um
    // bloco com ícone e o de baixo sem.
    n.icon && ICON_KINDS.has(n.icon.kind)
      ? h(Icon, { key: "i", icon: n.icon, className: "tr-palette-icon",
                  color: catColor(n) })
      : h("i", { key: "i", className: "tr-palette-dot",
                 style: { background: catColor(n) } }),
    h("span", { key: "l", className: "tr-palette-label" }, n.label),
    selo ? h("span", { key: "s", className: "tr-palette-seal" }, selo) : null,
  ]);

  return h("aside", { className: "tr-palette" }, [
    h("div", { key: "head", className: "tr-palette-head" }, [
      h("strong", { key: "title" }, "Blocos"),
      h("span", { key: "hint" }, "Clique para adicionar ou arraste para a tela"),
    ]),
    // Uma coleção só não tem o que escolher: a fileira some inteira em vez de
    // mostrar uma aba órfã sempre ativa.
    achando || cols.length < 2 ? null
      : h("div", { key: "tabs", className: "tr-palette-tabs" },
          cols.map((c) => h("button", {
            key: c.id,
            className: c.id === active ? "tr-palette-tab tr-palette-tab-on" : "tr-palette-tab",
            onClick: () => setTab(c.id),
          }, c.label || c.id))),
    h("input", { key: "q", className: "tr-search", placeholder: "Buscar blocos…", "aria-label": "Buscar blocos",
                 value: q, onChange: (e) => setQ(e.target.value) }),
    filterType ? h("div", { key: "f", className: "tr-filter" }, `aceita ${filterType}`) : null,
    achando ? h("div", { key: "c", className: "tr-count" },
      achados.length === 1 ? "1 resultado" : `${achados.length} resultados`) : null,
    h("div", { key: "b", className: "tr-palette-body" },
      achando
        // O selo responde "de que coleção veio?", pergunta que não existe
        // quando só há uma — mesma razão de a fileira de abas sumir ali.
        ? achados.map((n) => item(n, cols.length < 2 ? null : rotulo(n.id.split("/")[0])))
        : Object.entries(groups).map(([cid, items]) => {
            const meta = (catalog.categories || []).find((c) => c.id === cid);
            return h("section", { key: cid }, [
              h("h4", { key: "t" }, [
                h("i", { key: "d", style: { background: meta?.color || "#64748b" } }),
                meta?.label || cid,
              ]),
              items.map((n) => item(n, null)),
            ]);
          })),
  ]);
}

// --- Ajuda -----------------------------------------------------------------
// Toma o lugar da paleta, na mesma coluna: é o `?funcao` do R dentro do canvas,
// sem tirar ninguém de onde estava.

function Help({ catalog, typeId, onClose }) {
  const spec = (catalog.nodes || []).find((n) => n.id === typeId);
  if (!spec) return null;
  return h("aside", { className: "tr-help" }, [
    h("div", { key: "hd", className: "tr-help-head" }, [
      h("strong", { key: "t" }, spec.label || spec.id),
      h("button", { key: "x", className: "tr-help-close", title: "voltar à paleta",
                    onClick: onClose }, "×"),
    ]),
    h("code", { key: "id", className: "tr-help-id" }, spec.id),
    h("div", { key: "b", className: "tr-help-body" },
      spec.help ? md(spec.help) : h("p", null, spec.description || "sem ajuda")),
  ]);
}

function compatible(catalog, from, to) {
  if (from === to) return true;
  return (catalog.adapters || []).some((a) => a.from === from && a.to === to);
}

// Espelha exportFramePng (frames.js): Blob, e não data: URL, pelo mesmo
// motivo — um flow grande em base64 pesa um terço a mais no href. Revogado
// depois de dar tempo ao clique iniciar o download.
function exportFlowJson(doc, nomeArquivo) {
  const texto = JSON.stringify(doc, null, 2);
  exportText(texto, nomeArquivo, "application/json");
}

function exportText(texto, nomeArquivo, tipo = "text/plain;charset=utf-8") {
  const blob = new Blob([texto], { type: tipo });
  const u = URL.createObjectURL(blob);
  const a = document.createElement("a");
  a.href = u;
  a.download = nomeArquivo;
  a.click();
  setTimeout(() => URL.revokeObjectURL(u), 1000);
}

// --- Diálogo de projeto ----------------------------------------------------

// Navegador de pastas do SERVIDOR. O navegador não tem como escolher pasta do
// disco do R (`webkitdirectory` manda os arquivos, não o caminho), então cada
// passo é uma ida ao servidor: `tr_browse` sobe, `listing` desce. Até a
// primeira resposta `listagem` é `null` — daí o caminho reticente e a lista
// vazia, e não uma tela que só aparece depois.
//
// Não fecha ao clicar fora, ao contrário do lightbox de imagem: lá o clique
// errado custa reabrir a imagem, aqui custa o nome digitado e a pasta
// navegada. Sai pelo × ou pelo Esc, que são gestos deliberados.
function ProjectDialog({ listagem, atual, enviando, onBrowse, onOpen, onNew, onImport, onClose,
                         arquivoInicial }) {
  const [nome, setNome] = useState("");
  const [arquivo, setArquivo] = useState(null); // { nomeArquivo, conteudo } | null
  const [arrastando, setArrastando] = useState(false);
  const fundoRef = useRef(null);
  const caixaRef = useRef(null);
  const fileRef = useRef(null);
  const l = listagem;
  // `path` da raiz do disco é "/", e concatenar daria "//sub". `normalizePath`
  // do R pode preservar a barra dupla, e aí o caminho que sobe não é o mesmo
  // que desceu — o "../" deixaria de bater com a pasta de onde se veio.
  // O separador aqui é `/` e só: quem valida caminho é o R
  // (`.tr_project_path` recusa `/` e `\` no nome da pasta nova, e o teste da
  // raiz tem skip no Windows). O front não tenta adivinhar separador de
  // outro sistema — é escolha, não descuido.
  const descer = (n) => onBrowse(`${l.path.replace(/\/+$/, "")}/${n}`);
  const criar = () => onNew(l.path, nome.trim());
  // `enviando` trava as DUAS ações enquanto o pedido está em voo: a resposta
  // demora (abrir um projeto carrega o flow, valida o documento e dispara
  // `run_now`), nada na tela mudava nesse intervalo, e um segundo Enter —
  // garantido com a tecla em auto-repetição — mandava um segundo
  // `tr_project_new` que voltava "já tem trama.json" DEPOIS do sucesso do
  // primeiro. A navegação não é travada de propósito: descer uma pasta é uma
  // ida barata, e um bloqueio ali deixaria a lista morta se uma resposta se
  // perdesse.
  const podeCriar = !!l && !!nome.trim() && !enviando;
  const podeAbrir = !!l?.project && l.path !== atual && !enviando;

  // Nome sugerido é o do arquivo sem extensão — o usuário ainda pode trocar
  // antes de confirmar, mas datilografar de novo o que já está no nome do
  // arquivo seria atrito sem motivo.
  const lerArquivo = (file) => {
    if (!file) return;
    const leitor = new FileReader();
    leitor.onload = () => {
      setArquivo({ nomeArquivo: file.name, conteudo: leitor.result });
      if (!nome.trim()) setNome(file.name.replace(/\.json$/i, ""));
    };
    leitor.readAsText(file);
  };
  const importar = () => { if (arquivo && nome.trim()) onImport(l.path, nome.trim(), arquivo.conteudo); };
  const podeImportar = !!l && !!arquivo && !!nome.trim() && !enviando;

  // Arquivo solto na PÁGINA (fora deste diálogo) chega aqui já lido — o
  // diálogo mal montou e o gesto do usuário já aconteceu. Sem dependência de
  // `nome`: o efeito roda uma vez, ao montar, com o valor que `App` tinha na
  // hora do drop (ver `onDropGlobal`); ler `nome` de novo depois recriaria o
  // efeito a cada tecla digitada no campo.
  useEffect(() => {
    if (arquivoInicial) {
      setArquivo(arquivoInicial);
      setNome((n) => n.trim() ? n : arquivoInicial.nomeArquivo.replace(/\.json$/i, ""));
    }
  }, []);

  useEffect(() => {
    const esc = (e) => { if (e.key === "Escape") onClose(); };
    window.addEventListener("keydown", esc);
    return () => window.removeEventListener("keydown", esc);
  }, [onClose]);

  // Armadilha de foco por `inert` nos irmãos. Sem ela, Shift+Tab a partir do ×
  // saía do overlay e caía em "↶ Desfazer", na toolbar: invisível sob o
  // overlay, e um Enter ali desfaz a última edição sem nada na tela dizer —
  // Enter é ativação nativa de botão, não passa pela tabela de atalhos, então
  // a guarda de teclado do App não pega. `inert` foi preferido a um ciclo de
  // Tab mantido à mão porque não exige lista de elementos focáveis (que
  // envelhece a cada botão novo) e mata de quebra o clique e o foco em tudo
  // que está atrás. O banner fica de fora: ele aparece POR CIMA do diálogo, e
  // o clique que o dispensa tem que continuar funcionando.
  useEffect(() => {
    const fundo = fundoRef.current;
    const irmaos = Array.from(fundo?.parentElement?.children || [])
      .filter((el) => el !== fundo && !el.classList.contains("tr-banner"));
    irmaos.forEach((el) => { el.inert = true; });
    // Foco na caixa, e não no primeiro botão: o × acionado por um Enter
    // distraído fecharia o diálogo, e a lista ainda nem respondeu.
    caixaRef.current?.focus();
    return () => irmaos.forEach((el) => { el.inert = false; });
  }, []);

  return h("div", { className: "tr-lightbox tr-modal", ref: fundoRef },
    h("div", { className: "tr-dialog", role: "dialog", "aria-modal": "true",
               tabIndex: -1, ref: caixaRef }, [
      h("div", { key: "p", className: "tr-dialog-path" }, [
        h("span", { key: "c" }, l ? l.path : "…"),
        h("button", { key: "x", className: "tr-dialog-close", title: "fechar (Esc)",
                      onClick: onClose }, "×"),
      ]),
      h("div", {
        key: "ls", className: "tr-dialog-list" + (arrastando ? " tr-dialog-drop-on" : ""),
        onDragOver: (e) => { e.preventDefault(); setArrastando(true); },
        onDragLeave: () => setArrastando(false),
        onDrop: (e) => {
          e.preventDefault(); e.stopPropagation(); setArrastando(false);
          const f = e.dataTransfer.files?.[0];
          // Só `.json`: soltar qualquer outra coisa aqui hoje não tem onde ir,
          // e tentar interpretar (imagem? pasta?) é escopo que ninguém pediu.
          // `stopPropagation`: sem isto, o drop borbulhava até o listener
          // global da página (`onDropGlobal`, em `App`) e reabria o diálogo
          // do zero por cima do que o usuário já estava fazendo aqui dentro.
          if (f && /\.json$/i.test(f.name)) lerArquivo(f);
        },
      }, [
        l && l.parent
          ? h("button", { key: "..", className: "tr-dialog-row",
                          onClick: () => onBrowse(l.parent) }, "../")
          : null,
        ...((l?.entries || []).map((e) => h("button", {
          key: e.nome, className: "tr-dialog-row" + (e.projeto ? " tr-is-project" : ""),
          onClick: () => descer(e.nome) }, e.nome))),
      ]),
      h("div", { key: "ac", className: "tr-dialog-actions" }, [
        h("button", { key: "o", disabled: !podeAbrir,
                      title: !l?.project ? "esta pasta não é um projeto"
                        : l.path === atual ? "este projeto já está aberto"
                        : "abrir este projeto",
                      onClick: () => onOpen(l.path) },
          enviando === "abrir" ? "abrindo…" : "Abrir"),
        // Enter grava, como no título do frame e nos widgets de texto do
        // runtime: quem digitou o nome já disse o que queria.
        h("input", { key: "n", className: "tr-dialog-name", placeholder: "nome do projeto novo",
                     value: nome, onChange: (e) => setNome(e.target.value),
                     onKeyDown: (e) => { if (e.key === "Enter" && podeCriar) criar(); } }),
        h("button", { key: "c", disabled: !podeCriar, onClick: criar },
          enviando === "criar" ? "criando…" : "Criar aqui"),
        h("input", { key: "fi", type: "file", accept: ".json,application/json",
                     ref: fileRef, style: { display: "none" },
                     onChange: (e) => lerArquivo(e.target.files?.[0]) }),
        h("button", { key: "fb", onClick: () => fileRef.current?.click() },
          arquivo ? `📄 ${arquivo.nomeArquivo}` : "Escolher arquivo…"),
        h("button", { key: "im", disabled: !podeImportar, onClick: importar },
          enviando === "importar" ? "importando…" : "Importar aqui"),
      ]),
    ]));
}

// Nome de arquivo já existe em `data/` (drop de CSV/JSON no canvas): pergunta
// o que fazer em vez de sobrescrever sozinho. Mesmo estilo visual do
// `ProjectDialog` (`tr-lightbox`/`tr-dialog`), sem a árvore de pastas — só a
// pergunta e três botões.
function UploadConflictDialog({ nome, onOverwrite, onRename, onCancel }) {
  const [renomeando, setRenomeando] = useState(false);
  const [novoNome, setNovoNome] = useState(nome);
  return h("div", { className: "tr-lightbox tr-modal" },
    h("div", { className: "tr-dialog", role: "dialog", "aria-modal": "true" }, [
      h("p", { key: "msg" }, `Já existe um arquivo chamado "${nome}" em data/. O que fazer?`),
      h("div", { key: "ac", className: "tr-dialog-actions" },
        renomeando
          ? [
              h("input", { key: "n", className: "tr-dialog-name", value: novoNome, autoFocus: true,
                           onChange: (e) => setNovoNome(e.target.value),
                           onKeyDown: (e) => { if (e.key === "Enter" && novoNome.trim()) onRename(novoNome.trim()); } }),
              h("button", { key: "ok", disabled: !novoNome.trim(),
                            onClick: () => onRename(novoNome.trim()) }, "Confirmar"),
              h("button", { key: "cancel", onClick: onCancel }, "Cancelar"),
            ]
          : [
              h("button", { key: "ow", onClick: onOverwrite }, "Sobrescrever"),
              h("button", { key: "rn", onClick: () => setRenomeando(true) }, "Renomear"),
              h("button", { key: "cancel", onClick: onCancel }, "Cancelar"),
            ]),
    ]));
}

// --- App -------------------------------------------------------------------

// Espelha `.tr_presentation_ops` (R/document.R): op cosmética não recomputa
// nada, então não pode pintar o canvas inteiro de "na fila". Batch é cosmético
// só se TODA op dentro dele for, a mesma regra de `tr_op_semantic()`.
const COSMETICAS = new Set(["move", "rename", "resize", "set_view",
  "add_frame", "update_frame", "remove_frame", "reorder_frames", "set_fold"]);
const cosmetica = (op) =>
  op.op === "batch" ? op.ops.every(cosmetica) : COSMETICAS.has(op.op);

function App() {
  const [catalog, setCatalog] = useState(null);
  // Temas do projeto: a fonte que o widget `theme` lê é o estado de módulo do
  // runtime (`setThemes`); este espelho existe só pra entrar no `data` dos nós
  // e fazer os cards re-renderizarem quando a lista muda. `marca` vem na mesma
  // mensagem e mora aqui, e não no runtime: quem a usa é a exportação de
  // frame, não o card.
  const [temas, setTemas] = useState({ temas: {}, tema_padrao: null, marca: true });
  const [doc, setDoc] = useState(null);
  const [nodes, setNodes] = useState([]);
  const [edges, setEdges] = useState([]);
  const [dragType, setDragType] = useState(null);
  const [banner, setBanner] = useState(null);
  const [helpFor, setHelpFor] = useState(null);
  const [menu, setMenu] = useState(null);   // {kind, id, x, y}
  const [ferramenta, setFerramenta] = useState(null);   // "frame" | null
  const [editFrame, setEditFrame] = useState(null);     // id do frame com título em edição
  const [editNota, setEditNota] = useState(null);        // id da nota com textarea aberto
  const [painelFrames, setPainelFrames] = useState(false);
  // Painel ⚙ (temas do projeto). Os três painéis laterais dividem a mesma
  // coluna e abrir um fecha os outros: com dois estados ligados, o botão do
  // escondido ficaria aceso sem nada na tela que corresponda a ele.
  const [painelConfig, setPainelConfig] = useState(false);
  const [menuAcoes, setMenuAcoes] = useState(false);
  const [present, setPresent] = useState(null);         // {i, volta} | null
  const [exportando, setExportando] = useState(false);
  const [projeto, setProjeto] = useState(null);   // {root, flow} do que está aberto
  const [listagem, setListagem] = useState(null); // resposta do último tr_browse
  // Lista de imagens de `<projeto>/imagens/`, pro estado vazio do bloco de
  // imagem escolher. Pedida sob demanda, mesmo desenho de `tr_browse` acima:
  // uma vez quando o app fica pronto (documento aberto pode já ter notas de
  // imagem vazias esperando escolha) e de novo sempre que a ferramenta "I" é
  // ativada (o gesto mais provável de precisar da lista fresca, e barato
  // — a pasta raramente muda no meio de uma sessão, mas quando muda é
  // justamente porque alguém acabou de largar um arquivo lá pra usar agora).
  const [imagens, setImagens] = useState([]);
  const [abrindo, setAbrindo] = useState(false);  // diálogo visível
  const [enviando, setEnviando] = useState(null); // "abrir" | "criar" | "importar" | null: pedido em voo
  const [arquivoSolto, setArquivoSolto] = useState(null); // {nomeArquivo, conteudo} | null — drop na página, fora do diálogo
  // Drop de CSV/JSON no CANVAS (vira nó de leitura). `uploadsPendentesRef` é
  // ref (não state): guarda posição/tipo/conteúdo por `id` enquanto a
  // resposta do servidor não chega — reler não precisa re-renderizar nada.
  // `conflitoUpload` é o único pedaço visível na tela (o diálogo de nome
  // repetido), por isso é state.
  const uploadsPendentesRef = useRef({});
  const [conflitoUpload, setConflitoUpload] = useState(null); // {id, nome} | null
  // Proporção dos frames NOVOS (F e Ctrl+G). É preferência de quem usa este
  // navegador, e não estado do documento: não vira op, não entra no desfazer,
  // e abrir o mesmo projeto em outra máquina não herda a escolha. Cada frame
  // continua guardando a própria proporção no documento. `localStorage` pode
  // lançar (armazenamento bloqueado), e um valor gravado que não é mais uma
  // proporção conhecida cai no padrão.
  const [aspectoNovo, setAspectoNovo] = useState(() => {
    try {
      const a = localStorage.getItem("trama.aspectoNovo");
      if (a && Object.hasOwn(ASPECTS, a)) return a;
    } catch (_) {}
    return "16:9";
  });
  useEffect(() => {
    try { localStorage.setItem("trama.aspectoNovo", aspectoNovo); } catch (_) {}
  }, [aspectoNovo]);
  // Prancheta: popover aberto e o último pedido feito, como preferência do
  // navegador (mesma doutrina da proporção acima). A proporção NÃO é guardada
  // aqui: é a mesma `aspectoNovo` da toolbar, pra as duas nunca discordarem.
  // Só as chaves conhecidas voltam, e só de um objeto: `null`, lista ou número
  // gravados por outra versão caem no padrão. Número inválido passa: o popover
  // marca o campo e desliga o Criar. `externo` e `cor` não têm campo que
  // mostre erro, então tipo errado neles cai no padrão aqui.
  // `prancheta` é `null` (fechado) ou os valores iniciais do popover, tirados
  // UMA vez ao abrir: o popover só os lê ao montar, e reler o `localStorage`
  // a cada render com ele aberto era trabalho jogado fora.
  const [prancheta, setPrancheta] = useState(null);
  const pranchetaSalva = () => {
    let g = null;
    try { g = JSON.parse(localStorage.getItem("trama.prancheta") || "{}"); } catch (_) {}
    const ok = g && typeof g === "object" && !Array.isArray(g);
    const o = { ...PRANCHETA_PADRAO };
    if (ok) Object.keys(PRANCHETA_PADRAO).forEach((k) => {
      if (k !== "aspect" && Object.hasOwn(g, k)) o[k] = g[k];
    });
    if (typeof o.externo !== "boolean") o.externo = PRANCHETA_PADRAO.externo;
    if (o.cor !== "rodízio" && !FRAME_COLORS.includes(o.cor)) o.cor = PRANCHETA_PADRAO.cor;
    return o;
  };
  // Estável: é dependência do efeito que monta o Esc do popover.
  const fecharPrancheta = useCallback(() => setPrancheta(null), []);
  // Clique fora fecha. Na fase de CAPTURA e com teste de alvo, e não confiando
  // em quem para a propagação: o d3-zoom do xyflow dá
  // `stopImmediatePropagation` no `mousedown` do vazio, e um listener de bolha
  // na `window` nunca via o clique no canvas. O botão da toolbar fica de fora
  // pelo mesmo teste, senão o clique fecharia e o `onClick` reabriria.
  useEffect(() => {
    if (!prancheta) return;
    const fora = (e) => {
      if (e.target instanceof Element && e.target.closest(".tr-prancheta, [data-prancheta]")) return;
      setPrancheta(null);
    };
    window.addEventListener("mousedown", fora, true);
    return () => window.removeEventListener("mousedown", fora, true);
  }, [prancheta]);
  // Tema do app: preferência do navegador, como a proporção acima. O <head>
  // (R/app.R) já resolveu `data-tema` antes do primeiro paint com a mesma
  // regra; aqui ela é reaplicada a cada troca. Em "sistema" o efeito escuta o
  // SO — trocar o tema do sistema com o app aberto não pode exigir recarga — e
  // a limpeza solta o listener quando a escolha passa a ser fixa.
  const [temaApp, setTemaApp] = useState(() => {
    try {
      const t = localStorage.getItem("trama.temaApp");
      if (t === "claro" || t === "escuro" || t === "sistema") return t;
    } catch (_) {}
    return "sistema";
  });
  useEffect(() => {
    try { localStorage.setItem("trama.temaApp", temaApp); } catch (_) {}
    const root = document.documentElement;
    if (temaApp !== "sistema") { root.dataset.tema = temaApp; return undefined; }
    const mq = window.matchMedia("(prefers-color-scheme: light)");
    const aplica = () => { root.dataset.tema = mq.matches ? "claro" : "escuro"; };
    aplica();
    mq.addEventListener("change", aplica);
    return () => mq.removeEventListener("change", aplica);
  }, [temaApp]);
  // `tick` força redecoração quando só o estado de execução muda. Agrupado por
  // quadro: uma execução emite um evento POR NÓ, e re-renderizar a cada
  // mensagem não muda nada visualmente — só gasta.
  const [tick, setTick] = useState(0);
  const tickPending = useRef(false);
  const revRef = useRef(0);
  const stateRef = useRef({});      // por nó: state, handle, error, duration
  const paramsRef = useRef({});     // params editados localmente, antes do eco
  const viewsRef = useRef({});      // vista escolhida localmente, antes do eco
  const sizesRef = useRef({});      // tamanho arrastado localmente, antes do eco
  const foldsRef = useRef({});      // recolhimento local, antes do eco
  const seedsRef = useRef({});      // semente re-sorteada localmente, antes do eco
  const clipboardRef = useRef(null); // último Ctrl+C: snapshot de blocos/frames/notas
  const colagensRef = useRef(0);     // colagens seguidas do mesmo clipboard, pro deslocamento em cascata
  const projetoRef = useRef(null);  // raiz do projeto aberto, pro handler de `project`
  // Regiões de fluxo do plano ATUAL, por id do nó que carrega os eventos de
  // unidade (o primeiro colapso — `u$node`, em `R/transport.R`). É o mapa que
  // fecha o buraco herdado da Fase 5: sem ele, um membro interior ou um
  // segundo colapso nunca recebiam evento nenhum, porque o motor só emite sob
  // `u$node`. Mora em ref, e não em estado: chega numa mensagem própria
  // (`regions`), e usá-la é trabalho de `applyUnit`, que já roda fora do ciclo
  // de render (ver o comentário de `stateRef`).
  const regionsRef = useRef({});
  // Índice reverso: todo id que pertence a QUALQUER região, pro contorno do
  // card (Tarefa 8.1). REF PRÓPRIA, e não uma chave a mais dentro de
  // `regionsRef.current` (que é indexado por ID DE NÓ — `u$node`, o primeiro
  // colapso): id de fluxo é palavra livre, casada só contra
  // `^[A-Za-z0-9_.-]+$` (`inst/schema/document-v1.json`), documento é
  // escrito à mão ou por LLM (`tr_doc_validate` é quem barra o resto, não o
  // editor), e "_nodes" é um id LEGAL. Achado rodando o app de verdade com um
  // nó gerador chamado `_nodes`: `regionsRef.current._nodes` deixava de ser o
  // Set e virava a região daquele nó (objeto `{unit, members, roles,
  // outputs}`), e `applyUnit` (que busca `regionsRef.current[m.node]`) lia
  // esse objeto torto pra QUALQUER evento de nó chamado `_nodes` — o `.has`
  // que o contorno esperava não existe num objeto de região, e
  // `regiao.members.forEach` explodia (`TypeError: Cannot read properties of
  // undefined`) dentro do ÚNICO `addCustomMessageHandler("tr_event", ...)` da
  // página: matava o handler pra TODO evento seguinte, de qualquer nó. Uma
  // ref separada não pode colidir com uma chave de mapa, e ficou mais barata
  // que guardar toda vez com uma checagem (`regiaoFonte`, logo abaixo, tem a
  // checagem — `r && r.roles` — só porque sobrou de antes desta ref existir;
  // é redundante agora, não faz mal manter).
  const regionNodesRef = useRef(new Set());
  // Comando vivo (play/pause/tempo) da FONTE de cada região, só na tela: o
  // barramento não tem eco de `tr_stream_cmd` (R/transport.R, "comando não é
  // op de documento"), então o único jeito de o botão mostrar o próprio
  // estado é lembrar o que ele mesmo mandou. Por `key` da região, não por id
  // de nó: sobrevive a um documento com duas fontes.
  const streamCtlRef = useRef({});
  const catalogRef = useRef(null);
  const wrapRef = useRef(null);
  const arrastoRef = useRef(null);  // por frame arrastado: {x0, y0, itens:[{id,x,y}]}
  const caixaRef = useRef(null);    // canto inicial da caixa de seleção, em coords do fluxo
  const rf = useReactFlow();
  // Espelho do estado pra callbacks estáveis: eles leem nós e arestas atuais
  // sem pôr `nodes`/`edges` nas deps, porque callback que muda de identidade a
  // cada render realimenta o laço de remedição (ver `onParam`).
  const nodesRef = useRef([]); nodesRef.current = nodes;
  const edgesRef = useRef([]); edgesRef.current = edges;

  // Nós com a medida ATUAL de cada card, lida do DOM. O `measured` do React
  // Flow não é confiável na hora da geometria: o store o refaz a partir do nó
  // do estado a cada `setNodes`, e só volta a tê-lo quando o ResizeObserver
  // entrega — o que fica pra depois (ou nunca, com a aba sem renderizar), e
  // até lá o `rectOf` cai na ESTIMATIVA pelo tamanho do preview. Visto no
  // navegador: store com `measured` vazio pra todos os cards e o DOM certo;
  // o Organizar arrumava pela estimativa (pedidos 678 contra 787 reais) e os
  // cards altos vazavam pela borda do frame. `offsetWidth/offsetHeight` é o
  // tamanho de layout, sem o zoom do viewport, na mesma unidade das posições,
  // e está sempre em dia. Card fora do DOM (ou com 0x0, em `display:none`)
  // fica com o que já tinha. Frame fica de fora: o tamanho dele é o do
  // documento. Serve a toda geometria que decide pertencimento — Organizar,
  // Ctrl+G, contenção do arrasto de frame — e à medida preservada no eco.
  const comMedidas = useCallback((ns) => ns.map((n) => {
    if (n.type !== "ndNode") return n;
    const raiz = wrapRef.current || document;
    const el = raiz.querySelector(`.react-flow__node[data-id="${CSS.escape(n.id)}"]`);
    const width = el?.offsetWidth, height = el?.offsetHeight;
    if (!width || !height) return n;
    if (n.measured?.width === width && n.measured?.height === height) return n;
    return { ...n, measured: { width, height } };
  }), []);

  const bumpTick = useCallback(() => {
    if (tickPending.current) return;
    tickPending.current = true;
    requestAnimationFrame(() => { tickPending.current = false; setTick((t) => t + 1); });
  }, []);

  const typeColors = useMemo(() => Object.fromEntries(
    (catalog?.types || []).map((t) => [t.id, t.color])), [catalog]);
  const categories = useMemo(() => Object.fromEntries(
    (catalog?.categories || []).map((c) => [c.id, c])), [catalog]);

  // `onParam` precisa ser ESTÁVEL. Como ele entra em `data` de cada nó, uma
  // função nova a cada render faz todo nó ser um objeto novo; o React Flow
  // remede, emite mudança de dimensão, chama setNodes, e o ciclo não fecha —
  // o renderizador congela. É o mesmo laço que o insumo documenta em
  // `onNodesChange`, aqui pela outra ponta.
  const onParam = useCallback((nodeId, name, value) => {
    // Atualiza o param NO REF de estado, não recriando o array de nós — o
    // objetivo é o mesmo laço de realimentação: cada `setNodes` faz o React
    // Flow remedir todos os cards.
    paramsRef.current[nodeId] = { ...(paramsRef.current[nodeId] || {}), [name]: value };
    bumpTick();
    pushOp({ op: "set_param", node: nodeId, name, value });
  }, [bumpTick]);

  // Estável pelo mesmo motivo que `onParam`: entra em `data` de cada nó, e uma
  // função nova a cada render realimentaria o mesmo laço de remedição.
  const onHelp = useCallback((typeId) => {
    setPainelFrames(false); setPainelConfig(false); setHelpFor(typeId);
  }, []);

  // Estável e otimista pelos mesmos motivos de `onView`: quem sorteia é o
  // cliente, então ele já sabe o número, e `set_seed` não devolve o documento
  // (não é op de estrutura) — sem o ref a dica do botão mostraria a semente
  // ANTERIOR até o próximo eco do documento inteiro. Se o servidor recusar,
  // `tr_server` ressincroniza com o documento e o ref é zerado junto.
  //
  // Limite em 2^31-1 porque a semente vira `as.integer()` no R: um número
  // maior viraria `NA` com um mero warning, e o documento levaria uma semente
  // vazia (é o que `.tr_check_seed` existe pra impedir).
  const onReseed = useCallback((nodeId) => {
    const value = Math.floor(Math.random() * 2147483647);
    seedsRef.current[nodeId] = value;
    bumpTick();
    pushOp({ op: "set_seed", node: nodeId, value });
  }, [bumpTick]);

  // Estável pelo mesmo motivo, e pelo ref pelo mesmo motivo que `onParam`: a
  // escolha vale só até o eco do documento trazer a vista persistida.
  const onView = useCallback((nodeId, viewId) => {
    viewsRef.current[nodeId] = viewId;
    bumpTick();
    pushOp({ op: "set_view", node: nodeId, view: viewId });
  }, [bumpTick]);

  // Estável pelos mesmos motivos. Só chega aqui no fim do arrasto: durante ele
  // o `Grip` mexe nas variáveis CSS do card e não toca em estado nenhum.
  const onResize = useCallback((nodeId, w, hgt) => {
    sizesRef.current[nodeId] = [w, hgt];
    bumpTick();
    pushOp({ op: "resize", node: nodeId, w, h: hgt });
  }, [bumpTick]);

  // Estáveis pelo mesmo motivo de `onParam`. Frame não tem ref de otimismo
  // como params e tamanhos: ele já é nó do React Flow, e o gesto (arrasto,
  // alça) atualiza o estado local sozinho; aqui só sai a op. Título,
  // proporção e cor mudam por `setNodes`, que em frame é raro e barato.
  const onFrameRect = useCallback((id, p) => {
    pushOp({ op: "update_frame", frame: id, x: Math.round(p.x), y: Math.round(p.y),
             w: Math.round(p.width), h: Math.round(p.height) });
  }, []);
  const onFrameEdit = useCallback((id, patch) => {
    setNodes((ns) => ns.map((n) => (n.id !== id ? n : {
      ...n, data: { ...n.data, ...patch }, ...(patch.h != null ? { height: patch.h } : {}) })));
    pushOp({ op: "update_frame", frame: id, ...patch });
  }, []);
  // Um lugar só pra trocar a proporção: menu do frame e painel de frames.
  // Mantém a LARGURA e recalcula a altura; `livre` só troca o nome e deixa o
  // retângulo como está. O frame é lido pelo ref, e não pelo `nodes` do
  // render: o retângulo pode ter acabado de mudar num arrasto ou na alça.
  // Troca que não muda nada (mesma proporção, mesma altura) não sai: seria um
  // passo vazio no desfazer.
  const mudarProporcao = useCallback((id, a) => {
    const f = nodesRef.current.find((n) => n.id === id);
    if (!f) return;
    const r = ratioOf(a);
    const patch = r ? { aspect: a, h: Math.round(rectOf(f).w / r) } : { aspect: a };
    if (a === f.data.aspect && (patch.h == null || patch.h === rectOf(f).h)) return;
    onFrameEdit(id, patch);
  }, [onFrameEdit]);
  const onFrameEditStart = useCallback((id) => setEditFrame(id), []);
  const onFrameEditEnd = useCallback(() => setEditFrame(null), []);

  // Equivalentes de nota. `resolverSrc` monta a URL da rota `trama-imagens`,
  // registrada em `tr_ui()`/`R/transport.R` e servindo a pasta `imagens/` do
  // projeto — `src` gravado no documento é o caminho relativo a ela.
  const resolverSrc = useCallback((rel) => `trama-imagens/${rel}`, []);
  const onNotaRect = useCallback((id, p) => {
    pushOp({ op: "update_note", note: id, x: Math.round(p.x), y: Math.round(p.y),
             w: Math.round(p.width), h: Math.round(p.height) });
  }, []);
  const onNotaEdit = useCallback((id, patch) => {
    setNodes((ns) => ns.map((n) => (n.id !== id ? n : { ...n, data: { ...n.data, ...patch } })));
    pushOp({ op: "update_note", note: id, ...patch });
  }, []);
  const onNotaEditStart = useCallback((id) => setEditNota(id), []);
  const onNotaEditEnd = useCallback(() => setEditNota(null), []);

  // Estável pelos mesmos motivos de `onParam`. `set_fold` não devolve o
  // documento, então o ref segura o recolhimento até o próximo documento
  // chegar (e ele já vem com o valor gravado).
  const onFold = useCallback((nodeId, patch) => {
    const n = nodesRef.current.find((x) => x.id === nodeId);
    foldsRef.current[nodeId] = { ...((foldsRef.current[nodeId] ?? n?.data.fold) || {}), ...patch };
    bumpTick();
    pushOp({ op: "set_fold", node: nodeId, ...patch });
  }, [bumpTick]);

  // Comando vivo pra fonte de uma região: play, pause, um passo, tempo. Não é
  // op — não passa por `pushOp` nem carrega `base_rev` — é o precedente de
  // `sinks-ricos.md` que `R/transport.R` já documenta pro lado do servidor
  // (`tr_stream_cmd`, sem entrar no log de undo nem mexer em `rev`). O eco
  // otimista fica só no `streamCtlRef`, porque o barramento não devolve nada:
  // clicar "pausar" e nunca saber se pegou seria pior que não ter o botão.
  const onStreamCmd = useCallback((key, cmd, tempo) => {
    const atual = streamCtlRef.current[key] || { estado: "running", tempo: 0 };
    const novo = cmd === "play" ? { estado: "running", tempo: atual.tempo }
               : cmd === "pause" ? { estado: "paused", tempo: atual.tempo }
               : cmd === "tempo" ? { estado: atual.estado, tempo }
               : atual; // "step" não muda estado nenhum pra mostrar
    streamCtlRef.current[key] = novo;
    bumpTick();
    sendInput("tr_stream_cmd", { key, cmd, tempo: tempo ?? null, seq: ++seqCounter });
  }, [bumpTick]);

  // Acha, se houver, a região da qual `id` é a FONTE — é o único card que
  // ganha os controles (Decisão 5: `data/to_stream` é o âncora). Varre as
  // poucas regiões do plano, não os nós: um documento tem no máximo umas
  // poucas regiões, então isto é mais barato que manter mais um índice
  // reverso só pra um papel.
  const regiaoFonte = useCallback((id) => {
    for (const r of Object.values(regionsRef.current)) {
      if (r && r.roles && r.roles[id] === "source") return r;
    }
    return null;
  }, []);

  // Memoizado pela mesma razão: o array passado ao React Flow só pode mudar
  // quando algo de verdade mudou.
  const decorated = useMemo(() => nodes.map((n) => {
    if (n.type === "trFrame") {
      return { ...n, data: { ...n.data, editing: editFrame === n.id,
                              onFrameRect, onFrameEdit, onFrameEditStart, onFrameEditEnd } };
    }
    if (n.type === "trNota") {
      // `n.data.src` nasce em `docToFlow` como o caminho CRU do documento
      // (`n.src`, ex. "figuras/logo.png") — `docToFlow` é função pura, sem
      // acesso a `resolverSrc`, então não é ela quem resolve. Aqui, junto dos
      // outros campos injetados por prop (como o próprio `resolverSrc`, usado
      // por `Markdown` pra imagem embutida no corpo do texto), é o ponto certo:
      // é aqui que a nota ganha tudo que só o componente sabe. Sem isto,
      // `NotaImagem` recebia o caminho relativo cru como `src` do `<img>`,
      // furando o contrato documentado no topo de notas.js ("src — a URL já
      // resolvida da imagem").
      return { ...n, data: { ...n.data, src: n.data.src ? resolverSrc(n.data.src) : n.data.src,
                              editing: editNota === n.id, resolverSrc, imagens,
                              onNotaRect, onNotaEdit, onNotaEditStart, onNotaEditEnd } };
    }
    return { ...n, data: { ...n.data, ...(stateRef.current[n.id] || {}),
                      params: { ...n.data.params, ...(paramsRef.current[n.id] || {}) },
                      view: viewsRef.current[n.id] ?? n.data.view,
                      size: sizesRef.current[n.id] ?? n.data.size,
                      fold: foldsRef.current[n.id] ?? n.data.fold,
                      seed: seedsRef.current[n.id] ?? n.data.seed,
                      // Membro de QUALQUER região: contorno do card (8.1). Fonte de
                      // UMA região: controles de fluxo (8.2) — os dois lidos do
                      // ref, então não precisam de estado próprio nem de `tick` na
                      // lista de deps além do que a rajada de eventos já pede.
                      emRegiao: regionNodesRef.current.has(n.id),
                      streamSource: regiaoFonte(n.id),
                      streamCtl: streamCtlRef.current,
                      onStreamCmd,
                      typeColors, categories, onParam, onHelp, onView, onResize, onFold,
                      onReseed, temas } };
  }),
    // `temas` só muda quando chega mensagem `themes` (abrir projeto, salvar):
    // raro o bastante pra não realimentar o laço de remedição.
    [nodes, typeColors, categories, onParam, onHelp, onView, onResize, onFold, onReseed, tick, temas,
     editFrame, onFrameRect, onFrameEdit, onFrameEditStart, onFrameEditEnd, onStreamCmd, regiaoFonte,
     editNota, resolverSrc, imagens, onNotaRect, onNotaEdit, onNotaEditStart, onNotaEditEnd]);

  // --- Recepção ---
  useEffect(() => {
    if (!window.Shiny || !window.Shiny.addCustomMessageHandler) return;
    window.Shiny.addCustomMessageHandler("tr_event", (m) => {
      if (m.type === "catalog") { catalogRef.current = m.catalog; setCatalog(m.catalog); return; }
      // Módulo antes do estado: o re-render disparado por `setTemas` já encontra
      // `getThemes()` atualizado.
      if (m.type === "themes") {
        setThemes(m);
        setTemas({ temas: m.temas || {}, tema_padrao: m.tema_padrao ?? null,
                   // `?? true` pro servidor velho (ou uma mensagem que perdeu
                   // o campo) não desligar a marca sem ninguém ter pedido.
                   marca: m.marca ?? true });
        return;
      }

      if (m.type === "document") {
        revRef.current = m.doc.rev || 0;
        // Params locais já foram pro servidor e voltam no documento — o ref só
        // existia pro intervalo entre digitar e o eco chegar.
        paramsRef.current = {};
        viewsRef.current = {};
        sizesRef.current = {};
        foldsRef.current = {};
        seedsRef.current = {};
        setDoc(m.doc);
        const { nodes: novos, edges: e, needsLayout } = docToFlow(m.doc, catalogRef.current);
        // `docToFlow` refaz cada card sem `measured`, e até o React Flow medir
        // de novo o `rectOf` via o card como 0x0: Ctrl+G enquadrava errado e o
        // frame arrastado não levava card nenhum (com a janela escondida, sem
        // prazo pra medir). A medida anterior vai junto, pelo id; é só palpite
        // — o ResizeObserver do xyflow a sobrescreve se o card mudou de
        // tamanho, e medida igual não emite mudança nenhuma, então não há laço.
        // Frame fica de fora: a largura e a altura dele vêm do documento.
        // A medida vem do DOM (`comMedidas`), e não do `nodesRef` nem do store:
        // o ref só anda no render, e o store pode estar sem medida nenhuma
        // esperando o ResizeObserver.
        const medidas = new Map(comMedidas(nodesRef.current)
          .filter((x) => x.type === "ndNode" && x.measured)
          .map((x) => [x.id, x.measured]));
        const n = novos.map((x) => (x.type === "ndNode" && medidas.has(x.id)
          ? { ...x, measured: medidas.get(x.id) } : x));
        setNodes(n); setEdges(e);
        // Documento sem posições (escrito à mão ou por LLM): o dagre resolve e
        // as posições sobem como `move`, que é apresentação pura e não
        // recomputa nada.
        if (needsLayout) {
          const mv = n.filter((x) => x.type === "ndNode").map((x) => ({
            op: "move", node: x.id, x: x.position.x, y: x.position.y }));
          if (mv.length) sendOp(mv.length === 1 ? mv[0] : { op: "batch", ops: mv }, revRef.current);
        }
        if (m.problems?.length) {
          setBanner(m.problems.map((p) => `${p.kind}${p.node ? ` (${p.node})` : ""}`).join(" · "));
        }
        return;
      }

      if (m.type === "op_applied") {
        revRef.current = m.rev;
        return;
      }

      if (m.type === "op_rejected") {
        // A corrida do insumo (documento antigo, cache servido, preview
        // parado, sem erro) aqui é sempre explícita.
        revRef.current = m.rev ?? revRef.current;
        setBanner(m.message);
        return;
      }

      if (m.type === "export_code") {
        const nome = `${(projetoRef.current || "flow").split("/").filter(Boolean).pop() || "flow"}-${m.format === "quarto" ? "analise.qmd" : "analise.R"}`;
        exportText(m.code, nome, "text/plain;charset=utf-8");
        return;
      }

      // O diálogo fica aberto até o projeto TROCAR de verdade. Fechar ao
      // clicar "Abrir"/"Criar aqui" seria otimista: a recusa (nome inválido,
      // coleção que falta, pasta sem manifesto) volta como `warning`, e o
      // banner sozinho no canto não diz de qual pasta se falava nem deixa
      // corrigir o nome — só sobra recomeçar a navegação. Este é o único
      // evento de sucesso: reabrir o projeto JÁ aberto também o emite (o
      // servidor reconfirma onde o editor está, sem trocar nada — ver a guarda
      // no início de `abrir()`, em R/transport.R), e é por isso que fechar o
      // diálogo aqui nunca o deixa preso. "Abrir" é desabilitado na pasta
      // atual porque reabrir não faz nada, não porque não responderia.
      if (m.type === "project") {
        // Estado de execução é indexado por ID DE NÓ, e id de fluxo escrito à
        // mão é palavra ("ler", "filtrar", "total"): dois projetos colidem.
        // Sem zerar, um nó do projeto novo que chega `blocked` herdaria o
        // handle do homônimo do anterior — e `unitState` não limpa `handle` em
        // `blocked`/`failed` de propósito, pra o card guardar o último valor
        // bom DENTRO de um projeto. O preview do outro projeto ficaria na tela
        // pra sempre, sem erro nenhum. Aqui e não no eco de `document`: aquele
        // também desce em op estrutural e em op recusada, onde limpar apagaria
        // o preview de todos os cards a cada nó adicionado. Só quando a raiz
        // MUDA porque a reconfirmação do mesmo projeto não refaz o run: zerar
        // ali esvaziaria os cards sem nada para repovoá-los.
        // Mesma guarda de "raiz mudou": a lista de `imagens/` é da pasta do
        // projeto ANTERIOR até aqui, e sem refazer o pedido ela ficaria
        // oferecendo nomes que não existem no projeto novo — diferente do
        // preview de um card (que só fica com URL quebrada em silêncio),
        // aqui escolher um nome obsoleto GRAVA um `src` inválido no documento
        // novo. `tr_ready` só pede a lista uma vez, no boot; a troca de
        // projeto é o outro caminho que precisa dela fresca.
        if (projetoRef.current !== m.root) {
          stateRef.current = {};
          sendInput("tr_list_imagens", { seq: ++seqCounter });
        }
        projetoRef.current = m.root;
        setProjeto({ root: m.root, flow: m.flow });
        setAbrindo(false);
        setEnviando(null);
        // A recusa da tentativa anterior ("nome inválido") não pode ficar na
        // tela depois do acerto: o aviso passaria a falar de um projeto que
        // não é mais o aberto.
        setBanner(null);
        return;
      }

      if (m.type === "listing") { setListagem(m); return; }

      if (m.type === "imagens") { setImagens(m.files || []); return; }

      // Arquivo de dado (CSV/JSON) gravado com sucesso: o pendente guardava
      // ONDE soltar o nó (posição do drop) e QUE tipo — o servidor só sabia o
      // caminho. `id` casa a resposta com o pendente certo.
      if (m.type === "data_upload_ok") {
        const pend = uploadsPendentesRef.current[m.id];
        delete uploadsPendentesRef.current[m.id];
        if (pend) addAt(pend.tipo, pend.pos, { params: { path: m.path } });
        setConflitoUpload((c) => (c?.id === m.id ? null : c));
        return;
      }

      // Nome já existe em `data/`: o pendente continua guardado (a resposta
      // "ok" ainda pode chegar depois de Sobrescrever/Renomear), só abre o
      // diálogo perguntando o que fazer.
      if (m.type === "data_upload_conflict") {
        setConflitoUpload({ id: m.id, nome: m.nome });
        return;
      }

      // Regiões do plano ATUAL. Chega a cada `run_now` (R/transport.R) —
      // documento novo, ou só um param que mudou — então é sempre a lista
      // certa pro run em voo. Indexado por `unit` (o id do nó que carrega os
      // eventos), que é a chave de busca de `applyUnit`.
      if (m.type === "regions") {
        const mapa = {};
        (m.regions || []).forEach((r) => { mapa[r.unit] = r; });
        regionsRef.current = mapa;
        // Índice reverso pro contorno do card (Tarefa 8.1) — em REF PRÓPRIA,
        // não como chave dentro de `mapa` (ver o comentário de
        // `regionNodesRef`, onde declarada: um id de fluxo pode legalmente
        // SER "_nodes", e colidia com essa chave).
        regionNodesRef.current = new Set((m.regions || []).flatMap((r) => r.members));
        // Aresta entre dois membros ELEVADOS da mesma região só se sabe que é
        // de fluxo quando a mensagem `regions` chega (nenhuma das duas pontas
        // declara `stream` sozinha — ver `ehFluxo`/`portaFluxo`, em
        // `docToFlow`). Recalcular aqui, e não só em `document`, é o que fecha
        // a lacuna que o comentário de `docToFlow` documentava como aceita:
        // a mensagem chega depois, mas ela BATE — sem isto o traço tracejado
        // do contorno da região emolduraria uma aresta cinza sólida por
        // dentro, contradizendo o próprio contorno.
        setEdges((es) => es.map((e) => {
          const dentro = regionNodesRef.current.has(e.source) && regionNodesRef.current.has(e.target);
          return dentro && !e.animated ? { ...e, animated: true } : e;
        }));
        // `regions` não muda `stateRef`, então sem isto o contorno só
        // apareceria no próximo evento de unidade — perceptível quando a
        // região tem cache (roda zero unidade, evento nenhum chega).
        bumpTick();
        return;
      }

      // Toda recusa de abrir ou criar chega por aqui: é ela que devolve as
      // ações do diálogo: sem isto um `warning` deixaria os botões
      // desabilitados até fechar e reabrir.
      if (m.type === "warning") { setEnviando(null); setBanner(m.message); return; }

      if (m.type === "unit") { applyUnit(m); return; }

      if (m.type === "run_finished") {
        // Unidade que ficou "na fila" e nunca recebeu evento volta ao repouso —
        // o card nunca fica preso num spinner eterno.
        Object.keys(stateRef.current).forEach((k) => {
          if (stateRef.current[k].state === "pending") stateRef.current[k].state = "idle";
        });
        bumpTick();
        return;
      }
    });
    onShinyReady(() => {
      sendInput("tr_ready", Date.now());
      sendInput("tr_list_imagens", { seq: ++seqCounter });
    });
  }, []);

  // Ferramenta "I" ativada: pede a lista de novo, pelo mesmo motivo do
  // comentário acima de `imagens` — é o gesto mais provável de precisar dela
  // fresca (alguém acabou de largar um arquivo em `imagens/` pra usar agora).
  useEffect(() => {
    if (ferramenta === "imagem") sendInput("tr_list_imagens", { seq: ++seqCounter });
  }, [ferramenta]);

  // Estados terminais: quem chega aqui não vai mudar mais sozinho até o
  // próximo run — é o momento de desligar o "computando…" de quem só vivia
  // de parcial (a região inteira acabou, e cada membro interior não tem
  // evento PRÓPRIO de fim — ver o comentário longo em `regionMemberPatch`).
  const TERMINAL = new Set(["done", "cached", "failed", "blocked", "invalid", "cancelled"]);

  function applyUnit(m) {
    // Estado de execução vive no ref, não no array de nós: assim uma rajada de
    // eventos (um por nó, por execução) não reconstrói o grafo inteiro a cada
    // mensagem. O `tick` avisa a memoização que precisa redecorar.
    const patch = unitState(m);
    const regiao = regionsRef.current[m.node];
    if (!regiao) {
      // Caminho de sempre: nó comum, ou o parcial de um MEMBRO interior, que
      // já chega com `m.node` = id do próprio membro (`scheduler.R` sobrescreve
      // `node` no evento `partial` da região) — não precisa de região nenhuma
      // pra saber em qual card pintar.
      stateRef.current[m.node] = { ...(stateRef.current[m.node] || {}), ...patch };
      bumpTick();
      return;
    }
    // Evento DE UNIDADE de uma região (running/done/cached/failed/blocked/
    // invalid/progress/cancelled): `m.node` é só o PRIMEIRO colapso, mas o
    // evento é de TODA a região. Propaga pra cada membro — é o fechamento do
    // buraco herdado da Fase 5, e também o que desliga o card de um membro
    // interior que só tinha vivido de `partial` (Hole 2): ao chegar aqui um
    // estado TERMINAL, o `partial`/"running" que o parcial ligou apaga.
    regiao.members.forEach((id) => {
      stateRef.current[id] = { ...(stateRef.current[id] || {}), ...regionMemberPatch(m, patch, id, regiao) };
    });
    bumpTick();
  }

  // O patch de UM membro da região, a partir do evento da unidade inteira.
  //
  // Dois papéis, duas leituras do mesmo evento:
  //   - COLAPSO (`role === "collapse"`, o PRÓPRIO `m.node` incluído): tem
  //     artefato PRÓPRIO, e o handle dele mora em `m.handles` sob um nome
  //     QUALIFICADO (`region$outputs`, via a mensagem `regions`) — INCLUSIVE
  //     o primeiro. `.tr_region_out_name()` (`R/plan.R`) qualifica TODA saída
  //     assim que a região tem mais de um colapso — não só a do segundo em
  //     diante. Resolver o primeiro por posição (`Object.values(hs)[0]` em
  //     `firstHandle`) só dava certo porque `u$outputs` HOJE é construído na
  //     ordem de `region$collapse` e `unit === collapse[[1]]` — acoplamento
  //     que nada testava. Agora os dois resolvem pelo NOME, simetricamente;
  //     `firstHandle`/`unitState` continua como o palpite de fora de região
  //     (nó comum, sem `regiao` nenhuma em `applyUnit`).
  //   - SOURCE ou membro LIFTED: não tem chave própria (`R/plan.R` documenta:
  //     "nó interior... não grava artefato"). Só existe enquanto a região
  //     roda — então um estado TERMINAL não herda handle nenhum do evento
  //     (não tem um: por isso ele é REMOVIDO do patch aqui, e não só deixado
  //     como estava — `patch` vem de `unitState()`, que para "done"/"cached"
  //     já grava `handle` do PRÓPRIO evento, e essa é a chave errada pra um
  //     card que não produziu nada. Sem remover, o card de um elevado ou da
  //     fonte repintava com o handle do primeiro colapso — o histórico
  //     inteiro de OUTRO nó, às vezes sem nem as colunas certas). TEM que
  //     apagar o `partial`/"running" que o próprio parcial ligou, senão o
  //     card fica pra sempre com a última prévia girando (o Hole 2 da Fase
  //     8). O ERRO, ao contrário do handle, fica: se a região falhou, cada
  //     membro interior mostra a MESMA mensagem que os colapsos mostram — é
  //     a decisão da revisão da Fase 8 (Important 7): espelhar o estado
  //     terminal está certo (um nó elevado dentro de uma região que falhou
  //     realmente não produziu nada, e um card cinza do lado de um colapso
  //     vermelho sugeriria "esta parte deu certo"), mas apagar justamente o
  //     `error` tirava o único diagnóstico da tela — o card do nó que de fato
  //     falhou (`entra`/`filt`/`mut`) ficava vermelho e MUDO, e só as saídas
  //     mostravam a causa. O mínimo que resolve isso sem inventar um segundo
  //     canal: não apagar `error`, e todo membro passa a mostrar a mesma
  //     mensagem — que pelo menos é verdadeira sobre TODOS eles. (A engine já
  //     nomeia o nó que falhou dentro do TEXTO da mensagem —
  //     `.tr_region_call()`, R/stream-driver.R — mas extrair esse nome por
  //     regex pra rotear a mensagem SÓ pro card certo é frágil: quebra se o
  //     texto mudar de forma, e ainda deixaria os outros cards mudos de novo.
  //     Preferimos a garantia que não depende de parsear prosa.)
  function regionMemberPatch(m, patch, id, regiao) {
    if (regiao.roles[id] === "collapse") {
      if (!m.handles) return patch;
      const nomes = regiao.outputs[id] || [];
      const handle = nomes.map((n) => m.handles[n]).find(Boolean) || null;
      return { ...patch, handle };
    }
    // Source ou lifted. `progress` e `running` propagam como estão (mostra a
    // região andando no card dele também); um estado TERMINAL some com o
    // "rodando" e CONGELA o último parcial — E descarta o `handle` que o
    // patch trouxe (não é dele: ver o comentário acima), só desliga o
    // spinner. O ESTADO e o ERRO em si espelham os da região (falhou/bloqueou
    // junto, com a MESMA mensagem), porque um membro interior de uma região
    // que não terminou bem também não terminou bem.
    if (!TERMINAL.has(m.unit_type)) return patch;
    const { handle: _descartado, ...resto } = patch;
    return { ...resto, progress: null, partial: false };
  }

  function unitState(m) {
    const k = m.unit_type;
    const out = {};
    if (k === "running") { out.state = "running"; out.error = null; out.progress = null; out.partial = false; }
    else if (k === "done") {
      out.state = "done"; out.error = null; out.duration = m.duration;
      out.handle = firstHandle(m.handles); out.partial = false; out.progress = null;
    } else if (k === "cached") {
      out.state = "cached"; out.error = null; out.handle = firstHandle(m.handles);
      out.partial = false; out.progress = null;
    } else if (k === "failed") { out.state = "failed"; out.error = { message: m.message, traceback: m.traceback }; }
    else if (k === "blocked") { out.state = "blocked"; out.error = null; }
    else if (k === "invalid") { out.state = "invalid"; out.error = { message: m.reason }; }
    else if (k === "progress") { out.state = "running"; out.progress = { fraction: m.fraction, message: m.message }; }
    else if (k === "partial") { out.state = "running"; out.handle = m.handle; out.partial = true; }
    else if (k === "cancelled") { out.state = "idle"; out.progress = null; }
    return out;
  }

  const firstHandle = (hs) => (hs && (hs.out || Object.values(hs)[0])) || null;

  // --- Envio ---
  function pushOp(op) {
    // "Na fila" na hora, antes de qualquer resposta: sem isso os cards ficam
    // em branco em silêncio até a primeira mensagem chegar. O servidor manda
    // o estado real conforme roda; aqui é só feedback imediato.
    if (!cosmetica(op)) {
      Object.keys(stateRef.current).forEach((k) => {
        stateRef.current[k] = { ...stateRef.current[k], state: "pending" };
      });
      bumpTick();
    }
    sendOp(op, revRef.current);
  }

  // Várias ops de um gesto viajam num `batch`: uma revisão, um passo de undo.
  // Soltas, a segunda já sairia com `base_rev` velho (o front só avança a
  // revisão no eco) e o servidor recusaria todas menos a primeira, que é o
  // que "Organizar" fazia até aqui.
  function pushMany(ops) {
    if (ops.length === 0) return;
    pushOp(ops.length === 1 ? ops[0] : { op: "batch", ops });
  }

  const onNodesChange = useCallback((ch) => {
    // Frame arrastado leva junto os cards e frames que estavam INTEIROS dentro
    // dele quando o gesto começou (lista congelada em `onNodeDragStart`). O mesmo
    // delta do frame vira mudança de posição de cada um, com o mesmo
    // `dragging`, e cai no mesmo batch do fim do gesto.
    const levar = arrastoRef.current || {};
    const extra = [];
    ch.forEach((c) => {
      const l = c.type === "position" && c.position && levar[c.id];
      if (!l) return;
      const dx = c.position.x - l.x0, dy = c.position.y - l.y0;
      l.itens.forEach((k) => extra.push({ id: k.id, type: "position", dragging: c.dragging,
                                          position: { x: k.x + dx, y: k.y + dy } }));
    });
    const todas = extra.length ? [...ch, ...extra] : ch;
    setNodes((ns) => applyNodeChanges(todas, ns));
    // `dimensions` e `select` não têm significado pro documento e chegam a
    // cada remedição; emitir op por elas realimentaria o laço. Remoção não
    // passa por aqui: `onBeforeDelete` intercepta antes (ver `apagar`). As
    // posições finais de um arrasto chegam todas na MESMA chamada, então o
    // gesto inteiro (frames e os cards que eles levam) vira um batch só.
    const fim = todas.filter((c) => c.type === "position" && c.dragging === false && c.position);
    if (fim.length === 0) return;
    arrastoRef.current = null;
    const tipo = Object.fromEntries(nodesRef.current.map((n) => [n.id, n.type]));
    pushMany(fim.map((c) => {
      const x = Math.round(c.position.x), y = Math.round(c.position.y);
      if (tipo[c.id] === "trFrame") return { op: "update_frame", frame: c.id, x, y };
      if (tipo[c.id] === "trNota") return { op: "update_note", note: c.id, x, y };
      return { op: "move", node: c.id, x, y };
    }));
  }, []);

  const onNodeDragStart = useCallback((e, _n, dragged) => {
    // Alt: ajuste fino do retângulo, sem levar nada. Só o que está
    // selecionado anda, que é o comportamento de um nó comum. `e` é o evento
    // de origem do d3-drag (mouse ou toque), que carrega `altKey`.
    if (e?.altKey) { arrastoRef.current = null; return; }
    // Medida do DOM: com a estimativa, card alto que cabe no frame parecia
    // vazar e ficava pra trás no arrasto.
    const ns = comMedidas(nodesRef.current);
    const byId = Object.fromEntries(ns.map((n) => [n.id, n]));
    // Item já arrastado pela seleção, ou dentro de dois frames arrastados
    // juntos, entra uma vez só: senão receberia dois deltas. Todos os itens
    // de um mesmo gesto andam o mesmo delta, então tanto faz qual frame
    // arrastado reivindica o item primeiro. A contenção já é transitiva
    // (o externo pega as células e os cards delas), e só o frame ARRASTADO
    // distribui delta: frame levado não leva ninguém.
    const vistos = new Set(dragged.map((d) => d.id));
    const levar = {};
    dragged.filter((d) => d.type === "trFrame").forEach((f) => {
      const alvo = byId[f.id] || f;
      const itens = [...containedFrames(alvo, ns), ...containedCards(alvo, ns), ...containedNotes(alvo, ns)]
        .filter((id) => !vistos.has(id));
      itens.forEach((id) => vistos.add(id));
      levar[f.id] = { x0: f.position.x, y0: f.position.y,
                      itens: itens.map((id) => ({ id, x: byId[id].position.x, y: byId[id].position.y })) };
    });
    arrastoRef.current = levar;
  }, []);

  // Com `SelectionMode.Partial`, uma caixa desenhada DENTRO de um frame o
  // selecionaria também. Cards continuam por interseção; frames só ficam
  // selecionados se couberem inteiros na caixa.
  const onSelectionStart = useCallback((e) => {
    caixaRef.current = rf.screenToFlowPosition({ x: e.clientX, y: e.clientY });
  }, [rf]);
  const onSelectionEnd = useCallback((e) => {
    const a = caixaRef.current; caixaRef.current = null;
    if (!a) return;
    const b = rf.screenToFlowPosition({ x: e.clientX, y: e.clientY });
    const caixa = { x: Math.min(a.x, b.x), y: Math.min(a.y, b.y),
                    w: Math.abs(a.x - b.x), h: Math.abs(a.y - b.y) };
    // Clique seco no vazio também passa por aqui: sem frame a soltar, o array
    // fica o mesmo, e o React Flow não tem o que remedir.
    setNodes((ns) => {
      const soltar = (n) => n.type === "trFrame" && n.selected && !inside(rectOf(n), caixa);
      return ns.some(soltar) ? ns.map((n) => (soltar(n) ? { ...n, selected: false } : n)) : ns;
    });
  }, [rf]);

  // A seleção de aresta precisa entrar no estado, senão o Delete nunca a vê.
  // Remoção também não chega aqui (ver `onBeforeDelete`).
  const onEdgesChange = useCallback((ch) => setEdges((es) => applyEdgeChanges(ch, es)), []);

  // Menu de contexto. Posição em coordenadas do wrapper, não da tela: o menu é
  // filho de `.tr-canvas`, que é `position:relative`.
  const abrirMenu = useCallback((ev, kind, id) => {
    ev.preventDefault();
    const box = wrapRef.current?.getBoundingClientRect();
    if (!box) return;
    setMenu({ kind, id, x: ev.clientX - box.left, y: ev.clientY - box.top });
  }, []);

  // Todo apagar passa por aqui: tecla, menu e barra de seleção. Sai UM batch.
  // `remove_node` já leva as arestas do nó no servidor (`.tr_op_remove_node`),
  // então `disconnect` sai só pra aresta pedida cujas DUAS pontas sobrevivem.
  // Mandar os dois faria o segundo falhar com "aresta que não existe", e o
  // lote inteiro seria recusado.
  const apagar = useCallback((nodeIds, edgeIds = []) => {
    const alvo = new Set(nodeIds);
    const pedidas = new Set(edgeIds);
    const tipo = Object.fromEntries(nodesRef.current.map((n) => [n.id, n.type]));
    const ops = [];
    nodeIds.forEach((id) => {
      if (!tipo[id]) return;
      if (tipo[id] === "trFrame") ops.push({ op: "remove_frame", frame: id });
      else if (tipo[id] === "trNota") ops.push({ op: "remove_note", note: id });
      else ops.push({ op: "remove_node", node: id });
    });
    edgesRef.current.forEach((e) => {
      if (!pedidas.has(e.id) || alvo.has(e.source) || alvo.has(e.target)) return;
      ops.push({ op: "disconnect", from_node: e.source, from_port: e.sourceHandle,
                 to_node: e.target, to_port: e.targetHandle, index: e.data?.index });
    });
    setNodes((ns) => ns.filter((n) => !alvo.has(n.id)));
    setEdges((es) => es.filter((e) => !pedidas.has(e.id) && !alvo.has(e.source) && !alvo.has(e.target)));
    pushMany(ops);
    setMenu(null);
  }, []);

  // `false` cancela a remoção do próprio React Flow, que emitiria um `remove`
  // por nó E por aresta ligada — a colisão de ops que `apagar` evita.
  const onBeforeDelete = useCallback(async ({ nodes: ns, edges: es }) => {
    // O xyflow chama isto a CADA Delete/Backspace, mesmo sem nada escolhido.
    // Sem a guarda, cada tecla solta recriaria os arrays de nós e arestas (o
    // `filter` de `apagar` sempre devolve array novo, e o React Flow remede
    // tudo) e fecharia o menu, sem apagar nada.
    if (!ns.length && !es.length) return false;
    apagar(ns.map((n) => n.id), es.map((e) => e.id));
    return false;
  }, [apagar]);

  // Menu num card que faz parte de uma seleção múltipla age sobre a seleção
  // inteira, o gesto de Miro/Figma. Num card fora dela, só nele.
  const alvoMenu = (id) => {
    const sel = nodes.filter((n) => n.selected).map((n) => n.id);
    return sel.length > 1 && sel.includes(id) ? sel : [id];
  };

  const onConnect = useCallback((c) => {
    pushOp({ op: "connect", from_node: c.source, from_port: c.sourceHandle,
             to_node: c.target, to_port: c.targetHandle });
  }, []);

  const isValidConnection = useCallback((c) => {
    const cat = catalogRef.current; if (!cat) return false;
    const byId = Object.fromEntries(cat.nodes.map((n) => [n.id, n]));
    const s = nodes.find((n) => n.id === c.source), t = nodes.find((n) => n.id === c.target);
    if (!s || !t) return false;
    const op = byId[s.data.nodeType]?.outputs.find((p) => p.name === c.sourceHandle);
    const ip = byId[t.data.nodeType]?.inputs.find((p) => p.name === c.targetHandle);
    return !!(op && ip) && compatible(cat, op.type, ip.type);
  }, [nodes]);

  const addAt = useCallback((typeId, pos, extra) => {
    pushOp({ op: "add_node", type: typeId, position: [Math.round(pos.x), Math.round(pos.y)],
             ...extra });
  }, []);

  // Clicar na paleta cai numa cascata a partir do canto visível, em vez de um
  // ponto fixo: sem isso todo nó novo nasce exatamente em cima do anterior.
  const addPicked = useCallback((typeId) => {
    const box = wrapRef.current?.getBoundingClientRect();
    const origin = rf.screenToFlowPosition({
      x: (box?.left ?? 0) + 90, y: (box?.top ?? 0) + 90 });
    const k = nodes.length;
    addAt(typeId, { x: origin.x + (k % 4) * 275, y: origin.y + Math.floor(k / 4) * 230 });
  }, [rf, addAt, nodes.length]);

  // Extensão do arquivo solto -> tipo de nó de leitura. Só os dois formatos
  // que os leitores tratam como texto puro: o transporte (`tr_data_upload`,
  // ver R/transport.R) manda o conteúdo como STRING, lido no navegador com
  // `FileReader.readAsText`. Excel/Parquet/RDS são binários e exigiriam um
  // caminho de transporte à parte (base64) — fora de escopo por ora.
  const EXT_NODE_LEITURA = { csv: "data/read_csv", json: "data/read_json" };

  // Lê o arquivo e manda pro servidor gravar em `data/`; a posição e o tipo
  // de nó ficam pendentes (por `id`) até a resposta (`data_upload_ok` ou
  // `data_upload_conflict`) chegar — ver o handler de `tr_event`. Devolve
  // `false` sem tocar em nada quando a extensão não é reconhecida, pro
  // chamador saber que não deve interceptar o drop (deixa borbulhar).
  const iniciarUploadDado = useCallback((file, pos) => {
    const ext = (file.name.match(/\.([a-z0-9]+)$/i) || [])[1]?.toLowerCase();
    const tipo = EXT_NODE_LEITURA[ext];
    if (!tipo) return false;
    const leitor = new FileReader();
    leitor.onload = () => {
      const id = novoId();
      uploadsPendentesRef.current[id] = { tipo, pos, nome: file.name, conteudo: leitor.result };
      sendInput("tr_data_upload",
        { seq: ++seqCounter, id, nome: file.name, conteudo: leitor.result, overwrite: false });
    };
    leitor.readAsText(file);
    return true;
  }, []);

  const onDrop = useCallback((ev) => {
    ev.preventDefault();
    const t = ev.dataTransfer.getData("application/trama-type");
    if (t) { addAt(t, rf.screenToFlowPosition({ x: ev.clientX, y: ev.clientY })); return; }
    // Arquivo do SO (não o drag interno da paleta, tratado acima). Soltar
    // `.json` aqui passa a significar "quero ler isto como dado" — diferente
    // de soltar fora do canvas, que continua abrindo o diálogo de importar
    // projeto (`onDropGlobal`). `stopPropagation` é o que separa os dois: sem
    // ele, este mesmo evento borbulharia até lá e abriria os dois ao mesmo
    // tempo.
    const f = ev.dataTransfer.files?.[0];
    if (f && iniciarUploadDado(f, rf.screenToFlowPosition({ x: ev.clientX, y: ev.clientY }))) {
      ev.stopPropagation();
    }
  }, [rf, addAt, iniciarUploadDado]);

  const onConnectStart = useCallback((_e, p) => {
    const cat = catalogRef.current;
    const n = nodes.find((x) => x.id === p.nodeId);
    if (!cat || !n || p.handleType !== "source") return;
    const byId = Object.fromEntries(cat.nodes.map((x) => [x.id, x]));
    setDragType(byId[n.data.nodeType]?.outputs.find((o) => o.name === p.handleId)?.type || null);
  }, [nodes]);

  // O log de undo é do servidor: aqui só se pede. Log no cliente, montado com
  // os ecos, desfazia o passo errado quando havia op em voo — o R bloqueia
  // computando, o eco ainda não chegou, e o Ctrl+Z tirava do log uma op que
  // não era a última aplicada. O valor não carrega nada: `Date.now()` só
  // garante que cada Ctrl+Z seja um evento distinto.
  const desfazer = () => sendInput("tr_undo", Date.now());

  const selecionar = (sim) => {
    setNodes((ns) => ns.map((n) => (!!n.selected === sim ? n : { ...n, selected: sim })));
    if (!sim) setEdges((es) => es.map((e) => (e.selected ? { ...e, selected: false } : e)));
  };

  // Título padrão mandado daqui, e não deixado ao servidor: o servidor numera
  // por max(order)+1, e o selo do frame mostra a POSIÇÃO na lista ordenada.
  // Depois de apagar o frame 2 de 3, o novo teria selo 3 e título "Frame 4".
  const tituloNovo = () =>
    `Frame ${nodesRef.current.filter((n) => n.type === "trFrame").length + 1}`;

  const criarFrame = (r) => {
    setFerramenta(null);
    if (!r) return;
    pushOp({ op: "add_frame", x: Math.round(r.x), y: Math.round(r.y),
             w: Math.round(r.w), h: Math.round(r.h), aspect: aspectoNovo, title: tituloNovo() });
  };

  const criarNota = (kind, r) => {
    setFerramenta(null);
    if (!r) return;
    pushOp({ op: "add_note", kind, x: Math.round(r.x), y: Math.round(r.y),
             w: Math.round(r.w), h: Math.round(r.h) });
  };

  const frameDaSelecao = () => {
    const sel = comMedidas(nodesRef.current.filter((n) => n.selected && n.type === "ndNode"));
    if (sel.length === 0) return;
    // Bbox pelo mesmo `rectOf` do pertencimento: a medida que decide "está
    // dentro" é a mesma que desenha o frame, então o frame da seleção
    // sempre contém a seleção.
    const rs = sel.map(rectOf);
    const x0 = Math.min(...rs.map((r) => r.x)), y0 = Math.min(...rs.map((r) => r.y));
    const x1 = Math.max(...rs.map((r) => r.x + r.w)), y1 = Math.max(...rs.map((r) => r.y + r.h));
    const f = fitAspect({ x: x0, y: y0, width: x1 - x0, height: y1 - y0 }, aspectoNovo);
    pushOp({ op: "add_frame", ...f, aspect: aspectoNovo, title: tituloNovo() });
  };

  // O pack inteiro é UM batch: uma revisão, um passo de desfazer, e a ordem
  // das ops vira a ordem de slide (externo primeiro). Centro da área visível
  // pelo wrapper do canvas, e não pela janela: a paleta e o painel ocupam a
  // coluna da direita. `add_frame` num batch faz o servidor devolver o
  // documento, como o frame solto: os ids e a ordem chegam por lá.
  const criarPrancheta = (o) => {
    const { aspect, ...resto } = o;
    try { localStorage.setItem("trama.prancheta", JSON.stringify(resto)); } catch (_) {}
    if (aspect !== aspectoNovo) setAspectoNovo(aspect);
    const b = wrapRef.current.getBoundingClientRect();
    const centro = rf.screenToFlowPosition({ x: b.left + b.width / 2, y: b.top + b.height / 2 });
    const existentes = nodesRef.current.filter((n) => n.type === "trFrame").length;
    const fs = gradeDeFrames(o, centro, existentes);
    pushMany(fs.map((f) => ({ op: "add_frame", ...f })));
    setPrancheta(null);
    // Pacote maior que a tela (um 3×3 de 1600px no zoom 1) nasceria quase todo
    // fora dela e pareceria que nada aconteceu: afasta a câmera. Como a grade
    // é centrada no meio da tela, é só um zoom-out mantendo o centro. No
    // quadro seguinte, como no `organizarTudo`, pra não brigar com o eco.
    const x = Math.min(...fs.map((f) => f.x)), y = Math.min(...fs.map((f) => f.y));
    const width = Math.max(...fs.map((f) => f.x + f.w)) - x;
    const height = Math.max(...fs.map((f) => f.y + f.h)) - y;
    const zoom = rf.getViewport().zoom;
    if (width * zoom > b.width || height * zoom > b.height)
      requestAnimationFrame(() => rf.fitBounds({ x, y, width, height }, { padding: 0.1, duration: 300 }));
  };

  // Frame é bloco: o layout (`organizar`) mexe em cards e frames juntos, e
  // tudo sobe num batch só, um passo de desfazer. Só vai o que mudou: sem
  // isso, apertar duas vezes deixaria um passo vazio na pilha. `update_frame`
  // não devolve o documento, então o tamanho novo do frame entra no estado
  // aqui mesmo, como no arrasto.
  const organizarTudo = () => {
    // Medida do card lida do DOM (`comMedidas`): card mais baixo do que é
    // deixava o frame pequeno demais, o card vazava pela borda e, no
    // Organizar seguinte, já tinha outro dono. A mesma lista serve pro
    // "mudou?" abaixo, pra comparar com a régua do layout.
    const ns = comMedidas(nodesRef.current);
    const { cards, frames, notes } = organizar(ns, edgesRef.current);
    const ops = [];
    ns.forEach((n) => {
      const f = frames[n.id], c = cards[n.id], nt = notes[n.id];
      if (f) {
        const r = rectOf(n);
        const op = { op: "update_frame", frame: n.id };
        if (f.x !== r.x || f.y !== r.y) Object.assign(op, { x: f.x, y: f.y });
        if (f.w !== r.w) op.w = f.w;
        if (f.h !== r.h) op.h = f.h;
        if (Object.keys(op).length > 2) ops.push(op);
      } else if (c && (c.x !== n.position.x || c.y !== n.position.y)) {
        ops.push({ op: "move", node: n.id, x: c.x, y: c.y });
      } else if (nt && (nt.x !== n.position.x || nt.y !== n.position.y)) {
        ops.push({ op: "update_note", note: n.id, x: nt.x, y: nt.y });
      }
    });
    setNodes((atual) => atual.map((n) => {
      const f = frames[n.id], c = cards[n.id], nt = notes[n.id];
      if (f) return { ...n, position: { x: f.x, y: f.y }, width: f.w, height: f.h };
      if (c) return { ...n, position: c };
      return nt ? { ...n, position: nt } : n;
    }));
    // O dagre não sabe onde a câmera está: o grafo arrumado pode nascer
    // fora da tela. Enquadra no quadro seguinte, depois de o React Flow
    // já ter as posições novas.
    requestAnimationFrame(() => rf.fitView({ maxZoom: 1, padding: 0.25, duration: 300 }));
    pushMany(ops);
  };

  // Frames na ordem de slide, com o retângulo atual (que o gesto local pode
  // ter mudado antes de qualquer eco).
  const framesOrd = useMemo(() => nodes.filter((n) => n.type === "trFrame")
    .sort((a, b) => (a.data.order ?? 0) - (b.data.order ?? 0))
    .map((n) => ({ id: n.id, title: n.data.title, aspect: n.data.aspect, ...rectOf(n) })), [nodes]);
  const framesOrdRef = useRef([]); framesOrdRef.current = framesOrd;
  const presentRef = useRef(null); presentRef.current = present;
  const abrindoRef = useRef(false); abrindoRef.current = abrindo;
  // Estável porque é dependência do efeito que monta o Esc do diálogo: nova a
  // cada render, o listener seria trocado a cada tecla digitada no nome.
  const fecharDialogo = useCallback(() => {
    setAbrindo(false); setEnviando(null); setArquivoSolto(null);
  }, []);

  const enquadrar = (f, duration = 400, padding = 0.1) =>
    rf.fitBounds({ x: f.x, y: f.y, width: f.w, height: f.h }, { padding, duration });

  const apresentar = () => {
    if (framesOrd.length === 0) return;
    selecionar(false); setMenu(null); setFerramenta(null); setPrancheta(null);
    setPresent({ i: 0, volta: rf.getViewport() });
    document.documentElement.requestFullscreen?.().catch(() => {});
  };

  const sairApresentacao = () => {
    const p = presentRef.current;
    if (!p) return;
    // Zera o ref na hora: sair da tela cheia dispara `fullscreenchange`, que
    // chamaria isto de novo antes do próximo render.
    presentRef.current = null;
    setPresent(null);
    rf.setViewport(p.volta, { duration: 300 });
    if (document.fullscreenElement) document.exitFullscreen().catch(() => {});
  };
  const sairRef = useRef(null); sairRef.current = sairApresentacao;

  const passo = (d) => setPresent((p) => p && {
    ...p, i: Math.max(0, Math.min(framesOrdRef.current.length - 1, p.i + d)) });

  // O id do frame da vez também está nas deps: se um frame ANTERIOR sai no meio
  // da apresentação (eco de outra aba), `present.i` fica igual mas aponta pro
  // slide seguinte, e a tela tem que ir atrás dele.
  const idDaVez = present ? framesOrd[present.i]?.id : undefined;
  useEffect(() => {
    if (!present) return;
    const f = framesOrdRef.current[present.i];
    if (f) enquadrar(f, 400, 0);
  }, [present?.i, !!present, idDaVez]);

  // Frames podem sumir durante a apresentação: ela não edita nada, mas o
  // documento pode chegar mudado de outra aba. Sem frame nenhum não há o que
  // mostrar; com menos frames, o slide da vez passa a ser o último que
  // sobrou, em vez de um índice além do fim.
  useEffect(() => {
    const p = presentRef.current;
    if (!p) return;
    if (framesOrd.length === 0) sairApresentacao();
    else if (p.i >= framesOrd.length) setPresent((q) => q && { ...q, i: framesOrd.length - 1 });
  }, [framesOrd.length]);

  useEffect(() => {
    if (!present) return;
    // Entrar em tela cheia muda o tamanho do canvas DEPOIS do primeiro
    // enquadramento, então reenquadra. E sair dela pelo Esc do navegador (que
    // engole a tecla antes da página) tem que encerrar a apresentação.
    const refit = () => {
      const f = framesOrdRef.current[presentRef.current?.i];
      if (f) enquadrar(f, 0, 0);
    };
    const fsc = () => { if (!document.fullscreenElement) sairRef.current(); else refit(); };
    window.addEventListener("resize", refit);
    document.addEventListener("fullscreenchange", fsc);
    return () => {
      window.removeEventListener("resize", refit);
      document.removeEventListener("fullscreenchange", fsc);
    };
  }, [!!present]);

  // Na apresentação a roda sobre um preview fica com o navegador (ver
  // `noWheelClassName`), e o xyflow sai do handler dele sem `preventDefault`.
  // Ctrl+roda, e a pinça do trackpad que chega como Ctrl+roda, viraria então
  // zoom da PÁGINA, que o Chrome ainda lembra por site depois. O `onWheel` do
  // React é passivo e não pode cancelar nada: daí o listener nativo com
  // `passive: false`. O wrapper só existe depois do "carregando…", por isso
  // as deps: o efeito roda de novo quando catálogo e documento chegam.
  //
  // Editando, o mesmo listener faz Ctrl+roda ROLAR a tela (a roda sozinha é
  // zoom, e o xyflow não tem "rolar com modificador"): na vertical, e com
  // Shift também, na horizontal. Registrado na fase de CAPTURA do wrapper, que
  // é ancestral do pane onde o d3-zoom escuta: roda antes dele, e o
  // `stopPropagation` impede que o mesmo evento ainda vire zoom lá embaixo.
  // Custo aceito: a pinça do trackpad chega como Ctrl+roda e passa a rolar a
  // tela em vez de dar zoom; a roda sozinha continua dando zoom.
  const pronto = !!catalog && !!doc;
  useEffect(() => {
    const el = wrapRef.current;
    if (!el) return;
    const onWheel = (e) => {
      if (presentRef.current) { if (e.ctrlKey) e.preventDefault(); return; }
      if (!e.ctrlKey && !e.metaKey) return;
      e.preventDefault(); e.stopPropagation();
      // `deltaMode` 1 é em linhas e 2 em páginas; o viewport anda em pixels.
      // A página é a tela na direção em que se anda: largura na horizontal,
      // altura na vertical.
      const kx = e.deltaMode === 1 ? 16 : e.deltaMode === 2 ? el.clientWidth : 1;
      const ky = e.deltaMode === 1 ? 16 : e.deltaMode === 2 ? el.clientHeight : 1;
      // Com Shift, há navegador que já entrega a roda vertical como `deltaX`
      // (é o Shift+roda nativo); os outros mandam `deltaY`, que vira horizontal
      // aqui. Aceitar os dois é o que faz Ctrl+Shift+roda rolar de lado em ambos.
      const dx = (e.shiftKey && !e.deltaX ? e.deltaY : e.deltaX) * kx;
      const dy = e.shiftKey ? 0 : e.deltaY * ky;
      const vp = rf.getViewport();
      rf.setViewport({ x: vp.x - dx, y: vp.y - dy, zoom: vp.zoom });
    };
    el.addEventListener("wheel", onWheel, { passive: false, capture: true });
    return () => el.removeEventListener("wheel", onWheel, { capture: true });
  }, [pronto]);

  // Um PNG por frame, em sequência. Falha num frame vira banner e os outros
  // seguem; o número do arquivo é a posição na sequência de slides.
  const exportar = async (fs) => {
    const vp = wrapRef.current?.querySelector(".react-flow__viewport");
    if (!vp || fs.length === 0) return;
    setExportando(true);
    const falhas = [];
    try {
      for (const f of fs) {
        const i = framesOrdRef.current.findIndex((x) => x.id === f.id) + 1;
        // A lista `fs` foi tirada no clique e a exportação é assíncrona: um
        // frame apagado no meio do caminho não tem mais posição na sequência,
        // e sairia como "00-…". Some da leva em vez de ganhar número falso.
        if (i === 0) continue;
        try { await exportFramePng(vp, f, i, temas.marca); }
        catch (err) { falhas.push(`'${f.title || "sem título"}': ${err?.message || err}`); }
      }
    } finally { setExportando(false); }
    if (falhas.length) setBanner(`Não foi possível exportar ${falhas.join(" · ")}`);
  };

  // Ações em massa agem na seleção; sem seleção, no canvas inteiro. Frames
  // ficam de fora: não têm preview, parâmetro nem tamanho de card. O canvas
  // inteiro só vale quando NADA está selecionado, nem nó nem ligação: com só
  // frames ou só ligações selecionados, cair nos cards todos agiria longe de
  // onde o usuário apontou, e a ação vira nada.
  const alvos = () => {
    const todos = nodesRef.current;
    const cards = todos.filter((n) => n.type === "ndNode");
    const sel = cards.filter((n) => n.selected);
    if (sel.length) return sel;
    const algo = todos.some((n) => n.selected) || edgesRef.current.some((e) => e.selected);
    return algo ? [] : cards;
  };

  // Regra do Figma: se ALGUM alvo está aberto, todos fecham; senão, todos
  // abrem. Inverter card a card deixaria metade aberta e metade fechada.
  // Só entra quem TEM o que recolher: card sem parâmetro nenhum, e órfão (sem
  // spec, sem preview), nunca fecham de fato, então contariam como abertos para
  // sempre e o alternar ficaria preso em "fechar" sem mudar nada na tela.
  const alternarFold = (parte) => {
    const tem = parte === "params"
      ? (n) => (n.data.spec?.params || []).length > 0
      : (n) => !!n.data.spec;
    const alvo = alvos().filter(tem);
    if (alvo.length === 0) return;
    const foldDe = (n) => (foldsRef.current[n.id] ?? n.data.fold) || {};
    const aberto = (n) => foldDe(n)[parte] !== false;
    const valor = !alvo.some(aberto);
    const muda = alvo.filter((n) => aberto(n) !== valor);
    muda.forEach((n) => { foldsRef.current[n.id] = { ...foldDe(n), [parte]: valor }; });
    bumpTick();
    pushMany(muda.map((n) => ({ op: "set_fold", node: n.id, [parte]: valor })));
  };

  // Um lugar só pra restaurar: atalho, barra de seleção e menu do card. Quem
  // chama passa ids de CARD: `resize` é op de nó e aborta num id de frame, e
  // isso derrubaria o batch inteiro. O ref recebe o padrão em vez de `null`:
  // `resize` não é op estrutural, então o servidor não devolve o documento e o
  // eco não vem; limpar a entrada deixaria o card no tamanho ANTIGO, que é o
  // que `n.data.size` ainda diz. O par escrito aqui é o mesmo que a op grava lá.
  const restaurarTamanhos = (ids) => {
    if (ids.length === 0) return;
    ids.forEach((id) => { sizesRef.current[id] = [MIN_W, MIN_H]; });
    bumpTick();
    pushMany(ids.map((id) => ({ op: "resize", node: id, w: MIN_W, h: MIN_H })));
  };
  const restaurarAlvos = () => restaurarTamanhos(alvos().map((n) => n.id));

  // Deslocamento de cada cópia/colagem, em coordenadas do canvas: o suficiente
  // pra cópia nunca nascer exatamente em cima do original (ninguém enxergaria
  // que algo mudou) e pouco o bastante pra não fugir da tela numa seleção
  // grande.
  const DESLOCA_COPIA = 48;

  // Retrato do que existe AGORA nesses ids: cards, frames e notas com os
  // campos que uma cópia precisa recriar, e só as ligações com as DUAS pontas
  // dentro do grupo (uma ligação que sai do grupo não faz sentido duplicada:
  // o outro lado é o original, que já tem sua própria ligação). Tirado do
  // `nodesRef`/`edgesRef` (não do `doc`) porque é o que a tela mostra AGORA,
  // já com posição e tamanho arrastados que ainda não ecoaram.
  const retratoDoGrupo = (ids) => {
    const alvo = new Set(ids);
    const ns = nodesRef.current.filter((n) => alvo.has(n.id));
    if (ns.length === 0) return null;
    const es = edgesRef.current.filter((e) => alvo.has(e.source) && alvo.has(e.target));
    return {
      nodes: ns.map((n) => ({ id: n.id, type: n.type, position: { ...n.position },
                              width: n.width, height: n.height, data: { ...n.data } })),
      edges: es.map((e) => ({ source: e.source, sourceHandle: e.sourceHandle,
                              target: e.target, targetHandle: e.targetHandle })),
    };
  };

  // Um retrato -> as ops que recriam o grupo deslocado. Ids novos nascem AQUI,
  // no cliente (e não esperam o eco): é o que permite ligar as cópias entre si
  // no MESMO batch, como op de `connect` apontando pra um id que só existe
  // dentro deste lote. Tamanho e recolhimento de card são ops PARTE de
  // `add_node` — só entram se o original tinha algo fora do padrão, senão
  // colar 50 cards mandaria 100 ops à toa.
  const opsDoGrupo = (retrato, dx, dy) => {
    const novoDe = {};
    const criam = [];
    const depois = [];
    retrato.nodes.forEach((n) => {
      const id = novoId();
      novoDe[n.id] = id;
      const x = Math.round(n.position.x + dx), y = Math.round(n.position.y + dy);
      if (n.type === "ndNode") {
        criam.push({ op: "add_node", id, type: n.data.nodeType, position: [x, y],
                    label: n.data.label, params: n.data.params || {}, seed: n.data.seed });
        if (n.data.size) depois.push({ op: "resize", node: id, w: n.data.size[0], h: n.data.size[1] });
        const fold = n.data.fold || {};
        const patch = {};
        if (fold.preview === false) patch.preview = false;
        if (fold.params === false) patch.params = false;
        if (Object.keys(patch).length) depois.push({ op: "set_fold", node: id, ...patch });
      } else if (n.type === "trFrame") {
        criam.push({ op: "add_frame", id, x, y, w: n.width, h: n.height,
                    title: n.data.title, aspect: n.data.aspect, color: n.data.color });
      } else if (n.type === "trNota") {
        const base = { op: "add_note", id, x, y, w: n.width, h: n.height, kind: n.data.kind,
                       escala: n.data.escala, fundo: n.data.fundo, color: n.data.color };
        if (n.data.kind === "markdown") base.text = n.data.text;
        else { base.src = n.data.src; base.fit = n.data.fit; }
        criam.push(base);
      }
    });
    retrato.edges.forEach((e) => {
      if (!novoDe[e.source] || !novoDe[e.target]) return;
      depois.push({ op: "connect", from_node: novoDe[e.source], from_port: e.sourceHandle,
                   to_node: novoDe[e.target], to_port: e.targetHandle });
    });
    return [...criam, ...depois];
  };

  // Ctrl+C: guarda o retrato da seleção. Sem seleção não há o que copiar — ao
  // contrário de `alvos()` (ações em massa), aqui NADA selecionado não quer
  // dizer "a tela inteira": copiar o canvas todo por engano seria surpresa
  // grande demais pra um atalho tão comum.
  const copiar = () => {
    const sel = nodesRef.current.filter((n) => n.selected).map((n) => n.id);
    const retrato = retratoDoGrupo(sel);
    if (!retrato) return;
    clipboardRef.current = retrato;
    colagensRef.current = 0;
  };

  // Ctrl+V: cola o último Ctrl+C, deslocado. Colagens seguidas (sem novo
  // Ctrl+C no meio) escalam o deslocamento — a cascata do Figma — pra cada
  // colagem ficar visível, e não empilhada exatamente sobre a anterior.
  const colar = () => {
    const retrato = clipboardRef.current;
    if (!retrato) return;
    colagensRef.current += 1;
    const off = DESLOCA_COPIA * colagensRef.current;
    pushMany(opsDoGrupo(retrato, off, off));
  };

  // Duplicar (menu de contexto): copiar + colar num só passo, sem tocar o
  // clipboard — um Ctrl+V depois de duplicar continua colando o que o
  // usuário copiou por último, não o bloco duplicado.
  const duplicar = (ids) => {
    const retrato = retratoDoGrupo(ids);
    if (!retrato) return;
    pushMany(opsDoGrupo(retrato, DESLOCA_COPIA, DESLOCA_COPIA));
  };

  // Tabela refeita a cada render e lida pelo listener via ref: o listener é
  // registrado uma vez só, e as ações sempre enxergam o estado atual. As
  // chaves são `mod+` (Ctrl ou Cmd), `shift+`, e `e.key` em minúsculas.
  const atalhosRef = useRef({});
  atalhosRef.current = present ? {
    "arrowright": () => passo(1), "pagedown": () => passo(1), " ": () => passo(1),
    "arrowleft": () => passo(-1), "pageup": () => passo(-1),
    "home": () => setPresent((p) => p && { ...p, i: 0 }),
    "end": () => setPresent((p) => p && { ...p, i: framesOrdRef.current.length - 1 }),
    "escape": sairApresentacao,
  } : {
    "mod+z": desfazer,
    "mod+a": () => selecionar(true),
    "escape": () => { selecionar(false); setMenu(null); setFerramenta(null); setMenuAcoes(false); },
    "f": () => setFerramenta((t) => (t === "frame" ? null : "frame")),
    "m": () => setFerramenta((t) => (t === "markdown" ? null : "markdown")),
    "i": () => setFerramenta((t) => (t === "imagem" ? null : "imagem")),
    "mod+g": frameDaSelecao,
    "p": () => alternarFold("preview"),
    "o": () => alternarFold("params"),
    "shift+r": restaurarAlvos,
  };
  useEffect(() => {
    const onKey = (e) => {
      const t = e.target;
      // O diálogo de projeto é dono do teclado enquanto está aberto, e a
      // guarda vem ANTES de tudo. O guarda de `INPUT|TEXTAREA|SELECT` abaixo
      // cobre o campo de nome, mas o foco do diálogo vive nos BOTÕES dele —
      // cada pasta listada é um — e ali um "f" abria a ferramenta de frame e
      // um "p" recolhia o preview dos cards atrás do overlay, sem nada na tela
      // explicando o que mudou. Antes do `blur` de espaço logo abaixo porque
      // aquele desarma o espaço em QUALQUER botão, e aqui o espaço é a
      // segunda forma legítima de acionar a linha de pasta pelo teclado —
      // atrás do overlay não há tela para o espaço andar. O Esc daqui também
      // sai: quem o fecha é o listener do próprio diálogo.
      if (abrindoRef.current) return;
      // Espaço é o gesto de andar pela tela, e o navegador CLICA o botão com
      // foco quando o espaço é solto: depois de "↶ Desfazer", andar desfazia de
      // novo; depois de um item da paleta, nascia outro card. `preventDefault`
      // não resolve (o xyflow já o chama no keydown, e há navegador que clica
      // no keyup mesmo assim); tirar o foco resolve. Custo aceito: espaço
      // deixa de acionar botão pelo teclado — Enter continua acionando.
      if (e.key === " " && t && t.tagName === "BUTTON") t.blur();
      // Digitar um parâmetro não pode apagar card nem disparar atalho.
      if (t && (t.isContentEditable || /^(INPUT|TEXTAREA|SELECT)$/.test(t.tagName))) return;
      // Imagem ampliada por cima do slide é dona do teclado: seta e espaço
      // trocariam o slide escondido atrás dela, e o Esc que a fecha também
      // encerraria a apresentação. O lightbox tem o próprio listener.
      if (presentRef.current && document.querySelector(".tr-lightbox")) return;
      const nome = (e.ctrlKey || e.metaKey ? "mod+" : "") + (e.shiftKey ? "shift+" : "")
        + e.key.toLowerCase();
      // Ctrl+C/Ctrl+V ficam FORA da tabela de `atalhosRef`, de propósito: ela
      // dá `preventDefault` incondicional em qualquer tecla que tenha função,
      // e Ctrl+C é também o atalho do navegador pra copiar texto selecionado
      // (mensagem de erro, label). Bloqueando aquele sempre que HÁ nó
      // selecionado no canvas, um Ctrl+C sobre texto de verdade nunca
      // funcionaria enquanto um card estivesse selecionado ao fundo. A
      // checagem de seleção de TEXTO vem antes de decidir o que fazer.
      if (nome === "mod+c") {
        const selecaoDeTexto = window.getSelection?.().toString();
        if (selecaoDeTexto) return;
        const sel = nodesRef.current.filter((n) => n.selected);
        if (!sel.length) return;
        e.preventDefault();
        copiar();
        return;
      }
      if (nome === "mod+v") {
        if (!clipboardRef.current) return;
        e.preventDefault();
        colar();
        return;
      }
      const fn = atalhosRef.current[nome];
      if (!fn) return;
      e.preventDefault();
      fn();
    };
    window.addEventListener("keydown", onKey);
    return () => window.removeEventListener("keydown", onKey);
  }, []);

  if (!catalog || !doc) return h("div", { className: "tr-loading" }, "carregando…");

  function menuItens(m) {
    if (m.kind === "aresta") {
      return [h("button", { key: "d", onClick: () => apagar([], [m.id]) }, "Apagar ligação")];
    }
    if (m.kind === "frame") {
      const f = nodes.find((n) => n.id === m.id);
      if (!f) return [];
      const fechar = (fn) => () => { fn(); setMenu(null); };
      return [
        h("button", { key: "rn", onClick: fechar(() => setEditFrame(m.id)) }, "Renomear"),
        // Trocar a proporção mantém a LARGURA e recalcula a altura.
        h("div", { key: "ar", className: "tr-menu-row", title: "proporção" },
          Object.keys(ASPECTS).map((a) => h("button", {
            key: a, className: a === f.data.aspect ? "tr-menu-on" : "",
            onClick: fechar(() => mudarProporcao(m.id, a)),
          }, a))),
        // Sem cor (ou com nome desconhecido) o frame é desenhado azul
        // (`FrameNode`), então é a amostra azul que aparece marcada.
        h("div", { key: "cr", className: "tr-menu-row", title: "cor" },
          FRAME_COLORS.map((c) => h("button", {
            key: c, title: c,
            className: `tr-swatch tr-frame-${c}` +
              (c === (FRAME_COLORS.includes(f.data.color) ? f.data.color : "azul") ? " tr-menu-on" : ""),
            onClick: fechar(() => onFrameEdit(m.id, { color: c })),
          }))),
        h("button", { key: "ex", disabled: exportando,
                      onClick: fechar(() => exportar(framesOrd.filter((x) => x.id === m.id))) },
          "Exportar PNG"),
        h("button", { key: "du", onClick: fechar(() => duplicar([m.id])) }, "Duplicar"),
        h("button", { key: "d", onClick: () => apagar([m.id]) }, "Apagar frame"),
      ];
    }
    if (m.kind === "nota") {
      const n = nodes.find((x) => x.id === m.id);
      if (!n) return [];
      const fechar = (fn) => () => { fn(); setMenu(null); };
      const itens = [
        h("div", { key: "cr", className: "tr-menu-row", title: "cor" },
          ["nenhuma", ...FRAME_COLORS].map((c) => h("button", {
            key: c, title: c,
            className: `tr-swatch ${c === "nenhuma" ? "tr-swatch-nenhuma" : `tr-frame-${c}`}` +
              (c === (n.data.color || "nenhuma") ? " tr-menu-on" : ""),
            onClick: fechar(() => onNotaEdit(m.id, { color: c })),
          }))),
        h("div", { key: "fu", className: "tr-menu-row", title: "fundo" },
          ["cartao", "nenhum"].map((f) => h("button", {
            key: f, className: f === n.data.fundo ? "tr-menu-on" : "",
            onClick: fechar(() => onNotaEdit(m.id, { fundo: f })),
          }, f))),
        h("div", { key: "es", className: "tr-menu-row", title: "escala" },
          ["nota", "letreiro"].map((e) => h("button", {
            key: e, className: e === n.data.escala ? "tr-menu-on" : "",
            onClick: fechar(() => onNotaEdit(m.id, { escala: e })),
          }, e))),
      ];
      if (n.data.kind === "imagem") {
        itens.push(h("div", { key: "ft", className: "tr-menu-row", title: "ajuste" },
          ["contain", "cover"].map((f) => h("button", {
            key: f, className: f === n.data.fit ? "tr-menu-on" : "",
            onClick: fechar(() => onNotaEdit(m.id, { fit: f })),
          }, f))));
      }
      itens.push(h("button", { key: "du", onClick: fechar(() => duplicar([m.id])) }, "Duplicar"));
      itens.push(h("button", { key: "d", onClick: () => apagar([m.id]) }, "Apagar bloco"));
      return itens;
    }
    const alvo = alvoMenu(m.id);
    // Restaurar age sobre a mesma seleção que Apagar — dois itens do mesmo
    // menu com alcances diferentes enganam. Frame fica de fora (ver
    // `restaurarTamanhos`).
    const frame = new Set(nodes.filter((n) => n.type === "trFrame" || n.type === "trNota").map((n) => n.id));
    const cards = alvo.filter((id) => !frame.has(id));
    // Agindo sobre a seleção, as ligações escolhidas vão junto — é o que a
    // tecla Delete faz com a mesma seleção, e o menu não pode apagar menos.
    const ligacoes = alvo.length > 1 ? edges.filter((e) => e.selected).map((e) => e.id) : [];
    return [
      h("button", { key: "du", onClick: () => { duplicar(alvo); setMenu(null); } },
        alvo.length > 1 ? `Duplicar ${alvo.length} selecionados` : "Duplicar"),
      h("button", { key: "d", onClick: () => apagar(alvo, ligacoes) },
        alvo.length > 1 ? `Apagar ${alvo.length} selecionados` : "Apagar bloco"),
      cards.length ? h("button", { key: "rs", onClick: () => {
        restaurarTamanhos(cards);
        setMenu(null);
      } }, cards.length > 1 ? `Restaurar tamanho (${cards.length})` : "Restaurar tamanho") : null,
    ];
  }

  const selecionados = nodes.filter((n) => n.selected);
  const vazio = nodes.length === 0 && !present;
  const primeiroBloco = (catalog.nodes || []).find((n) =>
    (n.inputs || []).length === 0 && (n.outputs || []).length > 0);

  // Arrastar um `.json` de flow em QUALQUER lugar da página — não só dentro
  // do diálogo já aberto — abre o diálogo de projeto com o arquivo já lido.
  // Sem isto, "arrastar pra importar" só funcionava depois de abrir o 📁
  // manualmente primeiro, um passo que ninguém adivinha sozinho.
  //
  // `dataTransfer.types.includes("Files")` distingue arquivo do SO do drag
  // interno de tipo de nó (`application/trama-type`, ver `onDrop` do
  // canvas): só o primeiro tem "Files", e só esse deve ter o padrão
  // (abrir o navegador) bloqueado — senão soltar um bloco da paleta fora do
  // canvas também dispararia isto à toa.
  const onDragOverGlobal = (e) => {
    if ((e.dataTransfer?.types || []).includes("Files")) e.preventDefault();
  };
  const onDropGlobal = (e) => {
    // Diálogo já aberto tem sua PRÓPRIA zona de importação (dentro da lista
    // de pastas, com `stopPropagation`); soltar fora dela (título, ações) não
    // deve reabrir a navegação do zero por baixo do que já está na tela.
    if (abrindo) return;
    const f = e.dataTransfer?.files?.[0];
    if (!f || !/\.json$/i.test(f.name)) return;
    e.preventDefault();
    const leitor = new FileReader();
    leitor.onload = () => {
      setArquivoSolto({ nomeArquivo: f.name, conteudo: leitor.result });
      setBanner(null); setListagem(null); setEnviando(null); setAbrindo(true);
      sendInput("tr_browse", { seq: ++seqCounter, path: projeto?.root || "." });
    };
    leitor.readAsText(f);
  };

  // O painel de frames usa a mesma coluna larga da Ajuda.
  // `tr-app-dialog` existe só para o banner: ele precisa passar à frente do
  // diálogo QUANDO há diálogo, e voltar para trás do menu de contexto quando
  // não há (ver `.tr-banner` no CSS).
  return h("div", { className: ["tr-app", helpFor || painelFrames || painelConfig ? "tr-app-help" : "",
                                present ? "tr-presenting" : "",
                                abrindo ? "tr-app-dialog" : ""].filter(Boolean).join(" "),
                    onDragOver: onDragOverGlobal, onDrop: onDropGlobal }, [
    h("div", { key: "canvas", className: "tr-canvas", ref: wrapRef,
               onDragOver: (e) => { e.preventDefault(); e.dataTransfer.dropEffect = "copy"; },
               onDrop },
      h(ReactFlow, {
        nodes: decorated, edges, nodeTypes,
        onNodesChange, onEdgesChange, onConnect, isValidConnection,
        onNodeDragStart, onSelectionStart, onSelectionEnd,
        onConnectStart, onConnectEnd: () => setDragType(null),
        onBeforeDelete,
        // Na apresentação o menu também some: ele traz "Apagar", e o slide é
        // somente leitura.
        onNodeContextMenu: (ev, n) => (present ? ev.preventDefault()
          : abrirMenu(ev, n.type === "trFrame" ? "frame" : n.type === "trNota" ? "nota" : "no", n.id)),
        onEdgeContextMenu: (ev, e) => (present ? ev.preventDefault() : abrirMenu(ev, "aresta", e.id)),
        onPaneClick: () => { setMenu(null); setMenuAcoes(false); }, onNodeClick: () => setMenu(null),
        onMoveStart: () => setMenu(null),
        // Gestos: arrastar no vazio (botão principal ou do meio, ou com espaço)
        // ANDA pela tela; Shift + arrasto desenha a caixa de seleção, e
        // Shift+clique soma à seleção. A roda dá zoom; Ctrl+roda rola a tela,
        // e isso não é do xyflow: é o listener nativo mais abaixo (ver
        // `onWheel`). Com o Shift apertado o xyflow tira o `panOnDrag` do
        // d3-zoom sozinho, por isso os dois gestos não brigam pelo mesmo arrasto.
        // Apresentação é somente leitura: nada arrasta, liga, seleciona nem apaga.
        nodesDraggable: !present, nodesConnectable: !present, elementsSelectable: !present,
        selectionOnDrag: false, selectionKeyCode: "Shift", selectionMode: SelectionMode.Partial,
        // O `Space` sai junto com o diálogo pelo mesmo motivo do `deleteKeyCode`
        // logo abaixo: o `useKeyPress` que o observa dá `preventDefault` no
        // keydown, e com isso o espaço deixava de ACIONAR a linha de pasta que
        // está com o foco — a tela atrás nem anda, está inerte.
        panOnDrag: present ? false : [0, 1],
        panActivationKeyCode: present || abrindo ? null : "Space",
        // Com o botão principal no `panOnDrag`, quem pega o mousedown no vazio
        // é o d3-zoom, e com a distância padrão (0) qualquer tremida de 1px
        // já conta como arrasto: o `onPaneClick` não vinha, e o menu não
        // fechava nem a seleção limpava. 4px de folga ainda é clique.
        paneClickDistance: 4,
        panOnScroll: false, zoomOnScroll: !present, zoomOnDoubleClick: !present,
        zoomOnPinch: !present,
        multiSelectionKeyCode: "Shift",
        // A guarda de atalhos do App não alcança esta tecla: o `useKeyPress` do
        // xyflow escuta no `document` por conta própria, e o guarda interno
        // dele só ignora `INPUT|SELECT|TEXTAREA|contenteditable|.nokey` — o
        // foco no BOTÃO de uma linha de pasta passa batido. Com o diálogo
        // aberto, o Backspace que em Finder/Explorer sobe uma pasta (logo, a
        // tecla que se TENTA) apagava a seleção atrás do overlay: os nós
        // sumiam, `remove_node` subia, o autosave gravava, e o Ctrl+Z que
        // consertaria está bloqueado pela mesma guarda. (O campo de nome
        // escapava por sorte: a versão embarcada do xyflow monta ESTE
        // `useKeyPress` com `actInsideInputWithModifier: false`, então
        // Shift+Backspace ali não age. Não é contrato nosso, e desligar a
        // tecla não depende disso.)
        deleteKeyCode: present || abrindo ? null : ["Delete", "Backspace"],
        // Sem isto um frame selecionado subiria por cima dos cards.
        elevateNodesOnSelect: false,
        fitView: true, fitViewOptions: { maxZoom: 1, padding: 0.25 },
        // `fitBounds` respeita o `maxZoom` do store (2, o padrão): um frame
        // pequeno parava no dobro e sobrava tela em volta do slide. O teto só
        // sobe durante a apresentação; editando, 2 continua sendo o limite do
        // zoom à mão. A troca chega a tempo do enquadramento: o `StoreUpdater`
        // do xyflow aplica a prop num efeito de um FILHO do `ReactFlow`, e
        // efeitos de filho rodam antes dos do `App` no mesmo commit. O piso
        // desce pelo mesmo motivo: com 0.2, frame maior que ~5 telas não cabia.
        minZoom: present ? 0.05 : 0.2, maxZoom: present ? 8 : 2,
        proOptions: { hideAttribution: true },
        // A roda fica com o navegador sobre um preview na apresentação: lá a
        // roda não anda nem dá zoom, e sem isto o xyflow a engoliria
        // (`preventDefault`) antes de a tabela longa poder rolar. Editando, a
        // roda sobre o card continua dando zoom na tela.
        noWheelClassName: present ? "tr-preview" : "nowheel",
      }, [
        h(Background, { key: "bg", variant: BackgroundVariant.Dots, gap: 16 }),
        h(Controls, { key: "ct" }),
        // Máscara e contorno são cromo do app e seguem o tema: o xyflow joga a
        // máscara numa variável CSS e o nó em `style.fill/stroke`, e os dois
        // aceitam `var()`. A cor do nó é dado (categoria), e o cinza de reserva
        // é o mesmo `#64748b` de categoria desconhecida no resto do editor.
        h(MiniMap, { key: "mm", maskColor: "var(--tr-shadow)", nodeStrokeWidth: 6,
          // Frame só como contorno: cheio, ele cobriria os cards do minimapa.
          nodeColor: (n) => (n.type === "trFrame" ? "transparent"
            : categories[n.data?.spec?.category]?.color || "#64748b"),
          nodeStrokeColor: (n) => (n.type === "trFrame" ? "var(--tr-dim)" : "transparent") }),
      ]),
      menu ? h("div", { key: "menu", className: "tr-menu",
                        style: { left: menu.x, top: menu.y } }, menuItens(menu)) : null,
      selecionados.length > 1 ? h("div", { key: "sel", className: "tr-selbar" }, [
        h("span", { key: "n", className: "tr-selbar-n" }, `${selecionados.length} selecionados`),
        // Só com algum card na seleção: com só frames (e ligações) os quatro
        // agiriam sobre nada, e botão que não faz nada é botão que mente.
        ...(selecionados.some((n) => n.type === "ndNode") ? [
          h("button", { key: "pv", title: "P", onClick: () => alternarFold("preview") }, "Preview"),
          h("button", { key: "pm", title: "O", onClick: () => alternarFold("params") }, "Parâmetros"),
          h("button", { key: "rs", title: "Shift+R", onClick: restaurarAlvos }, "Tamanho"),
          h("button", { key: "fr", title: "Ctrl+G", onClick: frameDaSelecao }, "Frame"),
        ] : []),
        h("button", { key: "del", title: "Delete",
                      // Mesmo alcance da tecla Delete: cards E ligações escolhidas.
                      onClick: () => apagar(selecionados.map((n) => n.id),
                                            edges.filter((e) => e.selected).map((e) => e.id)) },
          "Apagar"),
      ]) : null,
      ferramenta === "frame"
        ? h(FrameDraw, { key: "fd", toFlow: rf.screenToFlowPosition, onDone: criarFrame,
                         aspect: aspectoNovo }) : null,
      (ferramenta === "markdown" || ferramenta === "imagem")
        ? h(NotaDraw, { key: "nd", toFlow: rf.screenToFlowPosition,
                        onDone: (r) => criarNota(ferramenta, r) }) : null,
      // `key` muda a cada slide: remonta o indicador e reinicia o fade.
      // O número passa pelo mesmo teto do efeito que corrige `present.i`: no
      // render em que a lista encolhe, o efeito ainda não rodou, e o
      // indicador mostraria "3 / 2" por um quadro.
      present ? h("div", { key: `pi${present.i}`, className: "tr-present-ind" },
        `${Math.min(present.i, framesOrd.length - 1) + 1} / ${framesOrd.length}`) : null),
    vazio ? h("section", { key: "welcome", className: "tr-welcome", "aria-label": "Comece seu fluxo" }, [
      h("img", { key: "logo", src: MARCA, alt: "", className: "tr-welcome-mark" }),
      h("span", { key: "eyebrow", className: "tr-welcome-eyebrow" }, "Seu espaço de trabalho"),
      h("h1", { key: "title" }, "Comece com um bloco"),
      h("p", { key: "body" }, "Escolha uma fonte de dados, conecte outros blocos e acompanhe o resultado em cada etapa."),
      h("div", { key: "actions", className: "tr-welcome-actions" }, [
        primeiroBloco ? h("button", { key: "first", className: "tr-welcome-primary",
          onClick: () => addPicked(primeiroBloco.id) }, `Adicionar ${primeiroBloco.label.toLowerCase()}`) : null,
        h("button", { key: "browse", onClick: () => document.querySelector(".tr-search")?.focus() },
          "Explorar blocos →"),
      ]),
      h("div", { key: "steps", className: "tr-welcome-steps" }, [
        h("span", { key: "a" }, "1  Adicione um bloco"),
        h("span", { key: "b" }, "2  Configure os parâmetros"),
        h("span", { key: "c" }, "3  Conecte e veja o resultado"),
      ]),
    ]) : null,
    nodes.length === 1 && nodes[0].type === "ndNode" && !present
      ? h("div", { key: "next", className: "tr-next-step", role: "status" },
          "Agora configure o bloco. Depois, arraste de uma porta para conectar o próximo.") : null,
    helpFor
      ? h(Help, { key: "help", catalog, typeId: helpFor, onClose: () => setHelpFor(null) })
      : painelConfig
        ? h(SettingsPanel, { key: "cfg", temas: temas.temas, padrao: temas.tema_padrao,
            marca: temas.marca,
            // `seq` porque o input do Shiny ignora valor idêntico ao anterior:
            // voltar a um estado já enviado (desfazer uma cor à mão) não
            // chegaria ao servidor.
            onSave: (m) => sendInput("tr_themes", { temas: m.temas, tema_padrao: m.tema_padrao,
                                                    marca: m.marca, seq: Date.now() }),
            onClose: () => setPainelConfig(false) })
      : painelFrames
        ? h(FramePanel, { key: "frames", frames: framesOrd, exportando,
            onGo: (id) => { const f = framesOrd.find((x) => x.id === id); if (f) enquadrar(f); },
            onReorder: (ids) => pushOp({ op: "reorder_frames", frames: ids }),
            onRename: (id, title) => onFrameEdit(id, { title }), onAspect: mudarProporcao,
            onPresent: apresentar, onExport: () => exportar(framesOrd),
            onClose: () => setPainelFrames(false) })
        : h(Palette, { key: "pal", catalog, filterType: dragType, onPick: addPicked }),
    h("div", { key: "tb", className: "tr-toolbar" }, [
      // `img`, e não botão: a marca é assinatura, não controle. Não clica, não
      // abre nada e — por não ser elemento focável — não entra na ordem de
      // tabulação, então quem navega pelo teclado cai direto no "⇶ Organizar".
      // A versão só existe do lado do R; ela chega aqui pelo `data-versao` que
      // `tr_ui()` põe na raiz, e some da dica se por algum motivo não vier.
      h("img", { key: "marca", className: "tr-marca", src: MARCA, alt: "trama",
                 draggable: false,
                 title: `trama ${document.getElementById("tr-root")?.dataset.versao || ""}`.trim() }),
      h("div", { key: "tools", className: "tr-toolbar-tools", role: "group", "aria-label": "Ferramentas" }, [
      h("button", { key: "l", onClick: organizarTudo }, "⇶ Organizar"),
      h("button", { key: "f", title: "F", className: ferramenta === "frame" ? "tr-on" : "",
                    onClick: () => setFerramenta((t) => (t === "frame" ? null : "frame")) },
        "▭ Frame"),
      h("button", { key: "m", title: "M", className: ferramenta === "markdown" ? "tr-on" : "",
                    onClick: () => setFerramenta((t) => (t === "markdown" ? null : "markdown")) },
        "▤ Markdown"),
      h("button", { key: "i", title: "I", className: ferramenta === "imagem" ? "tr-on" : "",
                    onClick: () => setFerramenta((t) => (t === "imagem" ? null : "imagem")) },
        "▥ Imagem"),
      // Segmentado, e não `<select>`: as cinco proporções cabem à vista e
      // trocam num clique. O botão que fica com o foco não prende o teclado —
      // o listener de atalhos só ignora campos de texto e `<select>`, então o
      // F seguinte ainda abre a ferramenta, e dígito nenhum troca a proporção
      // por busca por letra.
      h(Segmented, { key: "fa", options: Object.keys(ASPECTS), value: aspectoNovo,
                     onChange: setAspectoNovo, title: "proporção dos frames novos" }),
      h("button", { key: "pr", className: prancheta ? "tr-on" : "",
                    title: "grade de frames de uma vez", "data-prancheta": "",
                    onClick: () => setPrancheta((v) => v ? null
                      : { ...pranchetaSalva(), aspect: aspectoNovo }) }, "⊞ Prancheta"),
      h("button", { key: "fp", className: painelFrames ? "tr-on" : "",
                    onClick: () => { setHelpFor(null); setPainelConfig(false);
                                     setPainelFrames((v) => !v); } }, "▦ Frames"),
      ]),
      h("button", { key: "more", className: "tr-toolbar-more", title: "Mais ações",
        "aria-label": "Mais ações", "aria-expanded": menuAcoes,
        onClick: () => setMenuAcoes((v) => !v) }, "⋯"),
      menuAcoes ? h("div", { key: "actions", className: "tr-toolbar-actions",
        onClick: () => setMenuAcoes(false) }, [
      h("button", { key: "cfg", title: "configurações", className: painelConfig ? "tr-on" : "",
                    onClick: () => { setHelpFor(null); setPainelFrames(false);
                                     setPainelConfig((v) => !v); setMenuAcoes(false); } }, "⚙ Configurações"),
      h("button", { key: "u", onClick: desfazer, title: "Ctrl+Z" }, "↶ Desfazer"),
      h("button", { key: "r", onClick: () => sendInput("tr_rerun", Date.now()) }, "↻ Recalcular"),
      h("button", { key: "ex-flow", disabled: !doc,
                    onClick: () => exportFlowJson(doc,
                      `${(projeto?.root || "flow").split("/").filter(Boolean).pop()}-${projeto?.flow || "main"}.json`) },
        "⇩ Exportar flow"),
      h("button", { key: "ex-r", disabled: !doc,
                    onClick: () => sendInput("tr_export_code", { format: "r", seq: ++seqCounter }) },
        "⇩ Exportar R"),
      h("button", { key: "ex-qmd", disabled: !doc,
                    onClick: () => sendInput("tr_export_code", { format: "quarto", seq: ++seqCounter }) },
        "⇩ Exportar Quarto"),
      ]) : null,
      // Embrulhado: o segmentado da toolbar nasce colado no botão anterior (o da
      // proporção pertence ao "▭ Frame"), e o do tema é um grupo à parte.
      h("div", { key: "ta", className: "tr-toolbar-tema" },
        h(Segmented, { value: temaApp, onChange: setTemaApp, title: "tema do app",
                       options: [{ value: "claro", label: "☀", title: "tema claro" },
                                 { value: "sistema", label: "◐", title: "seguir o sistema" },
                                 { value: "escuro", label: "☾", title: "tema escuro" }] })),
      // Só o nome da pasta cabe na barra; o caminho inteiro fica na dica. Até
      // aqui nada na tela respondia "em que projeto eu estou" — o título da
      // janela é do Shiny e o canvas não diz de onde o grafo veio.
      // A navegação começa onde o projeto está, não na pasta de trabalho do R:
      // quem troca de projeto quase sempre vai para uma vizinha.
      // Limpar `listagem` ao abrir: ela é estado do App e sobrevive ao
      // fechamento, então o diálogo pintava NA HORA o caminho e as pastas da
      // navegação anterior — linhas clicáveis, e "Abrir" podendo estar
      // habilitado para uma pasta que o usuário não escolheu — até o
      // `tr_browse` da raiz responder. Com `null`, o componente mostra "…",
      // que é a verdade. O banner some pelo mesmo motivo: a recusa da sessão
      // passada não fala do que está na tela agora.
      // A raiz cai em `projeto.root` quando ele é "/" — `filter(Boolean).pop()`
      // devolve `undefined` numa string só de barras, e o rótulo virava
      // "📁 undefined". Nome comprido é aparado pelo CSS, não aqui.
      h("button", { key: "pj", className: "tr-toolbar-proj",
                    title: projeto ? projeto.root : "projeto",
                    onClick: () => { setBanner(null); setListagem(null); setEnviando(null);
                                     // Abrir pelo clique é gesto NOVO: um arquivo solto numa
                                     // visita anterior não pode reaparecer pré-carregado aqui.
                                     setArquivoSolto(null);
                                     setAbrindo(true);
                                     sendInput("tr_browse", { seq: ++seqCounter,
                                                              path: projeto?.root || "." }); } },
        `📁 ${projeto ? (projeto.root.split("/").filter(Boolean).pop() || projeto.root)
                      : "projeto"}`),
    ]),
    // Irmão da toolbar, no mesmo `.tr-app`: o CSS o põe logo abaixo dela.
    // `inicial` só é lido ao montar; é o retrato tirado ao abrir, com a
    // proporção da toolbar daquele momento.
    prancheta ? h(PranchetaPopover, { key: "prancheta",
      inicial: prancheta,
      existentes: nodes.filter((n) => n.type === "trFrame").length,
      onCriar: criarPrancheta, onFechar: fecharPrancheta }) : null,
    banner ? h("div", { key: "bn", className: "tr-banner", onClick: () => setBanner(null) },
      banner) : null,
    abrindo ? h(ProjectDialog, { key: "pd", listagem, atual: projeto?.root, enviando, arquivoInicial: arquivoSolto,
      onBrowse: (p) => sendInput("tr_browse", { seq: ++seqCounter, path: p }),
      onOpen: (p) => { setEnviando("abrir");
                       sendInput("tr_project_open", { seq: ++seqCounter, path: p }); },
      onNew: (p, nome) => { setEnviando("criar");
                            sendInput("tr_project_new", { seq: ++seqCounter, path: p, nome }); },
      onImport: (p, nome, conteudo) => { setEnviando("importar");
                            sendInput("tr_project_import", { seq: ++seqCounter, path: p, nome, conteudo }); },
      onClose: fecharDialogo }) : null,
    // Só aparece quando o pendente do conflito ainda existe — servidor lento
    // ou uma segunda resposta perdida não deixam o diálogo preso sem ação
    // nenhuma fazer sentido.
    conflitoUpload && uploadsPendentesRef.current[conflitoUpload.id]
      ? h(UploadConflictDialog, { key: "cu", nome: conflitoUpload.nome,
          onOverwrite: () => {
            const pend = uploadsPendentesRef.current[conflitoUpload.id];
            sendInput("tr_data_upload", { seq: ++seqCounter, id: conflitoUpload.id,
                                          nome: pend.nome, conteudo: pend.conteudo, overwrite: true });
            setConflitoUpload(null);
          },
          onRename: (novoNome) => {
            const pend = uploadsPendentesRef.current[conflitoUpload.id];
            pend.nome = novoNome;
            sendInput("tr_data_upload", { seq: ++seqCounter, id: conflitoUpload.id,
                                          nome: novoNome, conteudo: pend.conteudo, overwrite: false });
            setConflitoUpload(null);
          },
          onCancel: () => {
            delete uploadsPendentesRef.current[conflitoUpload.id];
            setConflitoUpload(null);
          } })
      : null,
  ]);
}

createRoot(document.getElementById("tr-root"))
  .render(h(ReactFlowProvider, null, h(App)));
