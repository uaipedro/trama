// inst/www/editor.js — o editor. Dirigido por catálogo: não conhece nenhum
// tipo de nó, nenhum tipo de dado, nenhuma categoria. Tudo — portas, cores,
// widgets de param, renderers de preview — vem do catálogo e dos registros do
// runtime. É a propriedade do insumo que mais se provou, e a única herdada
// sem repensar.
//
// Sem bundler, sem JSX: `React.createElement` direto. O custo é a verbosidade;
// o ganho é que uma coleção nova é um `.js` solto, sem toolchain.

import React, { Fragment, createContext, useCallback, useContext, useEffect, useMemo, useRef, useState } from "react";
import { createRoot } from "react-dom/client";
import {
  ReactFlow, Background, BackgroundVariant, MiniMap, Controls,
  Handle, Position, applyNodeChanges, applyEdgeChanges, SelectionMode,
  useReactFlow, ReactFlowProvider,
  BaseEdge, getSmoothStepPath, useInternalNode, EdgeLabelRenderer,
} from "@xyflow/react";
import { h, getRenderer, getViews, Segmented, setThemes } from "trama";
import { FrameNode, FrameDraw, ASPECTS, FRAME_COLORS, ratioOf, rectOf, inside,
         containedCards, containedFrames, containedNotes, fitAspect, FramePanel, exportFramePng,
         dagrePos, organizar, PranchetaPopover, gradeDeFrames, PRANCHETA_PADRAO,
         MARCA } from "./frames.js";
import { NotaNode, NotaDraw } from "./notas.js";
import { SettingsPanel } from "./settings.js";
import { contagemDoPasso } from "./params.js";
import { MODOS, modoDe, mostraPreview, mostraParams, precisaPainel, nomeDaTecla, dica,
         frameVizinho } from "./modos.js";
import { ModoPicker, ParamsList, ParamsDock, Vista, AtalhosPanel } from "./modos-ui.js";
import { corDaCategoria, tintaDaCategoria } from "./papeis.js";
import { Proximo, vaoAoLado, vaoPerto, alturaNova } from "./proximo.js";
import { registrar, lerHistorico } from "./historico.js";
import { sugerir } from "./sugestor.js";

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
              modo: (doc.ui && doc.ui.modes && doc.ui.modes[id]) || null },
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

function Preview({ state, handle, error, progress, partial, view, label }) {
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
        [bar, v ? h(v.component, { key: "p", artifact: handle.preview, handle, assetUrl, label }) : null]);
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
    h(v.component, { artifact: art, handle, assetUrl, label }));
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
// Quanto um insert encadeado espera, na fila, a origem voltar do servidor.
const FILA_PROX_PRAZO = 10000;
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
// Sem preview (modo `params`) a alça só mexe na largura: a altura do card vem
// da lista de parâmetros, e `hGuardada` é a altura de preview que o documento
// já tinha, reenviada intacta pra ela sobreviver à volta ao modo completo.
function Grip({ nodeId, onResize, hGuardada }) {
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
    if (!card) return;
    const pv = card.querySelector(".tr-preview");
    // Captura de ponteiro: sem ela, soltar o botão FORA da janela nunca entrega
    // o `pointerup` e os listeners ficariam pendurados, arrastando o card
    // sozinho no próximo movimento do mouse.
    try { ev.currentTarget.setPointerCapture(ev.pointerId); } catch (_) {}
    const x0 = ev.clientX, y0 = ev.clientY;
    const w0 = card.offsetWidth, h0 = pv ? pv.offsetHeight : (hGuardada || MIN_H);
    // Lido uma vez, no começo: o zoom não muda no meio de um arrasto, e reler
    // por quadro só daria a chance de o card pular se mudasse.
    const z = rf.getZoom() || 1;
    let w = w0, hgt = h0;
    const move = (e) => {
      w = Math.max(MIN_W, snap(w0 + (e.clientX - x0) / z));
      card.style.setProperty("--tr-w", `${w}px`);
      if (!pv) return;
      hgt = Math.max(MIN_H, snap(h0 + (e.clientY - y0) / z));
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
        if (pv) card.style.setProperty("--tr-h", `${h0}px`);
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
  return h("div", { className: "tr-grip nodrag" + (hGuardada === undefined ? "" : " tr-grip-w"),
                    title: hGuardada === undefined ? "redimensionar" : "ajustar largura",
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
  const modo = modoDe(data);
  const mini = modo === "mini";
  const semPreview = !mostraPreview(modo), semParams = !mostraParams(modo);
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
              data.emRegiao ? "tr-node-region" : "", mini ? "tr-node-mini" : ""]
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
    "--tr-w": w ? `${w}px` : undefined, "--tr-h": hgt ? `${hgt}px` : undefined,
    // O mini pinta só o bloco do ícone com a cor da categoria; o resto é a
    // placa do card.
    "--tr-cat": corDaCategoria(cat, spec), "--tr-cat-ink": tintaDaCategoria(cat, spec) } }, [
    h("div", { key: "hd", className: "tr-node-head",
               style: { background: corDaCategoria(cat, spec), color: tintaDaCategoria(cat, spec) } }, [
      // Sem `color`: aqui o ícone herda a cor de FRENTE da faixa, porque a
      // faixa já É a cor da categoria (o background logo acima). Mesmo
      // componente, contexto invertido.
      spec.icon ? h(Icon, { key: "i", icon: spec.icon, className: "tr-node-icon tr-node-icon-main" }) : null,
      h("span", { key: "l", className: "tr-node-title" }, data.label || spec.label),
      // Mini não tem preview pra denunciar falha: a bolinha de estado, logo
      // depois do título, é quem fala por ele.
      mini
        ? h("span", { key: "dot", className: `tr-mini-dot tr-${data.state || "idle"}`,
                      title: data.error?.message || data.state || "" })
        : null,
      // Com o preview recolhido (e fora do mini, que já tem a bolinha acima),
      // o estado de execução muda pro cabeçalho: um preview escondido não pode
      // esconder uma falha. O preview NÃO reabre sozinho, porque isso desfaria
      // a arrumação que o usuário escolheu.
      !mini && semPreview && falhou
        ? h("span", { key: "al", className: "tr-head-alert",
                      title: data.error?.message || "falhou" },
            h(Icon, { icon: { kind: "set", value: "triangle-alert" }, className: "tr-node-icon" }))
        : null,
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
      !mini && semPreview && data.state === "running"
        ? h("div", { key: "hp", className: "tr-head-progress" + (frac == null ? " tr-indet" : "") },
            h("div", { style: frac == null ? undefined : { width: `${Math.round(frac * 100)}%` } }))
        : null,
      // Sempre visível, na ponta direita — onde ficavam os botões de fold. O
      // `?` de ajuda saiu do cabeçalho junto com eles: a ajuda do bloco agora
      // é o H (Fase 2.3).
      h(ModoPicker, { key: "md", value: modo, onChange: (m) => data.onModo(id, m) }),
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
                 progress: data.progress, partial: data.partial, view: cur?.id,
                 label: data.label || spec.label }),
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
    // Sem parâmetros no card (modo mini ou preview): não há mais pílula pra
    // reabri-los ali — quem escondeu os parâmetros edita pelo painel à
    // esquerda (Fase 3). Aqui só resta decidir se desenha a lista ou nada.
    // O corpo da lista (o porquê do `div` em vez de `label`, o Fragment com
    // `key`, o fallback de `input` sem widget) mora em `ParamsList`
    // (modos-ui.js), reaproveitado aqui e no `ParamsDock`.
    semParams ? null
      : h(ParamsList, { key: "pm", id, spec, params: data.params, onParam: data.onParam }),
    h("div", { key: "po", className: "tr-ports" }, [
      h("div", { key: "in", className: "tr-in" }, (spec.inputs || []).map((p) =>
        h("div", { key: p.name, className: "tr-port" }, [
          h(Handle, { key: "h", type: "target", position: Position.Left, id: p.name,
                      style: { "--porta-cor": data.typeColors?.[p.type] || "#64748b" } }),
          mini ? null : h("span", { key: "n", title: p.type },
            p.name + (p.multiple ? " (N)" : "") + (p.required ? "" : "?")),
        ]))),
      h("div", { key: "out", className: "tr-out" }, (spec.outputs || []).map((p) =>
        h("div", { key: p.name, className: "tr-port tr-port-out" }, [
          mini ? null : h("span", { key: "n", title: p.type }, p.name),
          h(Handle, { key: "h", type: "source", position: Position.Right, id: p.name,
                      style: { "--porta-cor": data.typeColors?.[p.type] || "#64748b" } }),
          // "+" do próximo bloco: some no mini (e na apresentação, pelo CSS).
          mini || !data.onAbrirProximo ? null : h("button", {
            key: "mais", className: "tr-prox-mais nodrag nopan", type: "button",
            title: "Próximo bloco", "aria-label": `Próximo bloco a partir de ${p.name}`,
            onPointerDown: (e) => e.stopPropagation(),
            onClick: (e) => {
              e.stopPropagation();
              const r = e.currentTarget.getBoundingClientRect();
              data.onAbrirProximo(id, p.name, r.right + 6, r.top);
            } }, "+"),
        ]))),
    ]),
    // O mini tem largura automática, então fica sem alça.
    mini ? null : h(Grip, { key: "gr", nodeId: id, onResize: data.onResize,
                            hGuardada: semPreview ? (hgt || MIN_H) : undefined }),
  ]);
}

const nodeTypes = { ndNode: NdNode, trFrame: FrameNode, trNota: NotaNode };

// As portas (`Handle`) ficam sempre declaradas Left/Right — o PONTO e o LADO
// de entrada/saída da aresta nunca mudam aqui, só a rota até lá. O problema
// que esta aresta resolve: o smoothstep padrão do xyflow não sabe onde os
// cards estão — ele calcula a curva só a partir dos dois pontos de porta, e
// quando o card vizinho fica no meio do caminho (por exemplo dois cards
// empilhados, saída à direita de um entrando pela esquerda do outro), a
// curva "reta" atravessa o corpo do card pra alinhar os eixos.
//
// Aqui, se sobra um vão livre de verdade entre os dois retângulos no eixo
// PERPENDICULAR ao das portas (ex.: espaço vertical entre dois cards com
// portas Left/Right), a curva sai reto da porta, contorna por FORA dos dois
// retângulos (com folga `MARGEM_ARESTA`) atravessando só esse vão livre, e
// entra reto pelo mesmo lado de sempre no destino. Sem vão livre claro —
// cards lado a lado, o caso comum — cai de volta no smoothstep padrão, que
// já não tem obstáculo pra cruzar.
const MARGEM_ARESTA = 16;

function pontoParaFora(pos, x, y, d) {
  switch (pos) {
    case Position.Left: return { x: x - d, y };
    case Position.Right: return { x: x + d, y };
    case Position.Top: return { x, y: y - d };
    case Position.Bottom: return { x, y: y + d };
    default: return { x, y };
  }
}

function retanguloDoNo(n) {
  const w = n.measured?.width ?? n.width ?? 0;
  const h = n.measured?.height ?? n.height ?? 0;
  const { x, y } = n.internals.positionAbsolute;
  return { x1: x, y1: y, x2: x + w, y2: y + h };
}

// Uma "L"/"Z" ortogonal: sai da porta, atravessa o vão livre entre os dois
// cards por fora dos dois retângulos, entra na porta de destino. `null`
// quando não há vão livre claro nesse eixo — quem chama cai no smoothstep
// padrão.
function caminhoContornando(sourceX, sourceY, sourcePosition,
                             targetX, targetY, targetPosition,
                             origem, destino) {
  const eixoHorizontal = sourcePosition === Position.Left || sourcePosition === Position.Right;
  const sFora = pontoParaFora(sourcePosition, sourceX, sourceY, MARGEM_ARESTA);
  const tFora = pontoParaFora(targetPosition, targetX, targetY, MARGEM_ARESTA);

  if (eixoHorizontal) {
    // Vão livre no eixo Y (um card empilhado sobre o outro).
    let vaoTopo, vaoBase;
    if (destino.y1 - origem.y2 >= MARGEM_ARESTA) { vaoTopo = origem.y2; vaoBase = destino.y1; }
    else if (origem.y1 - destino.y2 >= MARGEM_ARESTA) { vaoTopo = destino.y2; vaoBase = origem.y1; }
    else return null;
    const meioY = (vaoTopo + vaoBase) / 2;
    return [
      { x: sourceX, y: sourceY }, { x: sFora.x, y: sourceY },
      { x: sFora.x, y: meioY }, { x: tFora.x, y: meioY },
      { x: tFora.x, y: targetY }, { x: targetX, y: targetY },
    ];
  }
  // Vão livre no eixo X (dois cards lado a lado, portas Top/Bottom).
  let vaoEsq, vaoDir;
  if (destino.x1 - origem.x2 >= MARGEM_ARESTA) { vaoEsq = origem.x2; vaoDir = destino.x1; }
  else if (origem.x1 - destino.x2 >= MARGEM_ARESTA) { vaoEsq = destino.x2; vaoDir = origem.x1; }
  else return null;
  const meioX = (vaoEsq + vaoDir) / 2;
  return [
    { x: sourceX, y: sourceY }, { x: sourceX, y: sFora.y },
    { x: meioX, y: sFora.y }, { x: meioX, y: tFora.y },
    { x: targetX, y: tFora.y }, { x: targetX, y: targetY },
  ];
}

// Cantos arredondados na polilinha, no mesmo espírito do `borderRadius` do
// xyflow: cada vértice interno vira um arco curto (`Q`) em vez de bico.
function caminhoComCantos(pontos, raio) {
  let d = `M${pontos[0].x},${pontos[0].y}`;
  for (let i = 1; i < pontos.length - 1; i++) {
    const p0 = pontos[i - 1], p1 = pontos[i], p2 = pontos[i + 1];
    const l1x = p0.x - p1.x, l1y = p0.y - p1.y, len1 = Math.hypot(l1x, l1y);
    const l2x = p2.x - p1.x, l2y = p2.y - p1.y, len2 = Math.hypot(l2x, l2y);
    const r = Math.min(raio, len1 / 2, len2 / 2);
    if (r <= 0 || len1 === 0 || len2 === 0) { d += `L${p1.x},${p1.y}`; continue; }
    const a = { x: p1.x + (l1x / len1) * r, y: p1.y + (l1y / len1) * r };
    const b = { x: p1.x + (l2x / len2) * r, y: p1.y + (l2y / len2) * r };
    d += `L${a.x},${a.y}Q${p1.x},${p1.y} ${b.x},${b.y}`;
  }
  const fim = pontos[pontos.length - 1];
  return d + `L${fim.x},${fim.y}`;
}

// Quem abre o popover de "inserir no meio": o App, que é dono do `prox`. Vazio
// (apresentação, ou antes de montar) deixa a aresta sem o "+".
const MeioCtx = createContext(null);

// Ponto a meio comprimento de uma poligonal: onde o "+" do meio fica.
function meioDaLinha(pontos) {
  const seg = pontos.slice(1).map((p, i) => Math.hypot(p.x - pontos[i].x, p.y - pontos[i].y));
  let resta = seg.reduce((a, b) => a + b, 0) / 2;
  for (let i = 0; i < seg.length; i++) {
    if (resta <= seg[i] && seg[i] > 0) {
      const a = pontos[i], b = pontos[i + 1], t = resta / seg[i];
      return [a.x + (b.x - a.x) * t, a.y + (b.y - a.y) * t];
    }
    resta -= seg[i];
  }
  return [pontos[0].x, pontos[0].y];
}

function TrAresta({ id, source, target, sourceX, sourceY, sourcePosition,
                     targetX, targetY, targetPosition, style, markerEnd,
                     sourceHandleId, targetHandleId, data, selected }) {
  const noOrigem = useInternalNode(source);
  const noDestino = useInternalNode(target);
  const abrirMeio = useContext(MeioCtx);
  const [sobre, setSobre] = useState(false);
  const sair = useRef(null);
  const entra = () => { clearTimeout(sair.current); setSobre(true); };
  // Com folga: o mouse precisa atravessar do fio até o "+" sem ele sumir.
  const sai = () => { clearTimeout(sair.current); sair.current = setTimeout(() => setSobre(false), 250); };
  useEffect(() => () => clearTimeout(sair.current), []);
  let [, meioX, meioY] = getSmoothStepPath({
    sourceX, sourceY, sourcePosition, targetX, targetY, targetPosition, borderRadius: 12 });
  let path;
  if (noOrigem && noDestino) {
    const pontos = caminhoContornando(sourceX, sourceY, sourcePosition,
      targetX, targetY, targetPosition,
      retanguloDoNo(noOrigem), retanguloDoNo(noDestino));
    if (pontos) {
      path = caminhoComCantos(pontos, 12);
      [meioX, meioY] = meioDaLinha(pontos);
    }
  }
  if (!path) {
    [path] = getSmoothStepPath({
      sourceX, sourceY, sourcePosition, targetX, targetY, targetPosition,
      borderRadius: 12,
    });
  }
  // Trilho largo e translúcido por baixo do fio (só visual, sem clique). Destino
  // falho deixa o fio tracejado em vermelho: dá pra ver onde a trama quebrou sem
  // abrir o card.
  const quebrou = ["failed", "invalid"].includes(noDestino?.data?.state);
  return h(Fragment, null, [
    h("g", { key: "g", onMouseEnter: entra, onMouseLeave: sai }, [
      h("path", { key: "t", d: path, className: "tr-fio-trilho" }),
      h(BaseEdge, { key: "f", id, path, style, markerEnd,
                    className: quebrou ? "tr-fio-quebrado" : undefined }),
    ]),
    abrirMeio && (sobre || selected) ? h(EdgeLabelRenderer, { key: "m" },
      h("button", {
        className: "tr-meio-mais nodrag nopan", type: "button",
        title: "Inserir bloco no meio", "aria-label": "Inserir bloco no meio da conexão",
        style: { transform: `translate(-50%, -50%) translate(${meioX}px, ${meioY}px)` },
        onMouseEnter: entra, onMouseLeave: sai,
        onPointerDown: (e) => e.stopPropagation(),
        onClick: (e) => {
          e.stopPropagation();
          const r = e.currentTarget.getBoundingClientRect();
          abrirMeio({ source, sourceHandle: sourceHandleId, target, targetHandle: targetHandleId,
                      index: data?.index }, r.right + 6, r.top);
        } }, "+")) : null,
  ]);
}

const edgeTypes = { trAresta: TrAresta };

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
function Palette({ catalog, filterType, dragFrom, onPick, modoNovo, onModoNovo }) {
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

  // Arrastando de uma porta com origem conhecida, a ordem é a do sugestor e
  // os 5 primeiros com pontuação ganham a marca; sem origem, alfabética.
  const { achados, marcados } = useMemo(() => {
    const alfa = [...hits].sort((a, b) => (a.label || a.id).localeCompare(b.label || b.id, "pt"));
    if (!filterType || !dragFrom) return { achados: alfa, marcados: new Set() };
    const r = sugerir(catalog, { de: dragFrom, tipo: filterType, historico: lerHistorico() });
    const pos = Object.fromEntries(r.map((s, i) => [s.id, i]));
    const achados = alfa.map((n, i) => [n, pos[n.id] ?? 1e6 + i]).sort((a, b) => a[1] - b[1]).map((x) => x[0]);
    const vivos = new Set(achados.map((n) => n.id));
    return { achados, marcados: new Set(r.filter((s) => s.score > 0 && vivos.has(s.id)).slice(0, 5).map((s) => s.id)) };
  }, [hits, catalog, filterType, dragFrom]);

  const rotulo = (id) => cols.find((c) => c.id === id)?.label || id;

  const catColor = (n) =>
    corDaCategoria((catalog.categories || []).find((c) => c.id === n.category), n);

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
    marcados.has(n.id) ? h("span", { key: "sg", className: "tr-palette-sug", title: "sugerido", role: "img", "aria-label": "sugerido" }) : null,
    selo ? h("span", { key: "s", className: "tr-palette-seal" }, selo) : null,
  ]);

  return h("aside", { className: "tr-palette" }, [
    h("div", { key: "head", className: "tr-palette-head" }, [
      h("strong", { key: "title" }, "Blocos"),
      h("span", { key: "hint" }, "Clique para adicionar ou arraste para a tela"),
      h("div", { key: "modo", className: "tr-palette-modo" }, [
        h("span", { key: "l" }, "entra como"),
        h(ModoPicker, { key: "p", value: modoNovo, onChange: onModoNovo }),
      ]),
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
                h("i", { key: "d", style: { background: corDaCategoria(meta) } }),
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

const PAPEIS_REF = [["teoria", "Teoria"], ["livro-texto", "Livro-texto"],
                    ["implementacao", "Implementação"], ["complementar", "Complementar"]];

// Referência em autor-data: `Autores (ano). Título. Fonte.` + DOI ou URL.
// Implementação mostra `pacote::funcao()` e a versão do pacote.
function Referencia({ r }) {
  const partes = [];
  if (r.papel === "implementacao" && r.pacote) {
    partes.push(h("code", { key: "f", className: "tr-help-ref-fn" },
      // Sem função declarada, só o pacote (nada de `pacote::` pendurado).
      r.funcao ? `${r.pacote}::${r.funcao}()` : r.pacote));
    if (r.versao) partes.push(h("span", { key: "v", className: "tr-help-ref-ver" }, ` versão ${r.versao}`));
    if (r.autores?.length || r.titulo) partes.push(h("br", { key: "br" }));
  }
  const cab = [];
  if (r.autores?.length) cab.push(r.autores.join("; "));
  if (r.ano) cab.push(`(${r.ano}).`);
  else if (cab.length) cab[cab.length - 1] += ".";
  if (cab.length) partes.push(cab.join(" ") + " ");
  if (r.titulo) partes.push(h("em", { key: "t" }, r.titulo.replace(/\.$/, "")), ". ");
  if (r.fonte) partes.push(r.fonte.replace(/\.$/, "") + ". ");
  // Defesa em profundidade: só vira link URL https; o resto aparece como texto.
  const urlOk = typeof r.url === "string" && /^https:\/\//i.test(r.url);
  const href = r.doi ? `https://doi.org/${encodeURI(r.doi)}` : (urlOk ? r.url : null);
  if (href) partes.push(h("a", { key: "a", href, target: "_blank", rel: "noopener" },
                          r.doi ? `doi:${r.doi}` : r.url));
  else if (r.url) partes.push(h("span", { key: "a" }, r.url));
  if (r.nota) partes.push(h("div", { key: "n", className: "tr-help-ref-nota" }, r.nota));
  return h("li", { className: "tr-help-ref" }, partes);
}

// Normaliza espaços para comparar descrição e ajuda sem tropeçar em quebras.
const normEsp = (t) => String(t || "").replace(/\s+/g, " ").trim();

// A descrição repete a ajuda quando o primeiro parágrafo do markdown (depois de
// um "## Descrição" opcional) começa pelo mesmo texto.
function descricaoRepete(desc, help) {
  if (!desc || !help) return false;
  const corpo = help.replace(/^\s*##\s*Descri[çc][ãa]o\s*\n/i, "").trimStart();
  const par = normEsp(corpo.split(/\n\s*\n/)[0]);
  const d = normEsp(desc).replace(/\.$/, "");
  return d.length > 0 && par.startsWith(d);
}

function Help({ catalog, typeId, onClose, onOpen }) {
  const spec = (catalog.nodes || []).find((n) => n.id === typeId);
  const titulo = useRef(null);
  const veioDeChip = useRef(false);
  // Depois que um chip troca o painel, o foco vai para o título da nova ajuda.
  useEffect(() => {
    if (veioDeChip.current && titulo.current) titulo.current.focus();
    veioDeChip.current = false;
  }, [typeId]);
  if (!spec) return null;
  const nos = catalog.nodes || [];
  const press = spec.pressupostos || [];
  const refs = spec.referencias || [];
  const chip = (id) => {
    const alvo = nos.find((n) => n.id === id);
    // Id fora do catálogo: nada para abrir, então texto cru e apagado, não botão.
    if (!alvo) return h("span", { key: id, className: "tr-help-chip tr-help-chip-off", title: id }, id);
    return h("button", { key: id, type: "button", className: "tr-help-chip", title: id,
                         onClick: () => { veioDeChip.current = true; onOpen && onOpen(id); } }, [
      alvo.icon && ICON_KINDS.has(alvo.icon.kind)
        ? h(Icon, { key: "i", icon: alvo.icon, className: "tr-palette-icon" }) : null,
      h("span", { key: "l" }, alvo.label || id),
    ]);
  };
  const secPress = press.length ? h("section", { key: "pr", className: "tr-help-sec" }, [
    h("h4", { key: "t" }, "Pressupostos"),
    h("ul", { key: "l", className: "tr-help-press" }, press.map((p, i) =>
      h("li", { key: i }, [
        h(Icon, { key: "ic", icon: { kind: "set", value: "circle-check" }, className: "tr-help-press-ic" }),
        h("div", { key: "c" }, [
          h("div", { key: "tx" }, mdInline(p.texto || "")),
          p.verificar?.length ? h("div", { key: "v", className: "tr-help-verif" },
            [h("span", { key: "r", className: "tr-help-rot" }, "Verificar:"), ...p.verificar.map(chip)]) : null,
          p.se_falhar ? h("div", { key: "f", className: "tr-help-falha" },
            [h("span", { key: "r", className: "tr-help-rot" }, "Se falhar: "), mdInline(p.se_falhar)]) : null,
        ]),
      ]))),
  ]) : null;
  const secRefs = refs.length ? h("section", { key: "rf", className: "tr-help-sec" }, [
    h("h4", { key: "t" }, "Referências"),
    ...PAPEIS_REF.map(([papel, rot]) => {
      const grupo = refs.filter((r) => r.papel === papel);
      return grupo.length ? h("div", { key: papel, className: "tr-help-refgrp" }, [
        h("h5", { key: "t" }, rot),
        h("ul", { key: "l" }, grupo.map((r, i) => h(Referencia, { key: i, r }))),
      ]) : null;
    }),
  ]) : null;
  return h("aside", { className: "tr-help" }, [
    h("div", { key: "hd", className: "tr-help-head" }, [
      h("strong", { key: "t", ref: titulo, tabIndex: -1 }, spec.label || spec.id),
      h("button", { key: "x", className: "tr-help-close", title: "voltar à paleta",
                    onClick: onClose }, "×"),
    ]),
    h("code", { key: "id", className: "tr-help-id" }, spec.id),
    h("div", { key: "b", className: "tr-help-body" }, [
      spec.description && !descricaoRepete(spec.description, spec.help) ? h("p", { key: "d", className: "tr-help-desc" }, spec.description) : null,
      secPress, secRefs,
      spec.help ? h("div", { key: "md" }, md(spec.help))
        : (!spec.description && !secPress && !secRefs ? h("p", { key: "0" }, "sem ajuda") : null),
    ]),
  ]);
}

function compatible(catalog, from, to) {
  if (from === to) return true;
  return (catalog.adapters || []).some((a) => a.from === from && a.to === to);
}

// Template = JSON com a marca `trama: "template"`. Qualquer outro JSON segue
// sendo dado (ou flow, no diálogo de importar). O parse aqui é só pra decidir
// o caminho; validar de verdade é do servidor (`tr_template_parse`). O teste
// de texto antes do `JSON.parse` evita parsear todo texto colado à toa.
function ehTemplate(texto) {
  if (typeof texto !== "string" || texto.length > 5e6 || !/"trama"\s*:\s*"template"/.test(texto)) return false;
  try { const x = JSON.parse(texto); return !!x && !Array.isArray(x) && x.trama === "template"; }
  catch { return false; }
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
                         arquivoInicial, onColarTemplate }) {
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
  // Template escolhido aqui não vira projeto: é um trecho de flow, e o gesto
  // que faz sentido é colá-lo no canvas aberto. O botão troca de papel em vez
  // de recusar, porque o usuário só errou a porta de entrada.
  const template = !!arquivo && ehTemplate(arquivo.conteudo);
  const importar = () => {
    if (template) { onColarTemplate(arquivo.conteudo); onClose(); return; }
    if (arquivo && nome.trim()) onImport(l.path, nome.trim(), arquivo.conteudo);
  };
  const podeImportar = template ? !enviando : !!l && !!arquivo && !!nome.trim() && !enviando;

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
          template ? "Colar no canvas" : enviando === "importar" ? "importando…" : "Importar aqui"),
      ]),
      template ? h("p", { key: "tpl", className: "tr-dialog-note" },
        "Isto é um template — ele será colado no canvas atual.") : null,
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

// "Salvar como template": nome, descrição e ONDE gravar. Mesmo visual do
// `ProjectDialog` e a mesma regra de não fechar ao clicar fora — aqui o custo
// do clique errado é o nome e a descrição digitados. O destino padrão vem de
// quem abriu o editor (`origem`, na mensagem `project`): no launcher não há
// projeto que o usuário versione, então a biblioteca pessoal é o lugar
// natural; vindo do R, é o projeto.
//
// Nome repetido não fecha nem avisa no banner: o servidor devolve
// `template_conflict` e a pergunta aparece aqui, onde dá pra trocar o nome ou
// confirmar a substituição.
const DESTINOS_TEMPLATE = [
  { value: "biblioteca", label: "Minha biblioteca", nota: "disponível em qualquer projeto" },
  { value: "projeto", label: "Este projeto", nota: "fica em templates/, junto do projeto" },
  { value: "baixar", label: "Baixar arquivo", nota: "um .template.json para enviar a alguém" },
];
function TemplateDialog({ quantos, destinoPadrao, conflito, enviando, onSave, onClose }) {
  const [nome, setNome] = useState("");
  const [descricao, setDescricao] = useState("");
  const [destino, setDestino] = useState(destinoPadrao);
  const nomeRef = useRef(null);
  useEffect(() => { nomeRef.current?.focus(); }, []);
  useEffect(() => {
    const esc = (e) => { if (e.key === "Escape") onClose(); };
    window.addEventListener("keydown", esc);
    return () => window.removeEventListener("keydown", esc);
  }, [onClose]);
  const pode = !!nome.trim() && !enviando;
  // A pergunta vale para o nome E o destino que colidiram: trocou qualquer um
  // dos dois, é um pedido novo e volta a ser "Salvar".
  const colide = !!conflito && conflito.nome === nome.trim() && conflito.destino === destino;
  const salvar = (overwrite = false) => {
    if (pode) onSave({ nome: nome.trim(), descricao: descricao.trim(), destino, overwrite });
  };
  return h("div", { className: "tr-lightbox tr-modal" },
    h("div", { className: "tr-dialog tr-tpl-dialog", role: "dialog", "aria-modal": "true",
               "aria-label": "Salvar como template" }, [
      h("div", { key: "p", className: "tr-dialog-path" }, [
        h("span", { key: "c" }, quantos ? `Salvar ${quantos} ${quantos > 1 ? "blocos" : "bloco"} como template`
                                        : "Salvar o flow inteiro como template"),
        h("button", { key: "x", className: "tr-dialog-close", title: "fechar (Esc)",
                      onClick: onClose }, "×"),
      ]),
      h("div", { key: "f", className: "tr-tpl-form" }, [
        h("label", { key: "n" }, [
          h("span", { key: "r" }, "Nome"),
          h("input", { key: "i", ref: nomeRef, className: "tr-dialog-name", value: nome,
                       placeholder: "ex.: Limpeza padrão",
                       onChange: (e) => setNome(e.target.value),
                       onKeyDown: (e) => { if (e.key === "Enter") salvar(); } }),
        ]),
        h("label", { key: "d" }, [
          h("span", { key: "r" }, "Descrição"),
          h("textarea", { key: "i", className: "tr-dialog-name", rows: 2, value: descricao,
                          placeholder: "opcional — o que este trecho faz",
                          onChange: (e) => setDescricao(e.target.value) }),
        ]),
        h("fieldset", { key: "ds", className: "tr-tpl-destinos" }, [
          h("legend", { key: "l" }, "Destino"),
          ...DESTINOS_TEMPLATE.map((d) => h("label", { key: d.value, className: "tr-tpl-destino" }, [
            h("input", { key: "i", type: "radio", name: "tr-tpl-destino", value: d.value,
                         checked: destino === d.value, onChange: () => setDestino(d.value) }),
            h("span", { key: "t" }, d.label),
            h("small", { key: "n" }, d.nota),
          ])),
        ]),
      ]),
      colide ? h("p", { key: "cf", className: "tr-dialog-note tr-tpl-conflito", role: "alert" },
        `Já existe um template chamado "${conflito.nome}" aqui. Substituir, ou troque o nome.`) : null,
      h("div", { key: "ac", className: "tr-dialog-actions tr-tpl-actions" }, [
        h("button", { key: "c", onClick: onClose }, "Cancelar"),
        colide
          ? h("button", { key: "s", disabled: !pode, onClick: () => salvar(true) }, "Substituir")
          : h("button", { key: "s", disabled: !pode, onClick: () => salvar() },
              enviando ? "salvando…" : destino === "baixar" ? "Baixar" : "Salvar"),
      ]),
    ]));
}

// Painel de templates: o que as coleções trazem, a biblioteca pessoal e os do
// projeto, nessa ordem — do mais genérico ao mais local. A lista vem do
// servidor (`tr_template_list`), pedida a cada abertura: um template salvo em
// outra janela, ou largado à mão na pasta, aparece sem recarregar.
// Clique só foca o item (clique solto inseria sem querer). Botão direito abre
// um menu com "Colar template", que insere no centro da tela; Enter faz o
// mesmo pelo teclado; arrastar solta onde o mouse estiver. O
// `arquivo` que viaja é o caminho que o próprio servidor listou, e ele recusa
// qualquer outro (ver `tr_template_insert`).
const SECOES_TEMPLATE = [
  { escopo: "colecao", titulo: "Coleções" },
  { escopo: "biblioteca", titulo: "Minha biblioteca" },
  { escopo: "projeto", titulo: "Projeto" },
];
function TemplatesPanel({ templates, onInsert, onClose }) {
  // Menu próprio, em coordenadas da janela (`position:fixed`): o painel não é
  // filho de `.tr-canvas`, onde mora o menu do canvas. Mesmas classes.
  const [menuTpl, setMenuTpl] = useState(null); // {arquivo, x, y}
  useEffect(() => {
    if (!menuTpl) return;
    const fechar = () => setMenuTpl(null);
    const tecla = (e) => { if (e.key === "Escape") fechar(); };
    window.addEventListener("pointerdown", fechar);
    window.addEventListener("keydown", tecla);
    window.addEventListener("blur", fechar);
    return () => {
      window.removeEventListener("pointerdown", fechar);
      window.removeEventListener("keydown", tecla);
      window.removeEventListener("blur", fechar);
    };
  }, [menuTpl]);
  return h("aside", { className: "tr-frames tr-templates" }, [
    menuTpl ? h("div", { key: "menu", className: "tr-menu",
                         style: { position: "fixed", left: menuTpl.x, top: menuTpl.y },
                         onPointerDown: (e) => e.stopPropagation(),
                         onContextMenu: (e) => e.preventDefault() }, [
      h("button", { key: "c", onClick: () => { onInsert(menuTpl.arquivo); setMenuTpl(null); } },
        "Colar template"),
    ]) : null,
    h("div", { key: "hd", className: "tr-help-head" }, [
      h("strong", { key: "t" }, "Templates"),
      h("button", { key: "x", className: "tr-help-close", title: "voltar à paleta",
                    onClick: onClose }, "×"),
    ]),
    h("div", { key: "b", className: "tr-frames-body" }, templates === null
      ? h("p", { className: "tr-frames-empty" }, "carregando…")
      : SECOES_TEMPLATE.map((sec) => {
          const itens = templates.filter((t) => t.escopo === sec.escopo);
          return h("section", { key: sec.escopo, className: "tr-tpl-secao" }, [
            h("h4", { key: "h" }, sec.titulo),
            ...(itens.length ? itens.map((t) => h("div", {
              key: t.arquivo, role: "button", tabIndex: 0, draggable: true,
              className: "tr-frames-item tr-tpl-item",
              title: "arraste para o canvas ou clique com o botão direito → Colar template",
              onClick: (e) => e.currentTarget.focus(),
              onContextMenu: (e) => {
                e.preventDefault();
                setMenuTpl({ arquivo: t.arquivo, x: e.clientX, y: e.clientY });
              },
              // Mesmo trato do item do painel de frames: Espaço é a tecla de
              // andar pela tela, e não pode escapar até o `window`.
              onKeyDown: (e) => {
                if (e.key !== "Enter" && e.key !== " ") return;
                e.preventDefault(); e.stopPropagation();
                if (e.key === "Enter") onInsert(t.arquivo);
              },
              onDragStart: (e) => {
                e.dataTransfer.setData("application/trama-template", t.arquivo);
                e.dataTransfer.effectAllowed = "copy";
              },
            }, [
              h("span", { key: "n", className: "tr-tpl-nome" }, t.nome),
              t.descricao ? h("span", { key: "d", className: "tr-tpl-desc" }, t.descricao) : null,
            ])) : [h("p", { key: "v", className: "tr-frames-empty" },
                    sec.escopo === "colecao" ? "Nenhuma coleção carregada traz templates."
                      : "Selecione blocos e use Salvar como template.")]),
          ]);
        })),
  ]);
}

// Nome de arquivo a partir do nome do template, como `.tr_slug` no R.
const slugArquivo = (x) => (x || "").normalize("NFD").replace(/[\u0300-\u036f]/g, "")
  .toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/^-|-$/g, "") || "template";

// --- App -------------------------------------------------------------------

// Espelha `.tr_presentation_ops` (R/document.R): op cosmética não recomputa
// nada, então não pode pintar o canvas inteiro de "na fila". Batch é cosmético
// só se TODA op dentro dele for, a mesma regra de `tr_op_semantic()`.
const COSMETICAS = new Set(["move", "rename", "resize", "set_view",
  "add_frame", "update_frame", "remove_frame", "reorder_frames", "set_mode"]);
const cosmetica = (op) =>
  op.op === "batch" ? op.ops.every(cosmetica) : COSMETICAS.has(op.op);

// Ícones da toolbar: traço de 1.75 em grade de 24, herdando `currentColor`
// pra seguir o tema e o estado ligado sem CSS por ícone.
const ICONES = {
  pasta: "M3 7.5a2 2 0 0 1 2-2h3.6l2 2.2H19a2 2 0 0 1 2 2V17a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2z",
  organizar: "M4 4h6v5H4zM14 15h6v5h-6zM4 15h6v5H4zM7 9v6M7 12h10v3",
  frame: "M7 3v18M17 3v18M3 7h18M3 17h18",
  texto: "M5 7V5h14v2M12 5v14M9 19h6",
  imagem: "M4 5h16v14H4zM4 16l5-5 4 4 2-2 5 5M15.5 9.5h.01",
  slides: "M3 4h18M5 4v10h14V4M12 14v3M8 21l4-4 4 4",
  ajuda: "M12 21a9 9 0 1 0 0-18 9 9 0 0 0 0 18zM9.5 9.2a2.6 2.6 0 0 1 5 .9c0 1.7-2.5 2.2-2.5 3.6M12 17h.01",
  mais: "M4 12a1 1 0 1 0 2 0 1 1 0 1 0-2 0M11 12a1 1 0 1 0 2 0 1 1 0 1 0-2 0M18 12a1 1 0 1 0 2 0 1 1 0 1 0-2 0",
  grade: "M4 4h7v7H4zM13 4h7v7h-7zM4 13h7v7H4zM13 13h7v7h-7z",
  config: "M12 15a3 3 0 1 0 0-6 3 3 0 0 0 0 6zM19 12l2-1-1-3-2.2.2-1.3-1.5.3-2.2-3-1-1 2h-1.6l-1-2-3 1 .3 2.2L6.2 8.2 4 8l-1 3 2 1v0l-2 1 1 3 2.2-.2 1.3 1.5-.3 2.2 3 1 1-2h1.6l1 2 3-1-.3-2.2 1.3-1.5 2.2.2 1-3z",
  desfazer: "M9 14 4 9l5-5M4 9h10a6 6 0 0 1 0 12h-3",
  recalcular: "M20 11a8 8 0 1 0-2.3 5.7M20 4v7h-7",
  baixar: "M12 4v11M7 10l5 5 5-5M5 20h14",
  template: "M12 3 3 8l9 5 9-5zM3 12.5l9 5 9-5M3 17l9 5 9-5",
};
function Icone({ nome }) {
  return h("svg", { className: "tr-ic", viewBox: "0 0 24 24", width: 18, height: 18, fill: "none",
                    stroke: "currentColor", strokeWidth: 1.75, strokeLinecap: "round",
                    strokeLinejoin: "round", "aria-hidden": true },
    h("path", { d: ICONES[nome] }));
}
// Botão só com ícone: o nome vai pra `aria-label` e pra dica, com o atalho
// (`dica`) quando houver. `temOpcoes` desenha o triângulo de "tem mais aqui".
function BotaoIcone({ icone, rotulo, dica: atalho, extra, on, temOpcoes, ...resto }) {
  const titulo = [atalho || rotulo, extra].filter(Boolean).join(" · ");
  return h("button", { ...resto, type: "button", "aria-label": rotulo, title: titulo,
                       "aria-pressed": on ? true : undefined,
                       className: "tr-tb-btn" + (on ? " tr-on" : "") + (temOpcoes ? " tr-tb-opcoes" : "") },
    h(Icone, { nome: icone }));
}

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
  const [dragFrom, setDragFrom] = useState(null);
  const [banner, setBanner] = useState(null);
  const [helpFor, setHelpFor] = useState(null);
  const [vista, setVista] = useState(null); // id do card aberto em tela cheia (V)
  const [painelAtalhos, setPainelAtalhos] = useState(false);
  const [menu, setMenu] = useState(null);   // {kind, id, x, y}
  const [ferramenta, setFerramenta] = useState(null);   // "frame" | null
  const [editFrame, setEditFrame] = useState(null);     // id do frame com título em edição
  const [editNota, setEditNota] = useState(null);        // id da nota com textarea aberto
  const [painelFrames, setPainelFrames] = useState(false);
  // Painel ⚙ (temas do projeto). Os três painéis laterais dividem a mesma
  // coluna e abrir um fecha os outros: com dois estados ligados, o botão do
  // escondido ficaria aceso sem nada na tela que corresponda a ele.
  const [painelConfig, setPainelConfig] = useState(false);
  // Diálogo "Salvar como template": `{ ids, conflito, enviando }` | null.
  // `ids` é fixado ao abrir — clicar no canvas atrás não é possível (overlay),
  // mas o retrato evita depender disso.
  const [templateDlg, setTemplateDlg] = useState(null);
  const [painelTemplates, setPainelTemplates] = useState(false);
  const painelTemplatesRef = useRef(false); painelTemplatesRef.current = painelTemplates;
  const [templates, setTemplates] = useState(null); // lista do servidor; null = ainda não veio
  const [menuAcoes, setMenuAcoes] = useState(false);
  const [opcoesFrame, setOpcoesFrame] = useState(false);
  // Clique fora fecha os popovers da toolbar; dentro deles ou no botão que os
  // abre, não (o próprio botão alterna).
  useEffect(() => {
    if (!opcoesFrame && !menuAcoes) return;
    const fora = (e) => {
      if (e.target.closest?.(".tr-toolbar")) return;
      setOpcoesFrame(false); setMenuAcoes(false);
    };
    window.addEventListener("pointerdown", fora);
    return () => window.removeEventListener("pointerdown", fora);
  }, [opcoesFrame, menuAcoes]);
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
  // Proporção dos frames NOVOS (Shift+F e Ctrl+G). É preferência de quem usa este
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
  // Modo com que os blocos da paleta entram no canvas. Preferência do
  // navegador, como a proporção dos frames novos: não é do documento.
  const [modoNovo, setModoNovo] = useState(() => {
    try {
      const m = localStorage.getItem("trama.modoNovo");
      if (MODOS.includes(m)) return m;
    } catch (_) {}
    return "completo";
  });
  useEffect(() => {
    try { localStorage.setItem("trama.modoNovo", modoNovo); } catch (_) {}
  }, [modoNovo]);
  const modoNovoRef = useRef(modoNovo); modoNovoRef.current = modoNovo;
  // Painel de parâmetros recolhido ou não: preferência de como trabalhar,
  // global a todos os cards e guardada no navegador — recolher e sair
  // clicando em outros cards não pode reabrir o painel a cada clique.
  const [painelRecolhido, setPainelRecolhido] = useState(() => {
    try { return localStorage.getItem("trama.painelParams") === "recolhido"; } catch (_) { return false; }
  });
  useEffect(() => {
    try { localStorage.setItem("trama.painelParams", painelRecolhido ? "recolhido" : "aberto"); } catch (_) {}
  }, [painelRecolhido]);
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
  const modosRef = useRef({});      // modo escolhido localmente, antes do eco
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

  // V: card selecionado em tela cheia (`Vista`, modos-ui.js). Só com exatamente
  // UM card — mais de um não tem um óbvio pra mostrar, e frame/nota não têm
  // preview de renderer pra ampliar.
  const abrirVista = () => {
    const sel = nodesRef.current.filter((n) => n.selected && n.type === "ndNode");
    if (sel.length === 1) setVista(sel[0].id);
  };
  const fecharVista = useCallback(() => setVista(null), []);

  // H: com UM card selecionado, a ajuda dele (o que era o "?" do cabeçalho);
  // sem isso, a lista de atalhos. H de novo fecha o que estiver aberto.
  const ajuda = () => {
    if (helpFor || painelAtalhos) { setHelpFor(null); setPainelAtalhos(false); return; }
    const sel = nodesRef.current.filter((n) => n.selected && n.type === "ndNode");
    setPainelFrames(false); setPainelConfig(false); setPainelTemplates(false);
    if (sel.length === 1) setHelpFor(sel[0].data.nodeType);
    else setPainelAtalhos(true);
  };

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

  // Estável pelos mesmos motivos de `onParam`. `set_mode` não devolve o
  // documento, então o ref segura o modo até o próximo documento chegar.
  const onModo = useCallback((nodeId, modo) => {
    modosRef.current[nodeId] = modo;
    bumpTick();
    pushOp({ op: "set_mode", node: nodeId, modo });
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

  // Popover "próximo bloco": `{de, porta, tipo, x, y}` — `de` é o id do NÓ de
  // origem; o tipo de bloco dele sai do nó na hora de montar o popover.
  const [prox, setProx] = useState(null);
  const tipoDaSaida = (nodeId, porta) => {
    const cat = catalogRef.current;
    const n = nodesRef.current.find((x) => x.id === nodeId);
    const spec = n && cat?.nodes.find((x) => x.id === n.data.nodeType);
    const out = spec?.outputs?.find((o) => o.name === porta);
    return out ? { tipo: out.type, nodeType: n.data.nodeType } : null;
  };
  const fecharProx = useCallback(() => setProx(null), []);
  const abrirProximo = useCallback((nodeId, porta, x, y) => {
    const t = tipoDaSaida(nodeId, porta);
    if (t) setProx({ de: nodeId, deTipo: t.nodeType, porta, tipo: t.tipo, x, y });
  }, []);
  // Modo "meio": o "+" no meio de uma aresta. Filtra o que entra no tipo da
  // origem E alimenta a entrada do destino.
  const abrirMeio = useCallback((e, x, y) => {
    const cat = catalogRef.current;
    const t = tipoDaSaida(e.source, e.sourceHandle);
    const n = nodesRef.current.find((q) => q.id === e.target);
    const inp = n && cat?.nodes.find((q) => q.id === n.data.nodeType)?.inputs?.find((i) => i.name === e.targetHandle);
    if (t && inp) setProx({ modo: "meio", de: e.source, deTipo: t.nodeType, porta: e.sourceHandle,
                            tipo: t.tipo, tipoPara: inp.type, aresta: e, x, y });
  }, []);
  // Modo "origem": puxado de uma ENTRADA, sugere o que alimentaria a porta.
  const abrirOrigem = useCallback((nodeId, porta, x, y) => {
    const cat = catalogRef.current;
    const n = nodesRef.current.find((q) => q.id === nodeId);
    const inp = n && cat?.nodes.find((q) => q.id === n.data.nodeType)?.inputs?.find((i) => i.name === porta);
    if (inp) setProx({ modo: "origem", de: nodeId, deTipo: n.data.nodeType, porta, tipo: inp.type, x, y });
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
                      modo: modosRef.current[n.id] ?? n.data.modo,
                      seed: seedsRef.current[n.id] ?? n.data.seed,
                      // Membro de QUALQUER região: contorno do card (8.1). Fonte de
                      // UMA região: controles de fluxo (8.2) — os dois lidos do
                      // ref, então não precisam de estado próprio nem de `tick` na
                      // lista de deps além do que a rajada de eventos já pede.
                      emRegiao: regionNodesRef.current.has(n.id),
                      streamSource: regiaoFonte(n.id),
                      streamCtl: streamCtlRef.current,
                      onStreamCmd,
                      typeColors, categories, onParam, onView, onResize, onModo,
                      onReseed, onAbrirProximo: abrirProximo, temas } };
  }),
    // `temas` só muda quando chega mensagem `themes` (abrir projeto, salvar):
    // raro o bastante pra não realimentar o laço de remedição.
    [nodes, typeColors, categories, onParam, onView, onResize, onModo, onReseed, tick, temas, abrirProximo,
     editFrame, onFrameRect, onFrameEdit, onFrameEditStart, onFrameEditEnd, onStreamCmd, regiaoFonte,
     editNota, resolverSrc, imagens, onNotaRect, onNotaEdit, onNotaEditStart, onNotaEditEnd]);

  // Card do painel de parâmetros à esquerda (Fase 3): existe fora da
  // apresentação, com exatamente UM card selecionado, que não mostra os
  // próprios parâmetros (mini ou só preview — `precisaPainel`). Mais de um
  // selecionado já tem a barra de seleção pra modo em massa; o painel é por
  // card, então não tenta decidir qual dos vários mostrar.
  const selDecorado = decorated.filter((n) => n.selected);
  const noDoPainel = !present && selDecorado.length === 1 && selDecorado[0].type === "ndNode"
    && selDecorado[0].data.spec && precisaPainel(modoDe(selDecorado[0].data))
    ? selDecorado[0] : null;

  // Card aberto em `Vista` (V). Se ele for apagado enquanto aberto, some daqui
  // sozinho — `vista` fica com um id obsoleto, inofensivo (só reabriria se o
  // mesmo id voltasse a existir, o que undo pode fazer, e aí reabrir é certo).
  const noDaVista = vista && decorated.find((n) => n.id === vista);

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
        modosRef.current = {};
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
        const selNovo = selNovoRef.current;
        const n = novos.map((x) => (x.type === "ndNode" && medidas.has(x.id)
          ? { ...x, measured: medidas.get(x.id) } : x))
          .map((x) => (selNovo && x.id === selNovo ? { ...x, selected: true } : x));
        if (selNovo && n.some((x) => x.id === selNovo)) selNovoRef.current = null;
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
        // Recusado com inserts encadeados na fila: eles dependem do bloco que
        // não entrou (ou sairiam de novo com revisão velha). Descarta a fila
        // em vez de deixá-la presa até recarregar.
        setBanner(descartarFila(m.message));
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
        // A lista de templates tem a seção "Este projeto": a do projeto
        // anterior não vale mais. Painel aberto pede de novo.
        setTemplates(null);
        if (painelTemplatesRef.current) sendInput("tr_template_list", { seq: ++seqCounter });
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
        setProjeto({ root: m.root, flow: m.flow, origem: m.origem });
        setAbrindo(false);
        setEnviando(null);
        // A recusa da tentativa anterior ("nome inválido") não pode ficar na
        // tela depois do acerto: o aviso passaria a falar de um projeto que
        // não é mais o aberto.
        setBanner(null);
        return;
      }

      if (m.type === "listing") { setListagem(m); return; }

      // Baixar/copiar: o servidor montou o template (tira dados, normaliza) e
      // devolve só o texto; arquivo e clipboard são coisa do navegador.
      // `navigator.clipboard` não existe fora de contexto seguro (o app
      // servido por IP da rede, sem https) e pode recusar sem gesto recente
      // do usuário — a resposta chega depois de uma ida ao R. Nos dois casos
      // o texto não pode se perder: vira download.
      if (m.type === "template_json") {
        const arq = `${slugArquivo(m.nome)}.template.json`;
        // Só fecha o diálogo quando a resposta é DELE (mesmo `seq`): um
        // Ctrl+Shift+C não fecha um diálogo de salvar aberto.
        setTemplateDlg((d) => (d && d.seq === m.seq ? null : d));
        if (m.acao === "copiar") {
          const baixar = () => {
            exportText(m.texto, arq, "application/json");
            setBanner("Não deu pra copiar para a área de transferência — o template foi baixado.");
          };
          if (navigator.clipboard?.writeText) {
            navigator.clipboard.writeText(m.texto)
              .then(() => setBanner("Template copiado. Cole com Ctrl+V em outro canvas."))
              .catch(baixar);
          } else baixar();
        } else {
          exportText(m.texto, arq, "application/json");
        }
        return;
      }
      if (m.type === "templates") { setTemplates(m.templates || []); return; }
      if (m.type === "template_conflict") {
        setTemplateDlg((d) => d && d.seq === m.seq
          ? { ...d, conflito: { nome: m.nome, destino: d.destino }, enviando: false } : d);
        return;
      }
      if (m.type === "template_saved") {
        setTemplateDlg((d) => (d && d.seq === m.seq ? null : d));
        setBanner(`Template "${m.nome}" salvo em ${m.destino === "biblioteca" ? "Minha biblioteca" : "Este projeto"}.`);
        return;
      }

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
      if (m.type === "warning") {
        setEnviando(null);
        setTemplateDlg((d) => d && { ...d, enviando: false });
        setBanner(m.message);
        return;
      }

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
  // Painel de templates: lista fresca a cada abertura (ver `TemplatesPanel`).
  useEffect(() => {
    if (painelTemplates) sendInput("tr_template_list", { seq: ++seqCounter });
  }, [painelTemplates]);

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
    setProx(null);
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

  // O modo da paleta ("entra como") vai no MESMO batch do `add_node`, com id
  // do cliente — o mesmo truque de `opsDoGrupo`: `set_mode` referenciando um
  // id que só existe dentro deste lote. Ausência de extra ops quando o modo é
  // o padrão evita mandar um `set_mode` inútil a cada bloco novo.
  const opsAdd = (typeId, pos, id, extra) => {
    const add = { op: "add_node", type: typeId,
                  position: [Math.round(pos.x), Math.round(pos.y)], ...extra };
    const modo = modoNovoRef.current;
    if (modo === "completo") return [id ? { ...add, id } : add];
    const nid = id || add.id || novoId();
    return [{ ...add, id: nid }, { op: "set_mode", node: nid, modo }];
  };
  const addAt = useCallback((typeId, pos, extra) => {
    pushMany(opsAdd(typeId, pos, null, extra));
  }, []);

  // Id do bloco recém-inserido: o documento que ecoa o batch refaz os nós
  // sem seleção, e é ali que ele ganha o `selected`.
  const selNovoRef = useRef(null);

  // Insere o bloco à direita da origem, já conectado, num batch só (um passo
  // de undo). Colidindo com um card, desce até achar vão.
  const filaProxRef = useRef([]);
  // Id do último bloco inserido que ainda não voltou do servidor. Enquanto
  // ele não ecoa, qualquer insert novo (encadeado, no meio ou de outra
  // origem) sairia com revisão defasada: vai pra fila atrás dele.
  const ultimoProxRef = useRef(null);
  const ultimoProxT = useRef(0);
  // Op perdida (recusa ou prazo) nunca terá `run_finished`: o "na fila"
  // que `pushOp` pintou volta ao repouso, senão fica preso até a próxima run.
  const liberarPendentes = () => {
    let mudou = false;
    Object.keys(stateRef.current).forEach((k) => {
      if (stateRef.current[k].state === "pending") {
        stateRef.current[k] = { ...stateRef.current[k], state: "idle" };
        mudou = true;
      }
    });
    if (mudou) bumpTick();
  };
  const descartarFila = (motivo) => {
    const n = filaProxRef.current.length;
    const pendente = n || ultimoProxRef.current;
    filaProxRef.current = [];
    ultimoProxRef.current = null;
    liberarPendentes();
    // O popover encadeando a partir de um bloco que não vai existir fecha:
    // senão o próximo Tab conectaria num nó que o servidor nunca viu.
    if (pendente) {
      setProx((q) => (q && q.modo !== "meio" && !nodesRef.current.some((x) => x.id === q.de) ? null : q));
    }
    if (!n) return motivo;
    const q = n === 1 ? "1 bloco encadeado descartado" : `${n} blocos encadeados descartados`;
    return motivo ? `${motivo} · ${q}` : q;
  };
  // Manda já, ou entra na fila esperando o bloco anterior ecoar. `nid` passa
  // a ser quem o próximo espera.
  const enviarProx = (ops, nid, extra) => {
    const ult = ultimoProxRef.current;
    const livre = !filaProxRef.current.length && !(ult && !nodesRef.current.some((n) => n.id === ult));
    if (livre) pushMany(ops);
    else filaProxRef.current.push({ de: ult, ops, t: Date.now(), ...extra });
    ultimoProxRef.current = nid;
    ultimoProxT.current = Date.now();
  };
  useEffect(() => {
    const f = filaProxRef.current[0];
    if (ultimoProxRef.current && !f && nodes.some((n) => n.id === ultimoProxRef.current)) {
      ultimoProxRef.current = null;
    }
    if (f && nodes.some((n) => n.id === f.de)) {
      filaProxRef.current.shift();
      // Quem sai agora recomeça o prazo de quem vem atrás.
      if (filaProxRef.current[0]) filaProxRef.current[0].t = Date.now();
      pushMany(f.ops);
    }
  }, [nodes]);
  // Rede de segurança: se o documento que traria a origem nunca chega (eco
  // perdido, op engolida sem `op_rejected`), a fila não fica presa. Passado o
  // prazo sem a cabeça andar, descarta tudo e avisa.
  useEffect(() => {
    const id = setInterval(() => {
      const f = filaProxRef.current[0];
      // Fila vazia com o último em voo há tempo demais: só para de esperar
      // por ele, sem aviso (nada foi descartado).
      if (!f && ultimoProxRef.current && Date.now() - ultimoProxT.current > FILA_PROX_PRAZO) {
        ultimoProxRef.current = null;
        liberarPendentes();
      }
      if (!f || Date.now() - (f.t ?? Date.now()) < FILA_PROX_PRAZO) return;
      setBanner(descartarFila("O servidor não confirmou o bloco anterior"));
    }, 1000);
    return () => clearInterval(id);
  }, []);
  // O bloco novo pode nascer fora da tela (um vão bem abaixo de um gráfico
  // alto, uma cadeia de Tab que passa da borda direita). Anda o mínimo pra
  // trazê-lo inteiro, com margem, sem mexer no zoom. Já visível, ou com o
  // usuário arrastando a tela agora, não mexe. Devolve o deslocamento em
  // pixels de tela (o popover encadeado soma isso pra ficar ao lado do card).
  const panUsuarioRef = useRef(false), panProprioRef = useRef(null);
  const mostrarBloco = (pos, hBloco, folgaDir = 0) => {
    const box = wrapRef.current?.getBoundingClientRect();
    if (!box || panUsuarioRef.current) return { dx: 0, dy: 0 };
    // Tab rápido: a andada anterior ainda anima; parte do destino dela.
    const vp = panProprioRef.current ?? rf.getViewport();
    const ax = box.left + vp.x + pos.x * vp.zoom, ay = box.top + vp.y + pos.y * vp.zoom;
    const bx = ax + MIN_W * vp.zoom + folgaDir, by = ay + hBloco * vp.zoom;
    const M = 48;
    const eixo = (lo, hi, min, max) => {
      if (lo >= min + M && hi <= max - M) return 0;
      if (hi - lo > max - min - 2 * M || lo < min + M) return min + M - lo;
      return max - M - hi;
    };
    const dx = eixo(ax, bx, box.left, box.right), dy = eixo(ay, by, box.top, box.bottom);
    const atual = rf.getViewport();
    const alvo = { x: vp.x + dx, y: vp.y + dy, zoom: vp.zoom };
    if (!dx && !dy) return { dx: alvo.x - atual.x, dy: alvo.y - atual.y };
    panProprioRef.current = alvo;
    Promise.resolve(rf.setViewport(alvo, { duration: 300 }))
      .finally(() => setTimeout(() => { if (panProprioRef.current === alvo) panProprioRef.current = null; }, 50));
    // Deslocamento de tela entre o viewport de agora e o final.
    return { dx: alvo.x - atual.x, dy: alvo.y - atual.y };
  };
  const inserirProximo = (tipoId, porta, { encadear, saida: saidaMeio } = {}) => {
    const p = prox; if (!p) return;
    if (p.modo === "meio") {
      // Nasce no meio das duas pontas; os nós à direita não se movem.
      const a = p.aresta;
      const o = nodesRef.current.find((n) => n.id === a.source);
      const d = nodesRef.current.find((n) => n.id === a.target);
      setProx(null);
      if (!o || !d) return;
      const nid = novoId();
      const pm = { x: (o.position.x + d.position.x) / 2, y: (o.position.y + d.position.y) / 2 };
      // Os vizinhos não se movem: o bloco é que procura o vão livre mais perto
      // do ponto médio (descendo, depois subindo); sem vão, fica no meio.
      const hMeio = alturaNova(modoNovoRef.current);
      const caixasMeio = nodesRef.current.filter((n) => n.type === "ndNode")
        .map((n) => ({ x: n.position.x, y: n.position.y,
                       w: n.measured?.width ?? n.width ?? MIN_W, h: n.measured?.height ?? n.height ?? 200 }))
        .concat(filaProxRef.current.map((f) => ({ ...f.pos, w: MIN_W, h: f.h ?? hMeio })));
      pm.y = vaoPerto(caixasMeio, { x: pm.x, y: pm.y, w: MIN_W, h: hMeio }, 30);
      const opsMeio = [
        { op: "disconnect", from_node: a.source, from_port: a.sourceHandle,
          to_node: a.target, to_port: a.targetHandle, index: a.index },
        ...opsAdd(tipoId, pm, nid),
        { op: "connect", from_node: a.source, from_port: a.sourceHandle, to_node: nid, to_port: porta },
        { op: "connect", from_node: nid, from_port: saidaMeio, to_node: a.target, to_port: a.targetHandle },
      ];
      // Com inserts encadeados na fila (ou o último ainda em voo), este entra
      // atrás deles: sair antes mandaria uma revisão que eles tornam defasada.
      enviarProx(opsMeio, nid, { pos: pm, h: hMeio });
      mostrarBloco(pm, hMeio);
      registrar(p.deTipo, tipoId);
      selNovoRef.current = nid;
      return;
    }
    const origem = nodesRef.current.find((n) => n.id === p.de);
    const cat = catalogRef.current;
    // Encadeando rápido, a origem pode ainda não ter voltado do servidor:
    // tipo e posição dela vêm guardados no próprio `prox`.
    const oPos = origem?.position ?? p.pos;
    const oTipo = origem?.data.nodeType ?? p.deTipo;
    if (!oPos || !oTipo || !cat) { setProx(null); return; }
    const origemModo = p.modo === "origem";
    // À direita, a distância conta a largura medida da origem: um card largo
    // (Quadro, 540) não pode ficar por baixo do bloco novo, nem o "+" dele.
    const oW = origem?.measured?.width ?? origem?.width ?? MIN_W;
    const pos = { x: oPos.x + (origemModo ? -300 : Math.max(300, oW + 60)), y: oPos.y };
    // Colisão por retângulo: largura e altura medidas de cada card (um
    // completo passa de 350, um Quadro tem 540 de largura) e também os inserts
    // ainda na fila, que não estão em `nodes`. O bloco novo entra com a altura
    // que o modo dele costuma ter. Batendo, desce pra logo abaixo do obstáculo
    // (margem de 30) e testa de novo: só passa de um card se o vão entre ele e
    // o próximo não comporta o novo. Frame e nota não contam: o bloco pode
    // nascer dentro de um frame.
    const hNovo = alturaNova(modoNovoRef.current);
    const caixas = nodesRef.current.filter((n) => n.type === "ndNode" && n.id !== p.de)
      .map((n) => ({ x: n.position.x, y: n.position.y,
                     w: n.measured?.width ?? n.width ?? MIN_W, h: n.measured?.height ?? n.height ?? 200 }))
      .concat(filaProxRef.current.map((f) => ({ ...f.pos, w: MIN_W, h: f.h ?? hNovo })));
    // Numa coluna cheia de cards (outro experimento empilhado à direita), o
    // vão livre pode estar milhares de unidades abaixo: aí o bloco anda uma
    // coluna à direita em vez de nascer longe da origem.
    Object.assign(pos, vaoAoLado(caixas, { x: pos.x, y: pos.y, w: MIN_W, h: hNovo }, 30,
                                 { limite: 600, passoX: origemModo ? -300 : 300, colunas: 6 }));
    const nid = novoId();
    // Em "origem", `porta` é a SAÍDA do bloco novo e a conexão vai dele ao alvo.
    const ops = [...opsAdd(tipoId, pos, nid), origemModo
      ? { op: "connect", from_node: nid, from_port: porta, to_node: p.de, to_port: p.porta }
      : { op: "connect", from_node: p.de, from_port: p.porta, to_node: nid, to_port: porta }];
    // O servidor recusa op com revisão defasada: se a origem ainda não ecoou
    // (Tab rápido), o insert espera na fila e sai quando ela chegar.
    enviarProx(ops, nid, { pos: { ...pos }, h: hNovo });
    if (origemModo) registrar(tipoId, oTipo); else registrar(oTipo, tipoId);
    selNovoRef.current = nid;
    const spec = cat.nodes.find((x) => x.id === tipoId);
    const saida = spec?.outputs?.[0];
    // Encadeando, sobra lugar à direita pro popover que reabre ao lado.
    const d = mostrarBloco(pos, hNovo, encadear ? 360 : 0);
    if (encadear && saida) {
      // Posição de tela já no viewport final (depois da andada).
      const t0 = rf.flowToScreenPosition({ x: pos.x + MIN_W, y: pos.y + 40 });
      const tela = { x: t0.x + d.dx, y: t0.y + d.dy };
      setProx({ de: nid, deTipo: tipoId, pos: { ...pos }, porta: saida.name, tipo: saida.type,
                presentes: [...presentes, tipoId],
                x: tela.x + 8, y: tela.y });
    } else setProx(null);
  };

  // Tipos de bloco A MONTANTE da origem (ela e tudo que chega nela pelas
  // arestas), não o fluxo inteiro: um ramo sem relação não deve pesar. Com o
  // encadeamento ainda na fila, a origem não ecoou: vale o que o `prox` traz.
  // Estável enquanto o conjunto não muda: o sugestor recalcula por referência.
  const presentesChave = (() => {
    if (!prox) return "";
    if (!nodes.some((n) => n.id === prox.de)) return [...(prox.presentes || [prox.deTipo])].sort().join("\n");
    const vistos = new Set([prox.de]), fila = [prox.de];
    while (fila.length) {
      const atual = fila.pop();
      for (const e of edges) if (e.target === atual && !vistos.has(e.source)) { vistos.add(e.source); fila.push(e.source); }
    }
    const tipoDe = Object.fromEntries(nodes.map((n) => [n.id, n.data?.nodeType]));
    return [...vistos].map((id) => tipoDe[id]).filter(Boolean).sort().join("\n");
  })();
  const presentes = useMemo(() => (presentesChave ? [...new Set(presentesChave.split("\n"))] : []),
    [presentesChave]);

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
    // Item do painel de templates: vem antes dos arquivos porque nem é
    // arquivo — é o caminho que o servidor listou.
    const arqTpl = ev.dataTransfer.getData("application/trama-template");
    if (arqTpl) {
      ev.stopPropagation();
      const p = rf.screenToFlowPosition({ x: ev.clientX, y: ev.clientY });
      sendInput("tr_template_insert", { seq: ++seqCounter, arquivo: arqTpl,
                                        x: Math.round(p.x), y: Math.round(p.y) });
      return;
    }
    const t = ev.dataTransfer.getData("application/trama-type");
    if (t) { addAt(t, rf.screenToFlowPosition({ x: ev.clientX, y: ev.clientY })); return; }
    // Arquivo do SO (não o drag interno da paleta, tratado acima). Soltar
    // `.json` aqui passa a significar "quero ler isto como dado" — diferente
    // de soltar fora do canvas, que continua abrindo o diálogo de importar
    // projeto (`onDropGlobal`). `stopPropagation` é o que separa os dois: sem
    // ele, este mesmo evento borbulharia até lá e abriria os dois ao mesmo
    // tempo.
    const f = ev.dataTransfer.files?.[0];
    const pos = rf.screenToFlowPosition({ x: ev.clientX, y: ev.clientY });
    // `.json` pode ser template ou dado, e só o conteúdo diz qual. A leitura
    // é assíncrona, então o `stopPropagation` vem antes de saber — o drop
    // global nunca deve ver um `.json` que caiu no canvas.
    if (f && /\.json$/i.test(f.name)) {
      ev.stopPropagation();
      f.text().then((texto) => {
        if (ehTemplate(texto)) inserirTemplateRef.current(texto, pos);
        else iniciarUploadDado(f, pos);
      }).catch(() => setBanner("Não foi possível ler o arquivo."));
      return;
    }
    if (f && iniciarUploadDado(f, pos)) ev.stopPropagation();
  }, [rf, addAt, iniciarUploadDado]);

  const onConnectStart = useCallback((_e, p) => {
    const cat = catalogRef.current;
    const n = nodes.find((x) => x.id === p.nodeId);
    if (!cat || !n || p.handleType !== "source") return;
    const byId = Object.fromEntries(cat.nodes.map((x) => [x.id, x]));
    setDragType(byId[n.data.nodeType]?.outputs.find((o) => o.name === p.handleId)?.type || null);
    setDragFrom(n.data.nodeType);
  }, [nodes]);

  // Conexão de uma saída solta no vazio abre o próximo bloco no ponto do
  // mouse. `fromHandle` é a porta onde o arrasto começou.
  const onConnectEnd = useCallback((ev, st) => {
    setDragType(null);
    setDragFrom(null);
    if (present || !st || st.toNode || !st.fromHandle) return;
    const pt = ev.changedTouches?.[0] || ev;
    if (pt.clientX == null) return;
    const abrir = st.fromHandle.type === "source" ? abrirProximo : abrirOrigem;
    abrir(st.fromHandle.nodeId, st.fromHandle.id, pt.clientX, pt.clientY);
  }, [present, abrirProximo, abrirOrigem]);

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

  // Último frame para onde se navegou (número, `,`/`.`, clique no painel). Fora
  // da apresentação é o ponto de partida de `,`/`.`; sem ele, vale o frame mais
  // perto do centro da tela (`frameVizinho`).
  const frameAtualRef = useRef(null);
  const centroDaTela = () => {
    const b = wrapRef.current?.getBoundingClientRect();
    return b ? rf.screenToFlowPosition({ x: b.left + b.width / 2, y: b.top + b.height / 2 })
             : { x: 0, y: 0 };
  };
  const irAoFrame = (i) => {
    const fs = framesOrdRef.current;
    if (i < 0 || i >= fs.length) return;
    frameAtualRef.current = fs[i].id;
    if (presentRef.current) setPresent((p) => p && { ...p, i });
    else enquadrar(fs[i]);
  };
  const passoFrame = (d) => {
    if (presentRef.current) { passo(d); return; }
    irAoFrame(frameVizinho(framesOrdRef.current, frameAtualRef.current, d, centroDaTela()));
  };

  const apresentar = () => {
    if (framesOrd.length === 0) return;
    selecionar(false); setMenu(null); setFerramenta(null); setPrancheta(null);
    // Começa no frame ATUAL (o último navegado), não sempre no primeiro: F
    // depois de já ter ido ao slide 4 no modo edição entra apresentando dali.
    const i0 = Math.max(0, framesOrd.findIndex((f) => f.id === frameAtualRef.current));
    setPresent({ i: i0, volta: rf.getViewport() });
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
    if (f) { enquadrar(f, 400, 0); frameAtualRef.current = f.id; }
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

  // W/A/S/D e a barra de seleção levam DIRETO a um modo — não é alternar: o
  // mesmo gesto repetido não desfaz. Órfão (sem spec) fica de fora: não tem
  // preview nem parâmetro pra mostrar ou esconder.
  const definirModo = (modo) => {
    const muda = alvos().filter((n) => n.data.spec
      && (modosRef.current[n.id] ?? modoDe(n.data)) !== modo);
    if (muda.length === 0) return;
    muda.forEach((n) => { modosRef.current[n.id] = modo; });
    bumpTick();
    pushMany(muda.map((n) => ({ op: "set_mode", node: n.id, modo })));
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
                              width: n.width, height: n.height,
                              // O modo pode ter mudado localmente (W/A/S/D, paleta)
                              // antes do eco: `nodesRef` ainda mostra o antigo, então
                              // o retrato sempre confere `modosRef` primeiro.
                              data: { ...n.data, modo: modosRef.current[n.id] ?? n.data.modo } })),
      edges: es.map((e) => ({ source: e.source, sourceHandle: e.sourceHandle,
                              target: e.target, targetHandle: e.targetHandle })),
    };
  };

  // Um retrato -> as ops que recriam o grupo deslocado. Ids novos nascem AQUI,
  // no cliente (e não esperam o eco): é o que permite ligar as cópias entre si
  // no MESMO batch, como op de `connect` apontando pra um id que só existe
  // dentro deste lote. Tamanho e modo de card são ops PARTE de `add_node` —
  // só entram se o original tinha algo fora do padrão, senão colar 50 cards
  // mandaria 100 ops à toa.
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
        if (n.data.modo && n.data.modo !== "completo") depois.push({ op: "set_mode", node: id, modo: n.data.modo });
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

  // Template entra pelo servidor (parse, ids novos, checagem de coleções) e
  // volta como `batch` comum. Sem posição explícita, nasce onde o mouse está
  // no canvas; mouse fora dele, no centro da tela.
  const mouseFlowRef = useRef(null);
  const inserirTemplate = (texto, pos) => {
    const p = pos || mouseFlowRef.current || centroDaTela();
    sendInput("tr_template_insert", { seq: ++seqCounter, conteudo: texto,
                                      x: Math.round(p.x), y: Math.round(p.y) });
  };
  // Refs pros listeners registrados uma vez só (paste, drop global)
  // enxergarem as funções do render atual.
  const inserirTemplateRef = useRef(inserirTemplate); inserirTemplateRef.current = inserirTemplate;
  const colarRef = useRef(colar); colarRef.current = colar;

  // Duplicar (menu de contexto): copiar + colar num só passo, sem tocar o
  // clipboard — um Ctrl+V depois de duplicar continua colando o que o
  // usuário copiou por último, não o bloco duplicado.
  const duplicar = (ids) => {
    const retrato = retratoDoGrupo(ids);
    if (!retrato) return;
    pushMany(opsDoGrupo(retrato, DESLOCA_COPIA, DESLOCA_COPIA));
  };

  // Template da seleção (nós, frames e notas — todos são nós do xyflow); nada
  // selecionado = o flow inteiro. Diferente do Ctrl+C: aqui é gesto explícito
  // de menu ou de atalho com Shift, então "tudo" não é surpresa.
  const selecionadosOuNull = () => {
    const s = nodesRef.current.filter((n) => n.selected).map((n) => n.id);
    return s.length ? s : null;
  };
  const abrirSalvarTemplate = (ids = selecionadosOuNull()) => {
    setMenu(null); setMenuAcoes(false);
    setTemplateDlg({ ids, conflito: null, enviando: false });
  };
  const copiarTemplate = (ids = selecionadosOuNull()) => {
    setMenu(null); setMenuAcoes(false);
    sendInput("tr_template_save", { seq: ++seqCounter, ids, nome: "Template", destino: "copiar" });
  };

  // Tabela refeita a cada render e lida pelo listener via ref: o listener é
  // registrado uma vez só, e as ações sempre enxergam o estado atual. As
  // chaves são `mod+` (Ctrl ou Cmd), `shift+`, e `e.key` em minúsculas.
  const atalhosRef = useRef({});
  // "+" com um card selecionado: popover na primeira saída dele, ancorado na
  // borda direita do card na tela.
  const abrirProximoDaSelecao = () => {
    const sel = nodesRef.current.filter((n) => n.selected);
    if (sel.length !== 1 || sel[0].type !== "ndNode") return;
    const n = sel[0];
    const saida = catalogRef.current?.nodes.find((x) => x.id === n.data.nodeType)?.outputs?.[0];
    if (!saida) return;
    const w = n.measured?.width ?? n.width ?? MIN_W;
    const tela = rf.flowToScreenPosition({ x: n.position.x + w, y: n.position.y + 40 });
    abrirProximo(n.id, saida.name, tela.x + 8, tela.y);
  };
  // 1…9/0 vão direto ao frame N; `,`/`.` andam um frame por vez a partir do
  // atual. As duas tabelas (apresentação e edição) compartilham essa base —
  // navegar entre frames é o mesmo gesto nos dois modos — e cada uma só
  // acrescenta o que muda (F sai de um jeito ou do outro, o resto é só delas).
  const numeros = Object.fromEntries(
    ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0"].map((k, i) => [k, () => irAoFrame(i)]));
  const navFrames = { ...numeros, ",": () => passoFrame(-1), ".": () => passoFrame(1) };
  atalhosRef.current = present ? {
    ...navFrames,
    "arrowright": () => passo(1), "pagedown": () => passo(1), " ": () => passo(1),
    "arrowleft": () => passo(-1), "pageup": () => passo(-1),
    "home": () => setPresent((p) => p && { ...p, i: 0 }),
    "end": () => setPresent((p) => p && { ...p, i: framesOrdRef.current.length - 1 }),
    "escape": sairApresentacao,
    "f": sairApresentacao,
  } : {
    ...navFrames,
    "mod+z": desfazer,
    "mod+a": () => selecionar(true),
    "escape": () => { selecionar(false); setMenu(null); setFerramenta(null); setMenuAcoes(false); setOpcoesFrame(false); },
    "f": apresentar,
    "shift+f": () => setFerramenta((t) => (t === "frame" ? null : "frame")),
    "m": () => setFerramenta((t) => (t === "markdown" ? null : "markdown")),
    "i": () => setFerramenta((t) => (t === "imagem" ? null : "imagem")),
    "mod+g": frameDaSelecao,
    "mod+shift+c": () => copiarTemplate(),
    "w": () => definirModo("preview"),
    "a": () => definirModo("mini"),
    "s": () => definirModo("params"),
    "d": () => definirModo("completo"),
    "shift+r": restaurarAlvos,
    "v": abrirVista,
    "h": ajuda,
    "+": abrirProximoDaSelecao,
  };
  useEffect(() => {
    const onKey = (e) => {
      const t = e.target;
      // O diálogo de projeto é dono do teclado enquanto está aberto, e a
      // guarda vem ANTES de tudo. O guarda de `INPUT|TEXTAREA|SELECT` abaixo
      // cobre o campo de nome, mas o foco do diálogo vive nos BOTÕES dele —
      // cada pasta listada é um — e ali um "f" abria a ferramenta de frame e
      // um "s" trocava o modo dos cards atrás do overlay, sem nada na tela
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
      // Lightbox — imagem ampliada, `Vista` (V) ou o painel ABNT — é dono do
      // teclado em QUALQUER modo: W/A/S/D e números mexeriam no canvas
      // escondido atrás dele, e o Esc que o fecha também limparia a seleção
      // (ou, em apresentação, encerraria o slide). Cada overlay tem o próprio
      // listener; este aqui só recua.
      if (document.querySelector(".tr-lightbox")) return;
      const nome = nomeDaTecla(e);
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
      // Ctrl+V não é tratado aqui: o `paste` abaixo é quem decide, porque só
      // ele enxerga o clipboard do SISTEMA. `preventDefault` no keydown
      // mataria o próprio evento `paste`.
      if (nome === "mod+v") return;
      const fn = atalhosRef.current[nome];
      if (!fn) return;
      e.preventDefault();
      fn();
    };
    // Colar do SISTEMA: um template copiado do site ou de uma mensagem vence
    // o clipboard interno. Qualquer outro texto cai no comportamento de antes
    // (colar os nós do último Ctrl+C) — texto solto no clipboard do SO não
    // pode sequestrar o Ctrl+V interno. Campo de texto focado não é conosco;
    // diálogo e lightbox recuam como no keydown.
    const onPaste = (e) => {
      const t = e.target;
      if (abrindoRef.current || document.querySelector(".tr-lightbox")) return;
      if (t && (t.isContentEditable || /^(INPUT|TEXTAREA|SELECT)$/.test(t.tagName))) return;
      const texto = e.clipboardData?.getData("text/plain") || "";
      if (ehTemplate(texto)) { e.preventDefault(); inserirTemplateRef.current(texto); return; }
      if (clipboardRef.current) { e.preventDefault(); colarRef.current(); }
    };
    window.addEventListener("keydown", onKey);
    window.addEventListener("paste", onPaste);
    return () => { window.removeEventListener("keydown", onKey); window.removeEventListener("paste", onPaste); };
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
      h("button", { key: "tpl", onClick: () => abrirSalvarTemplate(alvo) }, "Salvar como template…"),
      h("button", { key: "tplc", onClick: () => copiarTemplate(alvo) }, "Copiar como template"),
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
      // Template solto fora do canvas vai direto pro canvas: abrir o diálogo
      // de projeto pra algo que não é projeto seria um desvio sem saída útil.
      if (ehTemplate(leitor.result)) { inserirTemplate(leitor.result, centroDaTela()); return; }
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
  return h("div", { className: ["tr-app", helpFor || painelFrames || painelConfig || painelAtalhos || painelTemplates ? "tr-app-help" : "",
                                present ? "tr-presenting" : "",
                                abrindo || templateDlg ? "tr-app-dialog" : ""].filter(Boolean).join(" "),
                    onDragOver: onDragOverGlobal, onDrop: onDropGlobal }, [
    h("div", { key: "canvas", className: "tr-canvas", ref: wrapRef,
               onMouseMove: (e) => { mouseFlowRef.current = rf.screenToFlowPosition({ x: e.clientX, y: e.clientY }); },
               onMouseLeave: () => { mouseFlowRef.current = null; },
               onDragOver: (e) => { e.preventDefault(); e.dataTransfer.dropEffect = "copy"; },
               onDrop },
      h(MeioCtx.Provider, { key: "rf", value: present ? null : abrirMeio }, h(ReactFlow, {
        nodes: decorated, edges, nodeTypes, edgeTypes,
        // Conectores em ângulo reto com cantos arredondados; a direção da
        // curva (TrAresta) é recalculada por par de cards a cada render,
        // pra nunca cortar por cima do card vizinho quando o arranjo foge
        // do fluxo horizontal padrão.
        defaultEdgeOptions: { type: "trAresta" },
        onNodesChange, onEdgesChange, onConnect, isValidConnection,
        onNodeDragStart, onSelectionStart, onSelectionEnd,
        onConnectStart, onConnectEnd,
        onBeforeDelete,
        // Na apresentação o menu também some: ele traz "Apagar", e o slide é
        // somente leitura.
        onNodeContextMenu: (ev, n) => (present ? ev.preventDefault()
          : abrirMenu(ev, n.type === "trFrame" ? "frame" : n.type === "trNota" ? "nota" : "no", n.id)),
        onEdgeContextMenu: (ev, e) => (present ? ev.preventDefault() : abrirMenu(ev, "aresta", e.id)),
        onPaneClick: () => { setMenu(null); setMenuAcoes(false); }, onNodeClick: () => setMenu(null),
        // A andada automática até o bloco novo não fecha o popover encadeado;
        // um arrasto do usuário fecha e, enquanto dura, trava a andada.
        onMoveStart: (ev) => {
          if (panProprioRef.current && !ev) return;
          if (ev) panUsuarioRef.current = true;
          setMenu(null); setProx(null);
        },
        onMoveEnd: () => { panUsuarioRef.current = false; },
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
            : corDaCategoria(categories[n.data?.spec?.category], n.data?.spec)),
          nodeStrokeColor: (n) => (n.type === "trFrame" ? "var(--tr-dim)" : "transparent") }),
      ])),
      menu ? h("div", { key: "menu", className: "tr-menu",
                        style: { left: menu.x, top: menu.y } }, menuItens(menu)) : null,
      selecionados.length > 1 ? h("div", { key: "sel", className: "tr-selbar" }, [
        h("span", { key: "n", className: "tr-selbar-n" }, `${selecionados.length} selecionados`),
        // Só com algum card na seleção: com só frames (e ligações) os quatro
        // agiriam sobre nada, e botão que não faz nada é botão que mente.
        ...(selecionados.some((n) => n.type === "ndNode") ? [
          h(ModoPicker, { key: "md", value: null, onChange: definirModo, className: "tr-selbar-modos" }),
          h("button", { key: "rs", title: dica("tamanho"), onClick: restaurarAlvos }, "Tamanho"),
          h("button", { key: "fr", title: dica("frame-sel"), onClick: frameDaSelecao }, "Frame"),
        ] : []),
        h("button", { key: "del", title: "Delete",
                      // Mesmo alcance da tecla Delete: cards E ligações escolhidas.
                      onClick: () => apagar(selecionados.map((n) => n.id),
                                            edges.filter((e) => e.selected).map((e) => e.id)) },
          "Apagar"),
      ]) : null,
      noDoPainel
        ? h(ParamsDock, { key: "dock", node: noDoPainel, recolhido: painelRecolhido,
                          onRecolher: setPainelRecolhido, categories })
        : null,
      noDaVista ? h(Vista, { key: "vista", node: noDaVista, assetUrl, onClose: fecharVista }) : null,
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
    painelAtalhos
      ? h(AtalhosPanel, { key: "atalhos", onClose: () => setPainelAtalhos(false) })
      : helpFor
      ? h(Help, { key: "help", catalog, typeId: helpFor, onClose: () => setHelpFor(null), onOpen: setHelpFor })
      : painelConfig
        ? h(SettingsPanel, { key: "cfg", temas: temas.temas, padrao: temas.tema_padrao,
            marca: temas.marca,
            // `seq` porque o input do Shiny ignora valor idêntico ao anterior:
            // voltar a um estado já enviado (desfazer uma cor à mão) não
            // chegaria ao servidor.
            onSave: (m) => sendInput("tr_themes", { temas: m.temas, tema_padrao: m.tema_padrao,
                                                    marca: m.marca, seq: Date.now() }),
            onClose: () => setPainelConfig(false) })
      : painelTemplates
        ? h(TemplatesPanel, { key: "templates", templates,
            onInsert: (arquivo) => {
              const p = centroDaTela();
              sendInput("tr_template_insert", { seq: ++seqCounter, arquivo,
                                                x: Math.round(p.x), y: Math.round(p.y) });
            },
            onClose: () => setPainelTemplates(false) })
      : painelFrames
        ? h(FramePanel, { key: "frames", frames: framesOrd, exportando,
            onGo: (id) => irAoFrame(framesOrd.findIndex((x) => x.id === id)),
            onReorder: (ids) => pushOp({ op: "reorder_frames", frames: ids }),
            onRename: (id, title) => onFrameEdit(id, { title }), onAspect: mudarProporcao,
            onPresent: apresentar, onExport: () => exportar(framesOrd),
            onClose: () => setPainelFrames(false) })
        : h(Palette, { key: "pal", catalog, filterType: dragType, dragFrom, onPick: addPicked,
                       modoNovo, onModoNovo: setModoNovo }),
    prox && !present && catalog
      ? h(Proximo, { key: `prox-${prox.modo || "p"}-${prox.de}-${prox.porta}`, catalog,
          de: prox.deTipo, modo: prox.modo, tipoPara: prox.tipoPara,
          tipo: prox.tipo, presentes, x: prox.x, y: prox.y,
          onEscolher: inserirProximo, onFechar: fecharProx,
          renderIcone: (n) => (n.icon && ICON_KINDS.has(n.icon.kind)
            // Sem `color`: o ícone herda a tinta da faixa colorida da pílula.
            ? h(Icon, { icon: n.icon, className: "tr-palette-icon" })
            : null) })
      : null,
    h("div", { key: "tb", className: "tr-toolbar", role: "toolbar", "aria-label": "Ferramentas" }, [
      // `img`, e não botão: a marca é assinatura, não controle. Não clica, não
      // abre nada e não entra na ordem de tabulação. A versão só existe do
      // lado do R; chega pelo `data-versao` que `tr_ui()` põe na raiz.
      // Marca + nome, como logo largo. O nome já diz "trama", então a imagem
      // fica muda pro leitor de tela.
      h("span", { key: "marca", className: "tr-marca-wide",
                  title: `trama ${document.getElementById("tr-root")?.dataset.versao || ""}`.trim() }, [
        h("img", { key: "i", className: "tr-marca", src: MARCA, alt: "", draggable: false }),
        h("span", { key: "t", className: "tr-marca-nome" }, "trama")]),
      // Só o nome da pasta cabe na barra; o caminho inteiro fica na dica.
      // A navegação começa onde o projeto está. `listagem` e banner zerados
      // ao abrir: são estado do App e mostrariam a navegação anterior até o
      // `tr_browse` responder. A raiz "/" cai em `projeto.root`, senão
      // `filter(Boolean).pop()` dá `undefined`. Nome comprido: CSS apara.
      h("button", { key: "pj", className: "tr-toolbar-proj",
                    title: projeto ? `abrir projeto (${projeto.root})` : "abrir projeto",
                    onClick: () => { setBanner(null); setListagem(null); setEnviando(null);
                                     // Abrir pelo clique é gesto NOVO: um arquivo solto numa
                                     // visita anterior não pode reaparecer pré-carregado aqui.
                                     setArquivoSolto(null);
                                     setAbrindo(true);
                                     sendInput("tr_browse", { seq: ++seqCounter,
                                                              path: projeto?.root || "." }); } },
        [h(Icone, { key: "i", nome: "pasta" }),
         h("span", { key: "t" }, projeto ? (projeto.root.split("/").filter(Boolean).pop() || projeto.root)
                                         : "projeto")]),
      h("span", { key: "s1", className: "tr-toolbar-sep", "aria-hidden": true }),
      h(BotaoIcone, { key: "l", icone: "organizar", rotulo: "Organizar", onClick: organizarTudo }),
      // Proporção e prancheta são opções do Frame, não ferramentas à parte:
      // saem da barra e abrem no botão direito (ou no triângulo do canto).
      h(BotaoIcone, { key: "f", icone: "frame", rotulo: "Frame", dica: dica("frame"),
                      extra: "botão direito: proporção e prancheta",
                      on: ferramenta === "frame", temOpcoes: true,
                      onClick: () => setFerramenta((t) => (t === "frame" ? null : "frame")),
                      onContextMenu: (e) => { e.preventDefault(); setOpcoesFrame((v) => !v); } }),
      h(BotaoIcone, { key: "m", icone: "texto", rotulo: "Markdown", dica: dica("markdown"),
                      on: ferramenta === "markdown",
                      onClick: () => setFerramenta((t) => (t === "markdown" ? null : "markdown")) }),
      h(BotaoIcone, { key: "i", icone: "imagem", rotulo: "Imagem", dica: dica("imagem"),
                      on: ferramenta === "imagem",
                      onClick: () => setFerramenta((t) => (t === "imagem" ? null : "imagem")) }),
      h("span", { key: "s2", className: "tr-toolbar-sep", "aria-hidden": true }),
      h(BotaoIcone, { key: "fp", icone: "slides", rotulo: "Painel de frames", on: painelFrames,
                      onClick: () => { setHelpFor(null); setPainelConfig(false); setPainelAtalhos(false);
                                       setPainelTemplates(false); setPainelFrames((v) => !v); } }),
      h(BotaoIcone, { key: "tpl", icone: "template", rotulo: "Templates", on: painelTemplates,
                      onClick: () => { setHelpFor(null); setPainelConfig(false); setPainelAtalhos(false);
                                       setPainelFrames(false); setPainelTemplates((v) => !v); } }),
      h(BotaoIcone, { key: "aj", icone: "ajuda", rotulo: "Ajuda e atalhos", dica: dica("ajuda"),
                      on: painelAtalhos || !!helpFor, onClick: ajuda }),
      h(BotaoIcone, { key: "more", icone: "mais", rotulo: "Mais ações", on: menuAcoes,
                      "aria-expanded": menuAcoes, onClick: () => setMenuAcoes((v) => !v) }),
      opcoesFrame ? h("div", { key: "fo", className: "tr-toolbar-pop tr-frame-opcoes",
                               role: "dialog", "aria-label": "Opções do frame" }, [
        h("div", { key: "t", className: "tr-pop-rot" }, "Proporção dos frames novos"),
        // Segmentado, e não `<select>`: as cinco proporções cabem à vista.
        // O listener de atalhos só ignora campos de texto e `<select>`.
        h(Segmented, { key: "a", options: Object.keys(ASPECTS), value: aspectoNovo,
                       onChange: (a) => { setAspectoNovo(a); setOpcoesFrame(false); } }),
        h("button", { key: "p", className: "tr-pop-item", "data-prancheta": "",
                      onClick: () => { setOpcoesFrame(false);
                                       setPrancheta((v) => v ? null : { ...pranchetaSalva(), aspect: aspectoNovo }); } },
          [h(Icone, { key: "i", nome: "grade" }), h("span", { key: "t" }, "Prancheta: grade de frames")]),
      ]) : null,
      menuAcoes ? h("div", { key: "actions", className: "tr-toolbar-pop tr-toolbar-actions",
        onClick: () => setMenuAcoes(false) }, [
      // O tema mora aqui: na barra ele só ocupava lugar, e some em tela estreita.
      h("div", { key: "tema", className: "tr-pop-tema", onClick: (e) => e.stopPropagation() },
        h(Segmented, { value: temaApp, onChange: setTemaApp, title: "tema do app",
                       options: [{ value: "claro", label: "☀", title: "tema claro" },
                                 { value: "sistema", label: "◐", title: "seguir o sistema" },
                                 { value: "escuro", label: "☾", title: "tema escuro" }] })),
      h("button", { key: "cfg", className: "tr-pop-item" + (painelConfig ? " tr-on" : ""),
                    onClick: () => { setHelpFor(null); setPainelFrames(false); setPainelAtalhos(false);
                                     setPainelTemplates(false); setPainelConfig((v) => !v); } },
        [h(Icone, { key: "i", nome: "config" }), h("span", { key: "t" }, "Configurações")]),
      h("button", { key: "u", className: "tr-pop-item", onClick: desfazer, title: dica("desfazer") },
        [h(Icone, { key: "i", nome: "desfazer" }), h("span", { key: "t" }, "Desfazer")]),
      h("button", { key: "r", className: "tr-pop-item", onClick: () => sendInput("tr_rerun", Date.now()) },
        [h(Icone, { key: "i", nome: "recalcular" }), h("span", { key: "t" }, "Recalcular")]),
      h("div", { key: "sep", className: "tr-pop-sep" }),
      h("button", { key: "ex-flow", className: "tr-pop-item", disabled: !doc,
                    onClick: () => exportFlowJson(doc,
                      `${(projeto?.root || "flow").split("/").filter(Boolean).pop()}-${projeto?.flow || "main"}.json`) },
        [h(Icone, { key: "i", nome: "baixar" }), h("span", { key: "t" }, "Exportar flow")]),
      h("button", { key: "ex-r", className: "tr-pop-item", disabled: !doc,
                    onClick: () => sendInput("tr_export_code", { format: "r", seq: ++seqCounter }) },
        [h(Icone, { key: "i", nome: "baixar" }), h("span", { key: "t" }, "Exportar R")]),
      h("button", { key: "ex-qmd", className: "tr-pop-item", disabled: !doc,
                    onClick: () => sendInput("tr_export_code", { format: "quarto", seq: ++seqCounter }) },
        [h(Icone, { key: "i", nome: "baixar" }), h("span", { key: "t" }, "Exportar Quarto")]),
      h("div", { key: "sep2", className: "tr-pop-sep" }),
      // Sem seleção, o flow inteiro — o rótulo diz qual dos dois vai sair.
      h("button", { key: "tpl", className: "tr-pop-item", disabled: !doc,
                    onClick: () => abrirSalvarTemplate() },
        [h(Icone, { key: "i", nome: "template" }),
         h("span", { key: "t" }, selecionados.length ? "Salvar seleção como template…" : "Salvar flow como template…")]),
      h("button", { key: "tplc", className: "tr-pop-item", disabled: !doc, title: dica("copiar-template"),
                    onClick: () => copiarTemplate() },
        [h(Icone, { key: "i", nome: "template" }), h("span", { key: "t" }, "Copiar como template")]),
      ]) : null,
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
      onColarTemplate: (conteudo) => inserirTemplate(conteudo, centroDaTela()),
      onClose: fecharDialogo }) : null,
    templateDlg ? h(TemplateDialog, { key: "td", quantos: templateDlg.ids?.length || 0,
      destinoPadrao: projeto?.origem === "launcher" ? "biblioteca" : "projeto",
      conflito: templateDlg.conflito, enviando: templateDlg.enviando,
      onSave: ({ nome, descricao, destino, overwrite }) => {
        const seq = ++seqCounter;
        setTemplateDlg((d) => d && { ...d, enviando: true, destino, seq });
        sendInput("tr_template_save", { seq, ids: templateDlg.ids, nome, descricao,
                                        destino, overwrite });
      },
      onClose: () => setTemplateDlg(null) }) : null,
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
