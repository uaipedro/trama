import { sugerir } from "./sugestor.js";
import { motivoPrincipal } from "./motivo-proximo.js";

export const MODOS_FANTASMA = ["desligado", "demanda", "ligado"];
export const MAX_FANTASMAS = 3;

export function candidatosFantasma({ catalog, nodes = [], edges = [], modo = "demanda",
  descartados = [], historico = {} } = {}) {
  if (!MODOS_FANTASMA.includes(modo) || modo === "desligado" || !catalog) return [];
  const cards = nodes.filter((n) => n.type === "ndNode");
  const folhas = new Set(cards.map((n) => n.id));
  for (const e of edges) folhas.delete(e.source);
  const selecionado = cards.find((n) => n.selected)?.id;
  const origens = modo === "demanda" ? (selecionado && folhas.has(selecionado) ? [selecionado] : [])
    : [...folhas];
  const presentes = cards.map((n) => n.data?.nodeType).filter(Boolean);
  const descartes = new Set(descartados);
  const out = [];
  for (const origemId of origens) {
    const origem = cards.find((n) => n.id === origemId);
    const tipoOrigem = origem?.data?.nodeType;
    const spec = catalog.nodes?.find((n) => n.id === tipoOrigem);
    if (!origem || !spec) continue;
    const porId = new Map();
    for (const saida of spec.outputs || []) {
      for (const item of sugerir(catalog, { de: tipoOrigem, tipo: saida.type, presentes, historico })) {
        if (item.score <= 0) continue;
        const anterior = porId.get(item.id);
        if (!anterior || item.score > anterior.score) porId.set(item.id, { ...item, saida: saida.name });
      }
    }
    const ranking = [...porId.values()].sort((a, b) => b.score - a.score || a.id.localeCompare(b.id));
    let contagem = 0;
    for (const item of ranking) {
      const chave = `${origemId}>${item.id}`;
      if (descartes.has(chave)) continue;
      const alvo = catalog.nodes?.find((n) => n.id === item.id);
      if (!alvo) continue;
      const motivo = motivoPrincipal(item, { de: tipoOrigem,
        deLabel: spec.label || tipoOrigem, historico });
      if (!motivo) continue;
      out.push({ id: chave, origem: origemId, tipoOrigem, portaOrigem: item.saida,
        bloco: item.id, porta: item.porta, label: alvo.label || item.id, motivo,
        position: { x: origem.position.x, y: origem.position.y }, score: item.score });
      if (++contagem >= MAX_FANTASMAS) break;
    }
  }
  return out;
}

// Escolhe o primeiro vão abaixo da saída; avalia retângulos no espaço do fluxo.
export function posicionarFantasma(origem, ocupados, indice = 0, { largura = 220, altura = 86,
  separacao = 18, deslocamentoX = 300 } = {}) {
  const base = { x: origem.x + deslocamentoX, y: origem.y + indice * (altura + separacao) };
  const caixas = (ocupados || []).map((r) => ({ x: r.x, y: r.y, w: r.w, h: r.h }));
  for (let tent = 0; tent < 200; tent++) {
    const y = base.y + tent * (altura + separacao);
    const colide = caixas.some((r) => base.x < r.x + r.w + separacao && base.x + largura + separacao > r.x &&
      y < r.y + r.h + separacao && y + altura + separacao > r.y);
    if (!colide) return { x: base.x, y };
  }
  return base;
}

export function acaoTeclaFantasma(tecla) {
  if (tecla === "Tab") return "aceitar";
  if (tecla === "Escape") return "descartar";
  return null;
}

export function alvoEditavel(el) {
  return !!el?.closest?.('input,textarea,select,[contenteditable="true"],[role="textbox"]');
}
