// Popover "próximo bloco": tagzinhas ancoradas numa porta de saída. As
// sugestões ranqueadas por `sugestor.js` ficam em cima; o resto compatível,
// agrupado por categoria, embaixo. Enter escolhe, Tab escolhe e encadeia.
//
// O `Icon` mora dentro de `editor.js` e não é exportado: quem monta o popover
// passa `renderIcone(spec)`. Sem ele, cai sempre na bolinha da categoria.
import React, { useEffect, useLayoutEffect, useMemo, useRef, useState } from "react";
import { h } from "trama";
import { sugerir, sugerirOrigem, intermediarios } from "./sugestor.js";
import { lerHistorico } from "./historico.js";
import { corDaCategoria, tintaDaCategoria } from "./papeis.js";
import { moverFoco } from "./proximo-foco.js";

export { moverFoco };

const MARGEM = 8;

// Altura típica do card recém-criado, pelo modo com que ele entra: ainda não
// foi medido, e supor baixo demais faz o bloco nascer por cima do vizinho.
export const alturaNova = (modo) => (modo === "completo" ? 360 : modo === "mini" ? 80 : 220);

// Primeiro `y` livre para o retângulo `q`, descendo a partir de `q.y`: a cada
// card que bate (retângulos com `margem` de folga), o candidato vai para logo
// abaixo dele. Nunca sobe e nunca pula um vão que caiba.
export function primeiroVao(caixas, q, margem = 30) {
  let y = q.y;
  const bate = (c) => q.x < c.x + c.w + margem && c.x < q.x + q.w + margem &&
    y < c.y + c.h + margem && c.y < y + q.h + margem;
  for (let i = 0, c; i < 200 && (c = caixas.find(bate)); i++) y = c.y + c.h + margem;
  return y;
}

// Mesma regra da paleta (editor.js): `label description id`, sem caixa.
const casa = (n, termo) =>
  !termo || `${n.label} ${n.description || ""} ${n.id}`.toLowerCase().includes(termo);

// O motivo de maior peso vira a dica da tagzinha.
export function motivoPrincipal(s, { deLabel, historico, de, origem }) {
  const [chave] = Object.entries(s.motivos || {}).filter(([, v]) => v > 0)
    .sort((a, b) => b[1] - a[1])[0] || [];
  switch (chave) {
    case "transicao": return origem ? `aparece antes de ${deLabel} nos exemplos`
      : `aparece depois de ${deLabel} nos exemplos`;
    case "historico": return `usado por você ${historico?.[origem ? `${s.id}>${de}` : `${de}>${s.id}`] || 1}×`;
    case "relacionado": return origem ? `cita ${deLabel} na ajuda` : `citado na ajuda de ${deLabel}`;
    case "etapa": return origem ? "etapa anterior" : "próxima etapa";
    case "contexto": return "combina com o fluxo";
    default: return "";
  }
}

// `modo`: "proximo" (padrão) sugere o que vem depois de `de`, cuja saída tem
// `tipo`; "origem" sugere o que vem ANTES de `de`, cuja entrada espera `tipo`,
// e aí o Tab não encadeia; "meio" é o próximo restrito aos blocos que também
// alimentam `tipoPara` (inserir numa aresta), e cada item leva a `saida`.
export function Proximo({ catalog, de, tipo, tipoPara, modo = "proximo", presentes, x, y,
                          onEscolher, onFechar, renderIcone }) {
  const [q, setQ] = useState("");
  const [foco, setFoco] = useState(-1);
  const [pos, setPos] = useState({ left: x, top: y });
  const caixa = useRef(null);
  const input = useRef(null);

  const historico = useMemo(() => lerHistorico(), []);
  const ranking = useMemo(() => {
    const ctx = { tipo, presentes: presentes || [], historico };
    if (modo === "origem") return sugerirOrigem(catalog, { ...ctx, para: de });
    const r = sugerir(catalog, { ...ctx, de });
    if (modo !== "meio") return r;
    const cabe = Object.fromEntries(intermediarios(catalog, tipo, tipoPara).map((m) => [m.id, m]));
    return r.filter((s) => cabe[s.id]).map((s) => ({ ...s, saida: cabe[s.id].saida }));
  }, [catalog, de, tipo, tipoPara, modo, presentes, historico]);

  const byId = useMemo(
    () => Object.fromEntries((catalog.nodes || []).map((n) => [n.id, n])), [catalog]);
  const cats = catalog.categories || [];
  const catDe = (n) => cats.find((c) => c.id === n.category);
  const deLabel = byId[de]?.label || de;

  const { sugeridos, grupos, todos } = useMemo(() => {
    const termo = q.trim().toLowerCase();
    const vivos = ranking.filter((s) => byId[s.id] && casa(byId[s.id], termo));
    const sug = vivos.filter((s) => s.score > 0).slice(0, 5);
    const usados = new Set(sug.map((s) => s.id));
    const resto = vivos.filter((s) => !usados.has(s.id));
    const ordem = [...cats.map((c) => c.id), "outros"];
    const m = {};
    resto.forEach((s) => (m[byId[s.id].category || "outros"] ||= []).push(s));
    const gs = Object.keys(m)
      .sort((a, b) => (ordem.indexOf(a) + 1 || 1e9) - (ordem.indexOf(b) + 1 || 1e9))
      .map((cid) => ({ cid, itens: m[cid].sort((a, b) =>
        (byId[a.id].label || a.id).localeCompare(byId[b.id].label || b.id, "pt")) }));
    return { sugeridos: sug, grupos: gs, todos: [...sug, ...gs.flatMap((g) => g.itens)] };
  }, [ranking, q, byId, cats]);

  useEffect(() => { setFoco(todos.length ? 0 : -1); }, [q, todos.length]);
  useEffect(() => { input.current?.focus(); }, []);

  // Cabe na viewport: mede depois de montar e empurra para dentro.
  useLayoutEffect(() => {
    const el = caixa.current;
    if (!el) return;
    const r = el.getBoundingClientRect();
    const vw = window.innerWidth, vh = window.innerHeight;
    setPos({
      left: Math.max(MARGEM, Math.min(x, vw - r.width - MARGEM)),
      top: Math.max(MARGEM, Math.min(y, vh - r.height - MARGEM)),
    });
  }, [x, y, todos.length]);

  // Clique fora fecha. Captura, para ganhar do pane do xyflow.
  useEffect(() => {
    const fora = (e) => { if (caixa.current && !caixa.current.contains(e.target)) onFechar?.(); };
    document.addEventListener("pointerdown", fora, true);
    return () => document.removeEventListener("pointerdown", fora, true);
  }, [onFechar]);

  // Colunas da linha em foco: quantas tagzinhas dividem o mesmo `offsetTop`.
  const colunas = () => {
    const els = caixa.current?.querySelectorAll(".tr-prox-pill");
    const alvo = els?.[foco];
    if (!alvo) return 1;
    const sec = alvo.parentElement;
    return [...sec.children].filter((c) => c.offsetTop === alvo.offsetTop).length || 1;
  };

  const escolher = (i, encadear) => {
    const s = todos[i >= 0 ? i : 0];
    if (s) onEscolher?.(s.id, s.porta, { encadear: encadear && modo === "proximo", saida: s.saida });
  };

  const onKeyDown = (e) => {
    if (e.key === "Escape") { e.preventDefault(); onFechar?.(); return; }
    if (e.key === "Enter") { e.preventDefault(); escolher(foco, false); return; }
    if (e.key === "Tab") { e.preventDefault(); escolher(foco, true); return; }
    if (e.key.startsWith("Arrow")) {
      // Esquerda/direita no input andam o cursor enquanto houver texto.
      if ((e.key === "ArrowLeft" || e.key === "ArrowRight") && q) return;
      e.preventDefault();
      setFoco(moverFoco(foco, todos.length, e.key, colunas()));
    }
  };

  useEffect(() => {
    caixa.current?.querySelectorAll(".tr-prox-pill")[foco]?.scrollIntoView?.({ block: "nearest" });
  }, [foco]);

  let idx = 0;
  const pill = (s, title) => {
    const i = idx++;
    const n = byId[s.id];
    const meta = catDe(n);
    const icone = n.icon && renderIcone ? renderIcone(n) : null;
    return h("button", {
      key: s.id, type: "button", title: title || n.description || n.id,
      className: "tr-prox-pill" + (i === foco ? " tr-prox-on" : ""),
      style: { "--tr-cat": corDaCategoria(meta, n), "--tr-cat-ink": tintaDaCategoria(meta, n) },
      onMouseEnter: () => setFoco(i),
      onClick: () => escolher(i, false),
    }, [
      h("span", { key: "i", className: "tr-prox-icon" },
        icone || h("i", { className: "tr-prox-dot" })),
      h("span", { key: "l", className: "tr-prox-label" }, n.label || n.id),
    ]);
  };

  const ctx = { deLabel, historico, de, origem: modo === "origem" };
  return h("div", {
    ref: caixa, className: "tr-prox nodrag nowheel", role: "dialog", "aria-label": modo === "origem" ? "bloco de origem" : "próximo bloco",
    style: { left: pos.left, top: pos.top }, onKeyDown,
  }, [
    h("input", { key: "q", ref: input, className: "tr-prox-q", placeholder: "digite para filtrar…",
                 "aria-label": "filtrar blocos", value: q, onChange: (e) => setQ(e.target.value) }),
    todos.length === 0
      ? h("div", { key: "v", className: "tr-prox-vazio" }, `nada compatível com ${tipo}`)
      : h("div", { key: "b", className: "tr-prox-body" }, [
          sugeridos.length ? h("section", { key: "s", className: "tr-prox-sec tr-prox-sug" }, [
            h("h5", { key: "t" }, "sugeridos"),
            h("div", { key: "p", className: "tr-prox-pills" },
              sugeridos.map((s) => pill(s, motivoPrincipal(s, ctx)))),
          ]) : null,
          ...grupos.map(({ cid, itens }) => {
            const meta = cats.find((c) => c.id === cid);
            return h("section", { key: cid, className: "tr-prox-sec" }, [
              h("h5", { key: "t" }, [
                h("i", { key: "d", style: { background: corDaCategoria(meta) } }),
                meta?.label || cid,
              ]),
              h("div", { key: "p", className: "tr-prox-pills" }, itens.map((s) => pill(s))),
            ]);
          }),
        ]),
  ]);
}
