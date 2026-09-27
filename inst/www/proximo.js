// Busca de próximo bloco: os próprios resultados ranqueados viram previews.
import React, { useEffect, useMemo, useRef, useState } from "react";
import { h } from "trama";
import { sugerir, sugerirOrigem, intermediarios } from "./sugestor.js";
import { lerHistorico } from "./historico.js";
import { corDaCategoria, tintaDaCategoria } from "./papeis.js";
import { moverFantasma, buscarFantasma, mostrarFantasma } from "./fantasmas.js";
import { moverFoco, alturaNova, primeiroVao, vaoPerto, vaoAoLado } from "./proximo-foco.js";

export { moverFoco, alturaNova, primeiroVao, vaoPerto, vaoAoLado };

export function Proximo({ catalog, de, tipo, tipoPara, modo = "proximo", presentes, x, y, sugestoes = true,
                          onEscolher, onFechar, renderIcone }) {
  const [q, setQ] = useState("");
  const [foco, setFoco] = useState(0);
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
  const porId = useMemo(() => Object.fromEntries((catalog.nodes || []).map((n) => [n.id, n])), [catalog]);
  const categoria = useMemo(() => Object.fromEntries((catalog.categories || []).map((c) => [c.id, c])), [catalog]);
  const resultados = useMemo(() => mostrarFantasma(sugestoes, q)
    ? buscarFantasma(ranking, catalog, q) : [],
    [ranking, catalog, q, sugestoes]);

  useEffect(() => { setFoco(resultados.length ? 0 : -1); }, [q, resultados.length]);
  useEffect(() => { input.current?.focus(); }, []);
  useEffect(() => {
    const el = caixa.current;
    if (!el) return;
    const r = el.getBoundingClientRect();
    setPos({ left: Math.max(8, Math.min(x, innerWidth - r.width - 8)),
      top: Math.max(8, Math.min(y, innerHeight - r.height - 8)) });
  }, [x, y, resultados.length]);
  useEffect(() => {
    const fora = (e) => { if (caixa.current && !caixa.current.contains(e.target)) onFechar?.(); };
    document.addEventListener("pointerdown", fora, true);
    return () => document.removeEventListener("pointerdown", fora, true);
  }, [onFechar]);

  const escolher = (i, encadear = false) => {
    const s = resultados[i >= 0 ? i : 0];
    if (s) onEscolher?.(s.id, s.porta, { encadear: encadear && modo === "proximo", saida: s.saida });
  };
  const onKeyDown = (e) => {
    if (e.key === "Escape") { e.preventDefault(); onFechar?.(); }
    else if (e.key === "Enter" || e.key === "Tab") {
      e.preventDefault(); escolher(foco, false);
    } else if (e.key.startsWith("Arrow")) {
      if ((e.key === "ArrowLeft" || e.key === "ArrowRight") && q) return;
      e.preventDefault(); setFoco((f) => moverFantasma(f, resultados.length, e.key));
    }
  };
  return h("div", { ref: caixa, className: "tr-prox tr-prox-fantasmas nodrag nowheel", role: "dialog",
    "aria-label": "buscar próximo bloco", style: { left: pos.left, top: pos.top }, onKeyDown }, [
    h("input", { key: "q", ref: input, className: "tr-prox-q", placeholder: "buscar bloco…",
      "aria-label": "buscar bloco", value: q, onChange: (e) => setQ(e.target.value) }),
    resultados.length ? h("div", { key: "cards", className: "tr-prox-fantasma-list" }, resultados.map((s, i) => {
      const n = porId[s.id], cat = categoria[n.category];
      const icon = n.icon && renderIcone ? renderIcone(n) : null;
      return h("button", { key: s.id, type: "button", className: "tr-prox-ghost" + (i === foco ? " tr-prox-ghost-on" : ""),
        title: n.description || n.id, "aria-label": n.label || n.id,
        style: { "--tr-cat": corDaCategoria(cat, n), "--tr-cat-ink": tintaDaCategoria(cat, n) },
        onMouseEnter: () => setFoco(i), onClick: () => escolher(i, false) }, [
          h("span", { key: "head", className: "tr-prox-ghost-head" }, [
            h("span", { key: "icon", className: "tr-prox-ghost-icon" }, icon || h("i", { className: "tr-prox-dot" })),
            h("strong", { key: "name" }, n.label || n.id)]),
          h("span", { key: "role", className: "tr-prox-ghost-role" }, cat?.label || n.category || "bloco"),
        ]);
    })) : q ? h("div", { key: "v", className: "tr-prox-vazio" }, "nenhum bloco compatível") : null,
  ]);
}
