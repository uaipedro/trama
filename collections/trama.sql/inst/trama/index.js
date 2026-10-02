// Editor SQL e preview da fonte da coleção `sql`. Registrados no runtime do
// trama, carregados depois dele e antes do editor.
//
// O editor é CodeMirror 6 com a gramática PostgreSQL (SQL padrão), num bundle
// único em `vendor/sql-vendor.js` (montado por `tools/sql-vendor/`). Toda a
// análise — sintaxe, extras do DuckDB recusados, tabela/coluna inexistente —
// mora em `lint.js`, puro e testado; aqui só se liga ao CodeMirror.
//
// O esquema vem do `ctx` do widget: o handle da porta `fonte` traz no preview
// as tabelas e colunas (`sql/source`). Sem fonte ligada, o editor só cobra
// sintaxe.

import React from "react";
import ReactDOM from "react-dom";
import { h, registerWidget, registerRenderer } from "trama";
import {
  EditorView, EditorState, Compartment, Prec, keymap, placeholder, basicSetup,
  sql, PostgreSQL, linter, lintGutter, formatarSql, Parser,
} from "./vendor/sql-vendor.js";
import { analisar, tabelasDoHandle, esquemaCompletar } from "./lint.js";

const parser = new Parser();

function EditorSql({ spec, value, onChange, ctx }) {
  const salvo = value ?? spec.default ?? "";
  const tabelas = React.useMemo(() => tabelasDoHandle(ctx?.entradas?.fonte), [ctx?.entradas?.fonte]);
  const chaveTabelas = JSON.stringify(tabelas);
  const host = React.useRef(null);
  const view = React.useRef(null);
  const cfg = React.useRef(new Compartment());
  const tabelasRef = React.useRef(tabelas);
  const onChangeRef = React.useRef(onChange);
  const [status, setStatus] = React.useState({ ok: true, n: 0, sujo: false });
  const [grande, setGrande] = React.useState(false);
  const salvoRef = React.useRef(salvo);
  const hostGrande = React.useRef(null);
  tabelasRef.current = tabelas;
  onChangeRef.current = onChange;

  // Grava o param só com SQL sem erro de sintaxe: o param mudado dispara a
  // execução, e rodar o que o editor já sabe que é inválido só gera ruído.
  const gravar = React.useCallback(() => {
    const v = view.current;
    if (!v) return true;
    const texto = v.state.doc.toString();
    const r = analisar(texto, { parser, tabelas: tabelasRef.current });
    if (!r.ok) return true;
    if (texto !== salvoRef.current) { salvoRef.current = texto; onChangeRef.current(texto); }
    setStatus((s) => ({ ...s, sujo: false }));
    return true;
  }, []);

  React.useEffect(() => {
    const lint = linter((v) => {
      const r = analisar(v.state.doc.toString(), { parser, tabelas: tabelasRef.current });
      const len = v.state.doc.length;
      setStatus((s) => ({ ...s, ok: r.ok, n: r.diagnosticos.length }));
      return r.diagnosticos.map((d) => ({
        from: Math.min(d.from, len), to: Math.min(d.to, len),
        severity: d.grave ? "error" : "warning", message: d.mensagem,
      }));
    }, { delay: 300 });
    view.current = new EditorView({
      parent: host.current,
      state: EditorState.create({
        doc: salvo,
        extensions: [
          basicSetup,
          cfg.current.of(sql({ dialect: PostgreSQL, upperCaseKeywords: true, schema: esquemaCompletar(tabelas) })),
          lint, lintGutter(),
          placeholder(spec.example ?? "SELECT * FROM tabela"),
          // `Prec.highest`: o `basicSetup` já liga Mod-Enter a "inserir linha".
          Prec.highest(keymap.of([{ key: "Mod-Enter", run: gravar }])),
          EditorView.lineWrapping,
          EditorView.updateListener.of((u) => {
            if (u.docChanged) setStatus((s) => ({ ...s, sujo: u.state.doc.toString() !== salvoRef.current }));
            if (u.focusChanged && !u.view.hasFocus) gravar();
          }),
        ],
      }),
    });
    return () => { view.current?.destroy(); view.current = null; };
  }, []);

  // Fonte trocada ou re-executada: o autocompletar passa a ver o esquema novo.
  React.useEffect(() => {
    view.current?.dispatch({ effects: cfg.current.reconfigure(
      sql({ dialect: PostgreSQL, upperCaseKeywords: true, schema: esquemaCompletar(tabelas) })) });
  }, [chaveTabelas]);

  // Param mudado por fora (desfazer, agente): o editor acompanha se não há
  // edição pendente.
  React.useEffect(() => {
    const v = view.current;
    if (!v || salvo === salvoRef.current) return;
    salvoRef.current = salvo;
    if (v.state.doc.toString() !== salvo) v.dispatch({ changes: { from: 0, to: v.state.doc.length, insert: salvo } });
  }, [salvo]);

  const inserir = (texto) => {
    const v = view.current;
    if (!v) return;
    const { from, to } = v.state.selection.main;
    v.dispatch({ changes: { from, to, insert: texto }, selection: { anchor: from + texto.length } });
    v.focus();
  };
  const formatar = () => {
    const v = view.current;
    if (!v) return;
    try {
      const novo = formatarSql(v.state.doc.toString(), { language: "postgresql", keywordCase: "upper" });
      v.dispatch({ changes: { from: 0, to: v.state.doc.length, insert: novo } });
    } catch (_) { /* SQL inválido: o lint já mostra onde */ }
    v.focus();
  };

  const barra = h("div", { key: "b", className: "tr-sql-barra" }, [
    h("span", { key: "s", className: "tr-sql-status " + (!status.ok ? "tr-sql-erro" : status.n ? "tr-sql-aviso" : "") },
      !status.ok ? "erro" : status.n ? `${status.n} aviso${status.n > 1 ? "s" : ""}`
        : status.sujo ? "Ctrl+Enter roda" : "ok"),
    h("button", { key: "f", type: "button", title: "formatar SQL", onClick: formatar }, "Formatar"),
    h("button", { key: "g", type: "button", title: grande ? "voltar ao card" : "editor grande",
                  onClick: () => setGrande(!grande) }, grande ? "Fechar" : "Ampliar"),
    h("button", { key: "r", type: "button", className: "tr-sql-rodar", disabled: !status.ok,
                  title: "rodar (Ctrl+Enter)", onClick: gravar }, "Rodar"),
  ]);
  const lista = tabelas.length ? h("div", { key: "t", className: "tr-sql-tabelas" }, tabelas.map((t) =>
    h("div", { key: t.nome, className: "tr-sql-tabela" }, [
      h("button", { key: "n", type: "button", title: "inserir nome da tabela", onClick: () => inserir(t.nome) }, t.nome),
      grande ? h("div", { key: "c", className: "tr-sql-colunas" }, t.colunas.map((c) =>
        h("button", { key: c.nome, type: "button", title: c.tipo || "", onClick: () => inserir(c.nome) }, c.nome))) : null,
    ]))) : h("div", { key: "t", className: "tr-sql-tabelas tr-sql-vazio" }, "ligue uma fonte para ver as tabelas");

  // Ampliado, o MESMO EditorView muda de pai (o card fica sob `transform` do
  // React Flow, onde `position: fixed` não escapa): move-se o nó do DOM, sem
  // recriar o editor nem perder desfazer e cursor.
  React.useEffect(() => {
    const v = view.current;
    const alvo = grande ? hostGrande.current : host.current;
    if (v && alvo && v.dom.parentNode !== alvo) { alvo.appendChild(v.dom); v.focus(); }
  }, [grande]);
  React.useEffect(() => {
    if (!grande) return;
    const esc = (e) => { if (e.key === "Escape") { e.preventDefault(); e.stopPropagation(); setGrande(false); } };
    window.addEventListener("keydown", esc, true);
    return () => window.removeEventListener("keydown", esc, true);
  }, [grande]);

  const card = h("div", { className: "tr-sql nodrag nowheel nokey", onClick: (e) => e.stopPropagation() }, [
    h("div", { key: "e", className: "tr-sql-main" }, [h("div", { key: "cm", ref: host, className: "tr-sql-cm" }),
                                                      grande ? null : barra]),
    grande ? null : lista,
  ]);
  // Sempre a mesma raiz (lista com o card e o portal opcional): trocar o tipo
  // da raiz remontaria o card a cada Ampliar.
  return [
    h(React.Fragment, { key: "c" }, card),
    grande ? ReactDOM.createPortal(
      h("div", { className: "tr-lightbox", onClick: () => setGrande(false) },
        h("div", { className: "tr-sql tr-sql-grande nokey", onClick: (e) => e.stopPropagation() }, [
          lista,
          h("div", { key: "e", className: "tr-sql-main" }, [h("div", { key: "cm", ref: hostGrande, className: "tr-sql-cm" }), barra]),
        ])), document.body, "sql-grande") : null,
  ];
}

registerWidget("sql", (spec, value, onChange, ctx) => h(EditorSql, { spec, value, onChange, ctx }));

// Preview da fonte: as tabelas e colunas que ela oferece à consulta.
function Fonte({ artifact }) {
  const tabelas = artifact?.data?.tabelas || [];
  if (!tabelas.length) return h("div", { className: "tr-empty" }, "nenhuma tabela na fonte");
  return h("div", { className: "tr-sql-fonte" }, tabelas.map((t) =>
    h("details", { key: t.nome, open: tabelas.length <= 3 }, [
      h("summary", { key: "s" }, [h("strong", { key: "n" }, t.nome),
                                  h("span", { key: "k" }, ` ${(t.colunas || []).length} colunas`)]),
      h("ul", { key: "c" }, (t.colunas || []).map((c) =>
        h("li", { key: c.nome }, [c.nome, h("em", { key: "t" }, ` ${c.tipo || ""}`)]))),
    ])));
}
registerRenderer("sql/source", Fonte);
