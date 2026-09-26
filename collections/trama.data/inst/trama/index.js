// Widgets da coleção `data`. Registrados no runtime do trama, carregados
// depois dele e antes do editor. O núcleo não sabe o que é uma coluna nem uma
// expressão de dplyr — só sabe procurar um widget registrado sob um `kind`.

import React from "react";
import ReactDOM from "react-dom";
import { h, registerWidget, registerRenderer } from "trama";

// `expr`: expressão de R. Textarea em vez de input porque condição e resumo
// crescem, e commit no blur (não a cada tecla) evita mandar op por caractere.
//
// O placeholder era `spec.label`, que repete em cinza o rótulo escrito logo
// acima do campo e não ensina nada. `spec.example` vem do catálogo (o `...` de
// `tr_param()` viaja inteiro até o front), então o campo vazio passa a mostrar
// um valor válido de verdade — que é a única forma de ensinar o formato de um
// campo de texto livre sem abrir a ajuda.
registerWidget("expr", (spec, value, onChange) =>
  h("textarea", {
    className: "nodrag tr-expr", rows: 2, spellCheck: false,
    defaultValue: value ?? spec.default,
    placeholder: spec.example ?? spec.label,
    title: spec.example ? `ex.: ${spec.example}` : undefined,
    onBlur: (e) => { if (e.target.value !== (value ?? spec.default)) onChange(e.target.value); },
  }));

// `cols` saiu daqui: o núcleo (`inst/www/runtime.js`) o registra, com o
// mesmo textarea quando a entrada ainda não tem schema e um select/chips
// alimentado pela tabela de entrada quando tem.

// `path`: caminho de arquivo.
registerWidget("path", (spec, value, onChange) =>
  h("input", {
    type: "text", className: "nodrag", spellCheck: false,
    defaultValue: value ?? spec.default,
    placeholder: spec.example ?? "dados.csv",
    title: spec.example ? `ex.: ${spec.example}` : undefined,
    onBlur: (e) => { if (e.target.value !== (value ?? spec.default)) onChange(e.target.value); },
  }));

// Mesma regra de formatação da tabela compacta do núcleo (`fmt()` em
// runtime.js, não exportada) — duplicar aqui é mais barato que exportar uma
// função de 4 linhas do núcleo só pra uma coleção reusar.
function fmtCell(v) {
  if (v === null || v === undefined) return { text: "NA", num: false, na: true };
  if (typeof v === "number") return { text: Number.isInteger(v) ? String(v) : v.toFixed(3), num: true };
  return { text: String(v), num: false };
}

// Tabela nas normas de apresentação tabular do IBGE (3. ed., 1993): três
// linhas horizontais (topo, sob o cabeçalho, base), NENHUMA vertical, título
// acima. Número alinha à direita pelo separador decimal (Ferreira,
// Estatística Básica) — mesma leitura de "registro" que a tabela compacta já
// usa, só que sem os limites de altura fixa do card.
function AbntTable({ artifact, label }) {
  const rows = (artifact.data && artifact.data.rows) || [];
  const cols = (artifact.data && artifact.data.columns) || (rows[0] ? Object.keys(rows[0]) : []);
  return h("div", null, [
    h("p", { key: "t", className: "tr-abnt-title" }, `Tabela — ${label || "sem título"}`),
    !rows.length || !cols.length
      ? h("div", { key: "e", className: "tr-empty" }, cols.length ? "sem linhas" : "sem colunas")
      : h("table", { key: "tb", className: "tr-abnt" }, [
          h("thead", { key: "h" }, h("tr", null, cols.map((c) => h("th", { key: c }, c)))),
          h("tbody", { key: "b" }, rows.map((r, i) =>
            h("tr", { key: i }, cols.map((c) => {
              const cell = fmtCell(r[c]);
              return h("td", { key: c, className: cell.num ? "tr-abnt-num" : undefined },
                cell.na ? h("span", { className: "tr-na" }, "NA") : cell.text);
            })))),
        ]),
  ]);
}

// Mesma tabela compacta do núcleo (`Table` em runtime.js), com o gesto de
// ampliar que o `Image` do núcleo já usa: Ctrl/⌘+clique abre o overlay, clique
// comum continua arrastando o card — sem disputa, porque um clique com
// modificador não gera deslocamento (o xyflow só inicia drag com deslocamento
// de verdade).
function Table({ artifact, label }) {
  const [aberto, setAberto] = React.useState(false);
  React.useEffect(() => {
    if (!aberto) return;
    const onKey = (e) => { if (e.key === "Escape") setAberto(false); };
    window.addEventListener("keydown", onKey);
    return () => window.removeEventListener("keydown", onKey);
  }, [aberto]);

  const rows = (artifact.data && artifact.data.rows) || [];
  const cols = (artifact.data && artifact.data.columns) || (rows[0] ? Object.keys(rows[0]) : []);
  const vazio = !rows.length || !cols.length;

  const overlay = aberto ? ReactDOM.createPortal(
    h("div", { className: "tr-lightbox", onClick: () => setAberto(false) },
      h("div", { className: "tr-abnt-panel tr-modal", onClick: (e) => e.stopPropagation() }, [
        h("button", { key: "x", className: "tr-lightbox-close", title: "fechar (Esc)",
                      onClick: () => setAberto(false) }, "×"),
        h(AbntTable, { key: "t", artifact, label }),
      ])),
    document.body) : null;

  return h("div", {
    className: "tr-table-wrap",
    title: "ctrl/⌘+clique para ver no padrão ABNT",
    onClick: (e) => { if (e.ctrlKey || e.metaKey) { e.stopPropagation(); setAberto(true); } },
  }, [
    vazio
      ? h("div", { key: "e", className: "tr-empty" }, cols.length ? "sem linhas" : "sem colunas")
      : h("table", { key: "tb", className: "tr-table" }, [
          h("thead", { key: "h" }, h("tr", null, cols.map((c) => h("th", { key: c }, c)))),
          h("tbody", { key: "b" }, rows.map((r, i) =>
            h("tr", { key: i }, cols.map((c) => h("td", { key: c }, fmtCell(r[c]).na
              ? h("span", { className: "tr-na" }, "NA") : fmtCell(r[c]).text))))),
        ]),
    overlay,
  ]);
}

// Renderer próprio (não mais o passthrough pro núcleo): a coleção ganha
// ordenação zero-esforço de manter, e o Ctrl/⌘+clique abre a vista no padrão
// ABNT (IBGE, 1993) que uma tabela compacta de card não comporta.
//
// `id: "preview"` e rótulo "table" são o que a forma de função gerava: o
// documento grava o id da vista, e mudar quebraria a vista salva.
registerRenderer("data/table", { views: [{ id: "preview", label: "table", component: Table }],
                                 expand: AbntTable });
