// Card e widget da coleção `publicar`: a conferência do pacote (com o botão
// Visualizar, ação de card do núcleo) e o editor simples do layout da página.
// As regras do layout moram em layout.js, puro e testado.

import React from "react";
import { h, registerWidget, registerRenderer, pedirAcao } from "trama";
import { ler, gravar, mover, tirar, por, trocar, diferencas, rotuloDoBloco, automatico } from "./layout.js";

const tamanho = (b) => (b >= 1024 ** 2 ? `${(b / 1024 ** 2).toFixed(1)} MB` : b >= 1024 ? `${(b / 1024).toFixed(0)} KB` : `${b} B`);

function Conferencia({ artifact, handle }) {
  const d = artifact?.data || {};
  const [estado, setEstado] = React.useState({ fase: "pronto" });
  const visualizar = (e) => {
    e.stopPropagation();
    // A aba abre no clique (gesto do usuário) e recebe o endereço quando a
    // prévia fica pronta: aberta depois, o navegador a bloquearia.
    const aba = window.open("", "_blank");
    if (aba) aba.document.write("<p style='font:14px sans-serif;padding:2em'>Montando o pacote e renderizando a página com o Quarto…</p>");
    setEstado({ fase: "rodando" });
    pedirAcao(handle, "visualizar").then((r) => {
      if (r.guia) { if (aba) aba.close(); setEstado({ fase: "guia", texto: r.guia }); return; }
      const url = new URL(r.url, window.location.href).toString();
      if (aba) aba.location = url;
      setEstado({ fase: "ok", url, pasta: r.pasta, avisos: r.conferencia?.avisos || [] });
    }).catch((err) => {
      if (aba) aba.close();
      setEstado({ fase: "erro", texto: err.message });
    });
  };
  const lista = (titulo, xs, cls) => xs && xs.length
    ? h("div", { className: `tr-pub-${cls}` }, [h("strong", { key: "t" }, titulo),
        h("ul", { key: "l" }, xs.map((x, i) => h("li", { key: i }, x)))]) : null;
  const avisosExtra = estado.fase === "ok" ? estado.avisos.filter((a) => !(d.avisos || []).includes(a)) : [];
  return h("div", { className: "tr-pub" }, [
    h("div", { key: "cab", className: "tr-pub-cab" }, [
      h("strong", { key: "t" }, d.titulo || "(sem título)"),
      h("span", { key: "v" }, ` · versão ${d.versao || "?"} · ${(d.itens || []).length} itens`),
    ]),
    h("table", { key: "itens", className: "tr-pub-itens" }, h("tbody", null, (d.itens || []).map((it) =>
      h("tr", { key: it.id, title: (it.arquivos || []).join(", ") }, [
        h("td", { key: "t" }, it.titulo),
        h("td", { key: "k", className: "tr-pub-dim" }, it.tipo),
        h("td", { key: "b", className: "tr-pub-dim" }, tamanho(it.bytes || 0)),
        h("td", { key: "p", className: "tr-pub-dim" }, it.na_pagina ? "página" : "só pacote"),
      ])))),
    lista("Erros", d.erros, "erros"),
    lista("Avisos", [...(d.avisos || []), ...avisosExtra], "avisos"),
    h("div", { key: "acao", className: "tr-pub-acao" }, [
      h("button", { key: "b", type: "button", className: "nodrag", disabled: estado.fase === "rodando",
                    onClick: visualizar, title: "monta o pacote, renderiza a página com o Quarto e abre numa aba" },
        estado.fase === "rodando" ? "Renderizando…" : "Visualizar"),
      estado.fase === "ok" ? h("a", { key: "a", href: estado.url, target: "_blank", rel: "noopener" }, "abrir a prévia") : null,
      estado.fase === "erro" ? h("pre", { key: "e", className: "tr-pub-erros" }, estado.texto) : null,
      estado.fase === "guia" ? h("p", { key: "g", className: "tr-pub-avisos" }, estado.texto) : null,
      estado.fase === "ok" && estado.pasta ? h("p", { key: "p", className: "tr-pub-dim" }, estado.pasta) : null,
    ]),
  ]);
}

registerRenderer("publicar/conferencia", Conferencia);

function Layout({ spec, value, onChange, ctx }) {
  const fontes = ctx?.fontes?.itens || [];
  const { blocos, automatico: auto, erro } = ler(value ?? spec.default, fontes);
  const salvar = (b) => onChange(gravar(b));
  const { fora, soltos } = diferencas(blocos, fontes);
  const botao = (key, txt, fn, title) => h("button", { key, type: "button", className: "nodrag", title,
                                                       onClick: (e) => { e.stopPropagation(); fn(); } }, txt);
  return h("div", { className: "tr-pub-layout" }, [
    erro ? h("p", { key: "erro", className: "tr-pub-erros" }, erro) : null,
    h("ol", { key: "l" }, blocos.map((b, i) => h("li", { key: i }, [
      h("div", { key: "r", className: "tr-pub-bloco" }, [
        h("span", { key: "n" }, rotuloDoBloco(b, fontes)),
        botao("c", "↑", () => salvar(mover(blocos, i, -1)), "subir"),
        botao("b", "↓", () => salvar(mover(blocos, i, 1)), "descer"),
        botao("x", "×", () => salvar(tirar(blocos, i)), "tirar da página"),
      ]),
      b.tipo === "texto" ? h("textarea", { key: "t", className: "nodrag", rows: 3, defaultValue: b.texto || "",
        placeholder: "Markdown: ## título, $equação$, listas…",
        onBlur: (e) => { if (e.target.value !== (b.texto || "")) salvar(trocar(blocos, i, { texto: e.target.value })); } }) : null,
      b.tipo === "item" ? h("input", { key: "g", className: "nodrag", type: "text", defaultValue: b.legenda || "",
        placeholder: "legenda (opcional)",
        onBlur: (e) => { if (e.target.value !== (b.legenda || "")) salvar(trocar(blocos, i, { legenda: e.target.value })); } }) : null,
    ]))),
    h("div", { key: "add", className: "tr-pub-add" }, [
      botao("tx", "+ texto", () => salvar(por(blocos, { tipo: "texto", texto: "" }))),
      ...fora.map((f) => botao(`i-${f.nome}`, `+ ${f.rotulo}`, () => salvar(por(blocos, { tipo: "item", id: f.nome })))),
      blocos.some((b) => b.tipo === "cabecalho") ? null : botao("cb", "+ cabeçalho", () => salvar(por(blocos, { tipo: "cabecalho" }, -1))),
      blocos.some((b) => b.tipo === "fluxo") ? null : botao("fl", "+ diagrama", () => salvar(por(blocos, { tipo: "fluxo" }))),
      auto ? null : botao("au", "automático", () => onChange(""), "volta ao layout automático"),
    ]),
    soltos.length ? h("p", { key: "s", className: "tr-pub-dim" }, `não ligados: ${soltos.join(", ")}`) : null,
    auto && !erro ? h("p", { key: "a", className: "tr-pub-dim" }, "automático: mude algo para fixar") : null,
  ]);
}

registerWidget("publicar_layout", (spec, value, onChange, ctx) => h(Layout, { spec, value, onChange, ctx }));
export { automatico };
