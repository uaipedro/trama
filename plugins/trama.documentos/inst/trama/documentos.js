// Janela Documentos (botão na barra do editor), "Inserir no documento…" no
// menu do card e o selo dos cards que estão num documento (plugin
// `trama.documentos`, #23, #37).
//
// A janela tem o formato da de Bases públicas do editor (`.tr-bases`): à
// esquerda os documentos e os modelos, no meio as regiões do documento (ou a
// lista de "+ Adicionar bloco"), à direita as ações e o que vai na publicação.
//
// Só desenha: o estado é o redutor de `painel.js`, e quem mexe nos arquivos
// é o R (`R/painel.R`). Canal próprio: `input$trama_documentos` ({acao, ...})
// para cima, mensagem `trama_documentos` ({tipo, ...}) para baixo.

import React from "react";
import ReactDOM from "react-dom";
import { h, registerToolbarItem, registerCardMenuItem, registerCardBadge, refreshCardBadges, editorAction } from "trama";
import { INICIAL, ESTADOS, reduzir, resumo, pendencias, pedidoInserir, ehTabela, nomeValido, resolucoes,
         textoOcupado, formatosExportar, nomeSaida, pedidoPublicar, filtrarCards, regioesDoCard, indiceSelos,
         tituloSelo, vaiSair } from "./painel.js";
import { linhas as etapasEnvio, links as linksEnvio } from "./envio.js";
import { AbaModelos } from "./modelos-ui.js";

// Shiny: `addCustomMessageHandler` existe assim que shiny.js carrega, mas
// `setInputValue` só depois que a sessão conecta. Espera por polling, como o
// editor e o trama.github.
const ouvintes = new Set();
function quando(pronto, fn, tentativas = 400) {
  if (pronto()) return fn();
  if (tentativas > 0) setTimeout(() => quando(pronto, fn, tentativas - 1), 50);
}
quando(() => !!window.Shiny?.addCustomMessageHandler, () =>
  window.Shiny.addCustomMessageHandler("trama_documentos", (m) => ouvintes.forEach((f) => f(m))));
const enviar = (acao, extra = {}) => quando(() => typeof window.Shiny?.setInputValue === "function", () =>
  window.Shiny.setInputValue("trama_documentos", { ...extra, acao, t: Date.now() }, { priority: "event" }));

// Item do menu do card: abre o painel no modo "inserir" com este card.
registerCardMenuItem("documentos-inserir", {
  rotulo: "Inserir no documento…",
  titulo: "Põe o resultado deste bloco num documento do projeto (relatório, artigo…)",
  quando: (card) => (card.saidas || []).length > 0,
  acao: (card) => ouvintes.forEach((f) => f({ tipo: "inserir_card", card })),
});

// Selo no cabeçalho do card que está num documento do fluxo na tela. O
// índice é refeito a cada lista do R (que vem depois de inserir, resolver,
// atualizar e ao abrir a janela); o clique abre a janela na região.
let selosPorNo = new Map();
registerCardBadge("documentos", {
  selo: (card) => {
    const l = selosPorNo.get(card.id);
    if (!l?.length) return null;
    return { icone: "file-text", titulo: tituloSelo(l),
             acao: () => ouvintes.forEach((f) => f({ tipo: "abrir_regiao", documento: l[0].documento, regiao: l[0].regiao })) };
  },
});

// Os cards do fluxo na tela, como o editor os dá (`infoDoCard`).
function lerCards() {
  let lista = [];
  editorAction("cards", (l) => { lista = l || []; });
  return lista;
}

// Outro plugin (o menu GitHub) pede para abrir um documento: evento do
// navegador, sem um plugin importar o outro.
window.addEventListener("trama:documentos", (e) => {
  if (e.detail?.acao === "abrir") ouvintes.forEach((f) => f({ tipo: "abrir_documento", documento: e.detail.documento }));
});

const abrirLink = (url) => window.open(url, "_blank", "noopener");

// Ícone de documento (traço, `currentColor`, como os da barra).
const Icone = () => h("svg", { className: "tr-ic", width: 18, height: 18, viewBox: "0 0 24 24", fill: "none",
  stroke: "currentColor", strokeWidth: 1.8, strokeLinecap: "round", strokeLinejoin: "round", "aria-hidden": true }, [
  h("path", { key: "a", d: "M14 3H7a2 2 0 0 0-2 2v14a2 2 0 0 0 2 2h10a2 2 0 0 0 2-2V8z" }),
  h("path", { key: "b", d: "M14 3v5h5M9 13h6M9 17h6" })]);

// Ícone do bloco como o editor desenha: o conjunto (sprite ao lado do
// runtime) ou a geometria que a coleção entrega.
let SPRITE = null;
try { SPRITE = new URL("vendor/lucide.svg", import.meta.resolve("trama")).href; } catch (e) { SPRITE = null; }
function IconeBloco({ icone }) {
  const p = { className: "tr-doc-ic-bloco", viewBox: "0 0 24 24", "aria-hidden": true };
  if (icone?.kind === "set" && SPRITE) return h("svg", p, h("use", { href: `${SPRITE}#${icone.value}` }));
  if (icone?.kind === "svg") return h("svg", { ...p, dangerouslySetInnerHTML: { __html: icone.value } });
  return h("span", { className: "tr-doc-ic-bloco tr-doc-ic-vazio", "aria-hidden": true });
}

function Botao({ primario, perigo, children, ...p }) {
  const classe = "tr-doc-btn" + (primario ? " tr-doc-primario" : "") + (perigo ? " tr-doc-perigo" : "");
  return h("button", { type: "button", className: classe, ...p }, children);
}
const Link = ({ children, ...p }) => h("button", { type: "button", className: "tr-doc-link", ...p }, children);
const Aviso = ({ texto, tom }) => texto
  ? h("div", { className: tom === "erro" ? "tr-doc-erro" : "tr-doc-ok", role: tom === "erro" ? "alert" : "status" }, texto)
  : null;
const Chip = ({ estado }) => {
  const e = ESTADOS[estado] || { rotulo: estado, tom: "dim" };
  return h("span", { className: `tr-doc-chip tr-doc-chip-${e.tom}` }, e.rotulo);
};
const Campo = ({ rotulo, children }) => h("label", { className: "tr-doc-campo" }, [
  h("span", { key: "r", className: "tr-doc-dim" }, rotulo), children]);

function Guia({ passos, voltar }) {
  return h("div", { className: "tr-doc-guia" }, [
    h("div", { key: "t", className: "tr-doc-titulo" }, "Instalar o Quarto"),
    h("ol", { key: "l", className: "tr-doc-passos" }, passos.map((p, i) => h("li", { key: i }, [
      h("strong", { key: "t" }, p.titulo), h("p", { key: "x" }, p.texto),
      p.link ? h(Link, { key: "a", onClick: () => abrirLink(p.link) }, `${p.rotulo || "Abrir"} ↗`) : null]))),
    h(Botao, { key: "v", onClick: voltar }, "Voltar"),
  ]);
}

function Novo({ s, despachar }) {
  const n = s.novo;
  const ok = nomeValido(n.nome);
  const criar = () => ok && enviar("criar", { nome: n.nome, titulo: n.titulo || null, modelo: n.modelo, perfil: n.perfil });
  const campo = (c) => (e) => despachar({ tipo: "novo_campo", campo: c, valor: e.target.value });
  return h("div", { className: "tr-doc-caixa" }, [
    h("div", { key: "t", className: "tr-doc-titulo" }, "Novo documento"),
    h(Campo, { key: "n", rotulo: "Nome do arquivo" },
      h("input", { type: "text", value: n.nome, placeholder: "relatorio-final", onChange: campo("nome"), autoFocus: true,
                   onKeyDown: (e) => { if (e.key === "Enter") criar(); } })),
    n.nome && !ok ? h("div", { key: "x", className: "tr-doc-alerta" }, "Use letras, dígitos, - e _ (sem espaço).") : null,
    h(Campo, { key: "ti", rotulo: "Título" },
      h("input", { type: "text", value: n.titulo, placeholder: "Relatório da análise", onChange: campo("titulo") })),
    h(Campo, { key: "m", rotulo: "Modelo" },
      h("select", { value: n.modelo, onChange: campo("modelo") },
        s.modelos.map((m) => h("option", { key: m.nome, value: m.nome, disabled: m.valido === false },
          `${m.titulo} (${m.formatos})${m.valido === false ? " — com problema" : ""}`)))),
    h(Campo, { key: "p", rotulo: "Autor" },
      h("select", { value: n.perfil, onChange: campo("perfil") }, [
        ...s.perfis.map((p) => h("option", { key: p.perfil, value: p.perfil }, `${p.nome} (${p.perfil})`)),
        h("option", { key: "", value: "" }, "Sem autor")])),
    s.perfis.length ? null : h("p", { key: "sp", className: "tr-doc-dim tr-doc-nota" },
      "Sem perfil de autor: crie um no R com tr_documento_perfil_salvar()."),
    h("div", { key: "b", className: "tr-doc-linha" }, [
      h(Botao, { key: "c", primario: true, disabled: !ok, onClick: criar }, "Criar"),
      h(Botao, { key: "x", onClick: () => despachar({ tipo: "novo_cancelar" }) }, "Cancelar")]),
  ]);
}

function Inserir({ s, despachar }) {
  const ins = s.inserir;
  const pedido = pedidoInserir(ins);
  const campo = (c, f = (e) => e.target.value) => (e) => despachar({ tipo: "inserir_campo", campo: c, valor: f(e) });
  const ir = () => { if (!pedido) return; despachar({ tipo: "inserir_enviando" }); enviar("inserir", pedido); };
  const tabela = ehTabela(ins);
  return h("div", { className: "tr-doc-inserir" }, [
    h("div", { key: "id", className: "tr-bases-id" }, `${ins.card.tipo || ""} · ${ins.card.id}`),
    h("h3", { key: "t", className: "tr-doc-h3" }, [h(IconeBloco, { key: "i", icone: ins.card.icone }),
                                                   h("span", { key: "r" }, `Inserir “${ins.card.rotulo}”`)]),
    s.documentos.length
      ? h(Campo, { key: "d", rotulo: "No documento" },
          h("select", { value: ins.documento || "", onChange: campo("documento") }, [
            h("option", { key: "", value: "" }, "Escolha…"),
            ...s.documentos.map((d) => h("option", { key: d.caminho, value: d.caminho }, d.nome))]))
      : h("p", { key: "d", className: "tr-doc-dim" }, "O projeto ainda não tem documento: crie um à esquerda."),
    (ins.card.saidas || []).length > 1
      ? h(Campo, { key: "p", rotulo: "Saída" },
          h("select", { value: ins.porta || "", onChange: campo("porta") },
            ins.card.saidas.map((p) => h("option", { key: p.porta, value: p.porta }, `${p.porta} (${p.tipo})`))))
      : null,
    h(Campo, { key: "m", rotulo: "Modo" }, h("div", { className: "tr-doc-seg" }, [
      h("button", { key: "r", type: "button", className: "tr-doc-on", title: "O resultado do bloco (figura, tabela, teste), como no card" }, "Resultado"),
      h("button", { key: "c", type: "button", disabled: true,
                    title: "O código do bloco no documento chega com o modo de reprodutibilidade" }, "Código")])),
    tabela ? h(Campo, { key: "l", rotulo: "Linhas mostradas (padrão 20)" },
      h("input", { type: "number", min: 1, value: ins.linhas, placeholder: "20", onChange: campo("linhas") })) : null,
    tabela ? h("label", { key: "c", className: "tr-doc-check" }, [
      h("input", { key: "i", type: "checkbox", checked: !!ins.completa, onChange: campo("completa", (e) => e.target.checked) }),
      h("span", { key: "t" }, "Levar a tabela inteira para o arquivo CSV ao lado do documento")]) : null,
    ins.pedirAceite ? h("label", { key: "a", className: "tr-doc-check tr-doc-aceite" }, [
      h("input", { key: "i", type: "checkbox", checked: !!ins.aceitar, onChange: campo("aceitar", (e) => e.target.checked) }),
      h("span", { key: "t" }, "Aceito pôr no documento um modelo que leva linhas dos dados de treino")]) : null,
    h("p", { key: "n", className: "tr-doc-dim tr-doc-nota" },
      "A região entra no fim do documento; no seu editor de texto, mova-a para onde quiser."),
    h("div", { key: "b", className: "tr-dialog-actions tr-bases-acoes" }, [
      h("button", { key: "x", type: "button", onClick: () => despachar({ tipo: "inserir_cancelar" }) }, "Cancelar"),
      h("button", { key: "i", type: "button", className: "tr-primario",
                    disabled: !pedido || (ins.pedirAceite && !ins.aceitar), onClick: ir },
        ins.enviando ? "Inserindo…" : "Inserir")]),
  ]);
}

// O nome de uma região: o rótulo do card, se ele está no fluxo na tela.
const nomeRegiao = (r, porId) => porId.get(r.no)?.rotulo || `#trama-${r.regiao}`;

function Regioes({ s, doc, porId, despachar }) {
  const regs = s.regioes[doc.caminho];
  const foco = React.useRef(null);
  React.useEffect(() => { foco.current?.scrollIntoView({ block: "nearest" }); }, [s.foco, regs]);
  if (!regs) return h("p", { className: "tr-bases-vazio" }, "Lendo as regiões…");
  if (!regs.length) return h("div", { className: "tr-bases-vazio" }, [
    h("p", { key: "a" }, "Nenhum resultado do trama neste documento ainda."),
    h("p", { key: "b" }, "Use “+ Adicionar bloco” acima, ou o botão direito num card → “Inserir no documento…”.")]);
  const resolver = (r, como) => {
    if (como === "manter") return enviar("resolver", { documento: doc.caminho, regiao: r.regiao, como });
    despachar({ tipo: "pedir_confirmacao", documento: doc.caminho, regiao: r.regiao, como });
  };
  const irAoCard = (no) => { if (editorAction("irAoBloco", no)) despachar({ tipo: "fechar" }); };
  return h("ol", { className: "tr-doc-regioes" }, regs.map((r) => {
    const card = porId.get(r.no);
    const emFoco = s.foco === r.regiao;
    return h("li", { key: `${r.regiao}-${r.linha}`, ref: emFoco ? foco : null,
                     className: "tr-bases-item tr-doc-regiao" + (emFoco ? " tr-on" : "") }, [
      h("span", { key: "t", className: "tr-bases-item-t tr-doc-linha" }, [
        h(IconeBloco, { key: "i", icone: card?.icone }),
        h("span", { key: "n", className: "tr-doc-cresce" }, nomeRegiao(r, porId)),
        h(Chip, { key: "c", estado: r.estado })]),
      h("span", { key: "m", className: "tr-bases-item-m" }, [
        h("code", { key: "c" }, `#trama-${r.regiao}`),
        h("span", { key: "l" }, `linha ${r.linha}`),
        card?.tipo ? h("span", { key: "ti" }, card.tipo) : null]),
      h("span", { key: "a", className: "tr-doc-linha tr-doc-acoes" }, [
        card ? h(Link, { key: "ir", title: "Fecha a janela e seleciona o card no canvas", onClick: () => irAoCard(r.no) }, "Ir ao card") : null,
        ...resolucoes(r.estado).map((x) => h(Link, { key: x.como, onClick: () => resolver(r, x.como) }, x.rotulo))]),
    ]);
  }));
}

function Confirmar({ c }) {
  const texto = c.como === "remover" ? `Apagar a região #trama-${c.regiao} do texto?`
    : `Trocar o que está no texto em #trama-${c.regiao} pela versão do trama?`;
  const ir = (forte) => enviar("resolver", { documento: c.documento, regiao: c.regiao, como: c.como, confirmar: forte });
  return h("div", { className: "tr-doc-confirmar" }, [
    h("p", { key: "p" }, c.forte ? "A região tem cercas ::: dentro: confira acima as linhas que sairiam." : texto),
    h("div", { key: "b", className: "tr-doc-linha" }, [
      h(Botao, { key: "s", perigo: true, onClick: () => ir(!!c.forte) }, c.forte ? "Apagar mesmo assim" : "Sim"),
      h(Botao, { key: "n", onClick: () => ouvintes.forEach((f) => f({ tipo: "cancelar_confirmacao" })) }, "Cancelar")]),
  ]);
}

// "+ Adicionar bloco": os cards do fluxo com saída, com busca; os que já
// estão no documento levam o selo. Escolher abre o inserir à direita.
function Adicionar({ s, doc, despachar }) {
  const a = s.adicionar;
  const regs = s.regioes[doc.caminho] || [];
  const lista = filtrarCards(a.cards, a.busca);
  const escolhido = s.inserir?.card?.id;
  const escolher = (c) => despachar({ tipo: "inserir_card", card: c, documento: doc.caminho });
  return h("div", { className: "tr-doc-meio" }, [
    h("div", { key: "c", className: "tr-doc-meio-cab" }, [
      h("div", { key: "t", className: "tr-doc-titulo tr-doc-cresce" }, `Adicionar bloco a ${doc.nome}`),
      h(Link, { key: "p", onClick: () => despachar({ tipo: "adicionar_fechar" }) }, "Voltar às regiões")]),
    h("input", { key: "q", className: "tr-dialog-name tr-bases-busca tr-doc-busca", value: a.busca, autoFocus: true,
                 placeholder: "buscar bloco por nome ou tipo…",
                 onChange: (e) => despachar({ tipo: "adicionar_busca", busca: e.target.value }),
                 onKeyDown: (e) => { if (e.key === "Enter" && lista.length === 1) escolher(lista[0]); } }),
    h("div", { key: "l", className: "tr-doc-rolo", role: "listbox", "aria-label": "Blocos do fluxo" },
      lista.length ? lista.map((c) => {
        const ja = regioesDoCard(regs, c.id).length > 0;
        return h("button", { key: c.id, type: "button", role: "option", "aria-selected": c.id === escolhido,
                             className: "tr-bases-item" + (c.id === escolhido ? " tr-on" : ""), onClick: () => escolher(c) }, [
          h("span", { key: "t", className: "tr-bases-item-t tr-doc-linha" }, [
            h(IconeBloco, { key: "i", icone: c.icone }), h("span", { key: "r", className: "tr-doc-cresce" }, c.rotulo)]),
          h("span", { key: "m", className: "tr-bases-item-m" }, [
            h("code", { key: "t" }, c.tipo || ""),
            h("span", { key: "id" }, c.id),
            ja ? h("span", { key: "s", className: "tr-bases-selo tr-ok", title: "Este bloco já tem uma região no documento" },
                   "no documento") : null]),
        ]);
      }) : h("p", { className: "tr-bases-vazio" },
        (a.cards || []).length ? "Nenhum bloco com essa busca." : "O fluxo na tela não tem bloco com resultado para inserir.")),
  ]);
}

// Publicar no GitHub: o repositório, os avisos e o envio. O que sai vem logo
// acima (`VaiSair`). Bloqueio desliga o botão; o R pede confirmação antes de
// criar repositório privado ou de sobrescrever um repositório alheio.
function Publicar({ s, despachar }) {
  const p = s.publicar;
  const c = p.conferencia;
  const pedido = pedidoPublicar(p);
  const ir = (confirmado) => {
    const ped = pedidoPublicar(p, confirmado);
    if (!ped) return;
    despachar({ tipo: "publicar_enviando" });
    enviar("publicar", ped);
  };
  const fechar = () => despachar({ tipo: "publicar_cancelar" });
  if (p.carregando || !c) return h("p", { className: "tr-doc-dim" }, "Conferindo o documento…");
  const lista = (itens, classe) => (itens || []).length
    ? h("ul", { className: classe }, itens.map((x, i) => h("li", { key: i }, x))) : null;
  return h("div", { className: "tr-doc-caixa tr-doc-publicar" }, [
    h("p", { key: "o", className: "tr-doc-dim tr-doc-nota" },
      `“${c.titulo || "sem título"}” · ${(c.autores || []).join("; ") || "sem autor"} · licença ${c.licenca}` +
      ` · próxima versão: ${c.tipo_versao}` + (c.ultima ? ` (última: ${c.ultima.versao})` : "") +
      ". Tudo vai num pacote RO-Crate; o GitHub Actions renderiza a página e cria a release."),
    h(Campo, { key: "p", rotulo: c.registrado ? "Repositório (o desta publicação)" : "Repositório (novo, público)" },
      h("input", { type: "text", value: p.repo, disabled: p.enviando,
                   onChange: (e) => despachar({ tipo: "publicar_campo", campo: "repo", valor: e.target.value }) })),
    lista(c.erros, "tr-doc-erro tr-doc-lista-msg"),
    lista(c.avisos, "tr-doc-avisos tr-doc-lista-msg"),
    p.confirmar ? h("div", { key: "c", className: "tr-doc-confirmar", role: "alertdialog" }, [
      h("p", { key: "t" }, p.confirmar),
      h("div", { key: "b", className: "tr-doc-linha" }, [
        h(Botao, { key: "s", primario: true, onClick: () => ir(true) }, "Sim, publicar"),
        h(Botao, { key: "n", onClick: fechar }, "Cancelar")])]) : null,
    p.confirmar ? null : h("div", { key: "b", className: "tr-doc-linha" }, [
      h(Botao, { key: "i", primario: true, disabled: !pedido, onClick: () => ir(false),
                 title: (c.erros || []).length ? "Resolva o que está em vermelho" : "Atualiza, monta o pacote e envia" },
        p.enviando ? "Enviando…" : "Publicar"),
      h(Botao, { key: "x", onClick: fechar }, "Cancelar")]),
  ]);
}

// Onde está o último envio do documento: etapas e links.
function Envio({ e }) {
  return h("div", { className: "tr-doc-envio" }, [
    h("div", { key: "t", className: "tr-doc-linha" }, [
      h("strong", { key: "v" }, `GitHub · ${e.tag}`),
      h("span", { key: "d", className: "tr-doc-dim tr-doc-cresce" }, e.detalhe || "")]),
    h("ol", { key: "l", className: "tr-doc-etapas" }, etapasEnvio(e).map((l) =>
      h("li", { key: l.id, className: `tr-doc-etapa-${l.situacao}` }, l.rotulo))),
    h("div", { key: "a", className: "tr-doc-linha tr-doc-acoes" }, linksEnvio(e).map((l) =>
      h(Link, { key: l.rotulo, onClick: () => abrirLink(l.url) }, `${l.rotulo} ↗`))),
  ]);
}

// O que vai na publicação: a lista da conferência do R, quando já veio; antes
// dela, as regiões do documento.
function VaiSair({ s, doc, porId }) {
  const conf = s.publicar?.documento === doc.caminho ? s.publicar.conferencia : null;
  const itens = conf ? (conf.regioes || []).map((r) => ({ chave: r.chave, nome: r.titulo || `#trama-${r.chave}` }))
    : (s.regioes[doc.caminho] || []).filter((r) => r.estado !== "duplicada")
        .map((r) => ({ chave: r.regiao, nome: nomeRegiao(r, porId) }));
  return h("div", { className: "tr-doc-vai" }, [
    h("p", { key: "t", className: "tr-doc-vai-t" }, vaiSair(itens.length)),
    itens.length ? h("ul", { key: "l", className: "tr-doc-vai-l" }, itens.map((x) =>
      h("li", { key: x.chave }, [h("span", { key: "n" }, x.nome), h("code", { key: "c" }, `#trama-${x.chave}`)]))) : null,
  ]);
}

function Lado({ s, doc, porId, despachar }) {
  if (s.inserir) return h("div", { className: "tr-bases-det" }, h(Inserir, { s, despachar }));
  if (s.novo) return h("div", { className: "tr-bases-vazio" },
    "O documento nasce do modelo, com o autor preenchido. Depois é só inserir os resultados.");
  if (!doc) return h("div", { className: "tr-bases-vazio" }, "Escolha ou crie um documento à esquerda.");
  const ocupado = s.ocupado[doc.caminho];
  const exp = s.exportados[doc.caminho];
  const modelo = s.modelos.find((m) => m.nome === doc.modelo);
  const render = (acao, extra = {}) => s.quarto ? enviar(acao, { documento: doc.caminho, ...extra }) : enviar("guia_quarto");
  const abrir = (alvo, extra = {}) => enviar("abrir", { documento: doc.caminho, alvo, ...extra });
  const dd = (k, rot, v) => v ? h(React.Fragment, { key: k }, h("dt", null, rot), h("dd", null, v)) : null;
  return h("div", { className: "tr-bases-det" }, [
    h("div", { key: "id", className: "tr-bases-id" }, doc.caminho),
    h("h3", { key: "t" }, doc.nome),
    h("dl", { key: "m", className: "tr-bases-meta" }, [
      dd("mo", "Modelo", modelo?.titulo || doc.modelo),
      dd("fo", "Formatos", formatosExportar(doc).map(([, r]) => r).join(", ")),
      dd("fl", "Fluxo", doc.fluxo && doc.fluxo !== "main" ? doc.fluxo : null),
      dd("re", "Regiões", resumo(doc.resumo))]),
    ocupado ? h("div", { key: "o", className: "tr-doc-ocupado", role: "status" }, textoOcupado(ocupado)) : null,
    h("div", { key: "a", className: "tr-doc-linha" }, [
      h(Botao, { key: "u", primario: true, disabled: !!ocupado, title: "Refaz só as regiões cujo resultado mudou",
                 onClick: () => enviar("atualizar", { documento: doc.caminho }) }, "Atualizar"),
      h(Botao, { key: "v", disabled: !!ocupado, title: "Renderiza em HTML e abre numa aba nova",
                 onClick: () => render("visualizar") }, "Visualizar"),
      h("select", { key: "e", className: "tr-doc-exportar", value: "", disabled: !!ocupado, "aria-label": "Exportar",
                    onChange: (e) => { if (e.target.value) render("exportar", { formato: e.target.value }); } }, [
        h("option", { key: "", value: "" }, "Exportar…"),
        ...formatosExportar(doc).map(([k, rotulo]) => h("option", { key: k, value: k }, rotulo))])]),
    exp ? h("div", { key: "x", className: "tr-doc-linha tr-doc-acoes" }, [
      h("span", { key: "t", className: "tr-doc-dim tr-doc-cresce" }, nomeSaida(exp.arquivo)),
      h(Link, { key: "a", onClick: () => abrir("saida", { arquivo: exp.arquivo }) }, "Abrir arquivo"),
      h(Link, { key: "p", onClick: () => abrir("pasta") }, "Pasta")]) : null,
    h("div", { key: "l", className: "tr-doc-linha tr-doc-acoes" }, [
      h(Link, { key: "e", title: "Abre o .qmd no seu editor de texto (RStudio, Positron, VS Code…)",
                onClick: () => abrir("documento") }, "Abrir no editor de texto"),
      h(Link, { key: "p", onClick: () => abrir("pasta") }, "Abrir a pasta")]),
    h("section", { key: "pub", className: "tr-doc-secao" }, [
      h("div", { key: "t", className: "tr-doc-linha" }, [
        h("div", { key: "t", className: "tr-doc-titulo tr-doc-cresce" }, "Publicar no GitHub"),
        s.publicar?.documento === doc.caminho ? null : h(Botao, { key: "b",
          title: "Confere o documento: pacote reprodutível, release e página no GitHub Pages",
          onClick: () => { despachar({ tipo: "publicar_abrir", documento: doc.caminho });
                           enviar("publicar_conferir", { documento: doc.caminho }); } }, "Preparar…")]),
      h(VaiSair, { key: "v", s, doc, porId }),
      s.publicar?.documento === doc.caminho ? h(Publicar, { key: "p", s, despachar }) : null,
      s.envios[doc.caminho] ? h(Envio, { key: "env", e: s.envios[doc.caminho] }) : null]),
  ]);
}

function Meio({ s, doc, porId, despachar }) {
  if (s.novo) return h("div", { className: "tr-doc-meio" }, h(Novo, { s, despachar }));
  if (s.fase !== "pronto") return h("p", { className: "tr-bases-vazio" }, "Lendo os documentos…");
  if (!doc) return h("div", { className: "tr-bases-vazio" }, [
    h("p", { key: "a" }, "Um documento é um .qmd seu: você escreve o texto, e o trama mantém atualizados os resultados que você insere."),
    h(Botao, { key: "n", primario: true, onClick: () => despachar({ tipo: "novo_abrir" }) }, "+ Novo documento")]);
  const outroFluxo = (doc.fluxo || "main") !== s.fluxo;
  if (s.adicionar && !outroFluxo) return h(Adicionar, { s, doc, despachar });
  return h("div", { className: "tr-doc-meio" }, [
    h("div", { key: "c", className: "tr-doc-meio-cab" }, [
      h("div", { key: "t", className: "tr-doc-titulo tr-doc-cresce" }, "Regiões, na ordem do texto"),
      h(Botao, { key: "a", primario: true, disabled: outroFluxo,
                 title: outroFluxo ? `Este documento é do fluxo ${doc.fluxo}: abra esse fluxo para adicionar blocos`
                                   : "Escolhe um bloco do fluxo e põe o resultado dele no documento",
                 onClick: () => despachar({ tipo: "adicionar_abrir", cards: lerCards() }) }, "+ Adicionar bloco")]),
    outroFluxo ? h("p", { key: "f", className: "tr-doc-dim tr-doc-nota" },
      `Este documento mostra o fluxo “${doc.fluxo}”, não o que está na tela.`) : null,
    s.confirmar ? h(Confirmar, { key: "x", c: s.confirmar }) : null,
    h("div", { key: "r", className: "tr-doc-rolo" }, h(Regioes, { s, doc, porId, despachar })),
  ]);
}

function Navegacao({ s, despachar }) {
  const modelos = s.aba === "modelos";
  const escolher = (d) => {
    if (modelos) despachar({ tipo: "aba", aba: "documentos" });
    despachar({ tipo: "selecionar", documento: d.caminho });
    enviar("estado", { documento: d.caminho });
  };
  return h("nav", { className: "tr-bases-filtros" }, [
    h("div", { key: "tt", className: "tr-pop-rot" }, "Documentos"),
    ...s.documentos.map((d) => {
      const total = d.resumo && !d.resumo.erro ? Object.values(d.resumo).reduce((a, b) => a + b, 0) : 0;
      const atencao = pendencias([d]);
      return h("button", { key: d.caminho, type: "button", title: resumo(d.resumo),
                           className: "tr-bases-filtro" + (!modelos && d.caminho === s.selecionado ? " tr-on" : ""),
                           onClick: () => escolher(d) }, [
        h("span", { key: "a", className: "tr-doc-nome-nav" }, d.nome),
        h("small", { key: "b", className: atencao ? "tr-doc-atencao" : "" }, atencao ? `${total} · ${atencao}!` : total)]);
    }),
    h("button", { key: "nv", type: "button", className: "tr-bases-filtro tr-doc-novo" + (s.novo ? " tr-on" : ""),
                  onClick: () => { if (modelos) despachar({ tipo: "aba", aba: "documentos" }); despachar({ tipo: "novo_abrir" }); } },
      h("span", null, "+ Novo documento")),
    h("div", { key: "mt", className: "tr-pop-rot" }, "Modelos"),
    h("button", { key: "m", type: "button", className: "tr-bases-filtro" + (modelos ? " tr-on" : ""),
                  title: "Criar, editar, duplicar, importar e exportar modelos",
                  onClick: () => despachar({ tipo: "aba", aba: modelos ? "documentos" : "modelos" }) },
      [h("span", { key: "a" }, "Galeria de modelos"), h("small", { key: "b" }, s.modelos.length)]),
  ]);
}

function Janela({ s, despachar }) {
  const fechar = () => despachar({ tipo: "fechar" });
  React.useEffect(() => {
    const esc = (e) => { if (e.key === "Escape") fechar(); };
    window.addEventListener("keydown", esc);
    return () => window.removeEventListener("keydown", esc);
  }, []);
  const porId = new Map(lerCards().map((c) => [c.id, c]));
  const doc = s.documentos.find((d) => d.caminho === s.selecionado) || null;
  const n = pendencias(s.documentos);
  const modelos = s.aba === "modelos";
  const contagem = s.fase !== "pronto" ? "" : `${s.documentos.length} ${s.documentos.length === 1 ? "documento" : "documentos"}` +
    (n ? ` · ${n} ${n === 1 ? "região pede" : "regiões pedem"} atenção` : "");
  return h("div", { className: "tr-lightbox tr-modal", onClick: (e) => { if (e.target === e.currentTarget) fechar(); } },
    h("div", { className: "tr-dialog tr-bases tr-doc tr-doc-janela", role: "dialog", "aria-modal": "true", "aria-label": "Documentos" }, [
      h("div", { key: "top", className: "tr-bases-top" }, [
        h(Icone, { key: "i" }),
        h("div", { key: "t", className: "tr-bases-titulo tr-doc-cresce" }, [
          h("strong", { key: "a" }, modelos ? "Documentos · Modelos" : "Documentos"),
          h("span", { key: "b" }, contagem)]),
        h("button", { key: "x", className: "tr-dialog-close", title: "fechar (Esc)", onClick: fechar }, "×")]),
      modelos ? null : h("div", { key: "f", className: "tr-doc-faixa" }, [
        !s.quarto ? h("div", { key: "q", className: "tr-doc-aviso" }, [
          h("span", { key: "t" }, "O Quarto não está instalado: dá para inserir e atualizar, mas não para visualizar e exportar. "),
          h(Link, { key: "g", onClick: () => enviar("guia_quarto") }, "Como instalar")]) : null,
        h(Aviso, { key: "e", texto: s.erro, tom: "erro" }),
        h(Aviso, { key: "a", texto: s.aviso, tom: "ok" })]),
      s.guia ? h("div", { key: "g", className: "tr-doc-guia-corpo" },
        h(Guia, { passos: s.guia, voltar: () => despachar({ tipo: "fechar_guia" }) }))
        : h("div", { key: "corpo", className: "tr-bases-corpo" }, [
          h(Navegacao, { key: "n", s, despachar }),
          modelos
            ? h("div", { key: "m", className: "tr-doc-modelos-area" }, h(AbaModelos, { s, enviar, despachar }))
            : h("div", { key: "meio", className: "tr-bases-lista tr-doc-coluna" }, h(Meio, { s, doc, porId, despachar })),
          modelos ? null : h("div", { key: "lado", className: "tr-bases-lado" }, h(Lado, { s, doc, porId, despachar })),
        ]),
    ]));
}

function PainelDocumentos() {
  const [s, despachar] = React.useReducer(reduzir, INICIAL);

  React.useEffect(() => {
    const ouvir = (m) => despachar(m);
    ouvintes.add(ouvir);
    enviar("listar");
    return () => ouvintes.delete(ouvir);
  }, []);

  // Prévia pronta: abre numa aba nova (servida pelo próprio editor).
  React.useEffect(() => {
    const ver = (m) => { if (m.tipo === "previa" && m.url) abrirLink(m.url); };
    ouvintes.add(ver);
    return () => ouvintes.delete(ver);
  }, []);

  // Os selos dos cards seguem a lista de documentos.
  React.useEffect(() => {
    selosPorNo = indiceSelos(s.documentos, s.fluxo);
    refreshCardBadges();
  }, [s.documentos, s.fluxo]);

  // Aberta: a lista e o estado do documento escolhido são lidos na hora (o
  // fluxo e o texto mudam fora da janela).
  React.useEffect(() => {
    if (!s.aberto) return;
    enviar("listar");
    if (s.selecionado && s.regioes[s.selecionado]) enviar("estado", { documento: s.selecionado });
  }, [s.aberto]);

  // Documento escolhido sem as regiões lidas (acabou de ser criado, veio do
  // selo de um card ou é o primeiro da lista): pede o estado.
  React.useEffect(() => {
    if (s.aberto && s.selecionado && !s.regioes[s.selecionado]) enviar("estado", { documento: s.selecionado });
  }, [s.aberto, s.selecionado]);

  const n = pendencias(s.documentos);
  return h("span", { className: "tr-doc" }, [
    h("button", { key: "b", type: "button", className: "tr-tb-btn" + (s.aberto ? " tr-on" : ""),
                  "aria-label": "Documentos", "aria-expanded": s.aberto, "aria-haspopup": "dialog",
                  title: n ? `Documentos · ${n} ${n === 1 ? "região pede" : "regiões pedem"} atenção` : "Documentos",
                  onClick: () => despachar({ tipo: s.aberto ? "fechar" : "abrir" }) }, [
      h(Icone, { key: "i" }),
      n ? h("span", { key: "l", className: "tr-doc-luz", "aria-hidden": true }) : null]),
    // Fora da barra (portal no body): a barra tem `z-index` próprio e a
    // janela tem de ficar por cima do canvas inteiro, como a de Bases.
    s.aberto ? ReactDOM.createPortal(h(Janela, { key: "j", s, despachar }), document.body) : null,
  ]);
}

registerToolbarItem("documentos", PainelDocumentos);
