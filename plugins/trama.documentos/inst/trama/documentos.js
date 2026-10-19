// Painel Documentos na barra do editor e "Inserir no documento…" no menu do
// card (plugin `trama.documentos`, #23).
//
// Só desenha: o estado é o redutor de `painel.js`, e quem mexe nos arquivos
// é o R (`R/painel.R`). Canal próprio: `input$trama_documentos` ({acao, ...})
// para cima, mensagem `trama_documentos` ({tipo, ...}) para baixo.

import React from "react";
import { h, registerToolbarItem, registerCardMenuItem, editorAction } from "trama";
import { INICIAL, ESTADOS, reduzir, resumo, pendencias, pedidoInserir, ehTabela, nomeValido, resolucoes,
         textoOcupado, formatosExportar, nomeSaida, pedidoPublicar } from "./painel.js";
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

const abrirLink = (url) => window.open(url, "_blank", "noopener");

// Ícone de documento (traço, `currentColor`, como os da barra).
const Icone = () => h("svg", { className: "tr-ic", width: 18, height: 18, viewBox: "0 0 24 24", fill: "none",
  stroke: "currentColor", strokeWidth: 1.8, strokeLinecap: "round", strokeLinejoin: "round", "aria-hidden": true }, [
  h("path", { key: "a", d: "M14 3H7a2 2 0 0 0-2 2v14a2 2 0 0 0 2 2h10a2 2 0 0 0 2-2V8z" }),
  h("path", { key: "b", d: "M14 3v5h5M9 13h6M9 17h6" })]);

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
  return h("div", { className: "tr-doc-caixa tr-doc-inserir" }, [
    h("div", { key: "t", className: "tr-doc-titulo" }, `Inserir “${ins.card.rotulo}”`),
    s.documentos.length
      ? h(Campo, { key: "d", rotulo: "No documento" },
          h("select", { value: ins.documento || "", onChange: campo("documento") }, [
            h("option", { key: "", value: "" }, "Escolha…"),
            ...s.documentos.map((d) => h("option", { key: d.caminho, value: d.caminho }, d.nome))]))
      : h("p", { key: "d", className: "tr-doc-dim" }, "O projeto ainda não tem documento: crie um."),
    s.novo ? null : h(Link, { key: "n", onClick: () => despachar({ tipo: "novo_abrir" }) }, "+ Novo documento"),
    (ins.card.saidas || []).length > 1
      ? h(Campo, { key: "p", rotulo: "Saída" },
          h("select", { value: ins.porta || "", onChange: campo("porta") },
            ins.card.saidas.map((p) => h("option", { key: p.porta, value: p.porta }, `${p.porta} (${p.tipo})`))))
      : null,
    h(Campo, { key: "m", rotulo: "Modo" }, h("div", { className: "tr-doc-seg" }, [
      h("button", { key: "r", type: "button", className: "tr-doc-on" }, "Resultado"),
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
    h("div", { key: "b", className: "tr-doc-linha" }, [
      h(Botao, { key: "i", primario: true, disabled: !pedido || (ins.pedirAceite && !ins.aceitar), onClick: ir },
        ins.enviando ? "Inserindo…" : "Inserir"),
      h(Botao, { key: "x", onClick: () => despachar({ tipo: "inserir_cancelar" }) }, "Cancelar")]),
  ]);
}

function Regioes({ s, doc, despachar }) {
  const regs = s.regioes[doc.caminho];
  if (!regs) return h("p", { className: "tr-doc-dim" }, "Lendo as regiões…");
  if (!regs.length) return h("p", { className: "tr-doc-dim tr-doc-nota" },
    "Nenhuma região ainda. No menu de um card (botão direito), use “Inserir no documento…”.");
  const resolver = (r, como) => {
    if (como === "manter") return enviar("resolver", { documento: doc.caminho, regiao: r.regiao, como });
    despachar({ tipo: "pedir_confirmacao", documento: doc.caminho, regiao: r.regiao, como });
  };
  return h("ul", { className: "tr-doc-lista tr-doc-regioes" }, regs.map((r) =>
    h("li", { key: `${r.regiao}-${r.linha}`, className: "tr-doc-regiao" }, [
      h("div", { key: "l", className: "tr-doc-linha" }, [
        h("button", { key: "n", type: "button", className: "tr-doc-link tr-doc-cresce", title: "Ir ao bloco",
                      onClick: () => { if (editorAction("irAoBloco", r.no)) despachar({ tipo: "fechar" }); } },
          `#trama-${r.regiao}`),
        h("span", { key: "ln", className: "tr-doc-dim" }, `linha ${r.linha}`),
        h(Chip, { key: "c", estado: r.estado })]),
      resolucoes(r.estado).length ? h("div", { key: "a", className: "tr-doc-linha tr-doc-acoes" },
        resolucoes(r.estado).map((x) => h(Link, { key: x.como, onClick: () => resolver(r, x.como) }, x.rotulo))) : null,
    ])));
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

// Publicar no GitHub: o que sai (título, versão, regiões, avisos), o
// repositório, e o envio. Bloqueio desliga o botão; o R pede confirmação
// antes de criar repositório privado ou de sobrescrever um repositório alheio.
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
  if (p.carregando || !c) {
    return h("div", { className: "tr-doc-caixa" }, [
      h("div", { key: "t", className: "tr-doc-titulo" }, "Publicar no GitHub"),
      h("p", { key: "c", className: "tr-doc-dim" }, "Conferindo o documento…")]);
  }
  const lista = (itens, classe) => (itens || []).length
    ? h("ul", { className: classe }, itens.map((x, i) => h("li", { key: i }, x))) : null;
  return h("div", { className: "tr-doc-caixa tr-doc-publicar" }, [
    h("div", { key: "t", className: "tr-doc-titulo" }, "Publicar no GitHub"),
    h("p", { key: "o", className: "tr-doc-dim" },
      `“${c.titulo || "sem título"}” · ${(c.autores || []).join("; ") || "sem autor"} · licença ${c.licenca}` +
      ` · próxima versão: ${c.tipo_versao}` + (c.ultima ? ` (última: ${c.ultima.versao})` : "")),
    h("p", { key: "r", className: "tr-doc-dim" },
      `${(c.regioes || []).length} ${(c.regioes || []).length === 1 ? "resultado" : "resultados"} do trama, o fluxo e o ` +
      "ambiente vão num pacote RO-Crate; o GitHub Actions renderiza a página e cria a release."),
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

function Documento({ s, doc, despachar }) {
  const aberto = s.selecionado === doc.caminho;
  const ocupado = s.ocupado[doc.caminho];
  const exp = s.exportados[doc.caminho];
  const semQuarto = () => enviar("guia_quarto");
  const render = (acao, extra = {}) => s.quarto ? enviar(acao, { documento: doc.caminho, ...extra }) : semQuarto();
  const abrir = (alvo, extra = {}) => enviar("abrir", { documento: doc.caminho, alvo, ...extra });
  return h("li", { className: "tr-doc-item" + (aberto ? " tr-doc-aberto" : "") }, [
    h("button", { key: "c", type: "button", className: "tr-doc-cab", "aria-expanded": aberto,
                  onClick: () => { despachar({ tipo: "selecionar", documento: doc.caminho });
                                   if (!aberto) enviar("estado", { documento: doc.caminho }); } }, [
      h("span", { key: "n", className: "tr-doc-nome tr-doc-cresce" }, doc.nome),
      h("span", { key: "r", className: "tr-doc-dim" }, resumo(doc.resumo))]),
    aberto ? h("div", { key: "corpo", className: "tr-doc-corpo" }, [
      ocupado ? h("div", { key: "o", className: "tr-doc-ocupado", role: "status" }, textoOcupado(ocupado)) : null,
      h("div", { key: "a", className: "tr-doc-linha" }, [
        h(Botao, { key: "u", primario: true, disabled: !!ocupado, title: "Refaz só as regiões cujo resultado mudou",
                   onClick: () => enviar("atualizar", { documento: doc.caminho }) }, "Atualizar"),
        h(Botao, { key: "v", disabled: !!ocupado, title: "Renderiza em HTML e abre numa aba nova",
                   onClick: () => render("visualizar") }, "Visualizar"),
        h("select", { key: "e", className: "tr-doc-exportar", value: "", disabled: !!ocupado, "aria-label": "Exportar",
                      onChange: (e) => { if (e.target.value) render("exportar", { formato: e.target.value }); } }, [
          h("option", { key: "", value: "" }, "Exportar…"),
          // Os formatos do modelo do documento (fase 3).
          ...formatosExportar(doc).map(([k, rotulo]) => h("option", { key: k, value: k }, rotulo))])]),
      exp ? h("div", { key: "x", className: "tr-doc-linha tr-doc-acoes" }, [
        h("span", { key: "t", className: "tr-doc-dim tr-doc-cresce" }, nomeSaida(exp.arquivo)),
        h(Link, { key: "a", onClick: () => abrir("saida", { arquivo: exp.arquivo }) }, "Abrir arquivo"),
        h(Link, { key: "p", onClick: () => abrir("pasta") }, "Pasta")]) : null,
      h("div", { key: "l", className: "tr-doc-linha tr-doc-acoes" }, [
        h(Link, { key: "e", title: "Abre o .qmd no seu editor de texto (RStudio, Positron, VS Code…)",
                  onClick: () => abrir("documento") }, "Abrir no editor de texto"),
        h(Link, { key: "p", onClick: () => abrir("pasta") }, "Abrir a pasta"),
        h(Link, { key: "g", title: "Pacote reprodutível, release e página no GitHub Pages",
                  onClick: () => { despachar({ tipo: "publicar_abrir", documento: doc.caminho });
                                   enviar("publicar_conferir", { documento: doc.caminho }); } }, "Publicar no GitHub…")]),
      s.publicar?.documento === doc.caminho ? h(Publicar, { key: "pub", s, despachar }) : null,
      s.envios[doc.caminho] ? h(Envio, { key: "env", e: s.envios[doc.caminho] }) : null,
      h(Regioes, { key: "r", s, doc, despachar }),
    ]) : null,
  ]);
}

function AbaDocumentos({ s, despachar }) {
  return h("div", { className: "tr-doc-aba" }, [
    !s.quarto ? h("div", { key: "q", className: "tr-doc-aviso" }, [
      h("span", { key: "t" }, "O Quarto não está instalado: dá para inserir e atualizar, mas não para visualizar e exportar. "),
      h(Link, { key: "g", onClick: () => enviar("guia_quarto") }, "Como instalar")]) : null,
    s.inserir ? h(Inserir, { key: "i", s, despachar }) : null,
    s.novo ? h(Novo, { key: "n", s, despachar }) : null,
    s.confirmar ? h(Confirmar, { key: "c", c: s.confirmar }) : null,
    h(Aviso, { key: "e", texto: s.erro, tom: "erro" }),
    h(Aviso, { key: "a", texto: s.aviso, tom: "ok" }),
    s.fase !== "pronto" ? h("p", { key: "l", className: "tr-doc-dim" }, "Lendo os documentos…")
      : s.documentos.length ? h("ul", { key: "d", className: "tr-doc-lista" },
          s.documentos.map((d) => h(Documento, { key: d.caminho, s, doc: d, despachar })))
      : h("p", { key: "v", className: "tr-doc-dim tr-doc-nota" },
          "Um documento é um .qmd seu: você escreve o texto, e o trama mantém atualizados os resultados que você insere."),
    s.novo || s.inserir ? null : h(Botao, { key: "nv", onClick: () => despachar({ tipo: "novo_abrir" }) }, "+ Novo documento"),
  ]);
}

function PainelDocumentos() {
  const [s, despachar] = React.useReducer(reduzir, INICIAL);
  const raiz = React.useRef(null);

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

  // Fecha no clique fora e no Esc (como o menu GitHub).
  React.useEffect(() => {
    if (!s.aberto) return undefined;
    const fora = (e) => { if (raiz.current && !raiz.current.contains(e.target)) despachar({ tipo: "fechar" }); };
    const esc = (e) => { if (e.key === "Escape") despachar({ tipo: "fechar" }); };
    document.addEventListener("pointerdown", fora, true);
    document.addEventListener("keydown", esc);
    return () => { document.removeEventListener("pointerdown", fora, true); document.removeEventListener("keydown", esc); };
  }, [s.aberto]);

  // Aberto: a lista e o estado do documento escolhido são lidos na hora (o
  // fluxo e o texto mudam fora do painel).
  React.useEffect(() => {
    if (!s.aberto) return;
    enviar("listar");
    if (s.selecionado && s.regioes[s.selecionado]) enviar("estado", { documento: s.selecionado });
  }, [s.aberto]);

  // Documento aberto sem as regiões lidas (acabou de ser criado, ou veio
  // selecionado por ser o único): pede o estado.
  React.useEffect(() => {
    if (s.aberto && s.selecionado && !s.regioes[s.selecionado]) enviar("estado", { documento: s.selecionado });
  }, [s.aberto, s.selecionado]);

  const n = pendencias(s.documentos);
  const abas = [["documentos", "Documentos"], ["modelos", "Modelos"]];
  return h("span", { ref: raiz, className: "tr-doc" }, [
    h("button", { key: "b", type: "button", className: "tr-tb-btn" + (s.aberto ? " tr-on" : ""),
                  "aria-label": "Documentos", "aria-expanded": s.aberto,
                  title: n ? `Documentos · ${n} ${n === 1 ? "região pede" : "regiões pedem"} atenção` : "Documentos",
                  onClick: () => despachar({ tipo: s.aberto ? "fechar" : "abrir" }) }, [
      h(Icone, { key: "i" }),
      n ? h("span", { key: "l", className: "tr-doc-luz", "aria-hidden": true }) : null]),
    s.aberto ? h("div", { key: "p", className: "tr-toolbar-pop tr-doc-pop", role: "dialog", "aria-label": "Documentos" },
      s.guia ? h(Guia, { passos: s.guia, voltar: () => despachar({ tipo: "fechar_guia" }) }) : [
        h("div", { key: "t", className: "tr-doc-abas", role: "tablist" }, abas.map(([id, rotulo]) =>
          h("button", { key: id, type: "button", role: "tab", "aria-selected": s.aba === id,
                        className: "tr-doc-tab" + (s.aba === id ? " tr-doc-on" : ""),
                        onClick: () => despachar({ tipo: "aba", aba: id }) }, rotulo))),
        s.aba === "modelos"
          ? h(AbaModelos, { key: "m", s, enviar, despachar })
          : h(AbaDocumentos, { key: "d", s, despachar }),
      ]) : null,
  ]);
}

registerToolbarItem("documentos", PainelDocumentos);
