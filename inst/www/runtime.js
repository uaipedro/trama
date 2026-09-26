// inst/www/runtime.js — o contrato público do front.
//
// Carregado ANTES de qualquer coleção e do editor. É o que uma coleção
// importa (`import { registerRenderer } from "trama"`) para trazer
// visualização e widget novos SEM tocar no núcleo — o ponto de extensão que
// motivou o projeto inteiro.
//
// Dispatch por ID DE RENDERER (string vinda do artefato de preview), nunca
// por tipo de objeto: é o que permite o front continuar burro. Ele não sabe
// o que é uma tabela, uma série ou um raster — sabe procurar um componente
// registrado sob um nome.

import React from "react";
import ReactDOM from "react-dom";
// Relativo, sem entrada no importmap: um especificador relativo resolve contra
// a URL do PRÓPRIO módulo, e `runtime.js` é sempre carregado pelo importmap em
// `./trama-<versão>/runtime.js` — o `params.js` vizinho sai do mesmo diretório
// (a dependência do núcleo usa `all_files = TRUE`). E nenhuma coleção precisa
// dele pelo nome: as regras chegam a elas através destes widgets.
import { layoutEnum, validarNumero } from "./params.js";
import { anotado, opcoes, sugerir, sumidas, alternativa } from "./colunas.js";
import { MARCAS, NIVEIS, posicao, estrelas, faixa, venceu, num, numP, eixoEfeito } from "./teste.js";

export const h = React.createElement;

const renderers = {};
const widgets = {};

// Um renderer é um CONJUNTO DE VISTAS do mesmo artefato. Passar uma função
// continua valendo e vira "uma vista só" — nenhuma coleção existente quebra.
// A vista recebe o mesmo `{artifact, handle, assetUrl}` de sempre: vistas são
// leituras diferentes do que JÁ trafega, não payloads paralelos. Uma vista que
// precise de dado novo é escolha explícita do `preview` do tipo.
// `expand` é a leitura em tela cheia do artefato (tecla V). Opcional: sem
// ele, a vista em tela cheia usa a própria vista do card, maior.
function normalizeRenderer(id, def) {
  if (typeof def === "function") {
    return { views: [{ id: "preview", label: id.split("/").pop(), component: def }], expand: null };
  }
  const vs = (def && def.views) || [];
  if (!vs.length) throw new Error(`[trama] renderer '${id}' sem vistas.`);
  return { views: vs.map((v) => {
    // Falta de `id` só apareceria muito depois, como aba sem rótulo e vista que
    // não persiste (o documento guarda id, não índice); falta de `component`,
    // como tela branca no primeiro render. Erro nomeado no registro é mais
    // barato que os dois.
    if (!v.id || typeof v.component !== "function") {
      throw new Error(`[trama] renderer '${id}': vista precisa de 'id' e 'component'.`);
    }
    return { id: v.id, label: v.label || v.id, component: v.component };
  }), expand: typeof def.expand === "function" ? def.expand : null };
}

export function registerRenderer(id, def) { renderers[id] = normalizeRenderer(id, def); }
export function registerWidget(kind, component) { widgets[kind] = component; }
// Devolve a forma NORMALIZADA (`{views:[...]}`), nunca a função crua: o editor
// não pode ver as duas formas.
export function getRenderer(id) { return renderers[id]; }
export function getWidget(kind) { return widgets[kind]; }

// A vista `resumo` é do NÚCLEO, não de uma coleção: `tr_type(summary=)` já grava
// `handle$summary` (R/store.R:140) e ele já viaja pro card. Todo tipo que declare
// um `summary` ganha a segunda vista de graça, e a coleção que quiser um resumo
// melhor sobrescreve pelo id.
function Resumo({ handle }) {
  return h(KeyValue, { artifact: { data: handle.summary } });
}

export function getViews(rendererId, handle) {
  const r = renderers[rendererId];
  const views = r ? r.views.slice() : [];
  const s = handle && handle.summary;
  if (s && Object.keys(s).length && !views.some((v) => v.id === "resumo")) {
    views.push({ id: "resumo", label: "resumo", component: Resumo });
  }
  return views;
}

// --- Renderers embutidos ---------------------------------------------------
// Universais o bastante pra morarem no núcleo. Tudo que for de domínio vem da
// coleção.

function Table({ artifact }) {
  const rows = (artifact.data && artifact.data.rows) || [];
  const cols = (artifact.data && artifact.data.columns) || (rows[0] ? Object.keys(rows[0]) : []);
  // Nx0 (um `select` que removeu tudo) tem linhas mas nenhuma coluna: sem o
  // segundo ramo, sai um `thead` vazio e N linhas em branco por baixo.
  if (!rows.length || !cols.length)
    return h("div", { className: "tr-empty" }, cols.length ? "sem linhas" : "sem colunas");
  return h("div", { className: "tr-table-wrap" },
    h("table", { className: "tr-table" }, [
      h("thead", { key: "h" }, h("tr", null, cols.map((c) => h("th", { key: c }, c)))),
      h("tbody", { key: "b" }, rows.map((r, i) =>
        h("tr", { key: i }, cols.map((c) => h("td", { key: c }, fmt(r[c])))))),
    ]));
}

function fmt(v) {
  if (v === null || v === undefined) return h("span", { className: "tr-na" }, "NA");
  if (typeof v === "number") return Number.isInteger(v) ? String(v) : v.toFixed(3);
  return String(v);
}

function KeyValue({ artifact }) {
  const d = artifact.data || {};
  return h("dl", { className: "tr-kv" },
    Object.keys(d).map((k) => h(React.Fragment, { key: k }, [
      h("dt", { key: "k" }, k), h("dd", { key: "v" }, fmt(d[k])),
    ])));
}

function Text({ artifact }) {
  return h("pre", { className: "tr-text" }, String((artifact.data && artifact.data.text) ?? ""));
}

// A PARTE REUTILIZÁVEL do lightbox: estado `aberto`, o listener de Escape e o
// portal com overlay+img+botão fechar. Extraída daqui (e não copiada) porque
// `notas.js` (bloco de imagem, Task 3.1) precisa do mesmo comportamento sobre
// um `src` já resolvido — a nota não tem `artifact.files`, só uma string de
// `src` —, e as duas formas de dado convergem pro mesmo `src` de string antes
// de chegar aqui. `Image` abaixo passa a usar esta função também, então não há
// duas cópias da lógica de portal/Escape/overlay a manter em sincronia.
// Devolve `{ aberto, abrir, node }`: `node` é o portal (ou `null` fechado),
// pronto pra entrar como filho de quem chama.
export function useLightbox(src) {
  const [aberto, setAberto] = React.useState(false);
  // Listener montado só quando aberto: um global permanente por card seria N
  // listeners num canvas com N gráficos.
  React.useEffect(() => {
    if (!aberto) return;
    const onKey = (e) => { if (e.key === "Escape") setAberto(false); };
    window.addEventListener("keydown", onKey);
    return () => window.removeEventListener("keydown", onKey);
  }, [aberto]);
  // O overlay é irmão da imagem, não filho: dentro do card ele herdaria o
  // `overflow:auto` do `.tr-preview` e o `transform` do canvas do React Flow,
  // e `position:fixed` deixaria de ser relativo à janela — o lightbox abriria
  // recortado dentro do card, que é o modo de falha mais difícil de
  // diagnosticar aqui. `createPortal` para o `<body>` resolve de vez.
  // Nome do arquivo baixado: o final do `src` sem query string. Genérico
  // (`preview.png` pro mesmo nome em todo gráfico) mas é só o nome SUGERIDO —
  // "salvar como" do navegador deixa o usuário trocar antes de gravar.
  const nome = String(src ?? "").split("/").pop().split("?")[0] || "imagem.png";
  const node = aberto ? ReactDOM.createPortal(
    h("div", { key: "lb", className: "tr-lightbox", role: "dialog",
               onClick: () => setAberto(false) }, [
      h("img", { key: "big", className: "tr-lightbox-img", src }),
      // `download` (não `onClick` com fetch+blob) porque o arquivo já é
      // servido pelo mesmo host do editor — o navegador salva direto, sem
      // round-trip extra nem CORS pra se preocupar.
      h("a", { key: "s", className: "tr-lightbox-save", href: src, download: nome,
                title: "salvar imagem como…",
                onClick: (e) => e.stopPropagation() }, "⭳"),
      h("button", { key: "x", className: "tr-lightbox-close",
                    title: "fechar (Esc)" }, "×"),
    ]), document.body) : null;
  return { aberto, abrir: () => setAberto(true), node };
}

// O preview de imagem cabe no card, mas o card é pequeno e a imagem é HD: o
// clique abre o MESMO arquivo em tela cheia, sem segunda requisição e sem
// segundo artefato. Mora no núcleo, e não numa coleção, porque toda coleção
// que produzir imagem quer isto — e porque uma coleção que precisasse
// reescrever o visualizador para ganhar um clique não seria extensível, seria
// só copiável.
//
// `object-fit: contain` no CSS é o que faz a proporção da imagem mandar dentro
// da caixa do card, que o usuário redimensiona por outro eixo: a imagem nunca
// distorce, sobra faixa. É a tradução visual da regra de que proporção é param
// (semântico, na chave de cache) e tamanho do card é cosmético.
function Image({ artifact, assetUrl }) {
  const f = artifact.files && (artifact.files.png || Object.values(artifact.files)[0]);
  const src = f ? assetUrl(f) : null;
  const { abrir, node } = useLightbox(src);
  if (!f) return h("div", { className: "tr-empty" }, "sem imagem");
  return h("div", { className: "tr-img-wrap" }, [
    // Sem `nodrag`: clique normal arrasta o CARD, como em qualquer outro
    // ponto dele — uma imagem grande cobrindo o card inteiro não pode ser um
    // buraco onde arrastar vira "abrir em tela cheia" sem querer. Ctrl/⌘+clique
    // é o gesto de abrir; um clique com o modificador não gera arrasto de
    // verdade (sem deslocamento, o xyflow nunca inicia o drag), então não há
    // disputa entre os dois.
    h("img", { key: "i", className: "tr-img", src, loading: "lazy",
               title: "ctrl/⌘+clique para ampliar",
               onClick: (e) => {
                 if (!e.ctrlKey && !e.metaKey) return;
                 e.stopPropagation();
                 abrir();
               } }),
    node,
  ]);
}

function ErrorView({ artifact }) {
  return h("div", { className: "tr-err" }, (artifact.data && artifact.data.message) || "erro");
}

// A vista em tela cheia (V) da imagem: mesmo arquivo do card, só que sem o
// recorte do `.tr-preview`. Não reusa `Image` (que embute o gesto de
// ctrl/⌘+clique do lightbox) — aqui quem abre e fecha é a `Vista` de
// `modos-ui.js`, então o corpo é só a `<img>`.
function ImagemGrande({ artifact, assetUrl }) {
  const f = artifact.files && (artifact.files.png || Object.values(artifact.files)[0]);
  return f ? h("img", { className: "tr-lightbox-img", src: assetUrl(f) })
           : h("div", { className: "tr-empty" }, "sem imagem");
}

// Rótulo em português explícito: o fallback `id.split("/").pop()` daria
// "table"/"image" numa interface que fala português.
registerRenderer("trama/table", { views: [{ id: "tabela", label: "tabela", component: Table }] });
registerRenderer("trama/keyvalue", { views: [{ id: "campos", label: "campos", component: KeyValue }] });
registerRenderer("trama/text", { views: [{ id: "texto", label: "texto", component: Text }] });
registerRenderer("trama/image", { views: [{ id: "imagem", label: "imagem", component: Image }], expand: ImagemGrande });
registerRenderer("trama/error", { views: [{ id: "erro", label: "erro", component: ErrorView }] });

// --- O card de teste de hipótese ---------------------------------------------
// `trama/test`: o mesmo desenho para todo teste de toda coleção. Com p-valor, a
// RÉGUA logarítmica e as estrelas; sem p-valor (teste de tabela de valores
// críticos, como o ADF), os três PONTINHOS de 10, 5 e 1%. As regras moram em
// `teste.js`; aqui é só desenho.
//
// Contrato de `artifact.data` (tudo que não é obrigatório pode faltar):
//   teste, h0, estatistica, rotulo_estat, decisao_5 ("rejeita H0" ou não),
//   conclusao — obrigatórios;
//   p_valor, gl, criticos {"10%","5%","1%"}, sentido ("menor"|"maior"),
//   efeito {rotulo, valor, li, ls}, extra {nome: valor}, nota, fonte.

// `Regua` e `Estrelas` são exportados: um quadro com p-valor por linha (a
// ANOVA da `models`) desenha a mesma régua em miniatura.
export function Estrelas({ estrelas: e }) {
  if (!e) return null;
  return h("span", { className: `tr-estrelas tr-faixa-${faixa(e)}`,
                     title: e === "ns" ? "não significativo a 10%" : `significância ${e}` }, e);
}

export function Regua({ p, estrelas: e, mini }) {
  const est = e ?? estrelas(p);
  const pos = posicao(p);
  return h("div", { className: "tr-regua" + (mini ? " tr-regua-mini" : "") + ` tr-faixa-${faixa(est)}`,
                    title: p == null ? "" : `p = ${num(p, 4)}` }, [
    h("div", { key: "t", className: "tr-regua-trilho" }, [
      h("div", { key: "b", className: "tr-regua-barra", style: { width: `${pos * 100}%` } }),
      ...MARCAS.map(([rot, alfa]) =>
        h("span", { key: rot, className: "tr-regua-marca" + (alfa === 0.05 ? " tr-regua-marca-5" : ""),
                    style: { left: `${posicao(alfa) * 100}%` } })),
      // A ponta marca ONDE o p está. Sem ela, p = 0,8 é uma barra de 2% que some
      // no trilho, e "não significativo" parece "não calculado".
      p == null ? null : h("span", { key: "pt", className: "tr-regua-ponta", style: { left: `${pos * 100}%` } }),
    ]),
    // O 10% fica só com a marca: em quatro décadas ele cai colado ao 5%, e os
    // dois rótulos se sobrepõem no card de 240px.
    mini ? null : h("div", { key: "r", className: "tr-regua-rotulos" },
      MARCAS.filter(([, alfa]) => alfa !== 0.1).map(([rot, alfa]) =>
        h("span", { key: rot, style: { left: `${posicao(alfa) * 100}%` } }, rot))),
  ]);
}

// Os pontinhos: preenchido = venceu o corte daquele nível; na cor de destaque
// quando a decisão a 5% é rejeitar, em cinza quando não. O pontinho de um nível
// não é o veredito do card — que é sempre o de 5% —, e por isso a dica diz
// "vence o corte", e não "rejeita H0".
function Pontinhos({ d }) {
  const rejeita = d.decisao_5 === "rejeita H0";
  return h("div", { className: "tr-pontos" }, NIVEIS.map(([rotulo, alfa]) => {
    const on = venceu(d, rotulo, alfa);
    return h("span", { key: rotulo, className: "tr-ponto-nivel", title: `${rotulo}: ${on ? "vence o corte" : "não vence o corte"}` }, [
      h("span", { key: "p", className: "tr-ponto" + (on ? (rejeita ? " tr-ponto-on" : " tr-ponto-fraco") : "") }),
      h("span", { key: "r", className: "tr-ponto-rot" }, rotulo),
    ]);
  }));
}

function VereditoTeste({ artifact }) {
  const d = artifact.data || {};
  const rejeita = d.decisao_5 === "rejeita H0";
  const temP = d.p_valor != null;
  const est = temP ? estrelas(d.p_valor) : "";
  const f = faixa(est);
  const estat = `${d.rotulo_estat || "estatística"} = ${num(d.estatistica)}${d.gl ? ` · gl ${d.gl}` : ""}`;
  return h("div", { className: `tr-mt tr-faixa-${f}` }, [
    h("div", { key: "topo", className: "tr-mt-topo" }, [
      h("span", { key: "n", className: "tr-mt-nome", title: d.teste }, d.teste),
      temP ? h(Estrelas, { key: "e", estrelas: est }) : null,
    ]),
    // H0 à vista: o card diz o que foi testado sem abrir o detalhe, e é o que
    // desfaz a leitura invertida (no Shapiro e no ADF, rejeitar diz coisas
    // opostas).
    d.h0 ? h("div", { key: "h0", className: "tr-mt-h0", title: `H0: ${d.h0}` }, [h("b", { key: "b" }, "H0 "), d.h0]) : null,
    h("div", { key: "meio", className: "tr-mt-meio" }, [
      h("div", { key: "v", className: "tr-mt-num" }, [
        h("div", { key: "a", className: "tr-mt-valor" + (rejeita ? " tr-mt-valor-sig" : "") },
          temP ? numP(d.p_valor) : num(d.estatistica)),
        // Sem p-valor o número grande JÁ é a estatística: repeti-la na legenda
        // gastaria a linha que diz com o que ela é comparada.
        h("div", { key: "b", className: "tr-mt-rotulo" }, temP ? `p-valor · ${estat}`
          : `estatística ${d.rotulo_estat || ""}${d.gl ? ` · gl ${d.gl}` : ""} · contra valores críticos`),
      ]),
      h("span", { key: "s", className: "tr-selo" + (rejeita ? " tr-selo-on" : ""), title: "decisão ao nível de 5%" },
        rejeita ? "rejeita H0" : "não rejeita H0"),
    ]),
    temP ? h(Regua, { key: "r", p: d.p_valor, estrelas: est }) : h(Pontinhos, { key: "r", d }),
    h("div", { key: "c", className: "tr-mt-conclusao", title: d.conclusao }, d.conclusao),
  ]);
}

function CamposTeste({ campos }) {
  const filhos = [];
  campos.forEach(([k, v], i) => {
    if (v == null || v === "") return;
    filhos.push(h("dt", { key: `k${i}` }, k));
    filhos.push(h("dd", { key: `v${i}` }, String(v)));
  });
  return h("dl", { className: "tr-kv" }, filhos);
}

// O efeito desenhado: ponto na estimativa, traço no intervalo e a referência. É
// a pergunta que o p-valor não responde — de QUANTO —, e o intervalo que cruza a
// referência se vê antes de se ler.
function Efeito({ e }) {
  const g = eixoEfeito(e);
  const pc = (v) => `${g.x(v)}%`;
  return h("div", { className: "tr-mt-efeito" }, [
    h("div", { key: "l" }, [
      h("span", { key: "v", className: "tr-mt-efeito-valor" }, num(e.valor)),
      g.temIC ? h("span", { key: "ic", className: "tr-mt-rotulo" }, ` IC 95% [${num(e.li)}; ${num(e.ls)}]`) : null,
    ]),
    h("div", { key: "g", className: "tr-mt-efeito-eixo" + (g.cruza ? " tr-mt-efeito-cruza" : "") }, [
      h("span", { key: "z", className: "tr-mt-efeito-ref", style: { left: pc(g.ref) }, title: `referência ${g.ref}` },
        h("i", null, String(g.ref))),
      g.temIC ? h("span", { key: "ic", className: "tr-mt-efeito-ic",
                            style: { left: pc(e.li), width: `${g.x(e.ls) - g.x(e.li)}%` } }) : null,
      h("span", { key: "p", className: "tr-mt-efeito-ponto", style: { left: pc(e.valor) } }),
    ]),
  ]);
}

function Secao({ titulo, children }) {
  return h("section", { className: "tr-mt-secao" }, h("h5", null, titulo), children);
}

function DetalheTeste({ artifact }) {
  const d = artifact.data || {};
  const rejeita = d.decisao_5 === "rejeita H0";
  const temP = d.p_valor != null;
  const resultado = [[d.rotulo_estat || "estatística", num(d.estatistica, 4)], ["gl", d.gl]];
  if (temP) resultado.push(["p-valor", `${numP(d.p_valor)}  ${estrelas(d.p_valor)}`]);
  for (const [rotulo] of NIVEIS) {
    if (d.criticos && d.criticos[rotulo] != null) resultado.push([`crítico ${rotulo}`, num(d.criticos[rotulo])]);
  }
  resultado.push(["a 5%", rejeita ? "rejeita H0" : "não rejeita H0"], ["conclusão", d.conclusao]);
  // `extra` não é só número: o Pettitt manda o rótulo de um período ("1953
  // jun"). Sem a guarda, `num()` trataria a string como número.
  const extra = Object.keys(d.extra || {}).map((k) => {
    const v = d.extra[k];
    return [k.replace(/_/g, " "), typeof v === "number" ? num(v) : String(v)];
  });
  return h("div", { className: "tr-mt-detalhe" }, [
    h(Secao, { key: "h", titulo: d.teste }, h(CamposTeste, { campos: [["H0", d.h0]] })),
    h(Secao, { key: "r", titulo: "resultado" }, h(CamposTeste, { campos: resultado })),
    d.efeito ? h(Secao, { key: "e", titulo: d.efeito.rotulo }, h(Efeito, { e: d.efeito })) : null,
    extra.length ? h(Secao, { key: "x", titulo: "por partes" }, h(CamposTeste, { campos: extra })) : null,
    (d.nota || d.fonte) ? h(Secao, { key: "f", titulo: "referência" },
      h(CamposTeste, { campos: [["nota", d.nota], ["fonte", d.fonte]] })) : null,
  ]);
}

registerRenderer("trama/test", { views: [
  { id: "veredito", label: "veredito", component: VereditoTeste },
  { id: "detalhe", label: "detalhe", component: DetalheTeste },
] });

export { num, numP, estrelas, faixa };

// --- Widgets de param embutidos --------------------------------------------
// Um `kind` desconhecido cai num campo de texto com JSON — degradação, não
// erro: a coleção pode registrar o widget dela depois sem nada quebrar.
//
// As três primitivas abaixo são EXPORTADAS (e vão pro `window.tr`) porque não
// são só dos params: a toolbar e o tema do app usam o mesmo segmentado, e uma
// coleção que desenhe o próprio widget deve poder reaproveitá-las em vez de
// reinventar botão com outra borda. As regras — quando um enum cabe segmentado,
// o que é número válido — moram em `params.js`, testadas sem DOM; aqui é só
// desenho.

// Opção pode ser string (enum de param: rótulo = valor) ou `{value, label,
// title}` (toolbar e tema, onde o que se mostra não é o que se grava).
function opcao(o) {
  if (o !== null && typeof o === "object") {
    const label = o.label ?? String(o.value);
    return { value: o.value, label, title: o.title ?? label };
  }
  return { value: o, label: String(o), title: String(o) };
}

// `button`, e não radio nativo: radio precisa de `name` único por grupo, e
// vários cards com o mesmo param na tela compartilhariam o grupo. `type=button`
// porque o default de botão é submit. `nodrag` pelo mesmo motivo de todo
// controle dentro do card: sem ele o React Flow toma o clique como início de
// arrasto do nó. `stopPropagation` para o clique não chegar ao card (seleção).
//
// `title` com o texto inteiro: com `text-overflow` a opção pode aparecer
// cortada, e a dica é o único lugar onde ela ainda se lê.
export function Segmented({ options, value, onChange, wide, title }) {
  const ops = (options || []).map(opcao);
  return h("div", {
    className: "tr-seg" + (wide ? " tr-seg-wide" : ""), role: "radiogroup", title,
  }, ops.map((o) => {
    // Comparação por texto: um `choices` numérico vindo do R e o valor do
    // documento podem chegar um como número e outro como string.
    const on = String(o.value) === String(value);
    return h("button", {
      key: String(o.value), type: "button", role: "radio", "aria-checked": on,
      title: o.title, className: "nodrag" + (on ? " tr-seg-on" : ""),
      // Reclicar a opção atual não manda op: cada op entra no histórico de
      // desfazer e invalida cache, e "nada mudou" não merece nenhum dos dois.
      onClick: (e) => { e.stopPropagation(); if (!on) onChange(o.value); },
    }, o.label);
  }));
}

// Chave em vez de checkbox: o checkbox nativo não aceita cor de tema e, com
// `width:100%` da linha do param, ficava perdido no meio da coluna.
export function Toggle({ value, onChange, title }) {
  const on = !!value;
  return h("button", {
    type: "button", role: "switch", "aria-checked": on, title,
    className: "tr-toggle nodrag" + (on ? " tr-toggle-on" : ""),
    onClick: (e) => { e.stopPropagation(); onChange(!on); },
  });
}

// `type=text` com `inputMode=decimal`, e não `type=number`: o input numérico do
// navegador engole "2,5" e "abc" e devolve `value` vazio — não sobra texto
// nenhum sobre o qual mostrar erro, e o usuário vê o campo simplesmente não
// obedecer. Com texto, `validarNumero` vê exatamente o que foi digitado (e
// aceita a vírgula que se digita em português); `inputMode` ainda traz o
// teclado numérico no celular.
//
// Mesma filosofia do widget `text`: valida a cada tecla (o erro aparece
// enquanto se digita), mas só COMITA no blur/Enter — cada op recomputa o nó.
// Aqui o campo é controlado por estado LOCAL (e não `defaultValue` + `key`),
// porque é preciso segurar o texto inválido na tela junto da mensagem; o eco
// externo entra pelo `useEffect`.
export function NumberField({ spec, value, onChange }) {
  const salvo = value == null ? "" : String(value);
  const [texto, setTexto] = React.useState(salvo);
  const [erro, setErro] = React.useState(null);
  // Valor do documento mudou por fora (desfazer, outra aba, o próprio eco do
  // commit): o campo passa a mostrá-lo e qualquer erro pendente perde sentido.
  React.useEffect(() => { setTexto(salvo); setErro(null); }, [salvo]);

  const comitar = () => {
    // Blur sem edição não valida: um param sem default acusaria "obrigatório"
    // só por ter recebido foco.
    if (texto === salvo) { setErro(null); return; }
    const r = validarNumero(spec, texto);
    // Inválido não viaja: mandar o default ou o último válido esconderia o erro
    // atrás de um valor que o usuário não digitou. Fica o texto e a mensagem.
    if (!r.ok) { setErro(r.erro); return; }
    // Comparado como texto: documento antigo pode guardar "30" como string, e
    // 30 !== "30" mandaria uma op que não muda nada.
    if (String(r.valor) !== salvo) onChange(r.valor);
    // Mesmo valor escrito de outro jeito ("30,0" num inteiro que já é 30): não
    // há eco para ressincronizar, então o campo volta sozinho à forma salva.
    else setTexto(salvo);
  };
  const limites = [spec.min != null ? `mín. ${spec.min}` : null,
                   spec.max != null ? `máx. ${spec.max}` : null].filter(Boolean);

  return h("div", { className: "tr-numf" }, [
    h("input", {
      key: "i", type: "text", inputMode: "decimal", value: texto,
      className: "nodrag" + (erro ? " tr-invalid" : ""),
      title: limites.length ? limites.join(" · ") : undefined,
      "aria-invalid": erro ? true : undefined,
      onChange: (e) => {
        setTexto(e.target.value);
        const r = validarNumero(spec, e.target.value);
        setErro(r.ok ? null : r.erro);
      },
      onBlur: comitar,
      onKeyDown: (e) => {
        if (e.key === "Enter") e.target.blur();
        else if (e.key === "Escape") { setTexto(salvo); setErro(null); }
      },
    }),
    erro ? h("div", { key: "e", className: "tr-field-err" }, erro) : null,
  ]);
}

registerWidget("number", (spec, value, onChange) =>
  h(NumberField, { spec, value: value ?? spec.default, onChange }));
registerWidget("integer", (spec, value, onChange) =>
  h(NumberField, { spec, value: value ?? spec.default, onChange }));
// `example` é campo livre de `tr_param()` e chega aqui pelo catálogo. Vale a
// pena o núcleo conhecê-lo: campo de texto vazio não tem como ensinar o formato
// que espera, e um exemplo em cinza custa uma linha.
//
// Comita no BLUR, não a cada tecla: o param viaja como op, entra na chave de
// cache e dispara recomputação. Digitar "receita por região" no título de um
// gráfico mandava vinte ops e renderizava vinte PNGs. É a mesma decisão que os
// widgets `expr`/`cols`/`path` da coleção `data` já tomaram em separado — aqui
// ela sobe pro núcleo, e é `defaultValue` (não `value`) pelo mesmo motivo que
// lá: campo controlado por estado que só volta no eco do servidor pisca a cada
// tecla.
//
// `key` amarrado ao valor do documento: sem ele, o eco de um `set_param` vindo
// de fora (desfazer, outra aba, documento recarregado) não reapareceria no
// campo, porque um input não controlado ignora mudança de `defaultValue`.
registerWidget("text", (spec, value, onChange) => {
  const atual = value ?? spec.default;
  return h("input", {
    // Prefixo no `key` porque ele divide o namespace de irmãos com o rótulo do
    // param, que é `key: "n"` (editor.js). Um param de texto cujo valor fosse
    // exatamente "n" produziria duas chaves iguais: aviso do React e
    // reconciliação indefinida naquela linha, só nesse valor.
    key: "v" + String(atual), type: "text", className: "nodrag", defaultValue: atual,
    placeholder: spec.example, title: spec.example ? `ex.: ${spec.example}` : undefined,
    onBlur: (e) => { if (e.target.value !== atual) onChange(e.target.value); },
    onKeyDown: (e) => { if (e.key === "Enter") e.target.blur(); },
  });
});
// `cols`: nome(s) de coluna da tabela que chega na entrada. Morava na coleção
// `data` como textarea; subiu pro núcleo porque qualquer coleção declara
// params de coluna (`tr_param_col()`), e porque só o editor sabe o schema da
// entrada — chega no 4º argumento, `ctx` (montado por `ParamsList`,
// modos-ui.js). Três formas:
//   - sem `ctx.colunas` (entrada desligada, ou ainda não rodou): o textarea de
//     sempre, idêntico ao antigo da coleção `data` — sem tabela não há o que
//     listar, e o usuário ainda pode digitar;
//   - anotado com uma coluna só: `<select>`, as que servem ao papel primeiro,
//     as outras num grupo à parte (servir é conselho, não trava);
//   - sem anotação, ou `multi`: o textarea, com as colunas da entrada em chips
//     clicáveis embaixo.
// Qualquer escolha daqui sai por `onChange` — `set_param` sem origem —, o que
// desmarca o "sugerido": escolha à mão nunca é pisada por sugestão.
const colsVazio = (v) => v === "" || v === null || v === undefined;
function colsTexto(spec, atual, onChange) {
  return h("textarea", {
    // `key` no valor pelo mesmo motivo do widget `text`: o eco de fora (undo,
    // sugestão, "usar Y") precisa reaparecer num campo não controlado.
    key: "v" + String(atual), className: "nodrag tr-expr", rows: 2, spellCheck: false,
    defaultValue: atual,
    placeholder: spec.example ?? "col1, col2",
    title: spec.example ? `ex.: ${spec.example}` : undefined,
    onBlur: (e) => { if (e.target.value !== atual) onChange(e.target.value); },
  });
}
registerWidget("cols", (spec, value, onChange, ctx) => {
  const atual = value ?? spec.default ?? "";
  const schema = ctx?.colunas;
  if (!schema) return colsTexto(spec, atual, onChange);
  const lista = opcoes(spec, schema);
  const serve = lista.filter((o) => o.serve), outras = lista.filter((o) => !o.serve);
  const falta = sumidas([spec], { [spec.name]: atual }, schema)[0];
  let campo;
  if (anotado(spec) && !spec.multi) {
    // Flag desligado e campo vazio: a sugestão fica a um clique, como
    // primeira opção — pedir é o gesto, então sai COM origem (selo).
    const sug = !ctx.sugestoes && colsVazio(atual)
      ? sugerir(ctx.params, ctx.valores, ctx.sugeridos, schema).find((s) => s.name === spec.name)
      : null;
    const opt = (o) => h("option", { key: o.nome, value: o.nome }, o.nome);
    campo = h("select", {
      key: "s", className: "nodrag", value: atual,
      onChange: (e) => {
        const v = e.target.value;
        if (sug && v === "\u0000sugerir") ctx.onSugerir(sug.value, sug.motivo);
        else onChange(v);
      },
    }, [
      sug ? h("option", { key: "\u0000s", value: "\u0000sugerir" }, `sugerir: ${sug.value}`) : null,
      h("option", { key: "\u0000v", value: "" }, "—"),
      // Valor que não está mais na tabela continua visível (desabilitado): um
      // select que mostrasse "—" esconderia que há um valor, e qual.
      falta ? h("option", { key: "\u0000f", value: atual, disabled: true }, `${atual} (sumiu)`) : null,
      ...serve.map(opt),
      outras.length ? h("optgroup", { key: "\u0000o", label: "outras" }, outras.map(opt)) : null,
    ]);
  } else {
    // Chip acrescenta ao que está NO CAMPO (lido do DOM), não ao valor do
    // documento: o texto digitado e ainda não comitado não pode se perder.
    // `onMouseDown` + `preventDefault` segura o foco no textarea, senão o
    // blur comitaria antes e o clique somaria sobre o valor velho.
    const somar = (e, nome) => {
      e.preventDefault();
      const ta = e.currentTarget.closest(".tr-cols")?.querySelector("textarea");
      const agora = (ta ? ta.value : String(atual)).trim();
      const novo = agora ? `${agora.replace(/,\s*$/, "")}, ${nome}` : nome;
      if (ta) ta.value = novo;
      onChange(novo);
    };
    campo = h("div", { key: "t", className: "tr-cols-multi" }, [
      colsTexto(spec, atual, onChange),
      h("div", { key: "c", className: "tr-cols-chips" }, [...serve, ...outras].map((o) =>
        h("button", { key: o.nome, type: "button", tabIndex: -1,
                      className: "nodrag tr-cols-chip" + (o.serve ? "" : " tr-cols-chip-fora"),
                      title: `acrescentar ${o.nome} (${o.papel})`,
                      onMouseDown: (e) => somar(e, o.nome) }, o.nome))),
    ]);
  }
  let aviso = null;
  if (falta) {
    const alt = alternativa(spec, falta.faltam[0], schema);
    const troca = () => {
      const nomes = String(atual).split(",").map((x) => x.trim()).filter(Boolean);
      onChange(nomes.map((n) => (n === falta.faltam[0] ? alt : n)).join(", "));
    };
    aviso = h("div", { key: "a", className: "tr-field-err" }, [
      `${falta.faltam.join(", ")} não existe na entrada`,
      alt ? h("button", { key: "u", type: "button", className: "nodrag tr-cols-usar",
                          onClick: (e) => { e.stopPropagation(); troca(); } }, `usar ${alt}`) : null,
    ]);
  }
  return h("div", { className: "tr-cols" + (ctx.sugerido ? " tr-sugerido" : ""),
                    title: ctx.sugerido ? ctx.motivo : undefined }, [
    campo,
    ctx.sugerido ? h("span", { key: "p", className: "tr-sug-pill" }, "sugerido") : null,
    aviso,
  ]);
});
registerWidget("boolean", (spec, value, onChange) =>
  h(Toggle, { value: !!(value ?? spec.default), onChange }));
// O formato do enum sai de `layoutEnum`: poucas opções curtas ficam ao lado do
// rótulo, um pouco mais longas ganham a linha inteira (`tr-seg-wide`, que o CSS
// usa pra colapsar a grade da linha), e daí pra cima o `<select>` continua —
// segmentado com opção cortada em três letras é pior que um menu.
registerWidget("enum", (spec, value, onChange) => {
  const atual = value ?? spec.default;
  const layout = layoutEnum(spec.choices);
  if (layout === "select") {
    return h("select", { className: "nodrag", value: atual,
                         onChange: (e) => onChange(e.target.value) },
      (spec.choices || []).map((c) => h("option", { key: c, value: c }, c)));
  }
  return h(Segmented, { options: spec.choices, value: atual,
                        wide: layout === "wide", onChange });
});

// --- Temas do projeto --------------------------------------------------------
// Estado de MÓDULO, e não prop: a assinatura de widget é `(spec, value,
// onChange)`, sem argumento de contexto, e as coleções já registram widgets
// contra ela — pôr um quarto argumento só pro tema quebraria esse contrato. O
// editor grava aqui ao receber a mensagem `themes` e, por fora, avisa os cards
// (o `data` do nó muda) pra que o widget releia na próxima renderização.
let temasProjeto = { temas: {}, tema_padrao: null };
export function setThemes(s) {
  temasProjeto = { temas: (s && s.temas) || {}, tema_padrao: s?.tema_padrao ?? null };
}
export function getThemes() { return temasProjeto; }

// Param `theme`: "padrão" (segue o tema padrão do projeto, que pode mudar
// depois) ou o nome de um tema. Reaproveita `layoutEnum` sobre os NOMES pra
// decidir o formato, igual ao enum.
//
// Tema gravado que não existe mais (apagado ou renomeado no painel) não some
// do documento: o card diz que o servidor está usando o padrão, mas NÃO marca
// "padrão" — marcado, reclicá-lo não mandaria op (`Segmented` e o `<select>`
// ignoram a opção atual) e não haveria como fixar "padrão" de propósito.
// Sem opção acesa, escolher "padrão" grava e limpa o órfão. O erro só aparece
// com temas CARREGADOS: antes da mensagem `themes` a lista vazia não prova
// nada, e acusar erro em todo card na abertura seria alarme falso.
registerWidget("theme", (spec, value, onChange) => {
  const { temas, tema_padrao } = getThemes();
  const atual = value ?? spec.default ?? "padrão";
  const nomes = Object.keys(temas);
  const carregados = nomes.length > 0;
  const orfao = carregados && atual !== "padrão" && !nomes.includes(atual);
  const lista = ["padrão", ...(carregados ? nomes : (atual !== "padrão" ? [atual] : []))];
  const rotuloPadrao = tema_padrao ? `padrão (${tema_padrao})` : "padrão";
  const layout = layoutEnum(lista);
  const controle = layout === "select"
    // No `<select>` o valor órfão precisa de uma opção própria (desabilitada):
    // sem ela o navegador mostraria a primeira, "padrão", como escolhida.
    ? h("select", { key: "c", className: "nodrag", value: atual,
                    onChange: (e) => onChange(e.target.value) }, [
        orfao ? h("option", { key: "\u0000orfao", value: atual, disabled: true }, `${atual} (não existe)`) : null,
        ...lista.map((n) => h("option", { key: n, value: n }, n === "padrão" ? rotuloPadrao : n)),
      ])
    // No segmentado o rótulo fica curto ("padrão") pra caber; o nome do tema
    // padrão vai na dica. O valor órfão não casa com opção nenhuma: nada aceso.
    : h(Segmented, { key: "c", value: atual, wide: layout === "wide", onChange,
                     options: lista.map((n) => ({ value: n, label: n,
                       title: n === "padrão" ? rotuloPadrao : n })) });
  if (!orfao) return controle;
  return h("div", { className: "tr-numf" }, [
    controle,
    h("div", { key: "e", className: "tr-field-err" }, `tema '${atual}' não existe; usando o padrão`),
  ]);
});

// Namespace global também, pra coleção que preferir `<script>` simples a ESM.
if (typeof window !== "undefined") {
  window.tr = { h, registerRenderer, registerWidget, getRenderer, getViews, getWidget,
               Segmented, Toggle, NumberField, Regua, Estrelas, setThemes, getThemes, React };
}
