// inst/www/markdown.js — markdown para React, sem nunca produzir HTML cru.
//
// Por que só o lexer (`marked.parse()` NÃO é usado aqui): `parse()` devolve
// uma STRING de HTML, que só entra na tela via `innerHTML`/
// `dangerouslySetInnerHTML` — e a partir daí a segurança do editor passa a
// depender de uma sanitização ficar correta pra sempre (e de continuar
// correta a cada atualização do `marked`). Usando só o `Lexer` — que devolve
// uma ÁRVORE de tokens, nunca uma string — e mapeando token por token pra
// elemento React, um `<script>` digitado no texto nunca vira marcação: é só
// texto, e não existe HTML nenhum pra sanitizar.
//
// O subconjunto suportado é FECHADO de propósito (heading 1-3, parágrafo com
// negrito/itálico/código inline, lista com um nível de aninhamento, bloco de
// código, citação, régua, tabela simples, link e imagem). Tudo que o marked
// tokeniza mas que a gente decidiu não suportar — heading nível 4+, HTML cru
// no meio do texto, qualquer token de tipo desconhecido — sai como o `raw`
// dele, LITERAL e VISÍVEL, num `<span>`. É de propósito: a pessoa vê na hora
// que aquilo não funcionou, que é a única forma de descobrir o limite do
// suporte sem precisar ler documentação nenhuma.
//
// `resolverSrc` entra por PARÂMETRO, nunca importado: quem sabe transformar
// `logo.png` em `trama-imagens/logo.png` é o editor (mesmo desenho do
// `assetUrl` passado aos renderers, ver `inst/www/editor.js:65`) — o
// markdown não tem que conhecer a rota de armazenamento.
//
// `h` (o `React.createElement` de `trama`) também entra por parâmetro, e não
// por `import { h } from "trama"` no topo do arquivo: esse especificador nu
// só resolve dentro do bundle do navegador (importmap de `R/app.R`); sob
// `node --test` ele não resolve NUNCA — é a mesma classe de problema já
// documentada em `tests/js/editor-streamcontrols.test.mjs` pra "react". Um
// `import` estático de "trama" no topo faria o módulo inteiro falhar ao
// carregar sob Node, e junto com ele `tokensDe` — que é justamente a parte
// que MAIS precisa de teste (é o coração da feature de segurança). Injetando
// `h`, o arquivo inteiro — árvore intermediária E mapeamento — fica testável
// com `node --test` puro, sem depender de React de verdade.
//
// Esquema de link em ALLOWLIST, não blocklist: só `http:`, `https:` e
// caminho relativo (sem esquema nenhum) viram link clicável. Qualquer outra
// coisa com esquema — `javascript:`, `data:`, `vbscript:`, o que vier depois
// — sai literal. Uma blocklist promete segurança até o próximo esquema que
// ninguém pensou em proibir; a allowlist não tem esse jeito de errar.

import { Lexer } from "./vendor/marked.js";
// Caminho relativo, não o especificador nu "marked" do importmap: pelo mesmo
// motivo do `h` acima, só assim `tokensDe` roda sob `node --test` E no
// bundle do navegador com o MESMO import — sem duplicar lógica entre os dois
// ambientes.

const SEM_RESOLVER = (rel) => rel;

// --- árvore intermediária -----------------------------------------------------

// `tokensDe` nunca lança: texto vazio (ou só espaço) devolve array vazio, e
// qualquer token que o marked tokenize mas a gente não decidiu suportar cai
// no fallback literal — nunca estoura o editor por causa de um markdown
// estranho.
export function tokensDe(texto, resolverSrc = SEM_RESOLVER) {
  if (!texto || !String(texto).trim()) return [];
  const tokens = new Lexer({ gfm: true }).lex(String(texto));
  return tokens.map((t) => blocoDe(t, resolverSrc)).filter((n) => n !== null);
}

function esquemaPermitido(href) {
  if (!href) return false;
  const esquema = /^([a-zA-Z][a-zA-Z0-9+.-]*):/.exec(href);
  if (!esquema) return true; // sem "algo:" na frente: caminho relativo, permitido
  return /^https?$/i.test(esquema[1]);
}

function inlineFilhos(tokens, resolverSrc) {
  return (tokens || []).map((t) => inlineNode(t, resolverSrc));
}

function inlineNode(t, resolverSrc) {
  switch (t.type) {
    case "text":
      return { tipo: "texto", texto: t.text };
    case "strong":
      return { tipo: "negrito", filhos: inlineFilhos(t.tokens, resolverSrc) };
    case "em":
      return { tipo: "italico", filhos: inlineFilhos(t.tokens, resolverSrc) };
    case "codespan":
      return { tipo: "codigo_inline", texto: t.text };
    case "link":
      if (!esquemaPermitido(t.href)) return { tipo: "literal", raw: t.raw };
      return { tipo: "link", href: t.href, filhos: inlineFilhos(t.tokens, resolverSrc) };
    case "image":
      return { tipo: "imagem", alt: t.text || "", src: resolverSrc(t.href) };
    default:
      // inclui o token "html" (ex: `<b>` solto no meio do parágrafo) e
      // qualquer coisa que o marked venha a tokenizar no futuro sem a gente
      // ter decidido suportar: literal, visível, nunca interpretado.
      return { tipo: "literal", raw: t.raw };
  }
}

function itemDe(item, resolverSrc) {
  const filhos = [];
  for (const tt of item.tokens || []) {
    if (tt.type === "list") {
      // único nível de aninhamento que o subconjunto promete: uma sub-lista
      // dentro do item vira um nó de bloco igual a qualquer outra lista.
      filhos.push(blocoDe(tt, resolverSrc));
    } else if (tt.type === "text") {
      // o texto do item de lista vem com `.tokens` próprios (inline), pra
      // permitir `**negrito**` etc dentro do item.
      filhos.push(...inlineFilhos(tt.tokens, resolverSrc));
    } else {
      filhos.push(blocoDe(tt, resolverSrc));
    }
  }
  return { filhos };
}

function blocoDe(t, resolverSrc) {
  switch (t.type) {
    case "space":
      return null; // espaço em branco entre blocos: não vira nó, sem ruído na árvore
    case "heading":
      if (t.depth > 3) return { tipo: "literal", raw: t.raw }; // fora do subconjunto (só #, ##, ###)
      return { tipo: "titulo", nivel: t.depth, filhos: inlineFilhos(t.tokens, resolverSrc) };
    case "paragraph":
      return { tipo: "paragrafo", filhos: inlineFilhos(t.tokens, resolverSrc) };
    case "list":
      return { tipo: "lista", ordenada: !!t.ordered, itens: t.items.map((it) => itemDe(it, resolverSrc)) };
    case "code":
      // `t.text` já vem sem as cercas ```; preserva quebras de linha tal
      // qual foram digitadas — não é resumido nem colapsado em uma linha.
      return { tipo: "codigo", lingua: t.lang || null, texto: t.text };
    case "blockquote":
      return { tipo: "citacao", filhos: t.tokens.map((tt) => blocoDe(tt, resolverSrc)).filter((n) => n !== null) };
    case "hr":
      return { tipo: "regua" };
    case "table":
      return {
        tipo: "tabela",
        cabecalho: t.header.map((c) => inlineFilhos(c.tokens, resolverSrc)),
        linhas: t.rows.map((linha) => linha.map((c) => inlineFilhos(c.tokens, resolverSrc))),
      };
    default:
      // token "html" em bloco (`<script>...</script>`, `<div>...`) e
      // qualquer outro tipo fora do subconjunto: literal, visível.
      return { tipo: "literal", raw: t.raw };
  }
}

// --- mapeamento pra elementos React -------------------------------------------

function inlineParaElemento(n, h, key) {
  switch (n.tipo) {
    case "texto":
      return n.texto; // string pura: React trata como nó de texto, escapado por natureza
    case "negrito":
      return h("strong", { key }, n.filhos.map((f, i) => inlineParaElemento(f, h, i)));
    case "italico":
      return h("em", { key }, n.filhos.map((f, i) => inlineParaElemento(f, h, i)));
    case "codigo_inline":
      return h("code", { key }, n.texto);
    case "link":
      return h("a", { key, href: n.href, target: "_blank", rel: "noreferrer" },
        n.filhos.map((f, i) => inlineParaElemento(f, h, i)));
    case "imagem":
      return h("img", { key, src: n.src, alt: n.alt });
    case "literal":
      return h("span", { key }, n.raw);
    default:
      return null;
  }
}

function blocoParaElemento(n, h, key) {
  switch (n.tipo) {
    case "titulo":
      return h(`h${n.nivel}`, { key }, n.filhos.map((f, i) => inlineParaElemento(f, h, i)));
    case "paragrafo":
      return h("p", { key }, n.filhos.map((f, i) => inlineParaElemento(f, h, i)));
    case "lista":
      return h(n.ordenada ? "ol" : "ul", { key },
        n.itens.map((it, i) => h("li", { key: i },
          it.filhos.map((f, j) => (f.tipo === "lista" ? blocoParaElemento(f, h, j) : inlineParaElemento(f, h, j))))));
    case "codigo":
      return h("pre", { key }, h("code", { className: n.lingua ? `language-${n.lingua}` : undefined }, n.texto));
    case "citacao":
      return h("blockquote", { key }, n.filhos.map((f, i) => blocoParaElemento(f, h, i)));
    case "regua":
      return h("hr", { key });
    case "tabela":
      return h("table", { key }, [
        h("thead", { key: "cabecalho" },
          h("tr", null, n.cabecalho.map((c, i) => h("th", { key: i }, c.map((f, j) => inlineParaElemento(f, h, j)))))),
        h("tbody", { key: "linhas" },
          n.linhas.map((linha, i) => h("tr", { key: i },
            linha.map((c, j) => h("td", { key: j }, c.map((f, k) => inlineParaElemento(f, h, k))))))),
      ]);
    case "literal":
      return h("span", { key }, n.raw);
    default:
      return null;
  }
}

// `h` é obrigatório (ver comentário do topo do arquivo pra entender o
// porquê de não vir de `import`); `resolverSrc` é opcional porque nem todo
// markdown tem imagem.
export function Markdown({ texto, resolverSrc = SEM_RESOLVER, h }) {
  const arvore = tokensDe(texto, resolverSrc);
  if (!arvore.length) return null;
  return arvore.map((n, i) => blocoParaElemento(n, h, i));
}
