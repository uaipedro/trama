// Menu GitHub da barra do editor (pacote de apoio `trama.github`).
//
// Só desenha: o estado é o redutor de `estado.js`, e quem fala com o GitHub é
// o R (`R/extensao.R`). Canal próprio: `input$trama_github` ({acao}) para
// cima, mensagem `trama_github` ({tipo, ...}) para baixo.

import React from "react";
import { h, registerToolbarItem } from "trama";
import { INICIAL, reduzir, rotuloFonte, podeSair, indicador, dica } from "./estado.js";

// Shiny: `addCustomMessageHandler` existe assim que shiny.js carrega, mas
// `setInputValue` só depois que a sessão conecta (ver `shinyReady` no editor).
// Espera por polling, como o editor, e não pelo evento do jQuery.
const ouvintes = new Set();
function quando(pronto, fn, tentativas = 400) {
  if (pronto()) return fn();
  if (tentativas > 0) setTimeout(() => quando(pronto, fn, tentativas - 1), 50);
}
quando(() => !!window.Shiny?.addCustomMessageHandler, () =>
  window.Shiny.addCustomMessageHandler("trama_github", (m) => ouvintes.forEach((f) => f(m))));
const enviar = (acao) => quando(() => typeof window.Shiny?.setInputValue === "function", () =>
  window.Shiny.setInputValue("trama_github", { acao, t: Date.now() }, { priority: "event" }));

const abrirLink = (url) => window.open(url, "_blank", "noopener");

// Marca do GitHub (Octicons, MIT), em `currentColor` como os ícones da barra.
const MARCA = "M8 0c4.42 0 8 3.58 8 8a8.013 8.013 0 0 1-5.45 7.59c-.4.08-.55-.17-.55-.38 0-.27.01-1.13.01-2.2 0-.75-.25-1.23-.54-1.48 1.78-.2 3.65-.88 3.65-3.95 0-.88-.31-1.59-.82-2.15.08-.2.36-1.02-.08-2.12 0 0-.67-.22-2.2.82-.64-.18-1.32-.27-2-.27-.68 0-1.36.09-2 .27-1.53-1.03-2.2-.82-2.2-.82-.44 1.1-.16 1.92-.08 2.12-.51.56-.82 1.28-.82 2.15 0 3.06 1.86 3.75 3.64 3.95-.23.2-.44.55-.51 1.07-.46.21-1.61.55-2.33-.66-.15-.24-.6-.83-1.23-.82-.67.01-.27.38.01.53.34.19.73.9.82 1.13.16.45.68 1.31 2.69.94 0 .67.01 1.3.01 1.49 0 .21-.15.45-.55.38A7.995 7.995 0 0 1 0 8c0-4.42 3.58-8 8-8Z";
const Marca = ({ tamanho = 18 }) => h("svg", { className: "tr-ic", width: tamanho, height: tamanho,
  viewBox: "0 0 16 16", fill: "currentColor", "aria-hidden": true }, h("path", { d: MARCA }));

function Botao({ primario, children, ...p }) {
  return h("button", { type: "button", className: primario ? "tr-gh-btn tr-gh-primario" : "tr-gh-btn", ...p }, children);
}

function Guia({ passos, voltar }) {
  return h("div", { className: "tr-gh-guia" }, [
    h("div", { key: "t", className: "tr-gh-titulo" }, "Criar sua conta no GitHub"),
    h("ol", { key: "l", className: "tr-gh-passos" }, passos.map((p, i) =>
      h("li", { key: i }, [
        h("strong", { key: "t" }, p.titulo),
        h("p", { key: "x" }, p.texto),
        p.link ? h("button", { key: "a", type: "button", className: "tr-gh-link", onClick: () => abrirLink(p.link) },
          `${p.rotulo || "Abrir"} ↗`) : null,
      ]))),
    h(Botao, { key: "v", onClick: voltar }, "Voltar"),
  ]);
}

function Conteudo({ s, despachar }) {
  if (s.guia) return h(Guia, { passos: s.guia, voltar: () => despachar({ tipo: "fechar_guia" }) });
  const erro = s.erro ? h("div", { key: "erro", className: "tr-gh-erro", role: "alert" }, s.erro) : null;

  if (s.fase === "carregando") return h("div", { className: "tr-gh-texto" }, "Verificando a conexão…");
  if (s.fase === "pedindo") return h("div", { className: "tr-gh-texto" }, "Pedindo um código ao GitHub…");

  if (s.fase === "aguardando") {
    const copiarEAbrir = () => {
      navigator.clipboard?.writeText(s.codigo).then(() => despachar({ tipo: "copiado" }), () => {});
      abrirLink(s.link);
    };
    return h(React.Fragment, null, [
      h("div", { key: "t", className: "tr-gh-titulo" }, "Autorize o trama no GitHub"),
      h("p", { key: "p", className: "tr-gh-texto" }, "Digite este código na página do GitHub que vai abrir:"),
      h("div", { key: "c", className: "tr-gh-codigo", "aria-label": "código" }, s.codigo),
      h(Botao, { key: "a", primario: true, onClick: copiarEAbrir },
        s.copiado ? "Código copiado · abrir o GitHub de novo" : "Copiar o código e abrir o GitHub"),
      h("p", { key: "e", className: "tr-gh-espera" }, "Esperando a autorização… esta janela atualiza sozinha."),
      h(Botao, { key: "x", onClick: () => enviar("cancelar") }, "Cancelar"),
    ]);
  }

  if (s.fase === "conectado") {
    const u = s.usuario || {};
    return h(React.Fragment, null, [
      h("div", { key: "u", className: "tr-gh-usuario" }, [
        u.avatar ? h("img", { key: "a", src: u.avatar, alt: "", className: "tr-gh-avatar" }) : null,
        h("div", { key: "n" }, [
          h("div", { key: "n", className: "tr-gh-nome" }, u.nome || u.login),
          h("div", { key: "l", className: "tr-gh-dim" }, `@${u.login} · conectado ${rotuloFonte(s.fonte)}`),
        ]),
      ]),
      !s.persistente && s.fonte === "chaveiro"
        ? h("p", { key: "p", className: "tr-gh-aviso" },
            "Este computador não tem um chaveiro seguro: o login vale até fechar o trama.")
        : null,
      erro,
      h("div", { key: "s", className: "tr-pop-sep" }),
      h("button", { key: "perfil", type: "button", className: "tr-pop-item", onClick: () => abrirLink(u.perfil) },
        "Abrir meu perfil no GitHub ↗"),
      podeSair(s)
        ? h("button", { key: "sair", type: "button", className: "tr-pop-item", onClick: () => enviar("sair") }, "Sair")
        : h("p", { key: "fora", className: "tr-gh-dim tr-gh-nota" },
            `A credencial vem de fora do trama (${s.fonte}). Para desconectar, remova-a por lá.`),
    ]);
  }

  // desconectado
  return h(React.Fragment, null, [
    h("div", { key: "t", className: "tr-gh-titulo" }, "Conectar ao GitHub"),
    h("p", { key: "p", className: "tr-gh-texto" },
      "Com o GitHub, o trama guarda versões do seu projeto e publica resultados. Sem terminal e sem git."),
    erro,
    s.loginDisponivel
      ? h(Botao, { key: "e", primario: true, onClick: () => { despachar({ tipo: "entrando" }); enviar("entrar"); } },
          [h(Marca, { key: "m", tamanho: 16 }), h("span", { key: "t" }, "Entrar com o GitHub")])
      : h("p", { key: "sem", className: "tr-gh-aviso" },
          "O login pelo navegador ainda não está configurado neste trama. Se você já usa o GitHub neste " +
          "computador (GITHUB_PAT, gh ou git), o trama aproveita a credencial sozinho."),
    h("button", { key: "g", type: "button", className: "tr-pop-item", onClick: () => enviar("guia") },
      "Ainda não tenho conta no GitHub"),
  ]);
}

function MenuGitHub() {
  const [s, despachar] = React.useReducer(reduzir, INICIAL);
  const raiz = React.useRef(null);

  React.useEffect(() => {
    const ouvir = (m) => despachar(m);
    ouvintes.add(ouvir);
    enviar("estado");
    return () => ouvintes.delete(ouvir);
  }, []);

  // Fecha no clique fora e no Esc. Fechar durante a espera do código não
  // cancela o login: a conferência segue no R e a bolinha acende ao conectar.
  React.useEffect(() => {
    if (!s.aberto) return undefined;
    const fora = (e) => { if (raiz.current && !raiz.current.contains(e.target)) despachar({ tipo: "fechar" }); };
    const esc = (e) => { if (e.key === "Escape") despachar({ tipo: "fechar" }); };
    document.addEventListener("pointerdown", fora, true);
    document.addEventListener("keydown", esc);
    return () => { document.removeEventListener("pointerdown", fora, true); document.removeEventListener("keydown", esc); };
  }, [s.aberto]);

  const alternar = () => {
    if (!s.aberto && s.fase !== "aguardando" && s.fase !== "pedindo") enviar("estado");
    despachar({ tipo: s.aberto ? "fechar" : "abrir" });
  };
  const luz = indicador(s);
  return h("span", { ref: raiz, className: "tr-gh" }, [
    h("button", { key: "b", type: "button", className: "tr-tb-btn" + (s.aberto ? " tr-on" : ""),
                  "aria-label": "GitHub", title: dica(s), "aria-expanded": s.aberto, onClick: alternar }, [
      h(Marca, { key: "m" }),
      luz ? h("span", { key: "l", className: `tr-gh-luz tr-gh-luz-${luz}`, "aria-hidden": true }) : null,
    ]),
    s.aberto ? h("div", { key: "p", className: "tr-toolbar-pop tr-gh-pop", role: "dialog", "aria-label": "GitHub" },
      h(Conteudo, { s, despachar })) : null,
  ]);
}

registerToolbarItem("github", MenuGitHub);
