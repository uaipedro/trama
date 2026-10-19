// Menu GitHub da barra do editor (plugin `trama.github`).
//
// Só desenha: o estado é o redutor de `estado.js`, e quem fala com o GitHub é
// o R (`R/plugin.R`). Canal próprio: `input$trama_github` ({acao}) para
// cima, mensagem `trama_github` ({tipo, ...}) para baixo.

import React from "react";
import { h, registerToolbarItem } from "trama";
import { INICIAL, reduzir, rotuloFonte, podeSair, indicador, dica } from "./estado.js";
import { PAINEL_INICIAL, reduzirPainel, podeSalvar, nomeRepo, tamanho, resumoArquivos,
         dataCurta, situacaoExecucao } from "./painel.js";

// Conta (`estado.js`) e seções do painel (`painel.js`) recebem as mesmas
// mensagens; cada redutor cuida da sua parte.
const INICIAL_TUDO = { ...INICIAL, painel: PAINEL_INICIAL };
const reduzirTudo = (s, a) => ({ ...reduzir(s, a), painel: reduzirPainel(s.painel, a) });

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
const enviar = (acao, extra = {}) => quando(() => typeof window.Shiny?.setInputValue === "function", () =>
  window.Shiny.setInputValue("trama_github", { ...extra, acao, t: Date.now() }, { priority: "event" }));

const abrirLink = (url) => window.open(url, "_blank", "noopener");

// Marca do GitHub (Octicons, MIT), em `currentColor` como os ícones da barra.
const MARCA = "M8 0c4.42 0 8 3.58 8 8a8.013 8.013 0 0 1-5.45 7.59c-.4.08-.55-.17-.55-.38 0-.27.01-1.13.01-2.2 0-.75-.25-1.23-.54-1.48 1.78-.2 3.65-.88 3.65-3.95 0-.88-.31-1.59-.82-2.15.08-.2.36-1.02-.08-2.12 0 0-.67-.22-2.2.82-.64-.18-1.32-.27-2-.27-.68 0-1.36.09-2 .27-1.53-1.03-2.2-.82-2.2-.82-.44 1.1-.16 1.92-.08 2.12-.51.56-.82 1.28-.82 2.15 0 3.06 1.86 3.75 3.64 3.95-.23.2-.44.55-.51 1.07-.46.21-1.61.55-2.33-.66-.15-.24-.6-.83-1.23-.82-.67.01-.27.38.01.53.34.19.73.9.82 1.13.16.45.68 1.31 2.69.94 0 .67.01 1.3.01 1.49 0 .21-.15.45-.55.38A7.995 7.995 0 0 1 0 8c0-4.42 3.58-8 8-8Z";
const Marca = ({ tamanho = 18 }) => h("svg", { className: "tr-ic", width: tamanho, height: tamanho,
  viewBox: "0 0 16 16", fill: "currentColor", "aria-hidden": true }, h("path", { d: MARCA }));

function Botao({ primario, perigo, children, ...p }) {
  const classe = "tr-gh-btn" + (primario ? " tr-gh-primario" : "") + (perigo ? " tr-gh-perigo" : "");
  return h("button", { type: "button", className: classe, ...p }, children);
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

  if (s.fase === "conectado") return h(Painel, { s, despachar, erro });

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

function Conta({ s }) {
  const u = s.usuario || {};
  return h("div", { className: "tr-gh-conta" }, [
    h("div", { key: "u", className: "tr-gh-usuario" }, [
      u.avatar ? h("img", { key: "a", src: u.avatar, alt: "", className: "tr-gh-avatar" }) : null,
      h("div", { key: "n", className: "tr-gh-cresce" }, [
        h("div", { key: "n", className: "tr-gh-nome" }, u.nome || u.login),
        h("div", { key: "l", className: "tr-gh-dim" }, `@${u.login} · conectado ${rotuloFonte(s.fonte)}`),
      ]),
      h("button", { key: "p", type: "button", className: "tr-gh-link", onClick: () => abrirLink(u.perfil) }, "Perfil ↗"),
      podeSair(s) ? h("button", { key: "s", type: "button", className: "tr-gh-link", onClick: () => enviar("sair") }, "Sair") : null,
    ]),
    !s.persistente && s.fonte === "chaveiro"
      ? h("p", { key: "p", className: "tr-gh-aviso" },
          "Este computador não tem um chaveiro seguro: o login vale até fechar o trama.")
      : null,
    !podeSair(s)
      ? h("p", { key: "fora", className: "tr-gh-dim tr-gh-nota" },
          `A credencial vem de fora do trama (${s.fonte}). Para desconectar, remova-a por lá.`)
      : null,
  ]);
}

function Painel({ s, despachar, erro }) {
  return h(React.Fragment, null, [
    h(Conta, { key: "c", s }),
    erro,
    h(SecaoProjeto, { key: "p", pr: s.painel.projeto, login: s.usuario?.login, despachar }),
    h(SecaoPublicacoes, { key: "u", pb: s.painel.publicacoes, despachar }),
  ]);
}

const Cabecalho = ({ titulo, selo }) => h("div", { className: "tr-gh-secao-cab" }, [
  h("span", { key: "t" }, titulo), h("span", { key: "s", className: "tr-gh-selo" }, selo)]);
const Aviso = ({ texto, tom }) => texto
  ? h("div", { className: tom === "erro" ? "tr-gh-erro" : "tr-gh-ok", role: tom === "erro" ? "alert" : "status" }, texto)
  : null;
const OCUPADO = { criando: "Criando o repositório e salvando a primeira versão…", salvando: "Salvando a versão…",
                  restaurando: "Restaurando a versão…", marcando: "Atualizando…" };

function Dados({ pr, despachar }) {
  const dados = pr.arquivos?.dados || [];
  return h("div", { className: "tr-gh-dados" }, [
    h("div", { key: "r", className: "tr-gh-linha" }, [
      h("span", { key: "t", className: "tr-gh-dim tr-gh-cresce" }, resumoArquivos(pr.arquivos)),
      dados.length ? h("button", { key: "b", type: "button", className: "tr-gh-link",
                                   onClick: () => despachar({ tipo: "alternar_dados" }) },
        pr.dadosAbertos ? "Fechar" : "Escolher dados") : null,
    ]),
    pr.dadosAbertos ? h("ul", { key: "l", className: "tr-gh-lista" }, dados.map((d) =>
      h("li", { key: d.caminho }, h("label", { className: "tr-gh-dado" + (d.bloqueado ? " tr-gh-off" : "") }, [
        h("input", { key: "c", type: "checkbox", checked: !!d.incluido, disabled: d.bloqueado || !!pr.ocupado,
                     onChange: (e) => { despachar({ tipo: "ocupar", o: "marcando" });
                                        enviar("marcar", { caminho: d.caminho, incluir: e.target.checked }); } }),
        h("span", { key: "n", className: "tr-gh-cresce" }, [
          h("span", { key: "c" }, d.caminho),
          h("span", { key: "t", className: "tr-gh-dim" }, ` · ${tamanho(d.tamanho)}`),
          d.aviso ? h("span", { key: "a", className: "tr-gh-alerta" }, d.aviso) : null,
        ]),
      ])))) : null,
    pr.dadosAbertos ? h("p", { key: "n", className: "tr-gh-nota tr-gh-dim" },
      "Dado marcado vai para o repositório privado em cada versão. Não marque dados pessoais " +
      "sem autorização para guardá-los fora deste computador.") : null,
  ]);
}

function Versoes({ pr, despachar }) {
  if (!pr.versoes.length) return null;
  const c = pr.confirmar;
  return h("div", { className: "tr-gh-versoes" }, [
    h("div", { key: "t", className: "tr-gh-subtitulo" }, "Versões salvas"),
    h("ul", { key: "l", className: "tr-gh-lista" }, pr.versoes.map((v, i) =>
      h("li", { key: v.sha, className: "tr-gh-versao" }, [
        h("div", { key: "l", className: "tr-gh-linha" }, [
          h("div", { key: "m", className: "tr-gh-cresce" }, [
            h("div", { key: "m" }, v.mensagem),
            h("div", { key: "d", className: "tr-gh-dim" }, [dataCurta(v.data), i === 0 ? " · a mais recente" : ""].join("")),
          ]),
          h("button", { key: "v", type: "button", className: "tr-gh-link", onClick: () => abrirLink(v.url) }, "ver ↗"),
          h("button", { key: "r", type: "button", className: "tr-gh-link", disabled: !!pr.ocupado,
                        onClick: () => despachar({ tipo: "confirmar", c: { tipo: "restaurar", sha: v.sha, mensagem: v.mensagem } }) },
            "Restaurar"),
        ]),
        c?.tipo === "restaurar" && c.sha === v.sha ? h("div", { key: "c", className: "tr-gh-confirmar", role: "alertdialog" }, [
          h("p", { key: "p" }, `Restaurar “${v.mensagem}”? Fluxos, código e configuração voltam a como estavam ` +
            "nessa versão, e o editor recarrega o projeto. O estado atual fica guardado em .trama/restauracao. " +
            "Dados que não estão na versão não são apagados."),
          h("div", { key: "b", className: "tr-gh-linha" }, [
            h(Botao, { key: "s", perigo: true, onClick: () => { despachar({ tipo: "ocupar", o: "restaurando" });
                                                               enviar("restaurar", { sha: v.sha }); } }, "Restaurar esta versão"),
            h(Botao, { key: "n", onClick: () => despachar({ tipo: "cancelar_confirmacao" }) }, "Cancelar"),
          ]),
        ]) : null,
      ]))),
  ]);
}

function SecaoProjeto({ pr, login, despachar }) {
  const cab = h(Cabecalho, { key: "cab", titulo: "Projeto", selo: "privado" });
  if (pr.fase === "inicial" || pr.fase === "carregando") {
    return h("section", { className: "tr-gh-secao" }, [cab, h("p", { key: "c", className: "tr-gh-dim" }, "Lendo o projeto…")]);
  }
  if (pr.fase === "erro") {
    return h("section", { className: "tr-gh-secao" }, [cab, h(Aviso, { key: "e", texto: pr.erro, tom: "erro" }),
      h(Botao, { key: "t", onClick: () => { despachar({ tipo: "carregar" }); enviar("projeto"); } }, "Tentar de novo")]);
  }
  const avisos = [h(Aviso, { key: "e", texto: pr.erro, tom: "erro" }), h(Aviso, { key: "a", texto: pr.aviso, tom: "ok" }),
                  pr.ocupado ? h("p", { key: "o", className: "tr-gh-espera" }, OCUPADO[pr.ocupado] || "Trabalhando…") : null];

  if (!pr.ligado) {
    const criando = pr.confirmar?.tipo === "criar";
    return h("section", { className: "tr-gh-secao" }, [cab, ...avisos,
      criando ? h(React.Fragment, { key: "f" }, [
        h("p", { key: "p", className: "tr-gh-texto" },
          `O trama cria um repositório privado na sua conta${login ? ` (@${login})` : ""}: só você vê. ` +
          "Vão os fluxos, o código e a configuração; os dados ficam de fora até você marcar."),
        h("label", { key: "l", className: "tr-gh-campo" }, [
          h("span", { key: "t", className: "tr-gh-dim" }, "Nome do repositório"),
          h("input", { key: "i", type: "text", value: nomeRepo(pr), disabled: !!pr.ocupado,
                       onChange: (e) => despachar({ tipo: "nome", texto: e.target.value }) }),
        ]),
        h(Dados, { key: "d", pr, despachar }),
        h(Botao, { key: "c", primario: true, disabled: !nomeRepo(pr) || !!pr.ocupado,
                   onClick: () => { despachar({ tipo: "ocupar", o: "criando" }); enviar("criar_repo", { nome: nomeRepo(pr) }); } },
          "Criar e salvar a primeira versão"),
        h(Botao, { key: "x", disabled: !!pr.ocupado, onClick: () => despachar({ tipo: "cancelar_confirmacao" }) }, "Cancelar"),
      ]) : h(React.Fragment, { key: "f" }, [
        h("p", { key: "p", className: "tr-gh-texto" },
          "As versões deste projeto ainda não estão no GitHub. Guarde-as num repositório privado e volte a qualquer uma quando quiser."),
        h(Botao, { key: "b", primario: true, onClick: () => despachar({ tipo: "confirmar", c: { tipo: "criar" } }) },
          "Guardar versões no GitHub…"),
      ]),
    ]);
  }

  const salvar = () => { if (!podeSalvar(pr)) return;
                         despachar({ tipo: "ocupar", o: "salvando" }); enviar("salvar", { mensagem: pr.rascunho.trim() }); };
  return h("section", { className: "tr-gh-secao" }, [cab,
    h("div", { key: "r", className: "tr-gh-linha" }, [
      h("span", { key: "n", className: "tr-gh-cresce tr-gh-repo" }, pr.repo),
      h("button", { key: "a", type: "button", className: "tr-gh-link", onClick: () => abrirLink(pr.url) }, "Abrir ↗"),
    ]),
    ...avisos,
    h("div", { key: "s", className: "tr-gh-salvar" }, [
      h("input", { key: "i", type: "text", value: pr.rascunho, disabled: !!pr.ocupado, maxLength: 200,
                   placeholder: "O que mudou desde a última versão?", "aria-label": "O que mudou",
                   onChange: (e) => despachar({ tipo: "rascunho", texto: e.target.value }),
                   onKeyDown: (e) => { if (e.key === "Enter") salvar(); } }),
      h(Botao, { key: "b", primario: true, disabled: !podeSalvar(pr), onClick: salvar }, "Salvar versão"),
    ]),
    h(Dados, { key: "d", pr, despachar }),
    h(Versoes, { key: "v", pr, despachar }),
  ]);
}

function SecaoPublicacoes({ pb, despachar }) {
  const cab = h(Cabecalho, { key: "cab", titulo: "Publicações", selo: "públicas" });
  const corpo = pb.fase !== "pronto" ? h("p", { key: "c", className: "tr-gh-dim" }, "Lendo as publicações…")
    : !pb.disponivel ? h("p", { key: "c", className: "tr-gh-dim" }, "Para publicar documentos, instale o plugin trama.documentos.")
    : !pb.itens.length ? h("p", { key: "c", className: "tr-gh-dim" },
        "Nenhuma publicação neste projeto ainda. Publique um documento pelo painel Documentos.")
    : h("ul", { key: "l", className: "tr-gh-lista" }, pb.itens.map((p) => {
        const sit = situacaoExecucao(p.execucao);
        const n = (p.versoes || []).length;
        return h("li", { key: p.documento, className: "tr-gh-pub" }, [
          h("div", { key: "t", className: "tr-gh-linha" }, [
            h("span", { key: "n", className: "tr-gh-cresce tr-gh-repo", title: p.repo }, p.nome || (p.repo || "").split("/").pop()),
            sit ? h("span", { key: "s", className: `tr-gh-chip tr-gh-chip-${sit.tom}` }, sit.rotulo) : null,
          ]),
          h("div", { key: "d", className: "tr-gh-dim" },
            [p.versao ? `v${p.versao}` : null, n ? `${n} ${n === 1 ? "versão" : "versões"}` : null, p.data]
              .filter(Boolean).join(" · ")),
          h(Aviso, { key: "e", texto: p.erro, tom: "erro" }),
          h("div", { key: "a", className: "tr-gh-linha tr-gh-links" }, [
            p.pagina ? h("button", { key: "p", type: "button", className: "tr-gh-link", onClick: () => abrirLink(p.pagina) }, "Página ↗") : null,
            p.release ? h("button", { key: "r", type: "button", className: "tr-gh-link", onClick: () => abrirLink(p.release) }, "Release ↗") : null,
            p.url ? h("button", { key: "g", type: "button", className: "tr-gh-link", onClick: () => abrirLink(p.url) }, "Repositório ↗") : null,
            sit?.tom === "erro" && p.execucao?.url
              ? h("button", { key: "x", type: "button", className: "tr-gh-link", onClick: () => abrirLink(p.execucao.url) }, "Ver o erro ↗") : null,
            // O painel Documentos é de outro plugin: um evento do navegador,
            // sem um depender do outro.
            h("button", { key: "b", type: "button", className: "tr-gh-link", title: "Publicar uma versão nova pelo documento",
                          onClick: () => { window.dispatchEvent(new CustomEvent("trama:documentos",
                                             { detail: { acao: "abrir", documento: p.documento } }));
                                           despachar({ tipo: "fechar" }); } }, "Abrir o documento"),
          ]),
        ]);
      }));
  return h("section", { className: "tr-gh-secao" }, [cab, h(Aviso, { key: "e", texto: pb.erro, tom: "erro" }), corpo]);
}

function MenuGitHub() {
  const [s, despachar] = React.useReducer(reduzirTudo, INICIAL_TUDO);
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

  // Painel aberto com alguém conectado: projeto e publicações são lidos na
  // hora (as versões e o Actions mudam fora do editor).
  React.useEffect(() => {
    if (!s.aberto || s.fase !== "conectado") return;
    despachar({ tipo: "carregar" });
    enviar("projeto");
    enviar("publicacoes");
  }, [s.aberto, s.fase]);

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
