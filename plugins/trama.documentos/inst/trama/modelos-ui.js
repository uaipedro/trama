// Aba Modelos do painel Documentos: a galeria de modelos (fase 3, #24) —
// ver, criar, editar, duplicar, importar, exportar, remover.
//
// Contrato com o painel (`documentos.js`):
//   - o painel desenha `h(AbaModelos, { s, enviar, despachar })` quando a aba
//     "Modelos" está aberta; `s` é o estado do painel (`painel.js`), com
//     `s.modelos` (vem em toda mensagem `lista` e em `modelos`) e os campos
//     `modelo*` do redutor de `modelos.js`;
//   - `enviar("modelos_<acao>", {...})` chega ao R em `.td_painel_modelos()`
//     (`R/painel-modelos.R`), que responde pelo mesmo canal.
// Só desenha: a lógica com ramo mora em `modelos.js`, testada.

import { h } from "trama";
import { CORES, FONTES, nomeModeloValido, pedidoEditar, pedidoDuplicar, pedidoImportar, formatosDoFormulario,
         origemDe, formatosLegiveis } from "./modelos.js";

const Botao = ({ primario, perigo, children, ...p }) => h("button", {
  type: "button", className: "tr-doc-btn" + (primario ? " tr-doc-primario" : "") + (perigo ? " tr-doc-perigo" : ""), ...p,
}, children);
const Link = ({ children, ...p }) => h("button", { type: "button", className: "tr-doc-link", ...p }, children);
const Campo = ({ rotulo, children }) => h("label", { className: "tr-doc-campo" }, [
  h("span", { key: "r", className: "tr-doc-dim" }, rotulo), children]);
const Aviso = ({ texto, tom }) => texto
  ? h("div", { className: tom === "erro" ? "tr-doc-erro" : "tr-doc-ok", role: tom === "erro" ? "alert" : "status" }, texto)
  : null;

// Lê o arquivo escolhido como base64 ({nome, conteudo}), que é como o R o recebe.
function lerArquivo(arquivo, depois) {
  if (!arquivo) return;
  const r = new FileReader();
  r.onload = () => depois({ nome: arquivo.name, conteudo: String(r.result) });
  r.readAsDataURL(arquivo);
}

function Duplicar({ s, enviar, despachar }) {
  const d = s.modeloDuplicar;
  const pedido = pedidoDuplicar(d);
  const campo = (c) => (e) => despachar({ tipo: "modelo_duplicar_campo", campo: c, valor: e.target.value });
  const ir = () => { if (!pedido) return; despachar({ tipo: "modelo_duplicar_enviando" }); enviar("modelos_duplicar", pedido); };
  return h("div", { className: "tr-doc-caixa" }, [
    h("div", { key: "t", className: "tr-doc-titulo" }, d.novo ? "Novo modelo" : "Duplicar modelo"),
    d.novo ? h(Campo, { key: "b", rotulo: "A partir de" },
      h("select", { value: d.de, onChange: campo("de") },
        s.modelos.filter((m) => m.valido !== false).map((m) => h("option", { key: m.nome, value: m.nome }, m.titulo))))
      : h("p", { key: "b", className: "tr-doc-dim tr-doc-nota" }, `Cópia de “${(s.modelos.find((m) => m.nome === d.de) || {}).titulo || d.de}”.`),
    h(Campo, { key: "ti", rotulo: "Nome na galeria" },
      h("input", { type: "text", value: d.titulo, placeholder: "Relatório do laboratório", onChange: campo("titulo"), autoFocus: true })),
    h(Campo, { key: "n", rotulo: "Nome curto (pasta e formato no Quarto)" },
      h("input", { type: "text", value: d.nome, placeholder: "relatorio-lab", onChange: campo("nome"),
                   onKeyDown: (e) => { if (e.key === "Enter") ir(); } })),
    d.nome && !nomeModeloValido(d.nome)
      ? h("div", { key: "x", className: "tr-doc-alerta" }, "Use minúsculas, dígitos e - (e não um nome de formato, como html).") : null,
    h("div", { key: "a", className: "tr-doc-linha" }, [
      h(Botao, { key: "c", primario: true, disabled: !pedido, onClick: ir }, d.enviando ? "Criando…" : "Criar"),
      h(Botao, { key: "x", onClick: () => despachar({ tipo: "modelo_cancelar" }) }, "Cancelar")]),
  ]);
}

function Importar({ s, enviar, despachar }) {
  const i = s.modeloImportar;
  const ir = (como, dados) => {
    const p = pedidoImportar(dados ? { ...i, dados } : i, como);
    if (!p) return;
    despachar({ tipo: "modelo_importar_enviando", dados: p.arquivo });
    enviar("modelos_importar", p);
  };
  const escolher = (e) => lerArquivo(e.target.files?.[0], (dados) => ir(null, dados));
  return h("div", { className: "tr-doc-caixa" }, [
    h("div", { key: "t", className: "tr-doc-titulo" }, "Importar modelo"),
    h("p", { key: "p", className: "tr-doc-dim tr-doc-nota" },
      "Um .zip exportado do trama ou uma extensão Quarto (a pasta _extensions/ com o template.qmd). O modelo é conferido antes de entrar."),
    i.enviando ? h("div", { key: "o", className: "tr-doc-ocupado" }, `Importando ${i.arquivo || ""}…`)
      : i.conflito ? h("div", { key: "c", className: "tr-doc-confirmar" }, [
          h("p", { key: "p" }, `Você já tem um modelo com o nome de “${i.arquivo}”.`),
          h("div", { key: "s", className: "tr-doc-linha" }, [
            h(Botao, { key: "s", perigo: true, onClick: () => ir("substituir") }, "Substituir o meu")]),
          h(Campo, { key: "n", rotulo: "…ou importar com outro nome" },
            h("div", { className: "tr-doc-linha" }, [
              h("input", { key: "i", type: "text", value: i.nome || "", placeholder: "outro-nome",
                           onChange: (e) => despachar({ tipo: "modelo_importar_nome", valor: e.target.value }) }),
              h(Botao, { key: "b", disabled: !pedidoImportar(i, "nome"), onClick: () => ir("nome") }, "Importar")]))])
      : h("input", { key: "f", type: "file", accept: ".zip,application/zip", onChange: escolher }),
    h(Botao, { key: "x", onClick: () => despachar({ tipo: "modelo_cancelar" }) }, "Cancelar"),
  ]);
}

function Editar({ s, enviar, despachar }) {
  const f = s.modeloForm;
  const pedido = pedidoEditar(f);
  const set = (campo, valor, grupo) => despachar({ tipo: "modelo_editar_campo", campo, valor, grupo });
  const salvar = () => { if (!pedido) return; despachar({ tipo: "modelo_enviando" }); enviar("modelos_editar", pedido); };
  const cor = (k) => f.cores[k] || "";
  return h("div", { className: "tr-doc-caixa tr-doc-modelo-form" }, [
    h("div", { key: "t", className: "tr-doc-titulo" }, `Editar “${f.original.titulo}”`),
    f.origem !== "usuario" ? h("p", { key: "e", className: "tr-doc-dim tr-doc-nota" },
      "É um modelo embutido: salvar cria uma cópia sua com o mesmo nome, que passa a valer em documentos novos. O embutido fica intacto.") : null,
    h(Campo, { key: "ti", rotulo: "Nome na galeria" },
      h("input", { type: "text", value: f.titulo, onChange: (e) => set("titulo", e.target.value) })),
    h(Campo, { key: "de", rotulo: "Descrição" },
      h("input", { type: "text", value: f.descricao, placeholder: "Para que serve", onChange: (e) => set("descricao", e.target.value) })),
    h("fieldset", { key: "fm", className: "tr-doc-grupo" }, [
      h("legend", { key: "l", className: "tr-doc-dim" }, "Formatos"),
      h("div", { key: "c", className: "tr-doc-linha" }, formatosDoFormulario(f).map(([k, rotulo]) =>
        h("label", { key: k, className: "tr-doc-check" }, [
          h("input", { key: "i", type: "checkbox", checked: f.formatos.includes(k),
                       onChange: () => despachar({ tipo: "modelo_formato", formato: k }) }),
          h("span", { key: "t" }, rotulo)]))),
      f.formatos.length ? null : h("div", { key: "x", className: "tr-doc-alerta" }, "Deixe pelo menos um formato."),
    ]),
    h("fieldset", { key: "co", className: "tr-doc-grupo" }, [
      h("legend", { key: "l", className: "tr-doc-dim" }, "Cores (os gráficos do documento usam estas, num fundo claro)"),
      h("div", { key: "c", className: "tr-doc-cores" }, CORES.map(([k, rotulo]) =>
        h("label", { key: k, className: "tr-doc-cor", title: cor(k) || "sem cor" }, [
          h("input", { key: "i", type: "color", value: cor(k) || "#ffffff", onChange: (e) => set(k, e.target.value, "cores") }),
          h("span", { key: "t", className: cor(k) ? "" : "tr-doc-dim" }, rotulo)]))),
    ]),
    h("div", { key: "fo", className: "tr-doc-linha tr-doc-fontes" }, FONTES.map(([k, rotulo]) =>
      h(Campo, { key: k, rotulo: `Fonte: ${rotulo.toLowerCase()}` },
        h("input", { type: "text", value: f.fontes[k] || "", placeholder: k === "codigo" ? "Fira Code" : "Inter",
                     onChange: (e) => set(k, e.target.value, "fontes") })))),
    h("p", { key: "fn", className: "tr-doc-dim tr-doc-nota" }, "Para o PDF, a fonte precisa estar instalada no computador."),
    h(Campo, { key: "lo", rotulo: f.logo && !f.semLogo ? `Logo: ${f.novoLogo ? f.novoLogo.nome : f.logo}` : "Logo (png, svg, jpg)" },
      h("div", { className: "tr-doc-linha" }, [
        h("input", { key: "f", type: "file", accept: ".png,.svg,.jpg,.jpeg",
                     onChange: (e) => lerArquivo(e.target.files?.[0], (a) => { set("novoLogo", a); set("semLogo", false); }) }),
        f.logo || f.novoLogo ? h(Link, { key: "x", onClick: () => { set("novoLogo", null); set("semLogo", true); } },
          f.semLogo ? "sem logo" : "Tirar o logo") : null])),
    h(Campo, { key: "cs", rotulo: f.csl && !f.semCsl ? `Citações: ${f.novoCsl ? f.novoCsl.nome : f.csl}` : "Estilo de citação (.csl)" },
      h("div", { className: "tr-doc-linha" }, [
        h("input", { key: "f", type: "file", accept: ".csl",
                     onChange: (e) => lerArquivo(e.target.files?.[0], (a) => { set("novoCsl", a); set("semCsl", false); }) }),
        f.csl || f.novoCsl ? h(Link, { key: "x", onClick: () => { set("novoCsl", null); set("semCsl", true); } },
          f.semCsl ? "estilo padrão" : "Voltar ao padrão do Quarto") : null])),
    // Capa só nos modelos que a declaram (UFLA, Consultoria).
    typeof f.original.capa === "boolean" ? h("label", { key: "ca", className: "tr-doc-check" }, [
      h("input", { key: "i", type: "checkbox", checked: f.capa === true, onChange: (e) => set("capa", e.target.checked) }),
      h("span", { key: "t" }, "Capa")]) : null,
    h(Campo, { key: "pe", rotulo: "Perfil de autor sugerido" },
      h("select", { value: f.perfil, onChange: (e) => set("perfil", e.target.value) }, [
        h("option", { key: "", value: "" }, "Nenhum (o padrão)"),
        ...s.perfis.map((p) => h("option", { key: p.perfil, value: p.perfil }, `${p.nome} (${p.perfil})`)),
        f.perfil && !s.perfis.some((p) => p.perfil === f.perfil)
          ? h("option", { key: "?", value: f.perfil }, `${f.perfil} (ainda não existe)`) : null])),
    h("div", { key: "a", className: "tr-doc-linha" }, [
      h(Botao, { key: "s", primario: true, disabled: !pedido, onClick: salvar }, f.enviando ? "Salvando…" : "Salvar"),
      h(Botao, { key: "x", onClick: () => despachar({ tipo: "modelo_cancelar" }) }, "Cancelar")]),
  ]);
}

function Modelo({ s, m, enviar, despachar }) {
  const aberto = s.modeloAberto === m.nome;
  const seu = m.origem === "usuario";
  const conf = s.modeloConfirmar?.nome === m.nome ? s.modeloConfirmar : null;
  return h("li", { className: "tr-doc-item" + (aberto ? " tr-doc-aberto" : "") }, [
    h("button", { key: "c", type: "button", className: "tr-doc-cab", "aria-expanded": aberto,
                  onClick: () => despachar({ tipo: "modelo_abrir", nome: m.nome }) }, [
      h("span", { key: "n", className: "tr-doc-nome tr-doc-cresce" }, m.titulo || m.nome),
      m.valido === false ? h("span", { key: "v", className: "tr-doc-chip tr-doc-chip-erro" }, "com problema") : null,
      h("span", { key: "o", className: "tr-doc-chip" + (seu ? " tr-doc-chip-ok" : "") }, origemDe(m))]),
    aberto ? h("div", { key: "corpo", className: "tr-doc-corpo" }, [
      h("div", { key: "f", className: "tr-doc-dim" }, formatosLegiveis(m.formatos)),
      m.descricao ? h("p", { key: "d", className: "tr-doc-nota" }, m.descricao) : null,
      m.perfil ? h("p", { key: "p", className: "tr-doc-dim tr-doc-nota" }, `Sugere o perfil “${m.perfil}”.`) : null,
      m.problema ? h("div", { key: "x", className: "tr-doc-erro" }, `Não dá para criar documento com ele: ${m.problema}`) : null,
      conf ? h("div", { key: "cf", className: "tr-doc-confirmar" }, [
        h("p", { key: "p" }, conf.voltar ? `Desfazer suas mudanças em “${m.titulo}” e voltar ao embutido?`
          : `Apagar o modelo “${m.titulo}”? Documentos já criados com ele não mudam.`),
        h("div", { key: "b", className: "tr-doc-linha" }, [
          h(Botao, { key: "s", perigo: true, disabled: !!conf.enviando, onClick: () => {
            despachar({ tipo: "modelo_remover_enviando" });
            enviar("modelos_remover", { nome: m.nome });
          } }, conf.voltar ? "Voltar" : "Apagar"),
          h(Botao, { key: "n", onClick: () => despachar({ tipo: "modelo_remover_cancelar" }) }, "Cancelar")])]) : null,
      h("div", { key: "a", className: "tr-doc-linha" }, [
        h(Botao, { key: "e", primario: true, onClick: () => enviar("modelos_detalhe", { nome: m.nome }) }, "Editar"),
        h(Botao, { key: "d", onClick: () => despachar({ tipo: "modelo_duplicar_abrir", de: m.nome }) }, "Duplicar"),
        h(Botao, { key: "x", onClick: () => enviar("modelos_exportar", { nome: m.nome }) }, "Exportar zip")]),
      seu ? h("div", { key: "l", className: "tr-doc-linha tr-doc-acoes" }, [
        h(Link, { key: "p", title: "Para quem sabe Quarto: os arquivos da extensão (Typst, CSS, reference.docx…)",
                  onClick: () => enviar("modelos_abrir", { nome: m.nome }) }, "Abrir a pasta do modelo"),
        h(Link, { key: "r", onClick: () => despachar({ tipo: "modelo_remover_pedir", nome: m.nome, voltar: !!m.sobre_embutido }) },
          m.sobre_embutido ? "Voltar ao embutido" : "Apagar")]) : null,
    ]) : null,
  ]);
}

export function AbaModelos({ s, enviar, despachar }) {
  const modelos = s.modelos || [];
  const formulario = s.modeloForm || s.modeloDuplicar || s.modeloImportar;
  return h("div", { className: "tr-doc-aba" }, [
    h("p", { key: "t", className: "tr-doc-dim tr-doc-nota" },
      "Um modelo dá ao documento novo os formatos, as cores, as fontes, a capa e o esqueleto do texto. Documento já criado não muda."),
    s.modeloForm ? h(Editar, { key: "e", s, enviar, despachar }) : null,
    s.modeloDuplicar ? h(Duplicar, { key: "d", s, enviar, despachar }) : null,
    s.modeloImportar ? h(Importar, { key: "i", s, enviar, despachar }) : null,
    h(Aviso, { key: "er", texto: s.erro, tom: "erro" }),
    h(Aviso, { key: "av", texto: s.aviso, tom: "ok" }),
    s.exportadoModelo && s.aviso?.startsWith("Exportado")
      ? h(Link, { key: "xp", onClick: () => enviar("modelos_abrir", { alvo: "exportados" }) }, "Abrir a pasta do zip") : null,
    modelos.length
      ? h("ul", { key: "l", className: "tr-doc-lista" }, modelos.map((m) => h(Modelo, { key: m.nome, s, m, enviar, despachar })))
      : h("p", { key: "v", className: "tr-doc-dim" }, "Nenhum modelo encontrado."),
    formulario ? null : h("div", { key: "b", className: "tr-doc-linha" }, [
      h(Botao, { key: "n", onClick: () => despachar({ tipo: "modelo_duplicar_abrir", de: "basico", novo: true }) }, "+ Novo modelo"),
      h(Botao, { key: "i", onClick: () => despachar({ tipo: "modelo_importar_abrir" }) }, "Importar zip")]),
  ]);
}
