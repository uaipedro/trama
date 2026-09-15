// inst/www/settings.js — painel ⚙: os temas do projeto (trama.json).
//
// O painel edita um RASCUNHO local e manda o conjunto inteiro a cada edição
// concluída; quem decide o que vale é o servidor, que valida, grava e devolve
// a mensagem `themes` com o estado real. Por isso o rascunho se ressincroniza
// sempre que `temas`/`padrao` chegam: numa recusa, a volta desfaz na tela o
// que não foi gravado. As regras puras (nome livre, apagar o padrão,
// renomear) moram em `temas.js`, testadas sem DOM.

import React from "react";
import { h, Segmented, NumberField } from "trama";
import { layoutEnum } from "./params.js";
import { HEX, copiar, erroNome, duplicar, renomear, apagar } from "./temas.js";

const BASES = [{ value: "minimal", label: "mínimo" }, { value: "bw", label: "bordas" },
               { value: "classic", label: "clássico" }];
const FONTES = [{ value: "sans", label: "sem serifa" }, { value: "serif", label: "serifa" },
                { value: "mono", label: "mono" }];
const CONTINUAS = ["viridis", "magma", "cividis", "azuis", "divergente"];
const CORES = [["fundo", "fundo"], ["texto", "texto"], ["eixos", "eixos"], ["grade", "grade"]];
const TAMANHO = { kind: "number", min: 6, max: 40 };
const AVISO_CARDS = "cards que usam este tema passam a usar o padrão";

// `<input type=color>` solta `input` a cada passo do arrasto no seletor e
// `change` só quando ele fecha (Chromium e Firefox). O `onChange` do React é o
// `input` nativo — ligá-lo ao salvar mandaria dezenas de gravações do
// trama.json (e reexecuções) por arrasto. Então: `input` só mexe no rascunho
// (a miniatura e o campo hex acompanham), e o `change` nativo, que o React não
// expõe, salva.
function Cor({ value, onInput, onCommit, title }) {
  const ref = React.useRef(null);
  const commit = React.useRef(onCommit);
  commit.current = onCommit;
  React.useEffect(() => {
    const el = ref.current;
    const f = (e) => commit.current(e.target.value);
    el.addEventListener("change", f);
    return () => el.removeEventListener("change", f);
  }, []);
  return h("input", { ref, type: "color", className: "tr-settings-cor", value, title,
                      onChange: (e) => onInput(e.target.value) });
}

// Mesma filosofia do `NumberField`: valida a cada tecla, comita no blur/Enter,
// segura o texto inválido na tela junto da mensagem.
function Hex({ value, onCommit }) {
  const [texto, setTexto] = React.useState(value);
  const [erro, setErro] = React.useState(null);
  React.useEffect(() => { setTexto(value); setErro(null); }, [value]);
  const comitar = () => {
    const t = texto.trim().toLowerCase();
    if (!HEX.test(t)) { if (texto !== value) setErro("use #rrggbb"); return; }
    // "#FFFFFF" sobre "#ffffff" não muda nada e não gera eco: volta sozinho.
    if (t === value) setTexto(value); else onCommit(t);
  };
  return h("div", { className: "tr-numf" }, [
    h("input", { key: "i", type: "text", value: texto, spellCheck: false,
                 className: "tr-settings-hex" + (erro ? " tr-invalid" : ""),
                 "aria-invalid": erro ? true : undefined,
                 onChange: (e) => { setTexto(e.target.value);
                                    setErro(HEX.test(e.target.value.trim()) ? null : "use #rrggbb"); },
                 onBlur: comitar,
                 onKeyDown: (e) => {
                   // Enter só tira o foco, e o blur comita: comitar aqui E no blur
                   // gravaria duas vezes se o foco saísse antes do eco.
                   if (e.key === "Enter") e.target.blur();
                   else if (e.key === "Escape") { setTexto(value); setErro(null); }
                 } }),
    erro ? h("div", { key: "e", className: "tr-field-err" }, erro) : null,
  ]);
}

// Miniatura: o fundo do tema com as quatro primeiras cores da paleta — o que
// distingue dois temas de relance, sem renderizar gráfico nenhum.
function Miniatura({ tema }) {
  return h("span", { className: "tr-settings-thumb", style: { background: tema.fundo } },
    (tema.paleta || []).slice(0, 4).map((c, i) =>
      h("i", { key: i, style: { background: c } })));
}

function Campo({ rotulo, children }) {
  // Filhos como argumentos, não array: array pediria `key` ao filho que vem de fora.
  return h("div", { className: "tr-settings-field" },
    h("span", { className: "tr-settings-label" }, rotulo), children);
}

const primeiro = (e) => e.tema_padrao ?? Object.keys(e.temas)[0] ?? null;

export function SettingsPanel({ temas, padrao, onSave, onClose }) {
  const [rascunho, setRascunho] = React.useState(() => copiar({ temas, tema_padrao: padrao }));
  // O ref anda junto do estado e é lido nos callbacks: o `change` da cor chega
  // depois de uma rajada de `input`, e o fechamento do render anterior veria
  // um rascunho velho.
  const rascRef = React.useRef(rascunho);
  const [sel, setSel] = React.useState(() => primeiro(rascunho));
  const [renome, setRenome] = React.useState(null);   // {texto, erro} ou null

  const servidor = JSON.stringify({ temas: temas || {}, tema_padrao: padrao ?? null });
  const servRef = React.useRef(servidor);
  servRef.current = servidor;

  React.useEffect(() => {
    const r = copiar({ temas, tema_padrao: padrao });
    rascRef.current = r;
    setRascunho(r);
    setSel((s) => (s != null && Object.hasOwn(r.temas, s) ? s : primeiro(r)));
  }, [temas, padrao]);

  const aplicar = (r, salvar) => {
    rascRef.current = r;
    setRascunho(r);
    // Comparado com o que o servidor tem: segmento reclicado, hex reescrito
    // igual, cor escolhida e devolvida — nada disso merece gravação e rerun.
    if (salvar && JSON.stringify(r) !== servRef.current) onSave(r);
  };
  const campo = (k, v, salvar = true) => {
    const r = copiar(rascRef.current);
    if (!r.temas[sel]) return;
    r.temas[sel][k] = v;
    aplicar(r, salvar);
  };
  const cor = (i, v, salvar) => {
    const r = copiar(rascRef.current);
    if (!r.temas[sel]) return;
    r.temas[sel].paleta[i] = v;
    aplicar(r, salvar);
  };
  const paleta = (fn) => {
    const r = copiar(rascRef.current);
    if (!r.temas[sel]) return;
    r.temas[sel].paleta = fn(r.temas[sel].paleta);
    aplicar(r, true);
  };

  const nomes = Object.keys(rascunho.temas);
  const tema = sel != null ? rascunho.temas[sel] : null;

  const renomeRef = React.useRef(renome);
  renomeRef.current = renome;
  // Chamado pelo Enter e pelo blur; o ref impede a segunda gravação quando o
  // campo some depois da primeira. Nome inválido não sai nem fecha o campo: a
  // mensagem fica ao lado do que foi digitado, e Esc desiste.
  const confirmarRenome = () => {
    const rn = renomeRef.current;
    if (!rn || sel == null) return;
    const nome = rn.texto.trim();
    if (nome === sel) { setRenome(null); return; }
    const erro = erroNome(nome, Object.keys(rascRef.current.temas), sel);
    if (erro) { setRenome({ ...rn, erro }); return; }
    renomeRef.current = null;
    setRenome(null);
    setSel(nome);
    aplicar(renomear(rascRef.current, sel, nome), true);
  };

  const lista = h("div", { key: "l", className: "tr-settings-list" }, nomes.map((n) => {
    const t = rascunho.temas[n];
    const estrela = n === rascunho.tema_padrao
      ? h("span", { key: "p", className: "tr-settings-star", title: "tema padrão" }, "★") : null;
    if (renome && n === sel) {
      return h("div", { key: n, className: "tr-settings-item tr-settings-item-on" }, [
        h(Miniatura, { key: "m", tema: t }),
        h("div", { key: "i", className: "tr-numf tr-settings-grow" }, [
          h("input", { key: "i", type: "text", autoFocus: true, value: renome.texto,
                       className: "tr-settings-hex" + (renome.erro ? " tr-invalid" : ""),
                       onChange: (e) => {
                         const texto = e.target.value;
                         setRenome({ texto, erro: texto.trim() === sel ? null
                                     : erroNome(texto, Object.keys(rascRef.current.temas), sel) });
                       },
                       onBlur: confirmarRenome,
                       onKeyDown: (e) => {
                         if (e.key === "Enter") confirmarRenome();
                         else if (e.key === "Escape") setRenome(null);
                       } }),
          renome.erro ? h("div", { key: "e", className: "tr-field-err" }, renome.erro) : null,
        ]),
        estrela,
      ]);
    }
    return h("button", { key: n, type: "button",
                         className: "tr-settings-item" + (n === sel ? " tr-settings-item-on" : ""),
                         onClick: () => { setRenome(null); setSel(n); } }, [
      h(Miniatura, { key: "m", tema: t }),
      h("span", { key: "n", className: "tr-settings-name", title: n }, n),
      estrela,
    ]);
  }));

  const acoes = h("div", { key: "a", className: "tr-settings-actions" }, [
    // Sem temas carregados ainda, "Novo" gravaria um projeto com UM tema só
    // (o molde), apagando os que o servidor ainda não mandou.
    h("button", { key: "n", type: "button", title: "duplica o tema selecionado",
                  disabled: nomes.length === 0,
                  onClick: () => {
                    const { estado, novo } = duplicar(rascRef.current, sel);
                    setRenome(null); setSel(novo); aplicar(estado, true);
                  } }, "+ Novo"),
    h("button", { key: "r", type: "button", disabled: !tema, title: `renomear — ${AVISO_CARDS}`,
                  onClick: () => setRenome({ texto: sel, erro: null }) }, "Renomear"),
    h("button", { key: "d", type: "button", disabled: !tema || nomes.length <= 1,
                  title: nomes.length <= 1 ? "o projeto precisa de ao menos um tema"
                                           : `apagar — ${AVISO_CARDS}`,
                  onClick: () => {
                    const r = apagar(rascRef.current, sel);
                    setRenome(null); setSel(primeiro(r)); aplicar(r, true);
                  } }, "Apagar"),
    h("button", { key: "p", type: "button", disabled: !tema || sel === rascunho.tema_padrao,
                  title: "cards em 'padrão' passam a usar este tema",
                  onClick: () => aplicar({ ...copiar(rascRef.current), tema_padrao: sel }, true) },
      "★ Tornar padrão"),
  ]);

  const continua = layoutEnum(CONTINUAS);
  const editor = tema ? h("div", { key: "ed", className: "tr-settings-editor" }, [
    h("h4", { key: "t" }, sel),
    h(Campo, { key: "b", rotulo: "base" },
      h(Segmented, { options: BASES, value: tema.base, onChange: (v) => campo("base", v) })),
    h(Campo, { key: "f", rotulo: "fonte" },
      h(Segmented, { options: FONTES, value: tema.fonte, onChange: (v) => campo("fonte", v) })),
    h(Campo, { key: "s", rotulo: "tamanho" },
      h(NumberField, { spec: TAMANHO, value: tema.tamanho, onChange: (v) => campo("tamanho", v) })),
    ...CORES.map(([k, rotulo]) => h(Campo, { key: k, rotulo },
      h("div", { className: "tr-settings-corrow" }, [
        h(Cor, { key: "c", value: tema[k], title: rotulo,
                 onInput: (v) => campo(k, v, false), onCommit: (v) => campo(k, v, true) }),
        h(Hex, { key: "h", value: tema[k], onCommit: (v) => campo(k, v, true) }),
      ]))),
    h(Campo, { key: "p", rotulo: "paleta" },
      h("div", { className: "tr-settings-paleta" }, [
        ...(tema.paleta || []).map((c, i) => h("span", { key: i, className: "tr-settings-sw" }, [
          h(Cor, { key: "c", value: c, title: c,
                   onInput: (v) => cor(i, v, false), onCommit: (v) => cor(i, v, true) }),
          h("button", { key: "x", type: "button", className: "tr-settings-swx",
                        disabled: tema.paleta.length <= 1, title: "remover cor",
                        onClick: () => paleta((p) => p.filter((_, j) => j !== i)) }, "×"),
        ])),
        h("button", { key: "+", type: "button", className: "tr-settings-add",
                      title: "adicionar cor (repete a última)",
                      onClick: () => paleta((p) => [...p, p[p.length - 1] || "#888888"]) }, "+"),
      ])),
    h(Campo, { key: "co", rotulo: "contínua" }, continua === "select"
      ? h("select", { value: tema.continua, onChange: (e) => campo("continua", e.target.value) },
          CONTINUAS.map((c) => h("option", { key: c, value: c }, c)))
      : h(Segmented, { options: CONTINUAS, value: tema.continua, wide: continua === "wide",
                       onChange: (v) => campo("continua", v) })),
  ]) : null;

  return h("aside", { className: "tr-settings" }, [
    h("div", { key: "hd", className: "tr-help-head" }, [
      h("strong", { key: "t" }, "Configurações"),
      h("button", { key: "x", className: "tr-help-close", title: "voltar à paleta",
                    onClick: onClose }, "×"),
    ]),
    h("div", { key: "b", className: "tr-settings-body" }, [
      h("h4", { key: "h" }, "Temas do projeto"),
      nomes.length ? lista : h("p", { key: "l", className: "tr-settings-empty" }, "carregando temas…"),
      acoes,
      editor,
    ]),
  ]);
}
