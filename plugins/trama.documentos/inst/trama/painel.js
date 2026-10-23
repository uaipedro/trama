// Estado do painel Documentos: redutor puro (sem React, sem "trama"),
// testado em tests/js/documentos-painel.test.mjs. `documentos.js` só desenha.
//
// Ações vêm de dois lados: mensagens do R (`tipo` = lista, estado, criado,
// atualizado, inserido, resolvido, ocupado, previa, exportado, aberto, guia,
// modelos, migrado, publicacao, publicar_confirmar, publicar_bloqueado, envio,
// erro) e gestos do painel (abrir, fechar, aba, selecionar, abrir_regiao,
// novo_*, adicionar_*, inserir_*, publicar_*, …). Ações de modelos (`tipo` começando com "modelo") são do
// redutor de `modelos.js`.

import { reduzirModelos, erroModelos } from "./modelos.js";

export const INICIAL = Object.freeze({
  aberto: false, aba: "documentos", fase: "carregando",
  documentos: [], modelos: [], perfis: [], perfilPadrao: null, quarto: true,
  selecionado: null, regioes: {}, ocupado: {}, erro: null, aviso: null, fluxo: "main", foco: null,
  novo: null, adicionar: null, inserir: null, guia: null, exportados: {}, confirmar: null,
  publicar: null, envios: {},
  modeloAberto: null, modeloForm: null, modeloDuplicar: null, modeloImportar: null, modeloConfirmar: null,
});

// Rótulo e tom de cada estado de região (o motor: `tr_documento_estado()`).
export const ESTADOS = Object.freeze({
  atualizada: { rotulo: "atualizada", tom: "ok" },
  desatualizada: { rotulo: "desatualizada", tom: "aviso" },
  conflito: { rotulo: "editada à mão", tom: "erro" },
  orfa: { rotulo: "bloco removido", tom: "aviso" },
  indisponivel: { rotulo: "coleção ausente", tom: "aviso" },
  duplicada: { rotulo: "cópia", tom: "dim" },
});

const OCUPADO = { atualizar: "Atualizando…", previa: "Gerando a prévia…", html: "Exportando HTML…",
                  publicar: "Enviando ao GitHub…", pdf: "Exportando PDF…", docx: "Exportando Word…", revealjs: "Exportando os slides…",
                  dashboard: "Exportando o dashboard…" };

// Formatos de exportação, na ordem do menu, com o rótulo.
export const EXPORTAR = Object.freeze([["pdf", "PDF"], ["docx", "Word"], ["html", "HTML"],
                                       ["revealjs", "Slides"], ["dashboard", "Dashboard"]]);
// Os do documento (o R manda `formatos`); sem a lista, os três de sempre.
export const formatosExportar = (doc) => {
  const f = doc?.formatos;
  return EXPORTAR.filter(([k]) => (Array.isArray(f) && f.length ? f : ["pdf", "docx", "html"]).includes(k));
};
export const textoOcupado = (t) => OCUPADO[t] || "Trabalhando…";
// O arquivo exportado como o painel mostra: o caminho dentro de `_saida/`
// (slides e dashboard moram em subpastas, com o mesmo nome do HTML).
export const nomeSaida = (arquivo) => (arquivo || "").split("/_saida/").pop();

const semChave = (o, k) => { const r = { ...o }; delete r[k]; return r; };

export function reduzir(s, a) {
  switch (a.tipo) {
    case "abrir": return { ...s, aberto: true, aviso: null, erro: null };
    // Pedido de fora (o menu GitHub, na seção Publicações): abre neste documento.
    case "abrir_documento":
      return { ...s, aberto: true, aba: "documentos", selecionado: a.documento || s.selecionado, aviso: null, erro: null };
    case "fechar": return { ...s, aberto: false, confirmar: null, foco: null };
    // O selo de um card: abre o painel no documento, com a região em foco.
    case "abrir_regiao":
      return { ...s, aberto: true, aba: "documentos", selecionado: a.documento, foco: a.regiao || null,
               adicionar: null, inserir: null, novo: null, aviso: null, erro: null };
    // O aviso e o erro são da aba em que aconteceram.
    case "aba": return { ...s, aba: a.aba, aviso: null, erro: null };
    case "lista": {
      const docs = a.documentos || [];
      const aindaExiste = docs.some((d) => d.caminho === s.selecionado);
      return { ...s, fase: "pronto", documentos: docs, modelos: a.modelos || [], perfis: a.perfis || [],
               perfilPadrao: a.perfil_padrao ?? null, quarto: a.quarto !== false,
               fluxo: typeof a.fluxo === "string" && a.fluxo ? a.fluxo : "main",
               // A janela mostra sempre um documento: sem escolha, o primeiro.
               selecionado: aindaExiste ? s.selecionado : (docs[0]?.caminho ?? null) };
    }
    // Na janela, o documento escolhido fica escolhido; um inserir em curso
    // passa a mirar nele.
    case "selecionar":
      return { ...s, selecionado: a.documento, foco: null, erro: null, aviso: null, novo: null,
               inserir: s.inserir ? { ...s.inserir, documento: a.documento, pedirAceite: false, aceitar: false } : null };
    case "estado": return { ...s, regioes: { ...s.regioes, [a.documento]: a.regioes || [] } };
    case "ocupado": return { ...s, ocupado: { ...s.ocupado, [a.documento]: a.tarefa }, erro: null, aviso: null };

    case "novo_abrir":
      return { ...s, adicionar: null, novo: { nome: "", titulo: "",
                             modelo: s.modelos.some((m) => m.nome === "basico") ? "basico" : (s.modelos[0]?.nome || "basico"),
                             perfil: s.perfilPadrao || "" }, erro: null };
    case "novo_campo": {
      if (!s.novo) return s;
      const novo = { ...s.novo, [a.campo]: a.valor };
      // O modelo sugere um perfil (fase 3): vale se a pessoa tem esse perfil.
      if (a.campo === "modelo") {
        const sug = s.modelos.find((m) => m.nome === a.valor)?.perfil;
        if (sug && s.perfis.some((p) => p.perfil === sug)) novo.perfil = sug;
      }
      return { ...s, novo };
    }
    case "novo_cancelar": return { ...s, novo: null };
    case "criado": {
      const ins = s.inserir && !s.inserir.documento ? { ...s.inserir, documento: a.documento } : s.inserir;
      return { ...s, novo: null, selecionado: a.documento, inserir: ins, aviso: "Documento criado." };
    }

    // "+ Adicionar bloco": a lista dos cards do fluxo (o editor dá) no meio
    // da janela; escolher um abre o inserir dele à direita.
    case "adicionar_abrir":
      return { ...s, adicionar: { busca: "", cards: a.cards || [] }, inserir: null, novo: null, confirmar: null,
               erro: null, aviso: null };
    case "adicionar_busca": return s.adicionar ? { ...s, adicionar: { ...s.adicionar, busca: a.busca || "" } } : s;
    case "adicionar_fechar": return { ...s, adicionar: null, inserir: null };

    // "Inserir no documento…" do menu do card abre o painel já neste modo
    // (e a lista de "+ Adicionar bloco" também, com o documento da janela).
    case "inserir_card": {
      const c = a.card;
      const doc = a.documento || s.selecionado || (s.documentos.length === 1 ? s.documentos[0].caminho : null);
      return { ...s, aberto: true, aba: "documentos", erro: null, aviso: null, confirmar: null, novo: null,
               selecionado: doc || s.selecionado, foco: null,
               inserir: { card: c, documento: doc, porta: c.saidas?.[0]?.porta || null,
                          linhas: "", completa: false, aceitar: false, pedirAceite: false, enviando: false } };
    }
    case "inserir_campo": return s.inserir ? { ...s, inserir: { ...s.inserir, [a.campo]: a.valor } } : s;
    case "inserir_enviando": return s.inserir ? { ...s, inserir: { ...s.inserir, enviando: true }, erro: null } : s;
    case "inserir_cancelar": return { ...s, inserir: null };
    case "inserido":
      return { ...s, inserir: null, selecionado: a.documento, foco: a.regiao,
               aviso: `Inserido como #trama-${a.regiao}.` + (a.avisos?.length ? " " + a.avisos.join(" ") : "") };

    case "atualizado": {
      const r = a.resultado || [];
      const n = r.filter((x) => x.estado === "atualizada").length;
      const presas = r.filter((x) => ["conflito", "orfa", "erro", "indisponivel"].includes(x.estado)).length;
      return { ...s, ocupado: semChave(s.ocupado, a.documento),
               aviso: (n ? `${n} ${n === 1 ? "região atualizada" : "regiões atualizadas"}.` : "Tudo já estava em dia.") +
                      (presas ? ` ${presas} precisa${presas === 1 ? "" : "m"} de você.` : "") };
    }
    case "pedir_confirmacao": return { ...s, confirmar: { documento: a.documento, regiao: a.regiao, como: a.como } };
    case "cancelar_confirmacao": return { ...s, confirmar: null };
    case "resolvido": return { ...s, confirmar: null, aviso: `Região #trama-${a.regiao} resolvida.` };
    case "previa": return { ...s, ocupado: semChave(s.ocupado, a.documento) };
    case "exportado":
      return { ...s, ocupado: semChave(s.ocupado, a.documento),
               exportados: { ...s.exportados, [a.documento]: { formato: a.formato, arquivo: a.arquivo } },
               aviso: `Exportado: ${nomeSaida(a.arquivo)}` };
    case "aberto": return s;

    // Migração do trama.publicar (retirado): o painel abre com o aviso.
    case "migrado": {
      const docs = a.documentos || [];
      const nomes = docs.map((d) => d.documento.split("/").pop()).join(", ");
      return { ...s, aberto: true, aba: "documentos", erro: null,
               selecionado: docs.length === 1 ? docs[0].documento : s.selecionado,
               aviso: `O bloco Publicar saiu do trama e virou ${docs.length === 1 ? "o documento" : "os documentos"} ` +
                      `${nomes} (modelo Página web), com os mesmos itens e o mesmo endereço de publicação.` };
    }

    // Publicar no GitHub: conferir, (confirmar), enviar, acompanhar.
    case "publicar_abrir":
      return { ...s, publicar: { documento: a.documento, carregando: true, enviando: false }, erro: null, aviso: null };
    case "publicacao": {
      const envios = a.envio ? { ...s.envios, [a.documento]: a.envio } : s.envios;
      if (!s.publicar || s.publicar.documento !== a.documento) return { ...s, envios };
      return { ...s, envios, publicar: { documento: a.documento, carregando: false, enviando: false, confirmar: null,
                                         conferencia: a, repo: a.repo || "" } };
    }
    case "publicar_campo": return s.publicar ? { ...s, publicar: { ...s.publicar, [a.campo]: a.valor } } : s;
    case "publicar_enviando": return s.publicar ? { ...s, publicar: { ...s.publicar, enviando: true, confirmar: null }, erro: null } : s;
    case "publicar_cancelar": return { ...s, publicar: null };
    case "publicar_confirmar":
      return { ...s, ocupado: semChave(s.ocupado, a.documento),
               publicar: s.publicar ? { ...s.publicar, enviando: false, confirmar: a.texto, repo: a.repo || s.publicar.repo } : s.publicar };
    case "publicar_bloqueado": {
      const base = { ...s, ocupado: semChave(s.ocupado, a.documento) };
      if (!s.publicar) return { ...base, erro: (a.erros || []).join(" ") };
      const conf = { ...(s.publicar.conferencia || {}), erros: a.erros || [] };
      return { ...base, publicar: { ...s.publicar, enviando: false, conferencia: conf } };
    }
    case "envio": {
      const e = a.estado || {};
      const novo = !s.envios[a.documento] || s.envios[a.documento].tag !== e.tag;
      return { ...s, ocupado: semChave(s.ocupado, a.documento), envios: { ...s.envios, [a.documento]: e },
               // Só a resposta ao envio fecha a caixa: o acompanhamento de um
               // envio anterior também manda `envio`, e não pode fechá-la.
               publicar: s.publicar?.documento === a.documento && s.publicar.enviando ? null : s.publicar,
               aviso: novo && e.etapa === "enviado" ? `Versão ${e.tag} enviada a ${e.repo}.` : s.aviso };
    }
    case "guia": return { ...s, guia: a.passos || [] };
    case "fechar_guia": return { ...s, guia: null };
    case "erro": {
      if ((a.acao || "").startsWith("modelos_")) return { ...erroModelos(s, a), aviso: null, erro: a.mensagem };
      const base = { ...s, ocupado: a.documento ? semChave(s.ocupado, a.documento) : s.ocupado, aviso: null };
      // Modelo com dados de treino: o painel pede o aceite em vez de só mostrar o erro.
      if (a.acao === "inserir" && s.inserir && a.classe === "tr_documento_erro_dados_treino") {
        return { ...base, inserir: { ...s.inserir, enviando: false, pedirAceite: true }, erro: a.mensagem };
      }
      if (a.acao === "resolver" && a.classe === "tr_documento_erro_confirmar") {
        return { ...base, erro: a.mensagem, confirmar: s.confirmar ? { ...s.confirmar, forte: true } : null };
      }
      return { ...base, inserir: s.inserir ? { ...s.inserir, enviando: false } : null,
               publicar: s.publicar ? { ...s.publicar, enviando: false, carregando: false } : null, erro: a.mensagem };
    }
    default: return (a.tipo || "").startsWith("modelo") ? reduzirModelos(s, a) : s;
  }
}

// Resumo das regiões de um documento numa linha ("3 regiões · 1 desatualizada").
export function resumo(r) {
  if (!r) return "";
  if (r.erro) return r.erro;
  const total = Object.values(r).reduce((a, b) => a + b, 0);
  if (!total) return "sem regiões ainda";
  const partes = [`${total} ${total === 1 ? "região" : "regiões"}`];
  for (const k of ["desatualizada", "conflito", "orfa", "indisponivel"]) {
    if (r[k]) partes.push(`${r[k]} ${ESTADOS[k].rotulo}`);
  }
  return partes.join(" · ");
}

// Precisa de atenção? (a bolinha no botão da barra)
export function pendencias(documentos) {
  return (documentos || []).reduce((n, d) =>
    n + (d.resumo?.desatualizada || 0) + (d.resumo?.conflito || 0) + (d.resumo?.orfa || 0), 0);
}

// O que mandar ao R para inserir; `null` enquanto falta escolher.
export function pedidoInserir(ins) {
  if (!ins || !ins.documento || !ins.card?.id || ins.enviando) return null;
  const n = Number(ins.linhas);
  const linhas = ins.linhas !== "" && Number.isInteger(n) && n > 0 ? n : null;
  return { documento: ins.documento, no: ins.card.id, porta: ins.porta,
           opcoes: { linhas, completa: !!ins.completa }, aceitar: !!ins.aceitar };
}

// A saída escolhida é uma tabela? (mostra "linhas" e "tabela completa")
export function ehTabela(ins) {
  const t = (ins?.card?.saidas || []).find((p) => p.porta === ins.porta)?.tipo || "";
  return /(^|\/)(table|tabela)$/.test(t);
}

// Nome de documento válido (o mesmo que o R aceita).
export const nomeValido = (n) => /^[A-Za-z0-9][A-Za-z0-9_-]*$/.test(n || "");

// Ações de resolver que cabem em cada estado.
export function resolucoes(estado) {
  if (estado === "conflito") return [{ como: "manter", rotulo: "Ficar com a minha" }, { como: "trazer", rotulo: "Trazer a do trama" }];
  if (estado === "orfa") return [{ como: "manter", rotulo: "Manter como texto" }, { como: "remover", rotulo: "Remover" }];
  if (estado === "duplicada") return [{ como: "remover", rotulo: "Remover a cópia" }];
  return [];
}

// O que mandar ao R para publicar; `null` enquanto falta algo ou há bloqueio.
export function pedidoPublicar(p, confirmado = false) {
  if (!p || p.carregando || p.enviando || !p.conferencia) return null;
  if ((p.conferencia.erros || []).length) return null;
  const repo = (p.repo || "").trim();
  return { documento: p.documento, repositorio: repo || null, confirmado: !!confirmado };
}

// Busca sem caixa e sem acento ("regressao" acha "Regressão").
const dobrar = (t) => String(t || "").normalize("NFD").replace(/[̀-ͯ]/g, "").toLowerCase();

// Cards que o documento pode receber (os que têm saída), filtrados pela busca
// no rótulo, no tipo e no id. Ordem: a do fluxo.
export function filtrarCards(cards, busca) {
  const termos = dobrar(busca).split(/\s+/).filter(Boolean);
  return (cards || []).filter((c) => (c.saidas || []).length > 0).filter((c) => {
    const alvo = dobrar(`${c.rotulo} ${c.tipo} ${c.id}`);
    return termos.every((t) => alvo.includes(t));
  });
}

// As regiões do documento que mostram este card (cópias à mão não contam).
export const regioesDoCard = (regioes, id) =>
  (regioes || []).filter((r) => r.no === id && r.estado !== "duplicada");

// Selos dos cards: id do nó -> [{documento, nome, regiao}], só dos documentos
// do fluxo na tela (o mesmo id em outro fluxo é outro bloco).
export function indiceSelos(documentos, fluxo = "main") {
  const idx = new Map();
  for (const d of documentos || []) {
    if ((d.fluxo || "main") !== fluxo) continue;
    for (const r of d.regioes || []) {
      if (!r.no || r.estado === "duplicada") continue;
      if (!idx.has(r.no)) idx.set(r.no, []);
      idx.get(r.no).push({ documento: d.caminho, nome: d.nome, regiao: r.regiao });
    }
  }
  return idx;
}

// O tooltip do selo.
export function tituloSelo(lista) {
  const nomes = [...new Set((lista || []).map((x) => x.nome))];
  if (!nomes.length) return "";
  const qual = nomes.length === 1 ? `no documento ${nomes[0]}` : `nos documentos ${nomes.join(", ")}`;
  return `Está ${qual} (clique para ver no painel)`;
}

// O que vai na publicação, numa linha.
export const vaiSair = (n) =>
  `Vai sair: ${n} ${n === 1 ? "região" : "regiões"} + o fluxo, os dados ligados e o ambiente`;
