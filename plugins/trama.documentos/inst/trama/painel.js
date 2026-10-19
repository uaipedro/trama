// Estado do painel Documentos: redutor puro (sem React, sem "trama"),
// testado em tests/js/documentos-painel.test.mjs. `documentos.js` só desenha.
//
// Ações vêm de dois lados: mensagens do R (`tipo` = lista, estado, criado,
// atualizado, inserido, resolvido, ocupado, previa, exportado, aberto, guia,
// modelos, erro) e gestos do painel (abrir, fechar, aba, selecionar, novo_*,
// inserir_*, …). Ações de modelos (`tipo` começando com "modelo") são do
// redutor de `modelos.js`.

import { reduzirModelos, erroModelos } from "./modelos.js";

export const INICIAL = Object.freeze({
  aberto: false, aba: "documentos", fase: "carregando",
  documentos: [], modelos: [], perfis: [], perfilPadrao: null, quarto: true,
  selecionado: null, regioes: {}, ocupado: {}, erro: null, aviso: null,
  novo: null, inserir: null, guia: null, exportados: {}, confirmar: null,
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
                  pdf: "Exportando PDF…", docx: "Exportando Word…", revealjs: "Exportando os slides…",
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

const semChave = (o, k) => { const r = { ...o }; delete r[k]; return r; };

export function reduzir(s, a) {
  switch (a.tipo) {
    case "abrir": return { ...s, aberto: true, aviso: null, erro: null };
    case "fechar": return { ...s, aberto: false, confirmar: null };
    // O aviso e o erro são da aba em que aconteceram.
    case "aba": return { ...s, aba: a.aba, aviso: null, erro: null };
    case "lista": {
      const docs = a.documentos || [];
      const aindaExiste = docs.some((d) => d.caminho === s.selecionado);
      return { ...s, fase: "pronto", documentos: docs, modelos: a.modelos || [], perfis: a.perfis || [],
               perfilPadrao: a.perfil_padrao ?? null, quarto: a.quarto !== false,
               selecionado: aindaExiste ? s.selecionado : (docs.length === 1 ? docs[0].caminho : null) };
    }
    case "selecionar":
      return { ...s, selecionado: s.selecionado === a.documento ? null : a.documento, erro: null, aviso: null };
    case "estado": return { ...s, regioes: { ...s.regioes, [a.documento]: a.regioes || [] } };
    case "ocupado": return { ...s, ocupado: { ...s.ocupado, [a.documento]: a.tarefa }, erro: null, aviso: null };

    case "novo_abrir":
      return { ...s, novo: { nome: "", titulo: "",
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

    // "Inserir no documento…" do menu do card abre o painel já neste modo.
    case "inserir_card": {
      const c = a.card;
      const doc = s.selecionado || (s.documentos.length === 1 ? s.documentos[0].caminho : null);
      return { ...s, aberto: true, aba: "documentos", erro: null, aviso: null, confirmar: null,
               inserir: { card: c, documento: doc, porta: c.saidas?.[0]?.porta || null,
                          linhas: "", completa: false, aceitar: false, pedirAceite: false, enviando: false } };
    }
    case "inserir_campo": return s.inserir ? { ...s, inserir: { ...s.inserir, [a.campo]: a.valor } } : s;
    case "inserir_enviando": return s.inserir ? { ...s, inserir: { ...s.inserir, enviando: true }, erro: null } : s;
    case "inserir_cancelar": return { ...s, inserir: null };
    case "inserido":
      return { ...s, inserir: null, selecionado: a.documento,
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
               aviso: `Exportado: ${a.arquivo.split("/").pop()}` };
    case "aberto": return s;
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
      return { ...base, inserir: s.inserir ? { ...s.inserir, enviando: false } : null, erro: a.mensagem };
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
