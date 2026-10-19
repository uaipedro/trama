// Estado da aba Modelos (galeria, fase 3, #24): redutor puro, sem React e
// sem "trama", testado em tests/js/documentos-modelos.test.mjs. O redutor do
// painel (`painel.js`) passa para cá toda ação cujo `tipo` começa com
// "modelo"; `modelos-ui.js` só desenha.
//
// Mensagens do R: modelos (a galeria), modelo (o detalhe, abre a edição),
// modelo_salvo, modelo_exportado. Gestos: modelo_abrir (expande na lista),
// modelo_editar_campo, modelo_cancelar, modelo_duplicar_*, modelo_importar_*,
// modelo_remover_*.

// Formatos que a galeria liga e desliga, com o nome que a pessoa conhece.
export const FORMATOS = Object.freeze([
  ["html", "HTML"], ["typst", "PDF"], ["docx", "Word"], ["revealjs", "Slides"], ["dashboard", "Dashboard"],
]);
export const CORES = Object.freeze([
  ["primaria", "Primária"], ["secundaria", "Secundária"], ["destaque", "Destaque"], ["fundo", "Fundo"], ["texto", "Texto"],
]);
export const FONTES = Object.freeze([["texto", "Texto"], ["titulos", "Títulos"], ["codigo", "Código"]]);

// Nome de modelo válido (o mesmo que o R aceita): minúsculas, dígitos, "-",
// e não o nome de um formato do Quarto.
const RESERVADOS = new Set(["html", "pdf", "docx", "typst", "revealjs", "dashboard", "beamer", "pptx", "latex",
  "odt", "epub", "gfm", "markdown", "commonmark", "jats", "ipynb", "rtf", "context"]);
export const nomeModeloValido = (n) => /^[a-z0-9][a-z0-9-]*$/.test(n || "") && !RESERVADOS.has(n);

// Sugestão de nome a partir de um título ("Meu TCC" -> "meu-tcc").
export function nomeDe(titulo) {
  const n = (titulo || "").normalize("NFD").replace(/[̀-ͯ]/g, "").toLowerCase()
    .replace(/[^a-z0-9]+/g, "-").replace(/^-+|-+$/g, "");
  return nomeModeloValido(n) ? n : (n ? `${n}-modelo` : "");
}

// O formulário de edição a partir do detalhe que o R mandou.
export function formularioDe(m) {
  return {
    nome: m.nome, origem: m.origem, sobreEmbutido: !!m.sobre_embutido,
    titulo: m.titulo || "", descricao: m.descricao || "",
    formatos: [...(m.formatos || [])],
    cores: { ...(m.cores || {}) }, fontes: { ...(m.fontes || {}) },
    capa: typeof m.capa === "boolean" ? m.capa : null,
    perfil: m.perfil || "",
    logo: m.logo ? m.logo.split(/[\\/]/).pop() : null, csl: m.csl || null,
    novoLogo: null, novoCsl: null, semLogo: false, semCsl: false,
    original: m, enviando: false,
  };
}

const igual = (a, b) => JSON.stringify(a) === JSON.stringify(b);

// O que mandar ao R para salvar: só o que mudou. `null` quando nada mudou ou
// falta algo (sem formato, cor inválida).
export function pedidoEditar(f) {
  if (!f || f.enviando) return null;
  const o = f.original || {};
  const p = { nome: f.nome };
  if (f.titulo.trim() && f.titulo !== (o.titulo || "")) p.titulo = f.titulo.trim();
  if (f.descricao !== (o.descricao || "")) p.descricao = f.descricao.trim();
  if (!f.formatos.length) return null;
  if (!igual(f.formatos, o.formatos || [])) p.formatos = f.formatos;
  const cores = {};
  for (const [k] of CORES) {
    const v = f.cores[k];
    if (v && v !== (o.cores || {})[k]) {
      if (!/^#[0-9a-fA-F]{6}$/.test(v)) return null;
      cores[k] = v.toLowerCase();
    }
  }
  if (Object.keys(cores).length) p.cores = cores;
  const fontes = {};
  for (const [k] of FONTES) {
    const v = (f.fontes[k] || "").trim();
    if (v && v !== (o.fontes || {})[k]) fontes[k] = v;
  }
  if (Object.keys(fontes).length) p.fontes = fontes;
  if (f.capa !== null && f.capa !== (typeof o.capa === "boolean" ? o.capa : null)) p.capa = f.capa;
  if (f.perfil !== (o.perfil || "")) p.perfil = f.perfil || false;
  if (f.novoLogo) p.logo = f.novoLogo; else if (f.semLogo && o.logo) p.logo = false;
  if (f.novoCsl) p.csl = f.novoCsl; else if (f.semCsl && o.csl) p.csl = false;
  return Object.keys(p).length > 1 ? p : null;
}

// Duplicar (e "novo modelo", que é duplicar a partir de uma base).
export function pedidoDuplicar(d) {
  if (!d || d.enviando || !d.de || !nomeModeloValido(d.nome)) return null;
  return { de: d.de, nome: d.nome, titulo: d.titulo.trim() || null };
}

export function reduzirModelos(s, a) {
  switch (a.tipo) {
    case "modelos": return { ...s, modelos: a.modelos || [] };
    case "modelo_abrir": return { ...s, modeloAberto: s.modeloAberto === a.nome ? null : a.nome, erro: null, aviso: null };

    // Sem apagar o aviso: depois de duplicar, o R manda "modelo_salvo" e logo
    // este, que abre a edição da cópia.
    case "modelo": return { ...s, modeloForm: formularioDe(a.modelo), modeloDuplicar: null, erro: null };
    case "modelo_editar_campo": {
      const f = s.modeloForm;
      if (!f) return s;
      if (a.grupo) return { ...s, modeloForm: { ...f, [a.grupo]: { ...f[a.grupo], [a.campo]: a.valor } } };
      return { ...s, modeloForm: { ...f, [a.campo]: a.valor } };
    }
    case "modelo_formato": {
      const f = s.modeloForm;
      if (!f) return s;
      const tem = f.formatos.includes(a.formato);
      const formatos = tem ? f.formatos.filter((x) => x !== a.formato) : [...f.formatos, a.formato];
      return { ...s, modeloForm: { ...f, formatos } };
    }
    case "modelo_enviando":
      return s.modeloForm ? { ...s, modeloForm: { ...s.modeloForm, enviando: true }, erro: null } : s;
    case "modelo_cancelar": return { ...s, modeloForm: null, modeloDuplicar: null, modeloImportar: null, modeloConfirmar: null };

    case "modelo_duplicar_abrir": {
      const base = (s.modelos || []).find((m) => m.nome === a.de) || {};
      const titulo = a.novo ? "" : `${base.titulo || a.de} (cópia)`;
      return { ...s, modeloForm: null, modeloImportar: null, erro: null, aviso: null,
               modeloDuplicar: { de: a.de || "basico", novo: !!a.novo, titulo, nome: a.novo ? "" : nomeDe(`${a.de}-copia`),
                                 nomeTocado: false, enviando: false } };
    }
    case "modelo_duplicar_campo": {
      const d = s.modeloDuplicar;
      if (!d) return s;
      const n = { ...d, [a.campo]: a.valor };
      if (a.campo === "nome") n.nomeTocado = true;
      if (a.campo === "titulo" && !d.nomeTocado) n.nome = nomeDe(a.valor);
      return { ...s, modeloDuplicar: n };
    }
    case "modelo_duplicar_enviando":
      return s.modeloDuplicar ? { ...s, modeloDuplicar: { ...s.modeloDuplicar, enviando: true }, erro: null } : s;

    case "modelo_importar_abrir": return { ...s, modeloImportar: { enviando: false }, modeloDuplicar: null, modeloForm: null };
    case "modelo_importar_enviando":
      return s.modeloImportar ? { ...s, modeloImportar: { ...s.modeloImportar, enviando: true, arquivo: a.arquivo }, erro: null } : s;

    case "modelo_remover_pedir": return { ...s, modeloConfirmar: { nome: a.nome, voltar: !!a.voltar } };
    case "modelo_remover_cancelar": return { ...s, modeloConfirmar: null };

    case "modelo_salvo":
      return { ...s, modeloForm: null, modeloDuplicar: null, modeloImportar: null, modeloConfirmar: null,
               modeloAberto: a.nome || s.modeloAberto, aviso: a.mensagem || "Modelo salvo." };
    case "modelo_exportado": return { ...s, aviso: `Exportado: ${(a.arquivo || "").split(/[\\/]/).pop()}`, exportadoModelo: a.arquivo };
    default: return s;
  }
}

// Erro de uma ação de modelos: destrava o formulário que estava enviando.
export function erroModelos(s) {
  return {
    ...s,
    modeloForm: s.modeloForm ? { ...s.modeloForm, enviando: false } : s.modeloForm,
    modeloDuplicar: s.modeloDuplicar ? { ...s.modeloDuplicar, enviando: false } : s.modeloDuplicar,
    modeloImportar: s.modeloImportar ? { ...s.modeloImportar, enviando: false } : s.modeloImportar,
  };
}

// Rótulo curto da origem.
export function origemDe(m) {
  if (m.origem !== "usuario") return "embutido";
  return m.sobre_embutido ? "editado" : "seu";
}

// "typst, docx, html" -> "PDF · Word · HTML"
export function formatosLegiveis(txt) {
  const nomes = Object.fromEntries(FORMATOS);
  return (txt || "").split(/,\s*/).filter(Boolean).map((f) => nomes[f] || f).join(" · ");
}
