// Estado das seções Projeto e Publicações do painel GitHub: redutor puro
// (sem React, sem "trama"), testado em tests/js/github-painel.test.mjs.
//
// Mensagens do R: projeto, arquivos, salvo, restaurado, publicacoes e erro
// com `contexto` ("projeto" ou "publicacoes"). Gestos do painel: carregar,
// ocupar, rascunho, nome, confirmar, cancelar_confirmacao, alternar_dados.

export const PAINEL_INICIAL = Object.freeze({
  projeto: Object.freeze({
    fase: "inicial", ligado: false, repo: null, url: null, nomeSugerido: "", nome: null,
    arquivos: null, versoes: [], erro: null, aviso: null, ocupado: null, confirmar: null,
    rascunho: "", dadosAbertos: false,
  }),
  publicacoes: Object.freeze({ fase: "inicial", disponivel: true, itens: [], erro: null }),
});

const projeto = (p, mudar) => ({ ...p, projeto: { ...p.projeto, ...mudar } });
const publicacoes = (p, mudar) => ({ ...p, publicacoes: { ...p.publicacoes, ...mudar } });

export function reduzirPainel(p, a) {
  switch (a.tipo) {
    // Quem sai da conta não vê mais o projeto de ninguém.
    case "estado": return a.conectado ? p : PAINEL_INICIAL;
    case "carregar":
      return publicacoes(projeto(p, { fase: p.projeto.fase === "pronto" ? "pronto" : "carregando", erro: null }),
                         { fase: "carregando", erro: null });
    case "projeto":
      return projeto(p, { fase: "pronto", ligado: !!a.ligado, repo: a.repo || null, url: a.url || null,
                          nomeSugerido: a.nome_sugerido || "", arquivos: a.arquivos || null,
                          versoes: Array.isArray(a.versoes) ? a.versoes : [], erro: null, ocupado: null,
                          confirmar: null });
    case "arquivos": return projeto(p, { arquivos: a.arquivos || null, ocupado: null });
    case "salvo":
      return projeto(p, { ocupado: null, rascunho: "", confirmar: null,
                          aviso: a.primeira ? "Repositório criado e primeira versão salva."
                            : a.mudou ? "Versão salva." : "Nada mudou desde a última versão." });
    case "restaurado":
      return projeto(p, { ocupado: null, confirmar: null,
                          aviso: a.copia
                            ? `Versão restaurada. O estado anterior ficou guardado em ${a.copia}.`
                            : "Versão restaurada: os arquivos já estavam iguais." });
    case "publicacoes":
      return publicacoes(p, { fase: "pronto", disponivel: a.disponivel !== false,
                              itens: Array.isArray(a.itens) ? a.itens : [], erro: null });
    case "erro":
      if (a.contexto === "projeto") {
        return projeto(p, { erro: a.mensagem || "Algo deu errado.", ocupado: null, confirmar: null,
                            fase: p.projeto.fase === "carregando" ? "erro" : p.projeto.fase });
      }
      if (a.contexto === "publicacoes") return publicacoes(p, { fase: "pronto", erro: a.mensagem || "Algo deu errado." });
      return p;
    case "ocupar": return projeto(p, { ocupado: a.o, erro: null, aviso: null });
    case "rascunho": return projeto(p, { rascunho: a.texto ?? "" });
    case "nome": return projeto(p, { nome: a.texto ?? "" });
    case "confirmar": return projeto(p, { confirmar: a.c || null, aviso: null, erro: null });
    case "cancelar_confirmacao": return projeto(p, { confirmar: null });
    case "alternar_dados": return projeto(p, { dadosAbertos: !p.projeto.dadosAbertos });
    default: return p;
  }
}

export function podeSalvar(pr) {
  return pr.ligado && !pr.ocupado && pr.rascunho.trim().length > 0;
}

// Nome do repositório novo: o digitado ou o sugerido.
export function nomeRepo(pr) {
  return (pr.nome ?? pr.nomeSugerido ?? "").trim();
}

export function tamanho(bytes) {
  const b = Number(bytes) || 0;
  if (b < 1024) return `${b} B`;
  const [v, u] = b < 1024 ** 2 ? [b / 1024, "KB"] : b < 1024 ** 3 ? [b / 1024 ** 2, "MB"] : [b / 1024 ** 3, "GB"];
  return `${(v < 10 ? v.toFixed(1) : Math.round(v).toString()).replace(".", ",")} ${u}`;
}

export function resumoArquivos(arq) {
  if (!arq) return "";
  const dados = arq.dados || [];
  const marcados = dados.filter((d) => d.incluido).length;
  const n = arq.projeto_n;
  const base = `${n} ${n === 1 ? "arquivo" : "arquivos"} do projeto (${tamanho(arq.projeto_bytes)})`;
  if (!dados.length) return `${base} · nenhum arquivo de dados`;
  return `${base} · ${marcados} de ${dados.length} ${dados.length === 1 ? "arquivo" : "arquivos"} de dados`;
}

const dois = (x) => String(x).padStart(2, "0");
// Data e hora locais, "19/10/2026 14:30".
export function dataCurta(iso) {
  if (!iso) return "";
  const d = new Date(iso);
  if (Number.isNaN(d.getTime())) return "";
  return `${dois(d.getDate())}/${dois(d.getMonth() + 1)}/${d.getFullYear()} ${dois(d.getHours())}:${dois(d.getMinutes())}`;
}

// Última execução do Actions de uma publicação, em palavras.
export function situacaoExecucao(ex) {
  if (!ex) return null;
  if (ex.estado === "completed") {
    return ex.conclusao === "success" ? { rotulo: "publicada", tom: "ok" } : { rotulo: "falhou", tom: "erro" };
  }
  if (ex.estado === "in_progress") return { rotulo: "publicando…", tom: "neutro" };
  return { rotulo: "na fila", tom: "neutro" };
}
