// Estado do menu GitHub: redutor puro (sem React, sem "trama"), testado em
// tests/js/github-estado.test.mjs. `github.js` só desenha.
//
// Ações vêm de dois lados: mensagens do R (`tipo` = estado, codigo, erro,
// guia) e gestos do menu (abrir, fechar, entrando, copiado, fechar_guia).
//
// Fases: carregando → desconectado | conectado; desconectado → pedindo →
// aguardando (código na tela) → conectado, ou de volta a desconectado com erro.

export const INICIAL = Object.freeze({
  fase: "carregando", aberto: false, usuario: null, fonte: null, erro: null,
  loginDisponivel: true, persistente: true, codigo: null, link: null, copiado: false, guia: null,
});

export function reduzir(s, a) {
  switch (a.tipo) {
    case "abrir": return { ...s, aberto: true };
    case "fechar": return { ...s, aberto: false };
    case "estado":
      return { ...s, fase: a.conectado ? "conectado" : "desconectado",
               usuario: a.conectado ? (a.usuario || null) : null,
               fonte: a.fonte || null, erro: a.erro || null,
               loginDisponivel: a.login_disponivel !== false,
               persistente: a.persistente !== false,
               codigo: null, link: null, copiado: false };
    case "entrando": return { ...s, fase: "pedindo", erro: null, guia: null };
    case "codigo":
      return { ...s, fase: "aguardando", codigo: a.codigo, link: a.link, copiado: false, erro: null };
    case "copiado": return s.fase === "aguardando" ? { ...s, copiado: true } : s;
    // Erro de login volta para "desconectado"; erro com alguém conectado
    // (não deveria acontecer, mas a mensagem não pode sumir) mantém a conexão.
    // Erro das seções do painel (`contexto`) é de `painel.js`, não da conta.
    case "erro":
      if (a.contexto === "projeto" || a.contexto === "publicacoes") return s;
      return { ...s, fase: s.fase === "conectado" ? "conectado" : "desconectado",
               erro: a.mensagem || "Algo deu errado.", codigo: null, link: null };
    case "guia": return { ...s, guia: Array.isArray(a.passos) ? a.passos : [] };
    case "fechar_guia": return { ...s, guia: null };
    default: return s;
  }
}

const FONTES = {
  chaveiro: "pelo trama",
  GITHUB_PAT: "pela variável GITHUB_PAT",
  GITHUB_TOKEN: "pela variável GITHUB_TOKEN",
  gh: "pelo GitHub CLI (gh)",
  gitcreds: "pela credencial do git",
};
export function rotuloFonte(fonte) { return FONTES[fonte] || ""; }

// "Sair" só apaga o que o trama guardou; as outras credenciais são de outras
// ferramentas e continuariam valendo, então o botão mentiria.
export function podeSair(s) { return s.fase === "conectado" && s.fonte === "chaveiro"; }

// Bolinha no botão da barra: verde conectado, vermelha com erro, nada no resto.
export function indicador(s) {
  if (s.fase === "conectado") return "ok";
  if (s.erro) return "erro";
  return null;
}

export function dica(s) {
  if (s.fase === "conectado" && s.usuario) return `GitHub: conectado como ${s.usuario.login}`;
  if (s.fase === "aguardando") return "GitHub: esperando a autorização";
  return "GitHub: entrar";
}
