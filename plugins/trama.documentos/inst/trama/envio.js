// Etapas do envio ao GitHub, puras (sem React): o que o painel Documentos
// mostra para cada estado de `tr_documento_publicar()`/`_acompanhar()`.
// Teste em tests/js/documentos-envio.test.mjs.

export const ETAPAS = [
  { id: "enviado", rotulo: "Pacote enviado" },
  { id: "actions", rotulo: "GitHub Actions rodando" },
  { id: "release", rotulo: "Release criada" },
  { id: "no_ar", rotulo: "Página no ar" },
];

const ORDEM = { enviado: 0, actions: 1, release: 2, no_ar: 3 };

export const terminou = (estado) => estado?.etapa === "no_ar" || estado?.etapa === "falhou";

// `[{ id, rotulo, situacao }]` com situação "feita", "atual", "falhou" ou
// "pendente". Na falha, a etapa em que parou é a do Actions.
export function linhas(estado) {
  if (!estado?.etapa) return ETAPAS.map((e) => ({ ...e, situacao: "pendente" }));
  const falhou = estado.etapa === "falhou";
  const atual = falhou ? ORDEM.actions : ORDEM[estado.etapa] ?? 0;
  return ETAPAS.map((e, i) => ({
    ...e,
    situacao: falhou && i === atual ? "falhou"
      : i < atual || estado.etapa === "no_ar" ? "feita"
      : i === atual ? "atual" : "pendente",
  }));
}

// Os links que valem mostrar, na ordem: a página, a release, o log da falha
// (ou do Actions) e o repositório.
export function links(estado) {
  if (!estado) return [];
  const out = [];
  if (estado.etapa === "no_ar") out.push({ rotulo: "página", url: estado.pagina }, { rotulo: "última versão", url: estado.ultimo });
  if (estado.release && ORDEM[estado.etapa] >= ORDEM.release) out.push({ rotulo: "release", url: estado.release });
  if (estado.etapa === "falhou" && estado.log) out.push({ rotulo: "log da falha", url: estado.log });
  else if (estado.actions && !terminou(estado)) out.push({ rotulo: "Actions", url: estado.actions });
  if (estado.url) out.push({ rotulo: "repositório", url: estado.url });
  return out.filter((l) => l.url);
}
