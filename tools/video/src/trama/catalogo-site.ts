// src/trama/catalogo-site.ts — os fluxos dos dois vídeos da home do site.
//
// Os dois reaproveitam os specs e as tabelas de `catalogo.ts` (mesmos dados,
// mesma coleção `data`) porque inventar um resultado novo aqui seria o
// primeiro defeito que alguém rodando o exemplo em casa notaria. O que muda é
// o RECORTE do fluxo e a coreografia:
//
// - `SiteMontagem` mostra só três blocos (Ler CSV → Filtrar → Agrupar e
//   resumir) sendo montados, em loop: a home pede um clipe curto e legível, não
//   o fluxo inteiro de seis nós que o `TramaDemo` monta.
// - `SiteFluxoAmplo` mostra o fluxo inteiro do `catalogo.ts` (os seis nós),
//   mas já PRONTO — nada digitando, nada "computando" — porque aqui a câmera é
//   quem se move (um passeio pelo grafo), e não o fluxo sendo construído.
import { ARESTAS, CAFE, CONTAGEM, NOS, SPECS, VENDAS } from "./catalogo";
import type { ArestaFluxo, NoFluxo } from "./tipos";

// --- SiteMontagem: recorte de três blocos ----------------------------------

export const NOS_MONTAGEM: NoFluxo[] = [
  {
    id: "ler",
    spec: SPECS.ler_csv,
    x: 0,
    y: 40,
    resultado: { tipo: "tabela", tabela: VENDAS },
    duracao: "8ms",
    entra: 6,
    digita: 20,
    resulta: 54,
  },
  {
    id: "filtrar",
    spec: SPECS.filtrar,
    x: 330,
    y: 270,
    resultado: { tipo: "tabela", tabela: CAFE },
    duracao: "2ms",
    entra: 76,
    digita: 94,
    resulta: 140,
  },
  {
    id: "agrupar",
    spec: SPECS.agrupar,
    x: 660,
    y: 10,
    resultado: { tipo: "tabela", tabela: CONTAGEM },
    duracao: "4ms",
    entra: 160,
    digita: 178,
    resulta: 220,
  },
];

export const ARESTAS_MONTAGEM: ArestaFluxo[] = [
  { de: "ler", paraNo: "filtrar", paraPorta: 0, desenha: 70 },
  { de: "filtrar", paraNo: "agrupar", paraPorta: 0, desenha: 154 },
];

// --- SiteFluxoAmplo: o fluxo inteiro, já assentado --------------------------
//
// `entra`/`resulta` bem negativos (em vez de 0) fazem a mola do card e o traço
// da aresta já estarem no repouso final no primeiro quadro — inclusive nas
// bordas do vídeo, que aqui não tem cena anterior sobrepondo. `digita`
// removido porque não há datilografia: o card já nasce com o parâmetro certo.

export const NOS_AMPLO: NoFluxo[] = NOS.map((no) => ({
  ...no,
  entra: -999,
  digita: undefined,
  resulta: -999,
}));

export const ARESTAS_AMPLO: ArestaFluxo[] = ARESTAS.map((a) => ({ ...a, desenha: -999 }));
