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
import type { ArestaFluxo, NoFluxo, Tabela } from "./tipos";

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

// --- SiteVertical: quatro blocos, câmera passeando de cima pra baixo --------
//
// A câmera de `SiteFluxoAmplo` passeia na HORIZONTAL porque o grafo dela é
// largo; o loop vertical da home (coluna direita do hero, e o compartilhável
// pro WhatsApp) precisa do oposto: um fluxo que DESCE. As portas do card são
// sempre laterais (`out` na direita, `data` na esquerda — ver `Aresta.tsx`),
// então empilhar os quatro na mesma coluna x faria a aresta nascer do lado
// direito de um card e ter que voltar pro lado esquerdo do de baixo — a mesma
// curva "pra trás" que o xyflow desenha numa aresta cujo alvo fica atrás da
// origem (`deslocamento()` com distância negativa). Por isso os cards
// alternam x (0 e 300): metade das ligações vira essa curva de volta, mas
// como o y do alvo está bem mais abaixo, o traço lê como um S descendo, não
// como um laço.
//
// Os NÚMEROS não são inventados soltos: `FONTE` é UM array só, e toda tabela
// abaixo (a prévia do CSV, o filtro e a contagem) é CALCULADA dele. Rodar o
// filtro e o agrupar de verdade nesse array dá exactly os números que os
// cards mostram — é o que a tarefa deste vídeo pediu, e é o mesmo cuidado que
// `catalogo.ts` já tem com `VENDAS`/`CAFE`/`CONTAGEM` (só que aqui a fonte é
// explícita em vez de ficar implícita atrás de tabelas soltas).
type LinhaFonte = {
  regiao: string;
  produto: string;
  valor: number;
  qtd: number;
  mes: number;
};

// 22 vendas — café e açúcar, quatro regiões. As cinco primeiras são as MESMAS
// que `VENDAS`/`CAFE` de `catalogo.ts` (mesmo fluxo de exemplo do repositório);
// as demais só existem para o Agrupar ter o que contar.
const FONTE: LinhaFonte[] = [
  { regiao: "leste", produto: "cafe", valor: 98.69, qtd: 6, mes: 2 },
  { regiao: "leste", produto: "cafe", valor: 176.21, qtd: 2, mes: 7 },
  { regiao: "leste", produto: "acucar", valor: 57.08, qtd: 4, mes: 7 },
  { regiao: "oeste", produto: "acucar", valor: 157.65, qtd: 3, mes: 7 },
  { regiao: "norte", produto: "cafe", valor: 25.37, qtd: 5, mes: 5 },
  { regiao: "leste", produto: "cafe", valor: 179.33, qtd: 8, mes: 4 },
  { regiao: "sul", produto: "cafe", valor: 119.4, qtd: 3, mes: 6 },
  { regiao: "oeste", produto: "cafe", valor: 88.15, qtd: 5, mes: 9 },
  { regiao: "sul", produto: "acucar", valor: 42.1, qtd: 2, mes: 3 },
  { regiao: "norte", produto: "acucar", valor: 63.44, qtd: 6, mes: 11 },
  { regiao: "leste", produto: "acucar", valor: 71.02, qtd: 3, mes: 5 },
  { regiao: "oeste", produto: "cafe", valor: 134.77, qtd: 7, mes: 1 },
  { regiao: "norte", produto: "cafe", valor: 58.9, qtd: 4, mes: 8 },
  { regiao: "sul", produto: "cafe", valor: 96.6, qtd: 5, mes: 2 },
  { regiao: "leste", produto: "cafe", valor: 210.05, qtd: 9, mes: 10 },
  { regiao: "oeste", produto: "acucar", valor: 45.3, qtd: 2, mes: 6 },
  { regiao: "sul", produto: "acucar", valor: 88.88, qtd: 4, mes: 9 },
  { regiao: "norte", produto: "cafe", valor: 77.15, qtd: 3, mes: 12 },
  { regiao: "oeste", produto: "cafe", valor: 102.4, qtd: 6, mes: 3 },
  { regiao: "sul", produto: "cafe", valor: 133.22, qtd: 7, mes: 5 },
  { regiao: "leste", produto: "acucar", valor: 39.75, qtd: 1, mes: 8 },
  { regiao: "norte", produto: "acucar", valor: 91.6, qtd: 5, mes: 4 },
];

const linhasDaFonte = (ls: LinhaFonte[]): Tabela["linhas"] =>
  ls.map((l) => [l.regiao, l.produto, l.valor, l.qtd, l.mes]);

const COLUNAS_VENDA = ["regiao", "produto", "valor", "qtd", "mes"];

// A prévia da fonte: cinco linhas (o padrão do projeto — ver a nota "CINCO
// LINHAS" em `catalogo.ts`), não as 22.
export const VENDAS_VERTICAL: Tabela = {
  colunas: COLUNAS_VENDA,
  linhas: linhasDaFonte(FONTE.slice(0, 5)),
};

// O filtro de verdade: `produto == "cafe"` rodado no array inteiro. A prévia
// mostra as cinco primeiras, mas a CONTAGEM adiante usa as 13 inteiras — é
// isso que faz o card seguinte bater com o que este filtrou, e não com um
// número escrito à parte.
const CAFE_COMPLETO = FONTE.filter((l) => l.produto === "cafe");
export const CAFE_VERTICAL: Tabela = {
  colunas: COLUNAS_VENDA,
  linhas: linhasDaFonte(CAFE_COMPLETO.slice(0, 5)),
};

// `dplyr::n()` por região, nas 13 linhas de café — não nas 5 da prévia.
const contagemPorRegiao = new Map<string, number>();
for (const l of CAFE_COMPLETO) {
  contagemPorRegiao.set(l.regiao, (contagemPorRegiao.get(l.regiao) ?? 0) + 1);
}
const REGIOES_ORDENADAS = [...contagemPorRegiao.keys()].sort();
export const CONTAGEM_VERTICAL: Tabela = {
  colunas: ["regiao", "n"],
  linhas: REGIOES_ORDENADAS.map((r) => [r, contagemPorRegiao.get(r) as number]),
};

// O card final é `data/summary` (real, o mesmo `SPECS.resumo` do fluxo
// grande): descreve as DUAS colunas da contagem — não inventa uma terceira
// tabela solta. `distintos` sai de contar valores distintos de verdade.
const distintos = <T,>(xs: T[]) => new Set(xs).size;
export const RESUMO_VERTICAL: Tabela = {
  colunas: ["coluna", "tipo", "faltantes", "distintos"],
  linhas: [
    ["regiao", "character", 0, distintos(REGIOES_ORDENADAS)],
    ["n", "numeric", 0, distintos([...contagemPorRegiao.values()])],
  ],
};

// Geometria: largura de card padrão (240) nos quatro, x alternando 0/300 (o
// zig-zag que lê como descida), y crescendo pela altura real de cada card
// (`alturaCard`, a mesma conta de `metricas.ts`) mais 160px de respiro — é
// esse respiro que dá espaço pra aresta se desenhar sem encostar no card de
// baixo.
export const NOS_VERTICAL: NoFluxo[] = [
  {
    id: "ler",
    spec: SPECS.ler_csv,
    x: 0,
    y: 0,
    resultado: { tipo: "tabela", tabela: VENDAS_VERTICAL },
    duracao: "9ms",
    entra: 6,
    digita: 20,
    resulta: 54,
  },
  {
    id: "filtrar",
    spec: SPECS.filtrar,
    x: 300,
    y: 459,
    resultado: { tipo: "tabela", tabela: CAFE_VERTICAL },
    duracao: "3ms",
    entra: 76,
    digita: 94,
    resulta: 140,
  },
  {
    id: "agrupar",
    spec: SPECS.agrupar,
    x: 0,
    y: 908,
    resultado: { tipo: "tabela", tabela: CONTAGEM_VERTICAL },
    duracao: "4ms",
    entra: 160,
    digita: 178,
    resulta: 224,
  },
  {
    id: "resumo",
    spec: SPECS.resumo,
    x: 300,
    y: 1383,
    resultado: { tipo: "tabela", tabela: RESUMO_VERTICAL },
    duracao: "2ms",
    entra: 248,
    resulta: 290,
  },
];

export const ARESTAS_VERTICAL: ArestaFluxo[] = [
  { de: "ler", paraNo: "filtrar", paraPorta: 0, desenha: 70 },
  { de: "filtrar", paraNo: "agrupar", paraPorta: 0, desenha: 156 },
  { de: "agrupar", paraNo: "resumo", paraPorta: 0, desenha: 240 },
];
