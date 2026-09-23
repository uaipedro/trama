// src/trama/catalogo-modelos.ts — o fluxo do segundo vídeo: uma ANOVA em
// blocos casualizados, do dado às letras.
//
// OS NÚMEROS NÃO FORAM INVENTADOS. Todos saíram de rodar o fluxo de verdade,
// com o pacote instalado:
//
//   d   <- tr_models_example("milho_dbc")
//   fit <- tr_models_anova_dbc(d, "producao", "hibrido", "bloco")
//   tr_models_anova_table(fit, "I")
//   em  <- tr_models_emmeans(fit, especs = "hibrido", ajuste = "tukey")
//   tr_models_pairwise(em, "todos os pares", ajuste = "tukey")
//
// O `milho_dbc` é o exemplo que a própria ajuda do bloco descreve como
// construído para isto: 5 híbridos em 4 blocos, com o H3 uma tonelada e pouco
// acima do H1, "para o Tukey ter de separar o H3 dos demais". É exatamente o
// que as letras do vídeo mostram — e quem rodar em casa vê os mesmos valores.
//
// Os specs são transcritos de `collections/trama.models/R/`: `nos_ajustar.R`
// (exemplo e ANOVA · DBC) e `nos_resumir.R` (quadro, médias e comparações). O
// formato de cada enum é o que `layoutEnum` escolhe pelo número e tamanho das
// opções — `segmentado`, `largo` ou `select`.
import type { ArestaFluxo, NoFluxo, Quadro, Spec, Tabela } from "./tipos";
import type { CardDeModelo, Medias } from "./tipos";

const T = (nome: string) => ({ nome, obrigatoria: true });

const AJUSTES = ["tukey", "bonferroni", "holm", "sidak", "nenhum"];
const EXEMPLOS = [
  "PlantGrowth",
  "milho_dbc",
  "racao_dql",
  "ToothGrowth",
  "warpbreaks",
  "npk",
  "aveia",
  "sleepstudy",
  "InsectSprays",
  "mtcars",
  "cars",
];

export const SPECS: Record<string, Spec> = {
  exemplo: {
    id: "models/example",
    rotulo: "Exemplo de modelos",
    categoria: "modelo_fonte",
    icone: "database",
    entradas: [],
    saidas: [T("out")],
    vistas: ["tabela"],
    params: [
      // Onze opções: `layoutEnum` passa dos limites do segmentado e devolve
      // `select`.
      {
        nome: "dataset",
        rotulo: "Conjunto",
        tipo: "enum-select",
        valor: "milho_dbc",
        opcoes: EXEMPLOS,
      },
    ],
  },
  anova_dbc: {
    id: "models/anova_dbc",
    rotulo: "ANOVA · DBC",
    categoria: "modelo_anova",
    icone: "grid-3x3",
    entradas: [T("dados")],
    saidas: [T("out")],
    // `models/fit` registra duas vistas.
    vistas: ["ajuste", "efeitos"],
    params: [
      { nome: "resposta", rotulo: "Resposta", tipo: "campo", valor: "producao" },
      { nome: "tratamento", rotulo: "Tratamento", tipo: "campo", valor: "hibrido" },
      { nome: "bloco", rotulo: "Bloco", tipo: "campo", valor: "bloco" },
    ],
  },
  anova_table: {
    id: "models/anova_table",
    rotulo: "Quadro da ANOVA",
    categoria: "modelo_resumir",
    icone: "sheet",
    entradas: [T("modelo")],
    saidas: [T("out")],
    vistas: ["quadro", "significância"],
    params: [
      {
        nome: "tipo_sq",
        rotulo: "Soma de quadrados",
        tipo: "enum-inline",
        valor: "I",
        opcoes: ["I", "II", "III"],
      },
    ],
  },
  emmeans: {
    id: "models/emmeans",
    rotulo: "Médias ajustadas",
    categoria: "modelo_medias",
    icone: "chart-column",
    entradas: [T("modelo")],
    saidas: [T("out")],
    // O card do tipo `models/emm` é o GRÁFICO das médias: uma vista só.
    vistas: ["imagem"],
    params: [
      { nome: "especs", rotulo: "Médias de", tipo: "campo", valor: "hibrido" },
      { nome: "por", rotulo: "Por (desdobra…", tipo: "campo", valor: "", exemplo: "supp" },
      // Cinco opções somando 30 caracteres: não cabem ao lado do rótulo, e o
      // segmentado vira "largo" — rótulo em cima, opções embaixo.
      {
        nome: "ajuste",
        rotulo: "Ajuste das letras",
        tipo: "enum-largo",
        valor: "tukey",
        opcoes: AJUSTES,
      },
      { nome: "alfa", rotulo: "Nível (alfa)", tipo: "numero", valor: "0,05" },
      {
        nome: "escala",
        rotulo: "Escala (GLM)",
        tipo: "enum-inline",
        valor: "resposta",
        opcoes: ["resposta", "ligação"],
      },
    ],
  },
  pairwise: {
    id: "models/pairwise",
    rotulo: "Comparações de médias",
    categoria: "modelo_medias",
    icone: "git-compare",
    entradas: [T("medias")],
    saidas: [T("out")],
    vistas: ["quadro", "significância"],
    params: [
      {
        nome: "metodo",
        rotulo: "Comparar",
        tipo: "enum-largo",
        valor: "todos os pares",
        opcoes: ["todos os pares", "contra controle"],
      },
      { nome: "controle", rotulo: "Controle", tipo: "campo", valor: "", exemplo: "ctrl" },
      // Seis opções somando 37 caracteres: passa do "largo" e cai no `select`.
      {
        nome: "ajuste",
        rotulo: "Ajuste",
        tipo: "enum-select",
        valor: "tukey",
        opcoes: [...AJUSTES, "dunnett"],
      },
    ],
  },
};

// As cinco primeiras linhas do `milho_dbc`, como saem do R. Cinco porque é o
// que cabe na faixa de preview de 132px sem cortar a sexta ao meio.
export const MILHO: Tabela = {
  colunas: ["bloco", "hibrido", "producao"],
  linhas: [
    ["B1", "H1", 7.58],
    ["B2", "H1", 7.72],
    ["B3", "H1", 8.55],
    ["B4", "H1", 7.97],
    ["B1", "H2", 7.47],
  ],
};

// O card do modelo: `.tr_models_fit_preview`. Os quatro destaques saem na ordem
// em que o R os acrescenta, e o renderer mostra os três primeiros.
export const FIT: CardDeModelo = {
  rotulo: "ANOVA · DBC",
  formula: "producao ~ bloco + hibrido",
  n: 20,
  destaques: [
    { rotulo: "R²", valor: 0.858781279547765, barra: true },
    { rotulo: "R² aj.", valor: 0.776403692617294, barra: true },
    { rotulo: "CV", valor: 3.72005400281408, pct: true },
    { rotulo: "AIC", valor: 17.0588698947458 },
  ],
  // `.tr_models_teste_global`: nos delineamentos de um fator é o F do
  // tratamento.
  global: { rotulo: "F de hibrido", p: 0.000188257884363411 },
};

export const QUADRO_ANOVA: Quadro = {
  titulo: "Quadro da ANOVA · SQ tipo I",
  colunas: ["FV", "GL", "SQ", "QM", "Fc"],
  inteiras: [0],
  rotuloP: "Pr > F",
  linhas: [
    { termo: "bloco", valores: [3, 1.63228, 0.5440933, 5.844339], p: 0.0106467611 },
    { termo: "hibrido", valores: [4, 5.16147, 1.2903675, 13.860388], p: 0.000188257884363411 },
    { termo: "Resíduo", valores: [12, 1.11717, 0.0930975, null], p: null },
    // O Total só aparece no tipo I: é o único em que as SQ somam a SQ total.
    { termo: "Total", valores: [19, 7.91092, null, null], p: null },
  ],
  rodape: { CV: "3,7%", média: "8,202", n: "20" },
};

export const MEDIAS: Medias = {
  fator: "hibrido",
  rotuloY: "producao (média ajustada e IC 95%)",
  pontos: [
    { nivel: "H1", media: 7.955, li: 7.622602, ls: 8.287398, grupo: "bc" },
    { nivel: "H2", media: 7.8675, li: 7.535102, ls: 8.199898, grupo: "bc" },
    { nivel: "H3", media: 9.0675, li: 8.735102, ls: 9.399898, grupo: "a" },
    { nivel: "H4", media: 7.655, li: 7.322602, ls: 7.987398, grupo: "c" },
    { nivel: "H5", media: 8.465, li: 8.132602, ls: 8.797398, grupo: "ab" },
  ],
};

// As dez comparações, com o p já AJUSTADO por Tukey. O card mostra a vista
// `significância`: dez linhas do quadro completo não caberiam legíveis, e é
// esta vista que responde "quem difere de quem" de relance.
export const PARES: Quadro = {
  titulo: "Comparações entre pares",
  colunas: ["termo", "Estimativa", "EP", "GL", "t"],
  inteiras: [2],
  rotuloP: "Pr > |t|",
  linhas: [
    { termo: "H1 - H2", valores: [0.0875, 0.2157516, 12, 0.405559], p: 0.9935348932 },
    { termo: "H1 - H3", valores: [-1.1125, 0.2157516, 12, -5.156393], p: 0.0018082156 },
    { termo: "H1 - H4", valores: [0.3, 0.2157516, 12, 1.390488], p: 0.6445090414 },
    { termo: "H1 - H5", valores: [-0.51, 0.2157516, 12, -2.36383], p: 0.1910001753 },
    { termo: "H2 - H3", valores: [-1.2, 0.2157516, 12, -5.561952], p: 0.0009508739 },
    { termo: "H2 - H4", valores: [0.2125, 0.2157516, 12, 0.984929], p: 0.8570473807 },
    { termo: "H2 - H5", valores: [-0.5975, 0.2157516, 12, -2.769389], p: 0.1005623753 },
    { termo: "H3 - H4", valores: [1.4125, 0.2157516, 12, 6.546881], p: 0.0002172166 },
    { termo: "H3 - H5", valores: [0.6025, 0.2157516, 12, 2.792563], p: 0.096816066 },
    { termo: "H4 - H5", valores: [-0.81, 0.2157516, 12, -3.754318], p: 0.0189281582 },
  ],
  rodape: { ajuste: "tukey" },
};

// Os cards do quadro, do gráfico e das comparações são MAIORES que o padrão de
// 240px. Não é licença artística: no app o tamanho mora em `ui.sizes`, o usuário
// arrasta a alça, e um quadro de ANOVA com seis colunas ou uma figura de artigo
// em 240px é exatamente o card que se redimensiona primeiro.
export const NOS: NoFluxo[] = [
  {
    id: "dados",
    spec: SPECS.exemplo,
    x: 0,
    y: 40,
    resultado: { tipo: "tabela", tabela: MILHO },
    duracao: "6ms",
    entra: 2,
    resulta: 34,
  },
  {
    id: "anova",
    spec: SPECS.anova_dbc,
    x: 320,
    y: 20,
    resultado: { tipo: "modelo", modelo: FIT },
    duracao: "31ms",
    entra: 66,
    digita: 84,
    resulta: 118,
  },
  {
    id: "quadro",
    spec: SPECS.anova_table,
    x: 640,
    y: -150,
    tamanho: { largura: 390, preview: 150 },
    resultado: { tipo: "quadro", quadro: QUADRO_ANOVA },
    duracao: "12ms",
    entra: 156,
    resulta: 198,
  },
  {
    id: "medias",
    spec: SPECS.emmeans,
    x: 640,
    y: 200,
    tamanho: { largura: 420, preview: 236 },
    resultado: { tipo: "grafico", medias: MEDIAS },
    duracao: "0,4s",
    entra: 256,
    digita: 274,
    resulta: 316,
  },
  {
    id: "pares",
    spec: SPECS.pairwise,
    x: 1150,
    y: -60,
    tamanho: { largura: 400, preview: 215 },
    // A aba aberta é a segunda.
    vista: 1,
    resultado: { tipo: "quadro", quadro: PARES },
    duracao: "48ms",
    entra: 366,
    resulta: 412,
  },
];

export const ARESTAS: ArestaFluxo[] = [
  { de: "dados", paraNo: "anova", paraPorta: 0, desenha: 60 },
  // O mesmo modelo alimenta o quadro e as médias: é o ponto do fluxo — ajusta
  // uma vez, lê de várias maneiras.
  { de: "anova", paraNo: "quadro", paraPorta: 0, desenha: 150 },
  { de: "anova", paraNo: "medias", paraPorta: 0, desenha: 250 },
  { de: "medias", paraNo: "pares", paraPorta: 0, desenha: 360 },
];
