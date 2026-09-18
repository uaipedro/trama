// src/trama/catalogo.ts — os blocos do primeiro vídeo (coleção `data`) e a
// coreografia deles.
//
// Os specs são TRANSCRITOS de `collections/trama.data/R/collection.R`: rótulo,
// categoria, ícone, portas e params, na ordem em que o R os declara. Inventar
// um bloco plausível aqui seria pior que não ter vídeo: quem assiste vai
// procurar aquele campo no editor.
//
// A coreografia mora junto dos dados de propósito. Espalhar "em que quadro o
// Filtrar entra" pelas cenas é como um vídeo passa a ter dois donos do tempo.
import type { ArestaFluxo, NoFluxo, Spec, Tabela } from "./tipos";

const T = (nome: string) => ({ nome, obrigatoria: true });

// Todo bloco da coleção `data` devolve uma tabela, e o renderer dela tem uma
// vista só.
const VISTAS_TABELA = ["tabela"];

export const SPECS: Record<string, Spec> = {
  ler_csv: {
    id: "data/read_csv",
    rotulo: "Ler CSV",
    categoria: "source",
    icone: "file-spreadsheet",
    entradas: [],
    saidas: [T("out")],
    vistas: VISTAS_TABELA,
    params: [
      { nome: "path", rotulo: "Arquivo", tipo: "campo", valor: "vendas.csv" },
      {
        nome: "delim",
        rotulo: "Separador",
        tipo: "enum-inline",
        valor: ",",
        opcoes: [",", ";", "\\t", "|"],
      },
      { nome: "na", rotulo: "Marcas de fal…", tipo: "campo", valor: "NA" },
    ],
  },
  resumo: {
    id: "data/summary",
    rotulo: "Resumo",
    categoria: "inspect",
    icone: "clipboard-list",
    entradas: [T("data")],
    saidas: [T("out")],
    vistas: VISTAS_TABELA,
    params: [],
  },
  filtrar: {
    id: "data/filter",
    rotulo: "Filtrar",
    categoria: "transform",
    icone: "list-filter",
    entradas: [T("data")],
    saidas: [T("out")],
    vistas: VISTAS_TABELA,
    params: [
      { nome: "expr", rotulo: "Condição", tipo: "expr", valor: 'produto == "cafe"' },
      { nome: "by", rotulo: "Por grupo", tipo: "campo", valor: "", exemplo: "regiao" },
    ],
  },
  agrupar: {
    id: "data/group_summarise",
    rotulo: "Agrupar e resumir",
    categoria: "aggregate",
    icone: "sigma",
    entradas: [T("data")],
    saidas: [T("out")],
    vistas: VISTAS_TABELA,
    params: [
      { nome: "by", rotulo: "Agrupar por", tipo: "campo", valor: "regiao" },
      { nome: "name", rotulo: "Nome", tipo: "campo", valor: "n" },
      { nome: "expr", rotulo: "Resumo", tipo: "expr", valor: "dplyr::n()" },
    ],
  },
  juntar: {
    id: "data/join",
    rotulo: "Juntar",
    categoria: "aggregate",
    icone: "combine",
    entradas: [T("left"), T("right")],
    saidas: [T("out")],
    vistas: VISTAS_TABELA,
    params: [
      { nome: "by", rotulo: "Por", tipo: "campo", valor: "regiao" },
      {
        nome: "type",
        rotulo: "Tipo",
        tipo: "enum-largo",
        valor: "inner",
        opcoes: ["inner", "left", "right", "full", "anti"],
      },
    ],
  },
};

// As tabelas de preview. Os números são os do fluxo de exemplo do repositório —
// quatro regiões, café e açúcar — para o vídeo mostrar um resultado que se
// reproduz rodando o exemplo, e não um enfeite.
//
// CINCO LINHAS, sempre: a faixa de preview tem 132px, a linha da tabela mede
// ~20px e o cabeçalho outros ~20 — então cabem exatamente cinco. A sexta existe
// no DOM, rola dentro do card e no vídeo aparece cortada ao meio na borda de
// baixo. No app isso é informação ("tem mais tabela aí"); num quadro de vídeo é
// só um defeito.
const VENDAS: Tabela = {
  colunas: ["regiao", "produto", "valor", "qtd", "mes"],
  linhas: [
    ["leste", "cafe", 98.69, 6, 2],
    ["leste", "cafe", 176.21, 2, 7],
    ["leste", "acucar", 57.08, 4, 7],
    ["oeste", "acucar", 157.65, 3, 7],
    ["norte", "cafe", 25.37, 5, 5],
  ],
};

const RESUMO: Tabela = {
  colunas: ["coluna", "tipo", "faltantes", "distintos"],
  linhas: [
    ["regiao", "character", 0, 4],
    ["produto", "character", 0, 4],
    ["valor", "numeric", 0, 397],
    ["qtd", "numeric", 0, 9],
    ["mes", "numeric", 0, 12],
  ],
};

const CAFE: Tabela = {
  colunas: ["regiao", "produto", "valor", "qtd", "mes"],
  linhas: [
    ["leste", "cafe", 98.69, 6, 2],
    ["leste", "cafe", 176.21, 2, 7],
    ["norte", "cafe", 25.37, 5, 5],
    ["leste", "cafe", 179.33, 8, 4],
    ["sul", "cafe", 119.4, 3, 6],
  ],
};

const CONTAGEM: Tabela = {
  colunas: ["regiao", "n"],
  linhas: [
    ["leste", 37],
    ["norte", 24],
    ["oeste", 37],
    ["sul", 28],
  ],
};

const RECEITA: Tabela = {
  colunas: ["regiao", "valor"],
  linhas: [
    ["leste", 3946.78],
    ["norte", 2370.05],
    ["oeste", 4025.1],
    ["sul", 3228.88],
  ],
};

const JUNTO: Tabela = {
  colunas: ["regiao", "n", "valor"],
  linhas: [
    ["leste", 37, 3946.78],
    ["norte", 24, 2370.05],
    ["oeste", 37, 4025.1],
    ["sul", 28, 3228.88],
  ],
};

// O fluxo: lê o CSV, confere o que veio, fica só com o café, conta e soma por
// região, e junta as duas contas na mesma tabela. É o caminho que o README
// descreve — fonte, conhecer, transformar, agregar.
export const NOS: NoFluxo[] = [
  {
    id: "ler",
    spec: SPECS.ler_csv,
    x: 0,
    y: 70,
    resultado: { tipo: "tabela", tabela: VENDAS },
    duracao: "8ms",
    // 2, e não 8: as cenas se sobrepõem, então o quadro 0 desta cena JÁ é o
    // corte. Entrar em 8 deixava oito quadros de canvas vazio logo depois da
    // cena anterior sair — o piscar de tela vazia no corte.
    entra: 2,
    digita: 18,
    resulta: 44,
  },
  {
    id: "resumo",
    spec: SPECS.resumo,
    x: 330,
    y: -180,
    resultado: { tipo: "tabela", tabela: RESUMO },
    duracao: "3ms",
    entra: 68,
    resulta: 92,
  },
  {
    id: "filtrar",
    spec: SPECS.filtrar,
    x: 330,
    y: 190,
    resultado: { tipo: "tabela", tabela: CAFE },
    duracao: "2ms",
    entra: 144,
    digita: 164,
    resulta: 186,
  },
  {
    id: "contar",
    spec: SPECS.agrupar,
    x: 660,
    y: 60,
    resultado: { tipo: "tabela", tabela: CONTAGEM },
    duracao: "4ms",
    entra: 228,
    digita: 246,
    resulta: 268,
  },
  {
    id: "somar",
    spec: SPECS.agrupar,
    x: 660,
    y: 430,
    // O mesmo spec do `contar` com outros dois valores digitados. Um spec por
    // nó seria duplicar a transcrição do R e deixar os dois fora de sincronia
    // na primeira mudança.
    params: { name: "valor", expr: "sum(valor)" },
    resultado: { tipo: "tabela", tabela: RECEITA },
    duracao: "4ms",
    entra: 302,
    digita: 318,
    resulta: 338,
  },
  {
    id: "juntar",
    spec: SPECS.juntar,
    x: 990,
    y: 245,
    resultado: { tipo: "tabela", tabela: JUNTO },
    duracao: "5ms",
    entra: 350,
    digita: 362,
    resulta: 380,
  },
];

export const ARESTAS: ArestaFluxo[] = [
  { de: "ler", paraNo: "resumo", paraPorta: 0, desenha: 62 },
  { de: "ler", paraNo: "filtrar", paraPorta: 0, desenha: 138 },
  { de: "filtrar", paraNo: "contar", paraPorta: 0, desenha: 222 },
  { de: "filtrar", paraNo: "somar", paraPorta: 0, desenha: 296 },
  // As duas chegam no mesmo quadro: é UMA junção, e desenhá-las em sequência
  // sugeriria duas ligações independentes.
  { de: "contar", paraNo: "juntar", paraPorta: 0, desenha: 344 },
  { de: "somar", paraNo: "juntar", paraPorta: 1, desenha: 344 },
];

// Coleções que o README manda instalar, na ordem dele. `trama.data` e
// `trama.view` primeiro porque são o ponto de partida recomendado.
export const COLECOES = [
  { nome: "trama.data", o_que: "dados", cor: "source" },
  { nome: "trama.view", o_que: "gráficos", cor: "sink" },
  { nome: "trama.series", o_que: "séries temporais", cor: "transform" },
  { nome: "trama.models", o_que: "modelos", cor: "aggregate" },
  { nome: "trama.multi", o_que: "multivariada", cor: "reshape" },
  { nome: "trama.sampling", o_que: "amostragem", cor: "inspect" },
] as const;
