// src/trama/tipos.ts — o vocabulário compartilhado pelos dois vídeos.
//
// Separado dos catálogos porque o canvas, o card e a geometria não podem
// depender de QUAL vídeo está sendo renderizado: um importa o catálogo da
// coleção `data`, o outro o da `models`, e os dois desenham com o mesmo código.
import type { Tamanho } from "./metricas";

export type Porta = { nome: string; obrigatoria?: boolean };

export type Param = {
  nome: string;
  rotulo: string;
  // Os formatos que `layoutEnum` (inst/www/params.js) escolhe para um enum, mais
  // os widgets que as coleções registram. `campo` é input de uma linha
  // (path/cols/text), `expr` é o textarea `rows=2` da coleção `data`,
  // `enum-inline`/`enum-largo` são o segmentado do runtime e `enum-select` é o
  // `<select>` que ele usa quando há opção demais para caber.
  tipo: "campo" | "expr" | "enum-inline" | "enum-largo" | "enum-select" | "numero";
  valor: string;
  exemplo?: string;
  opcoes?: string[];
};

export type Spec = {
  id: string;
  rotulo: string;
  categoria: string;
  icone: string;
  entradas: Porta[];
  saidas: Porta[];
  params: Param[];
  // As vistas que o renderer do resultado oferece (a faixa de abas). Uma só
  // vista vira o nome dela em cinza; duas ou mais viram botões, com a corrente
  // sublinhada. A faixa é SEMPRE reservada, mesmo sem resultado.
  vistas: string[];
};

export type Tabela = { colunas: string[]; linhas: (string | number)[][] };

// Uma linha do quadro de efeitos (`models/effects`): o que a `Regua` e as
// `Estrelas` precisam, mais as colunas numéricas do quadro.
export type LinhaQuadro = {
  termo: string;
  valores: (number | null)[];
  p?: number | null;
};

export type Quadro = {
  titulo: string;
  // O primeiro rótulo é o da coluna de texto (FV na ANOVA, termo nas
  // comparações); os demais casam por posição com `valores`.
  colunas: string[];
  // Índices DENTRO de `valores` que são contagem, e saem sem decimal. Na ANOVA
  // é o GL (0); nas comparações entre pares o GL é o terceiro valor. Supor que
  // a primeira coluna numérica é sempre o GL dá certo num quadro e erra no
  // outro.
  inteiras?: number[];
  // Rótulo da coluna do p: "Pr > F" na ANOVA, "Pr > |t|" nas comparações.
  rotuloP: string;
  linhas: LinhaQuadro[];
  rodape?: Record<string, string>;
};

export type Destaque = { rotulo: string; valor: number; barra?: boolean; pct?: boolean };

export type CardDeModelo = {
  rotulo: string;
  formula: string;
  n: number;
  destaques: Destaque[];
  global: { rotulo: string; p: number };
};

export type Medias = {
  // O eixo x do gráfico (o fator) e o rótulo do y, como `tr_models_plot_means`
  // os escreve.
  fator: string;
  rotuloY: string;
  pontos: { nivel: string; media: number; li: number; ls: number; grupo: string }[];
};

// O resultado que o card desenha. É união discriminada porque o preview de um
// bloco não é "uma tabela": é o que o renderer daquele TIPO devolve, e os
// quatro que os dois vídeos usam desenham coisas completamente diferentes.
export type Resultado =
  | { tipo: "tabela"; tabela: Tabela }
  | { tipo: "modelo"; modelo: CardDeModelo }
  | { tipo: "quadro"; quadro: Quadro }
  | { tipo: "grafico"; medias: Medias };

export type NoFluxo = {
  id: string;
  spec: Spec;
  // Canto superior-esquerdo em coordenadas de canvas. A câmera é que traz cada
  // um para o centro do quadro.
  x: number;
  y: number;
  rotulo?: string;
  resultado: Resultado;
  tamanho?: Tamanho;
  // Índice da aba aberta. O quadro de efeitos tem duas vistas, e qual delas o
  // card mostra é escolha do documento (`ui.views`) — não do renderer.
  vista?: number;
  // Valores de param que este nó sobrescreve do spec (dois nós do mesmo bloco
  // com parâmetros diferentes).
  params?: Record<string, string>;
  duracao: string;
  // Quadros, relativos ao início da cena do fluxo.
  entra: number;
  // Quando o valor do primeiro param aparece, digitado. `undefined` = já nasce
  // preenchido.
  digita?: number;
  // Quando o preview deixa de estar "executando" e mostra o resultado.
  resulta: number;
};

export type ArestaFluxo = {
  de: string;
  paraNo: string;
  // Índice da porta de entrada do destino: o Juntar recebe `left` em 0 e
  // `right` em 1, e trocar isso é trocar o significado da junção.
  paraPorta: number;
  desenha: number;
};

export function valorParam(no: NoFluxo, param: Param): string {
  return no.params?.[param.nome] ?? param.valor;
}
