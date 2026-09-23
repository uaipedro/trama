// src/roteiros/anova-modos.ts — a ANOVA do milho, mostrando os modos do card.
//
// Dados reais (ver `trama/catalogo-modelos.ts`). Trocar a soma de quadrados
// de I pra III não muda o quadro, e é verdade: o `milho_dbc` é balanceado.
import type { Roteiro } from "../motor/roteiro";
import { FIT, MEDIAS, MILHO, QUADRO_ANOVA, SPECS } from "../trama/catalogo-modelos";

export const anovaModos: Roteiro = {
  id: "AnovaModos",
  blocos: {
    dados: { spec: SPECS.exemplo, x: 0, y: 170, modo: "mini", duracao: "6ms",
      resultado: { tipo: "tabela", tabela: MILHO } },
    anova: { spec: SPECS.anova_dbc, x: 200, y: 110, modo: "params", duracao: "31ms",
      resultado: { tipo: "modelo", modelo: FIT } },
    quadro: { spec: SPECS.anova_table, x: 560, y: -80, modo: "preview", duracao: "12ms",
      tamanho: { largura: 390, preview: 150 },
      resultado: { tipo: "quadro", quadro: QUADRO_ANOVA } },
    medias: { spec: SPECS.emmeans, x: 560, y: 250, duracao: "0,4s",
      tamanho: { largura: 420, preview: 236 },
      resultado: { tipo: "grafico", medias: MEDIAS } },
  },
  planos: [
    { faz: "entra", bloco: "dados", legenda: "um exemplo pronto: milho em blocos" },
    { faz: "entra", bloco: "anova" },
    { faz: "liga", de: "dados", para: "anova" },
    { faz: "param", bloco: "anova", param: "resposta", valor: "producao",
      legenda: "diga qual coluna é a resposta", destacar: ["resposta"] },
    { faz: "modo", bloco: "anova", modo: "completo",
      legenda: "abra o card pra ver o ajuste", destacar: ["ajuste"] },
    { faz: "entra", bloco: "quadro" },
    { faz: "liga", de: "anova", para: "quadro" },
    { faz: "param", bloco: "quadro", param: "tipo_sq", valor: "III",
      legenda: "sem parâmetros no card, eles abrem ao lado", destacar: ["ao", "lado"] },
    { faz: "entra", bloco: "medias" },
    { faz: "liga", de: "anova", para: "medias" },
    { faz: "espera", legenda: "as letras separam o H3", destacar: ["H3"] },
    { faz: "modo", bloco: "anova", modo: "mini", legenda: "recolha o que já leu" },
    { faz: "geral", legenda: "do dado às letras, num fluxo só", destacar: ["letras"] },
    { faz: "still", nome: "capa" },
    { faz: "espera" },
  ],
};
