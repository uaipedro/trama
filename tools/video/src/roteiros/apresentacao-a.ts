// src/roteiros/apresentacao-a.ts — segmento A da apresentação ("O Trama é
// bastante visual. Cada bloco representa um passo na sua linha de
// raciocínio."): o fatorial adubo × irrigação montado passo a passo.
//
// Dados reais (`capturas/apresentacao-fatorial.json`), os mesmos do segmento F.
// As legendas dizem o PASSO do raciocínio que cada bloco é. A duração no
// cabeçalho fica vazia: a captura não mede tempo de execução.
import type { Roteiro } from "../motor/roteiro";
import { FATORIAL } from "../trama/captura-fatorial";

export const apresentacaoA: Roteiro = {
  id: "ApresentacaoA",
  blocos: {
    dados: { spec: FATORIAL.dados, x: 0, y: 120, modo: "preview", duracao: "",
      tamanho: { largura: 300, preview: 250 },
      resultado: { tipo: "tabela", tabela: FATORIAL.tabela } },
    anova: { spec: FATORIAL.anova, x: 420, y: 90, modo: "params", duracao: "",
      resultado: { tipo: "modelo", modelo: FATORIAL.fit } },
    quadro: { spec: FATORIAL.quadroSpec, x: 800, y: -120, modo: "preview", duracao: "",
      tamanho: { largura: 400, preview: 170 },
      resultado: { tipo: "quadro", quadro: FATORIAL.quadro } },
    tukey: { spec: FATORIAL.tukeySpec, x: 800, y: 250, modo: "preview", duracao: "",
      tamanho: { largura: 420, preview: 250 },
      resultado: { tipo: "imagem", src: FATORIAL.tukeyPng } },
  },
  planos: [
    { faz: "entra", bloco: "dados", legenda: "o experimento: adubo e irrigação", destacar: ["experimento"] },
    { faz: "entra", bloco: "anova" },
    { faz: "liga", de: "dados", para: "anova" },
    { faz: "param", bloco: "anova", param: "resposta", valor: "producao",
      legenda: "o que foi medido", destacar: ["medido"] },
    { faz: "modo", bloco: "anova", modo: "completo",
      legenda: "o modelo: fatorial em blocos", destacar: ["modelo"] },
    { faz: "entra", bloco: "quadro" },
    { faz: "liga", de: "anova", para: "quadro" },
    { faz: "espera", legenda: "a pergunta: há interação?", destacar: ["interação"] },
    { faz: "entra", bloco: "tukey" },
    { faz: "liga", de: "anova", para: "tukey" },
    { faz: "espera", legenda: "a resposta: qual adubo, com e sem água", destacar: ["resposta"] },
    { faz: "modo", bloco: "dados", modo: "mini" },
    { faz: "geral", legenda: "cada bloco, um passo do raciocínio", destacar: ["passo"] },
    { faz: "still", nome: "capa" },
    { faz: "espera" },
  ],
};
