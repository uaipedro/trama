export const collections = [
  {
    id: "dados",
    title: "Dados",
    packageName: "trama.data",
    icon: "database",
    eyebrow: "Coleção trama.data",
    description: "Leia, inspecione, prepare, transforme e grave tabelas."
  },
  {
    id: "visualizacao",
    title: "Visualização",
    packageName: "trama.view",
    icon: "chart-scatter",
    eyebrow: "Coleção trama.view",
    description: "Escolha gráficos a partir da pergunta e da estrutura da tabela."
  },
  {
    id: "aprendizado",
    title: "Aprendizado de máquina",
    packageName: "trama.ml",
    icon: "brain-circuit",
    eyebrow: "Coleção trama.ml",
    description: "Prepare dados, treine modelos, faça previsões e avalie resultados."
  },
  {
    id: "modelos",
    title: "Modelos",
    packageName: "trama.models",
    icon: "sigma",
    eyebrow: "Coleção trama.models",
    description: "Ajuste modelos estatísticos, examine efeitos e confira pressupostos."
  },
  {
    id: "multivariada",
    title: "Multivariada",
    packageName: "trama.multi",
    icon: "chart-network",
    eyebrow: "Coleção trama.multi",
    description: "Explore relações entre variáveis, reduza dimensões e classifique observações."
  },
  {
    id: "amostragem",
    title: "Amostragem",
    packageName: "trama.sampling",
    icon: "scan-search",
    eyebrow: "Coleção trama.sampling",
    description: "Planeje amostras, selecione unidades e estime medidas da população."
  },
  {
    id: "series-temporais",
    title: "Séries temporais",
    packageName: "trama.series",
    icon: "activity",
    eyebrow: "Coleção trama.series",
    description: "Prepare séries, investigue padrões, ajuste modelos e avalie previsões."
  }
] as const;

export type CollectionId = (typeof collections)[number]["id"];
