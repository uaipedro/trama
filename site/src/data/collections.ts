export const collections = [
  {
    id: "dados",
    title: "Dados",
    packageName: "trama.data",
    icon: "database",
    eyebrow: "Coleção trama.data",
    description: "Leia, inspecione, prepare, transforme e grave tabelas.",
    steps: [
      { label: "Ler", tone: "source" },
      { label: "Conhecer", tone: "inspect" },
      { label: "Limpar", tone: "clean" },
      { label: "Transformar", tone: "transform" },
      { label: "Resumir", tone: "aggregate" },
      { label: "Gravar", tone: "sink" }
    ]
  },
  {
    id: "visualizacao",
    title: "Visualização",
    packageName: "trama.view",
    icon: "chart-scatter",
    eyebrow: "Coleção trama.view",
    description: "Escolha gráficos a partir da pergunta e da estrutura da tabela.",
    steps: [
      { label: "Pergunta", tone: "inspect" },
      { label: "Tabela", tone: "transform" },
      { label: "Gráfico", tone: "aggregate" },
      { label: "Leitura", tone: "sink" }
    ]
  },
  {
    id: "aprendizado",
    title: "Aprendizado de máquina",
    packageName: "trama.ml",
    icon: "brain-circuit",
    eyebrow: "Coleção trama.ml",
    description: "Prepare dados, treine modelos, faça previsões e avalie resultados.",
    steps: [
      { label: "Dados", tone: "source" },
      { label: "Treino", tone: "transform" },
      { label: "Modelo", tone: "aggregate" },
      { label: "Avaliação", tone: "inspect" }
    ]
  },
  {
    id: "modelos",
    title: "Modelos",
    packageName: "trama.models",
    icon: "sigma",
    eyebrow: "Coleção trama.models",
    description: "Ajuste modelos estatísticos, examine efeitos e confira pressupostos.",
    steps: [
      { label: "Dados", tone: "source" },
      { label: "Ajuste", tone: "transform" },
      { label: "Efeitos", tone: "aggregate" },
      { label: "Diagnóstico", tone: "inspect" }
    ]
  },
  {
    id: "multivariada",
    title: "Multivariada",
    packageName: "trama.multi",
    icon: "chart-network",
    eyebrow: "Coleção trama.multi",
    description: "Explore relações entre variáveis, reduza dimensões e classifique observações.",
    steps: [
      { label: "Variáveis", tone: "source" },
      { label: "Método", tone: "transform" },
      { label: "Resultado", tone: "aggregate" },
      { label: "Diagnóstico", tone: "inspect" }
    ]
  },
  {
    id: "amostragem",
    title: "Amostragem",
    packageName: "trama.sampling",
    icon: "scan-search",
    eyebrow: "Coleção trama.sampling",
    description: "Planeje amostras, selecione unidades e estime medidas da população.",
    steps: [
      { label: "Plano", tone: "source" },
      { label: "Seleção", tone: "transform" },
      { label: "Estimativa", tone: "aggregate" },
      { label: "Precisão", tone: "inspect" }
    ]
  },
  {
    id: "experimentos",
    title: "Experimentos",
    packageName: "trama.experiments",
    icon: "flask-conical",
    eyebrow: "Coleção trama.experiments",
    description: "Declare o delineamento, simule a resposta termo a termo, analise por contrastes e avalie o plano.",
    steps: [
      { label: "Planejar", tone: "source" },
      { label: "Analisar", tone: "transform" },
      { label: "Avaliar", tone: "inspect" }
    ]
  },
  {
    id: "series-temporais",
    title: "Séries temporais",
    packageName: "trama.series",
    icon: "activity",
    eyebrow: "Coleção trama.series",
    description: "Prepare séries, investigue padrões, ajuste modelos e avalie previsões.",
    steps: [
      { label: "Série", tone: "source" },
      { label: "Padrões", tone: "inspect" },
      { label: "Modelo", tone: "transform" },
      { label: "Previsão", tone: "aggregate" }
    ]
  }
] as const;

export type CollectionId = (typeof collections)[number]["id"];
