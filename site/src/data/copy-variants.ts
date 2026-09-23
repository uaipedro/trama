export const copyKeys = ["subtitle", "title", "lede", "ctaPrimary", "ctaSecondary", "metaDescription"] as const;
type CopyKey = (typeof copyKeys)[number];
export type Variant = { id: string; name: string; copy: Record<CopyKey, string> };

export const variants = [
  {
    id: "controle",
    name: "Controle (atual)",
    copy: {
      subtitle: "R em blocos executáveis",
      title: "Seu raciocínio, visível.",
      lede: "O trama organiza funções R em fluxos que você monta, executa e inspeciona etapa por etapa.",
      ctaPrimary: "Comece por aqui",
      ctaSecondary: "Explore as coleções",
      metaDescription: "R em blocos executáveis."
    }
  },
  {
    id: "monte",
    name: "Monte bloco a bloco",
    copy: {
      subtitle: "Monte suas análises bloco a bloco",
      title: "Seu raciocínio, visível.",
      lede: "Um programa de análise com o poder do R: você encaixa as etapas, vê o resultado de cada uma e muda qualquer passo sem refazer o resto.",
      ctaPrimary: "Comece por aqui",
      ctaSecondary: "Explore as coleções",
      metaDescription: "Monte suas análises bloco a bloco, com o poder do R."
    }
  },
  {
    id: "passo",
    name: "Passo a passo",
    copy: {
      subtitle: "Análises construídas passo a passo",
      title: "Seu raciocínio, visível.",
      lede: "Do dado ao gráfico, cada etapa vira um bloco na tela. É o R fazendo as contas, e você enxergando o caminho inteiro.",
      ctaPrimary: "Comece por aqui",
      ctaSecondary: "Explore as coleções",
      metaDescription: "Análises construídas passo a passo, com o R por baixo."
    }
  },
  {
    id: "programa",
    name: "Programa de análise",
    copy: {
      subtitle: "Um programa de análise, feito sobre o R",
      title: "Seu raciocínio, visível.",
      lede: "Leia, limpe, modele e visualize num canvas. Cada bloco mostra o que entrou e o que saiu, e o código R continua lá quando você quiser.",
      ctaPrimary: "Comece por aqui",
      ctaSecondary: "Explore as coleções",
      metaDescription: "Um programa de análise visual, feito sobre o R."
    }
  }
] as const satisfies readonly Variant[];

export const control = variants[0];
