import { defineCollection } from "astro:content";
import { glob } from "astro/loaders";
import { z } from "astro/zod";

const docs = defineCollection({
  loader: glob({ base: "./src/content/docs", pattern: "**/*.md" }),
  schema: z.object({
    title: z.string(),
    description: z.string(),
    section: z.enum(["comece", "conceitos", "colecoes", "receitas"]),
    collection: z.enum(["dados", "visualizacao", "aprendizado", "modelos", "multivariada", "amostragem", "series-temporais"]).optional(),
    node: z.string().optional(),
    category: z.string().optional(),
    order: z.number().default(99),
    related: z.array(z.string()).default([])
  })
});

export const collections = { docs };
