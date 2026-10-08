import { defineConfig } from "astro/config";
import { rehypeDocLinks } from "./src/lib/rehype-doc-links.ts";
import { rehypeFlowExample } from "./src/lib/rehype-flow-example.ts";
import { rehypeNodeDocs } from "./src/lib/rehype-node-docs.ts";
import { rehypeGlossario } from "./src/lib/rehype-glossario.ts";

const base = "/trama";

export default defineConfig({
  site: "https://uaipedro.github.io",
  base,
  output: "static",
  // Blocos aposentados: os F da regressão de série viraram leitores de models.
  redirects: {
    "/colecoes/series-temporais/f_global": "/trama/colecoes/modelos/fit-stats/",
    "/colecoes/series-temporais/f_seasonal": "/trama/colecoes/modelos/anova-table/",
    "/colecoes/series-temporais/f_trend": "/trama/colecoes/modelos/anova-table/"
  },
  markdown: {
    rehypePlugins: [rehypeFlowExample, [rehypeNodeDocs, { base }], [rehypeGlossario, { base }], rehypeDocLinks]
  }
});
