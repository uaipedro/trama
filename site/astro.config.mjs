import { defineConfig } from "astro/config";
import { rm } from "node:fs/promises";
import { rehypeDocLinks } from "./src/lib/rehype-doc-links.ts";
import { rehypeFlowExample } from "./src/lib/rehype-flow-example.ts";
import { rehypeNodeDocs } from "./src/lib/rehype-node-docs.ts";
import { rehypeGlossario } from "./src/lib/rehype-glossario.ts";

const base = "/trama";

export default defineConfig({
  site: "https://uaipedro.github.io",
  base,
  output: "static",
  // Páginas em src/pages/dev/ são laboratório local: servem no `astro dev`, saem do build.
  integrations: [{
    name: "sem-paginas-dev",
    hooks: { "astro:build:done": async ({ dir }) => rm(new URL("dev/", dir), { recursive: true, force: true }) }
  }],
  markdown: {
    rehypePlugins: [rehypeFlowExample, [rehypeNodeDocs, { base }], [rehypeGlossario, { base }], rehypeDocLinks]
  }
});
