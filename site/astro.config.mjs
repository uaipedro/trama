import { defineConfig } from "astro/config";
import { rehypeDocLinks } from "./src/lib/rehype-doc-links.ts";
import { rehypeFlowExample } from "./src/lib/rehype-flow-example.ts";
import { rehypeNodeDocs } from "./src/lib/rehype-node-docs.ts";

export default defineConfig({
  site: "https://uaipedro.github.io",
  base: "/trama",
  output: "static",
  markdown: {
    rehypePlugins: [rehypeFlowExample, rehypeNodeDocs, rehypeDocLinks]
  }
});
