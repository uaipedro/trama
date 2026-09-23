import { defineConfig } from "astro/config";
import { rehypeFlowExample } from "./src/lib/rehype-flow-example.ts";

export default defineConfig({
  site: "https://uaipedro.github.io",
  base: "/trama",
  output: "static",
  markdown: {
    rehypePlugins: [rehypeFlowExample]
  }
});
