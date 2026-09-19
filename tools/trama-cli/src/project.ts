import { mkdirSync, writeFileSync, existsSync } from "node:fs";
import { join } from "node:path";

/** Cria parent/name com a estrutura mínima de um projeto trama. */
export function createProject(parent: string, name: string): string {
  const dir = join(parent, name);
  mkdirSync(join(dir, "flows"), { recursive: true });
  const flow = { rev: 1, collections: {}, nodes: {}, edges: [], view: { positions: {}, sizes: {} } };
  writeFileSync(join(dir, "flows", "main.json"), JSON.stringify(flow, null, 2));
  return dir;
}

/** Confere se dir é a raiz de um projeto trama (tem flows/main.json). */
export function isProjectDir(dir: string): boolean {
  return existsSync(join(dir, "flows", "main.json"));
}
