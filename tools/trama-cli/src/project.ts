import { mkdirSync, writeFileSync, existsSync } from "node:fs";
import { join } from "node:path";

/**
 * Cria parent/name como projeto trama: o manifesto `trama.json` (o mesmo que
 * `trama::tr_project_new()` grava) e a pasta `flows/`. Não escreve
 * `flows/main.json`: o formato do documento é do pacote R, e um flow escrito
 * aqui à mão envelhece — foi o que travou o editor em "carregando..." quando
 * o formato passou a exigir o campo `format`. Sem o arquivo, o trama abre um
 * documento vazio no formato atual e grava o main.json no primeiro salvamento.
 */
export function createProject(parent: string, name: string, collections: string[] = []): string {
  const dir = join(parent, name);
  if (existsSync(join(dir, "trama.json"))) {
    throw new Error(`Já existe um projeto em '${dir}'. Escolha outro nome.`);
  }
  mkdirSync(join(dir, "flows"), { recursive: true });
  writeFileSync(join(dir, "trama.json"), JSON.stringify({ collections }, null, 2) + "\n");
  return dir;
}

/** Confere se dir é a raiz de um projeto trama (tem trama.json). */
export function isProjectDir(dir: string): boolean {
  return existsSync(join(dir, "trama.json"));
}
