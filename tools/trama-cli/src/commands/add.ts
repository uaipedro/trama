import { baseDir, libDir, rscriptPath } from "../core/paths.js";
import { buildInstallScript, resolveRepoUrl, runInstall } from "../core/installer.js";
import { readConfig, writeConfig, R_VERSION, ensureInstalled } from "./install.js";

/** Instala uma coleção na lib compartilhada e atualiza o config. */
export async function addCollection(collection: string, onProgress: (msg: string) => void = () => {}): Promise<void> {
  await ensureInstalled(onProgress);
  const base = baseDir();
  onProgress(`Instalando ${collection}...`);
  const script = buildInstallScript([`uaipedro/trama/collections/${collection}`], libDir(base), resolveRepoUrl());
  await runInstall(rscriptPath(base, R_VERSION), script);

  const cfg = readConfig(base);
  const collections = new Set(cfg.installedCollections ?? []);
  collections.add(collection);
  writeConfig(base, { ...cfg, installedCollections: [...collections] });
}
