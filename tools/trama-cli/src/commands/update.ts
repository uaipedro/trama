import { baseDir, libDir, rscriptPath } from "../core/paths.js";
import { checkEnv } from "../core/envcheck.js";
import { buildInstallScript, resolveRepoUrl, runInstall } from "../core/installer.js";
import { readConfig, writeConfig, CORE_PACKAGES, R_VERSION } from "./install.js";

/** Reinstala o núcleo e as coleções já instaladas, sempre pegando o HEAD do GitHub. */
export async function updateCommand(onProgress: (msg: string) => void = () => {}): Promise<void> {
  const base = baseDir();
  const status = checkEnv(base, R_VERSION);
  if (!status.rPortableInstalled) {
    throw new Error("trama ainda não está instalado. Rode `trama install` primeiro.");
  }

  const cfg = readConfig(base);
  const collections = cfg.installedCollections ?? [];
  const pkgs = [...CORE_PACKAGES, ...collections.map((c) => `uaipedro/trama/collections/${c}`)];

  onProgress("Atualizando trama e coleções instaladas...");
  const script = buildInstallScript(pkgs, libDir(base), resolveRepoUrl());
  await runInstall(rscriptPath(base, R_VERSION), script, (pkg) => onProgress(`  instalando ${pkg}...`));

  writeConfig(base, { ...cfg, rVersion: R_VERSION, installedCollections: collections });
}
