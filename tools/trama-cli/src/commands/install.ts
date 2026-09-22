import { baseDir, rDir, libDir, rscriptPath, configPath } from "../core/paths.js";
import { checkEnv } from "../core/envcheck.js";
import { resolveSource } from "../core/sources.js";
import { download } from "../core/download.js";
import { extractTarGz, extractZip } from "../core/extract.js";
import { buildInstallScript, resolveRepoUrl, runInstall } from "../core/installer.js";
import { mkdirSync, readFileSync, writeFileSync, existsSync } from "node:fs";
import { join } from "node:path";

export const R_VERSION = "4.4.1";
export const CORE_PACKAGES = ["uaipedro/trama", "uaipedro/trama/collections/trama.data", "uaipedro/trama/collections/trama.view"];

interface Config {
  rVersion?: string;
  installedCollections?: string[];
}

export function readConfig(base: string): Config {
  const path = configPath(base);
  if (!existsSync(path)) return {};
  return JSON.parse(readFileSync(path, "utf8"));
}

export function writeConfig(base: string, cfg: Config): void {
  mkdirSync(base, { recursive: true });
  writeFileSync(configPath(base), JSON.stringify(cfg, null, 2));
}

/** Garante R portátil + núcleo instalados. Idempotente. */
export async function ensureInstalled(onProgress: (msg: string) => void = () => {}): Promise<void> {
  const base = baseDir();
  mkdirSync(base, { recursive: true });
  let status = checkEnv(base, R_VERSION);

  if (!status.rPortableInstalled) {
    onProgress("Baixando R portátil...");
    const src = resolveSource(process.platform, R_VERSION);
    const cacheDir = join(base, "cache");
    mkdirSync(cacheDir, { recursive: true });
    const archive = join(cacheDir, `r-download.${src.format === "zip" ? "zip" : "tar.gz"}`);
    await download(src.url, src.checksum, archive, 2);
    const dest = rDir(base, R_VERSION);
    onProgress("Extraindo R portátil...");
    if (src.format === "zip") await extractZip(archive, dest);
    else await extractTarGz(archive, dest);
    status = checkEnv(base, R_VERSION);
  }

  if (!status.coreInstalled) {
    onProgress("Instalando núcleo do trama...");
    const script = buildInstallScript(CORE_PACKAGES, libDir(base), resolveRepoUrl());
    await runInstall(rscriptPath(base, R_VERSION), script);
    const cfg = readConfig(base);
    writeConfig(base, { ...cfg, rVersion: R_VERSION, installedCollections: cfg.installedCollections ?? [] });
  }
}
