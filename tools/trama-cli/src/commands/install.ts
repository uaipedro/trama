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

/** Nome do pacote R a partir de uma ref do remotes ("owner/repo/subdir" -> "subdir"). */
export function pkgNameFromRef(ref: string): string {
  return ref.split("/").pop()!;
}

export const CORE_PACKAGE_NAMES = CORE_PACKAGES.map(pkgNameFromRef);

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

/**
 * Reporta o progresso de um download em % direto no stdout via \r (sobrescreve a
 * mesma linha em vez de imprimir uma linha por tick). Fora do onProgress(msg) genérico
 * porque esse é feito pra marcos discretos (uma linha por etapa) — um "% baixado" a cada
 * chunk de rede geraria dezenas de linhas se passasse por ali. Baixa a % arredondada, não a
 * cada chunk, senão pisca a cada poucos KB numa conexão rápida.
 */
function reportDownloadProgress(label: string): (fraction: number) => void {
  let lastPercent = -1;
  return (fraction: number) => {
    const percent = Math.min(100, Math.round(fraction * 100));
    if (percent === lastPercent) return;
    lastPercent = percent;
    process.stdout.write(`\r${label} ${percent}%${percent === 100 ? "\n" : ""}`);
  };
}

/** Garante R portátil + núcleo instalados. Idempotente. */
export async function ensureInstalled(onProgress: (msg: string) => void = () => {}): Promise<void> {
  const base = baseDir();
  mkdirSync(base, { recursive: true });
  let status = checkEnv(base, R_VERSION, CORE_PACKAGE_NAMES);

  if (!status.rPortableInstalled) {
    onProgress("Baixando R portátil...");
    const src = resolveSource(process.platform, R_VERSION);
    const cacheDir = join(base, "cache");
    mkdirSync(cacheDir, { recursive: true });
    const archive = join(cacheDir, `r-download.${src.format === "zip" ? "zip" : "tar.gz"}`);
    await download(src.url, src.checksum, archive, 2, reportDownloadProgress("Baixando R portátil..."));
    const dest = rDir(base, R_VERSION);
    onProgress("Extraindo R portátil...");
    if (src.format === "zip") await extractZip(archive, dest);
    else await extractTarGz(archive, dest);
    status = checkEnv(base, R_VERSION, CORE_PACKAGE_NAMES);
  }

  if (!status.coreInstalled) {
    onProgress("Instalando núcleo do trama...");
    const script = buildInstallScript(CORE_PACKAGES, libDir(base), resolveRepoUrl(), true);
    await runInstall(rscriptPath(base, R_VERSION), script);
    const cfg = readConfig(base);
    writeConfig(base, { ...cfg, rVersion: R_VERSION, installedCollections: cfg.installedCollections ?? [] });
  }
}
