import { homedir } from "node:os";
import { join } from "node:path";

/** Diretório base do CLI: R portátil, lib de pacotes, config.json. */
export function baseDir(): string {
  return join(homedir(), ".trama-cli");
}

export function rDir(base: string, rVersion: string): string {
  return join(base, "r", rVersion);
}

export function libDir(base: string): string {
  return join(base, "lib");
}

export function rscriptPath(base: string, rVersion: string): string {
  const bin = process.platform === "win32" ? "Rscript.exe" : "Rscript";
  return join(rDir(base, rVersion), "bin", bin);
}

export function configPath(base: string): string {
  return join(base, "config.json");
}
