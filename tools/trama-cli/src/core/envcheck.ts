import { existsSync, statSync } from "node:fs";
import { join } from "node:path";
import { rscriptPath, libDir } from "./paths.js";

export interface EnvStatus {
  rPortableInstalled: boolean;
  coreInstalled: boolean;
  rscriptPath: string;
  libDir: string;
}

/** Inspeciona base/r/<rVersion>/bin e base/lib pra determinar o que falta instalar. */
export function checkEnv(base: string, rVersion: string): EnvStatus {
  const rscript = rscriptPath(base, rVersion);
  const lib = libDir(base);

  const rPortableInstalled = existsSync(rscript) && statSync(rscript).isFile();

  const descPath = join(lib, "trama", "DESCRIPTION");
  const coreInstalled = existsSync(descPath) && statSync(descPath).isFile();

  return { rPortableInstalled, coreInstalled, rscriptPath: rscript, libDir: lib };
}
