import { existsSync, statSync } from "node:fs";
import { join } from "node:path";
import { rscriptPath, libDir } from "./paths.js";

export interface EnvStatus {
  rPortableInstalled: boolean;
  coreInstalled: boolean;
  rscriptPath: string;
  libDir: string;
}

/**
 * Um pacote só está de verdade instalado quando Meta/package.rds existe —
 * é o artefato que o instalador do R grava por último, depois de copiar o
 * código, compilar e testar que carrega. DESCRIPTION sozinho não basta:
 * é escrito logo no início da instalação, então uma instalação que falhou
 * no meio do caminho (rede caiu, antivírus travou um arquivo no Windows
 * etc) ainda deixa DESCRIPTION no lugar, mas nunca chega a carregar —
 * bug real batido: `trama install` "concluía" com um pacote parcial,
 * checkEnv achava coreInstalled = true, e comandos seguintes (`create`,
 * `add`) pulavam a reinstalação; só na hora de `library(trama)` o R dava
 * "não há nenhum pacote chamado 'trama'".
 */
function packageComplete(lib: string, pkgName: string): boolean {
  const rdsPath = join(lib, pkgName, "Meta", "package.rds");
  return existsSync(rdsPath) && statSync(rdsPath).isFile();
}

/** Inspeciona base/r/<rVersion>/bin e base/lib pra determinar o que falta instalar. */
export function checkEnv(base: string, rVersion: string, corePackages: string[] = ["trama"]): EnvStatus {
  const rscript = rscriptPath(base, rVersion);
  const lib = libDir(base);

  const rPortableInstalled = existsSync(rscript) && statSync(rscript).isFile();
  const coreInstalled = corePackages.every((pkg) => packageComplete(lib, pkg));

  return { rPortableInstalled, coreInstalled, rscriptPath: rscript, libDir: lib };
}
