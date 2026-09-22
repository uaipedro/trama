import { describe, it, expect } from "vitest";
import { mkdtempSync, mkdirSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { checkEnv } from "../src/core/envcheck.js";
import { rscriptPath, libDir } from "../src/core/paths.js";

function tempBase() {
  return mkdtempSync(join(tmpdir(), "trama-cli-test-"));
}

describe("checkEnv", () => {
  it("reporta tudo ausente num diretório vazio", () => {
    const base = tempBase();
    const status = checkEnv(base, "4.4.1");
    expect(status.rPortableInstalled).toBe(false);
    expect(status.coreInstalled).toBe(false);
  });

  it("reporta R instalado mas núcleo ausente", () => {
    const base = tempBase();
    const rscript = rscriptPath(base, "4.4.1");
    mkdirSync(join(rscript, ".."), { recursive: true });
    writeFileSync(rscript, "");

    const status = checkEnv(base, "4.4.1");
    expect(status.rPortableInstalled).toBe(true);
    expect(status.coreInstalled).toBe(false);
  });

  it("reporta R e núcleo instalados quando Meta/package.rds existe", () => {
    const base = tempBase();
    const rscript = rscriptPath(base, "4.4.1");
    mkdirSync(join(rscript, ".."), { recursive: true });
    writeFileSync(rscript, "");

    const tramaDir = join(libDir(base), "trama");
    mkdirSync(join(tramaDir, "Meta"), { recursive: true });
    writeFileSync(join(tramaDir, "DESCRIPTION"), "Package: trama\n");
    writeFileSync(join(tramaDir, "Meta", "package.rds"), "");

    const status = checkEnv(base, "4.4.1");
    expect(status.rPortableInstalled).toBe(true);
    expect(status.coreInstalled).toBe(true);
  });

  it("reporta núcleo ausente quando só DESCRIPTION existe (instalação que ficou pela metade)", () => {
    const base = tempBase();
    const rscript = rscriptPath(base, "4.4.1");
    mkdirSync(join(rscript, ".."), { recursive: true });
    writeFileSync(rscript, "");

    const tramaDir = join(libDir(base), "trama");
    mkdirSync(tramaDir, { recursive: true });
    writeFileSync(join(tramaDir, "DESCRIPTION"), "Package: trama\n");
    // Sem Meta/package.rds: instalação incompleta (rede caiu, antivírus
    // travou um arquivo etc) — DESCRIPTION sozinho não pode contar como
    // "instalado", senão comandos seguintes pulam a reinstalação e o R
    // nunca consegue carregar o pacote de verdade.

    const status = checkEnv(base, "4.4.1");
    expect(status.rPortableInstalled).toBe(true);
    expect(status.coreInstalled).toBe(false);
  });

  it("checa todos os pacotes passados em corePackages, não só 'trama'", () => {
    const base = tempBase();
    const rscript = rscriptPath(base, "4.4.1");
    mkdirSync(join(rscript, ".."), { recursive: true });
    writeFileSync(rscript, "");

    const tramaDir = join(libDir(base), "trama");
    mkdirSync(join(tramaDir, "Meta"), { recursive: true });
    writeFileSync(join(tramaDir, "Meta", "package.rds"), "");
    // trama.data não tem Meta/package.rds — só "trama" está completo.

    const status = checkEnv(base, "4.4.1", ["trama", "trama.data"]);
    expect(status.coreInstalled).toBe(false);
  });
});
