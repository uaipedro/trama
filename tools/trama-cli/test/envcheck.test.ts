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

  it("reporta R e núcleo instalados", () => {
    const base = tempBase();
    const rscript = rscriptPath(base, "4.4.1");
    mkdirSync(join(rscript, ".."), { recursive: true });
    writeFileSync(rscript, "");

    const tramaDir = join(libDir(base), "trama");
    mkdirSync(tramaDir, { recursive: true });
    writeFileSync(join(tramaDir, "DESCRIPTION"), "Package: trama\n");

    const status = checkEnv(base, "4.4.1");
    expect(status.rPortableInstalled).toBe(true);
    expect(status.coreInstalled).toBe(true);
  });
});
