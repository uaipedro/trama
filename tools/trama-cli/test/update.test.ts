import { describe, it, expect, afterEach } from "vitest";
import { mkdtempSync, mkdirSync, writeFileSync, chmodSync, readFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { updateCommand } from "../src/commands/update.js";
import { rscriptPath, configPath } from "../src/core/paths.js";
import { R_VERSION } from "../src/commands/install.js";

function tempHome() {
  return mkdtempSync(join(tmpdir(), "trama-cli-update-home-"));
}

const originalHome = process.env.HOME;
afterEach(() => {
  process.env.HOME = originalHome;
});

describe("updateCommand", () => {
  it("recusa atualizar se o R portátil nunca foi instalado", async () => {
    process.env.HOME = tempHome();
    await expect(updateCommand()).rejects.toThrow(/trama ainda não está instalado/);
  });

  it("reinstala o núcleo e as coleções registradas no config", async () => {
    const home = tempHome();
    process.env.HOME = home;
    const base = join(home, ".trama-cli");

    const rscript = rscriptPath(base, R_VERSION);
    mkdirSync(join(rscript, ".."), { recursive: true });
    const logPath = join(home, "last-script.txt");
    writeFileSync(rscript, `#!/bin/sh\ncat "$1" > "${logPath}"\nexit 0\n`);
    chmodSync(rscript, 0o755);

    mkdirSync(base, { recursive: true });
    writeFileSync(
      configPath(base),
      JSON.stringify({ rVersion: R_VERSION, installedCollections: ["trama.ml"] }),
    );

    await updateCommand();

    const executedScript = readFileSync(logPath, "utf8");
    expect(executedScript).toContain('"uaipedro/trama"');
    expect(executedScript).toContain('"uaipedro/trama/collections/trama.data"');
    expect(executedScript).toContain('"uaipedro/trama/collections/trama.ml"');
  });
});
