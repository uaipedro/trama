import { describe, it, expect } from "vitest";
import { buildInstallScript, runInstall } from "../src/core/installer.js";
import { writeFileSync, mkdtempSync, chmodSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

describe("buildInstallScript", () => {
  it("gera script com bootstrap do remotes e install_github(build=FALSE)", () => {
    const script = buildInstallScript(
      ["uaipedro/trama", "uaipedro/trama/collections/trama.data"],
      "/home/user/.trama-cli/lib"
    );

    expect(script).toContain('options(repos = c(P3M = "https://packagemanager.posit.co/cran/latest"))');
    expect(script).toContain('if (!requireNamespace("remotes", quietly = TRUE))');
    expect(script).toContain('lib <- "/home/user/.trama-cli/lib"');
    expect(script).toContain(
      'remotes::install_github(pkg, lib = lib, build = FALSE, upgrade = "never", dependencies = NA)'
    );
    expect(script).toContain('"uaipedro/trama"');
    expect(script).toContain('"uaipedro/trama/collections/trama.data"');
  });

  it("escapa aspas duplas num path (JSON.stringify cobre isso)", () => {
    const script = buildInstallScript(["uaipedro/trama"], 'C:\\Users\\Nome "Estranho"\\lib');
    expect(script).toContain('lib <- "C:\\\\Users\\\\Nome \\"Estranho\\"\\\\lib"');
  });
});

describe("runInstall", () => {
  it("inclui a saída do processo na mensagem de erro em vez de só o exit code", async () => {
    const fakeRscript = join(mkdtempSync(join(tmpdir(), "trama-cli-fakescript-")), "fake.sh");
    writeFileSync(fakeRscript, "#!/bin/sh\necho \"$2\"\nexit 1\n");
    chmodSync(fakeRscript, 0o755);

    await expect(
      runInstall(fakeRscript, "Error: não há nenhum pacote chamado 'trama'")
    ).rejects.toThrow(/não há nenhum pacote chamado/);
  });

  it("resolve normalmente quando o processo sai com sucesso", async () => {
    const fakeRscript = join(mkdtempSync(join(tmpdir(), "trama-cli-fakescript-")), "ok.sh");
    writeFileSync(fakeRscript, "#!/bin/sh\necho ok\nexit 0\n");
    chmodSync(fakeRscript, 0o755);

    await expect(runInstall(fakeRscript, "qualquer coisa")).resolves.toBeUndefined();
  });
});
