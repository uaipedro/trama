import { describe, it, expect } from "vitest";
import { buildOpenExpr, runApp } from "../src/core/runner.js";
import { writeFileSync, mkdtempSync, chmodSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

describe("buildOpenExpr", () => {
  it("inclui .libPaths antes de trama::tr_app, na ordem certa", () => {
    const expr = buildOpenExpr("/home/user/.trama-cli/lib", "/home/user/meu-projeto");

    expect(expr).toContain('.libPaths(c("/home/user/.trama-cli/lib", .libPaths()))');
    expect(expr).toContain('trama::tr_app(trama::tr_project("/home/user/meu-projeto"))');
    expect(expr.indexOf(".libPaths")).toBeLessThan(expr.indexOf("trama::tr_app"));
  });
});

describe("runApp", () => {
  it("resolve com a porta real, lida da linha 'Listening on'", async () => {
    const fake = join(mkdtempSync(join(tmpdir(), "trama-cli-fakeapp-")), "fake.sh");
    writeFileSync(fake, "#!/bin/sh\necho 'Listening on http://127.0.0.1:8791'\nsleep 5\n");
    chmodSync(fake, 0o755);

    const result = await runApp(fake, "qualquer expressao");
    expect(result.port).toBe(8791);
    result.proc.kill();
  });

  it("rejeita com a saída capturada se o processo morrer antes de abrir a porta", async () => {
    const fake = join(mkdtempSync(join(tmpdir(), "trama-cli-fakeapp-")), "crash.sh");
    writeFileSync(fake, "#!/bin/sh\necho 'Error: nao ha pacote chamado trama'\nexit 1\n");
    chmodSync(fake, 0o755);

    await expect(runApp(fake, "qualquer expressao")).rejects.toThrow(/nao ha pacote chamado/);
  });
});
