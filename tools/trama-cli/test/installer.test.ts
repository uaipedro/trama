import { describe, it, expect } from "vitest";
import { buildInstallScript } from "../src/core/installer.js";

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
