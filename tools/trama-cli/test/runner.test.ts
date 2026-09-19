import { describe, it, expect } from "vitest";
import { buildOpenExpr } from "../src/core/runner.js";

describe("buildOpenExpr", () => {
  it("inclui .libPaths antes de trama::tr_app, na ordem certa", () => {
    const expr = buildOpenExpr("/home/user/.trama-cli/lib", "/home/user/meu-projeto");

    expect(expr).toContain('.libPaths(c("/home/user/.trama-cli/lib", .libPaths()))');
    expect(expr).toContain('trama::tr_app(trama::tr_project("/home/user/meu-projeto"))');
    expect(expr.indexOf(".libPaths")).toBeLessThan(expr.indexOf("trama::tr_app"));
  });
});
