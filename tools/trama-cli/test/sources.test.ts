import { describe, it, expect } from "vitest";
import { resolveSource } from "../src/core/sources.js";

describe("resolveSource", () => {
  it("resolve fonte pro Windows", () => {
    const src = resolveSource("win32", "4.4.1");
    expect(src.url).toContain("windows");
    expect(src.format).toBe("zip");
    expect(src.checksum).toHaveLength(64);
  });

  it("resolve fonte pro Linux", () => {
    const src = resolveSource("linux", "4.4.1");
    expect(src.url).toContain("manylinux");
    expect(src.format).toBe("tar.gz");
    expect(src.checksum).toHaveLength(64);
  });

  it("rejeita plataforma não suportada", () => {
    expect(() => resolveSource("darwin", "4.4.1")).toThrow();
  });

  it("rejeita versão de R não catalogada", () => {
    expect(() => resolveSource("linux", "9.9.9")).toThrow();
  });
});
