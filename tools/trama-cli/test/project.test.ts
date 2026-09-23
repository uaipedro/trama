import { describe, it, expect } from "vitest";
import { mkdtempSync, existsSync, readFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { createProject, isProjectDir } from "../src/project.js";

describe("createProject", () => {
  it("cria trama.json com as coleções e flows/ vazio (o flow é do pacote R)", () => {
    const parent = mkdtempSync(join(tmpdir(), "trama-cli-project-"));
    const dir = createProject(parent, "meu-projeto", ["trama.data", "trama.view"]);

    const cfg = JSON.parse(readFileSync(join(dir, "trama.json"), "utf8"));
    expect(cfg.collections).toEqual(["trama.data", "trama.view"]);
    expect(existsSync(join(dir, "flows"))).toBe(true);
    expect(existsSync(join(dir, "flows", "main.json"))).toBe(false);
  });

  it("recusa sobrescrever um projeto existente", () => {
    const parent = mkdtempSync(join(tmpdir(), "trama-cli-project-"));
    createProject(parent, "p");
    expect(() => createProject(parent, "p")).toThrow(/Já existe/);
  });

  it("isProjectDir detecta uma pasta de projeto existente", () => {
    const parent = mkdtempSync(join(tmpdir(), "trama-cli-project-"));
    const dir = createProject(parent, "outro-projeto");
    expect(isProjectDir(dir)).toBe(true);
    expect(isProjectDir(parent)).toBe(false);
  });
});
