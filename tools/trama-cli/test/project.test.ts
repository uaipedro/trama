import { describe, it, expect } from "vitest";
import { mkdtempSync, existsSync, readFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { createProject, isProjectDir } from "../src/project.js";

describe("createProject", () => {
  it("cria a pasta com flows/main.json válido", () => {
    const parent = mkdtempSync(join(tmpdir(), "trama-cli-project-"));
    const dir = createProject(parent, "meu-projeto");

    expect(existsSync(join(dir, "flows", "main.json"))).toBe(true);
    const flow = JSON.parse(readFileSync(join(dir, "flows", "main.json"), "utf8"));
    expect(flow.nodes).toEqual({});
    expect(flow.edges).toEqual([]);
  });

  it("isProjectDir detecta uma pasta de projeto existente", () => {
    const parent = mkdtempSync(join(tmpdir(), "trama-cli-project-"));
    const dir = createProject(parent, "outro-projeto");
    expect(isProjectDir(dir)).toBe(true);
    expect(isProjectDir(parent)).toBe(false);
  });
});
