import { describe, it, expect } from "vitest";
import { createServer } from "node:http";
import { mkdtempSync, readFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { createHash } from "node:crypto";
import { download } from "../src/core/download.js";

function withServer(handler: (req: any, res: any) => void) {
  const server = createServer(handler);
  return new Promise<{ url: string; close: () => Promise<void> }>((resolve) => {
    server.listen(0, "127.0.0.1", () => {
      const addr = server.address();
      const port = typeof addr === "object" && addr ? addr.port : 0;
      resolve({
        url: `http://127.0.0.1:${port}`,
        close: () => new Promise((r) => server.close(() => r())),
      });
    });
  });
}

describe("download", () => {
  it("baixa e valida checksum na primeira tentativa", async () => {
    const good = Buffer.from("conteudo-do-r-portatil");
    const checksum = createHash("sha256").update(good).digest("hex");
    const { url, close } = await withServer((_req, res) => res.end(good));

    const dest = join(mkdtempSync(join(tmpdir(), "trama-cli-dl-")), "out.bin");
    await download(url, checksum, dest, 2);
    expect(readFileSync(dest)).toEqual(good);
    await close();
  });

  it("tenta de novo se o checksum não bater na 1ª vez", async () => {
    const good = Buffer.from("conteudo-bom");
    const checksum = createHash("sha256").update(good).digest("hex");
    let attempt = 0;
    const { url, close } = await withServer((_req, res) => {
      attempt++;
      res.end(attempt === 1 ? Buffer.from("corrompido") : good);
    });

    const dest = join(mkdtempSync(join(tmpdir(), "trama-cli-dl-")), "out.bin");
    await download(url, checksum, dest, 2);
    expect(attempt).toBe(2);
    await close();
  });

  it("desiste depois de esgotar as tentativas", async () => {
    const { url, close } = await withServer((_req, res) => res.end("sempre-corrompido"));
    const dest = join(mkdtempSync(join(tmpdir(), "trama-cli-dl-")), "out.bin");
    await expect(download(url, "checksum-que-nunca-bate", dest, 2)).rejects.toThrow();
    await close();
  });
});
