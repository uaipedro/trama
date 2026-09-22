import { createWriteStream, existsSync, mkdirSync, rmSync } from "node:fs";
import { createHash } from "node:crypto";
import { dirname } from "node:path";
import { Readable } from "node:stream";
import { pipeline } from "node:stream/promises";
import { readFile } from "node:fs/promises";

/** Fração (0 a 1) baixada até agora. Sem Content-Length no servidor, nunca é chamado. */
export type ProgressListener = (fraction: number) => void;

/** Baixa url pra dest, validando sha256. Tenta até maxAttempts vezes. */
export async function download(
  url: string,
  checksum: string,
  dest: string,
  maxAttempts: number,
  onProgress?: ProgressListener
): Promise<void> {
  let lastErr: unknown;
  for (let attempt = 1; attempt <= maxAttempts; attempt++) {
    try {
      await downloadOnce(url, dest, onProgress);
      if (await verifyChecksum(dest, checksum)) return;
      rmSync(dest, { force: true });
      lastErr = new Error(`checksum não confere na tentativa ${attempt}`);
    } catch (err) {
      lastErr = err;
    }
  }
  throw new Error(`falha ao baixar ${url} após ${maxAttempts} tentativas: ${lastErr}`);
}

async function downloadOnce(url: string, dest: string, onProgress?: ProgressListener): Promise<void> {
  mkdirSync(dirname(dest), { recursive: true });
  const res = await fetch(url);
  if (!res.ok || !res.body) {
    throw new Error(`status HTTP ${res.status}`);
  }
  const total = Number(res.headers.get("content-length"));
  const source = Readable.fromWeb(res.body as any);
  if (onProgress && Number.isFinite(total) && total > 0) {
    let received = 0;
    source.on("data", (chunk: Buffer) => {
      received += chunk.length;
      onProgress(received / total);
    });
  }
  await pipeline(source, createWriteStream(dest));
}

async function verifyChecksum(path: string, want: string): Promise<boolean> {
  if (!existsSync(path)) return false;
  const data = await readFile(path);
  const got = createHash("sha256").update(data).digest("hex");
  return got === want;
}
