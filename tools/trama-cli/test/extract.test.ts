import { describe, it, expect } from "vitest";
import { mkdtempSync, existsSync, readFileSync, mkdirSync, writeFileSync, createWriteStream } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { create as createTar } from "tar";
import { extractTarGz, extractZip } from "../src/core/extract.js";
import yazl from "yazl";

function tempDir() {
  return mkdtempSync(join(tmpdir(), "trama-cli-extract-"));
}

describe("extractTarGz", () => {
  it("extrai e remove o diretório-raiz (ex: 4.4.1/bin/Rscript -> bin/Rscript)", async () => {
    const src = tempDir();
    mkdirSync(join(src, "4.4.1", "bin"), { recursive: true });
    writeFileSync(join(src, "4.4.1", "bin", "Rscript"), "conteudo");

    const archive = join(tempDir(), "r.tar.gz");
    await createTar({ gzip: true, file: archive, cwd: src }, ["4.4.1"]);

    const dest = tempDir();
    await extractTarGz(archive, dest);

    expect(existsSync(join(dest, "bin", "Rscript"))).toBe(true);
    expect(readFileSync(join(dest, "bin", "Rscript"), "utf8")).toBe("conteudo");
  });
});

describe("extractZip", () => {
  it("extrai e remove o diretório-raiz (ex: R-4.4.1/bin/Rscript.exe -> bin/Rscript.exe)", async () => {
    const zipfile = new yazl.ZipFile();
    zipfile.addBuffer(Buffer.from("conteudo"), "R-4.4.1/bin/Rscript.exe");
    const archive = join(tempDir(), "r.zip");
    const out = createWriteStream(archive);
    zipfile.outputStream.pipe(out);
    zipfile.end();
    await new Promise<void>((resolve) => out.on("close", () => resolve()));

    const dest = tempDir();
    await extractZip(archive, dest);

    expect(existsSync(join(dest, "bin", "Rscript.exe"))).toBe(true);
    expect(readFileSync(join(dest, "bin", "Rscript.exe"), "utf8")).toBe("conteudo");
  });
});
