import { extract as tarExtract } from "tar";
import extractZipLib from "extract-zip";
import {
  mkdtempSync,
  readdirSync,
  renameSync,
  rmSync,
  mkdirSync,
  lstatSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

/**
 * Extrai src (.tar.gz) em dest, removendo o único diretório-raiz do
 * arquivo (ex: R-builds da Posit empacotam tudo dentro de "4.4.1/").
 */
export async function extractTarGz(src: string, dest: string): Promise<void> {
  mkdirSync(dest, { recursive: true });
  await tarExtract({ file: src, cwd: dest, strip: 1 });
}

/**
 * Verifica recursivamente que nenhuma entrada extraída é um symlink.
 *
 * extract-zip (dependência usada abaixo) tem um advisory de segurança
 * conhecido (sem correção upstream) sobre criar symlinks durante sua
 * própria extração que podem apontar para fora do diretório de destino.
 * Isso é ortogonal à etapa de "mover" feita por esta função (que só usa
 * nomes de arquivo já resolvidos no disco via readdirSync, então não
 * introduz travessia de caminho por si só) — mas se extract-zip deixou um
 * symlink em `tmp`, mover (renameSync) esse symlink para `dest` preserva
 * o link, e código posterior que segue esse caminho (ex: rscriptPath)
 * acabaria lendo/escrevendo fora de `dest` sem saber.
 *
 * Como as fontes de download são uma lista pequena e fixa de URLs da
 * Posit, verificadas por checksum antes da extração (não upload de
 * usuário), o risco de exploração é baixo — mas essa checagem é barata e
 * fecha a lacuna de qualquer forma: builds legítimos do R não devem
 * conter symlinks.
 */
function assertNoSymlinks(dir: string): void {
  for (const entry of readdirSync(dir)) {
    const full = join(dir, entry);
    const stat = lstatSync(full);
    if (stat.isSymbolicLink()) {
      throw new Error(
        `arquivo suspeito: entrada "${full}" é um symlink (não esperado num build do R)`
      );
    }
    if (stat.isDirectory()) {
      assertNoSymlinks(full);
    }
  }
}

/**
 * Extrai src (.zip) em dest, removendo o único diretório-raiz do arquivo
 * (ex: o zip do Windows empacota tudo dentro de "R-4.4.1/", diferente do
 * tar.gz do Linux que usa "4.4.1/" — extract-zip não tem `strip` nativo
 * como o pacote tar, então extraímos num diretório temporário e movemos o
 * conteúdo do único subdiretório pra dest.
 */
export async function extractZip(src: string, dest: string): Promise<void> {
  const tmp = mkdtempSync(join(tmpdir(), "trama-cli-unzip-"));
  try {
    await extractZipLib(src, { dir: tmp });

    // Ver comentário de assertNoSymlinks: defesa em profundidade contra o
    // advisory conhecido de extract-zip antes de mover qualquer coisa pra
    // dest.
    assertNoSymlinks(tmp);

    const entries = readdirSync(tmp);
    if (entries.length !== 1) {
      throw new Error(
        `esperava um único diretório-raiz no zip, achou: ${entries.join(", ")}`
      );
    }

    const root = join(tmp, entries[0]);
    if (!lstatSync(root).isDirectory()) {
      throw new Error(`esperava que "${entries[0]}" fosse um diretório`);
    }

    mkdirSync(dest, { recursive: true });
    for (const entry of readdirSync(root)) {
      renameSync(join(root, entry), join(dest, entry));
    }
  } finally {
    rmSync(tmp, { recursive: true, force: true });
  }
}
