export interface Source {
  url: string;
  checksum: string;
  format: "zip" | "tar.gz";
}

// URLs e checksums verificados ao vivo (baixados e conferidos por sha256)
// durante o desenvolvimento do launcher Go — ver
// docs/plans/2026-09-18-launcher-fast-plan.md. Builds relocáveis
// publicados pela Posit (rstudio/r-builds), não instaladores.
const SOURCES: Record<string, Record<string, Source>> = {
  "4.4.1": {
    win32: {
      url: "https://cdn.posit.co/r/windows/R-4.4.1-windows.zip",
      checksum: "a82d78ef104d91f72570b7094cacd731fc4e5faced949add7628e0f7c081c351",
      format: "zip",
    },
    linux: {
      // Requer glibc >= 2.34 (Ubuntu 22.04+/Debian 12+).
      url: "https://cdn.posit.co/r/manylinux_2_34/R-4.4.1-manylinux_2_34.tar.gz",
      checksum: "23683241cd0c9035e38e00d548918a33a413372e8abdc1a388119b338e72053d",
      format: "tar.gz",
    },
  },
};

/** Resolve de onde baixar o R portátil pra essa plataforma/versão. platform é process.platform. */
export function resolveSource(platform: string, rVersion: string): Source {
  const byVersion = SOURCES[rVersion];
  if (!byVersion) {
    throw new Error(`versão de R não catalogada: ${rVersion}`);
  }
  const src = byVersion[platform];
  if (!src) {
    throw new Error(`SO não suportado: ${platform}`);
  }
  return src;
}
