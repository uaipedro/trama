import open from "open";
import { baseDir, libDir, rscriptPath } from "../core/paths.js";
import { buildOpenExpr, runApp } from "../core/runner.js";
import { R_VERSION } from "./install.js";

/** Sobe o editor no projectDir e abre o navegador. */
export async function openProject(projectDir: string): Promise<void> {
  const base = baseDir();
  const expr = buildOpenExpr(libDir(base), projectDir);
  const { port } = await runApp(rscriptPath(base, R_VERSION), expr);
  await open(`http://127.0.0.1:${port}`);
}
