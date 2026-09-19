/**
 * Monta a expressão R que abre o editor. Prepende libPath a .libPaths()
 * explicitamente (não depende de R_LIBS_USER/variáveis de ambiente) —
 * determinístico, não depende de como o R do usuário trata o ambiente
 * herdado do processo pai. Mesma lógica do buildTramaAppExpr do launcher Go.
 */
export function buildOpenExpr(libPath: string, projectDir: string): string {
  return `.libPaths(c(${JSON.stringify(libPath)}, .libPaths())); trama::tr_app(trama::tr_project(${JSON.stringify(projectDir)}))`;
}
