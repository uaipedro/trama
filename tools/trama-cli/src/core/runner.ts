import { spawn, type ChildProcess } from "node:child_process";
import { rEnv } from "./installer.js";

/**
 * Monta a expressão R que abre o editor. Prepende libPath a .libPaths()
 * explicitamente (não depende de R_LIBS_USER/variáveis de ambiente) —
 * determinístico, não depende de como o R do usuário trata o ambiente
 * herdado do processo pai. Mesma lógica do buildTramaAppExpr do launcher Go.
 */
export function buildOpenExpr(libPath: string, projectDir: string): string {
  return `.libPaths(c(${JSON.stringify(libPath)}, .libPaths())); trama::tr_app(trama::tr_project(${JSON.stringify(projectDir)}))`;
}

const LISTENING_RE = /Listening on http:\/\/[^:]+:(\d+)/;
const MAX_TAIL_LINES = 40;

export interface RunResult {
  proc: ChildProcess;
  port: number;
}

/**
 * Sobe `rscriptPath -e expr` e resolve assim que a linha "Listening on
 * http://host:PORT" aparecer na saída — isso dá a porta REAL que o Shiny
 * escolheu (tr_app() pula pra próxima porta livre se a padrão 8726 estiver
 * ocupada), então não precisamos assumir/checar uma porta fixa como o
 * launcher Go fazia. Se o processo morrer antes dessa linha aparecer,
 * rejeita com a saída capturada (mesmo raciocínio do WaitForAppReady do
 * launcher Go: detectar o crash na hora, não depois de um timeout cego).
 */
export function runApp(rscriptPath: string, expr: string): Promise<RunResult> {
  return new Promise((resolve, reject) => {
    const proc = spawn(rscriptPath, ["-e", expr], { env: rEnv() });
    const tail: string[] = [];
    let settled = false;

    const onData = (chunk: Buffer) => {
      const text = chunk.toString("utf8");
      for (const line of text.split(/\r?\n/)) {
        if (!line) continue;
        tail.push(line);
        if (tail.length > MAX_TAIL_LINES) tail.shift();
      }
      if (settled) return;
      const match = LISTENING_RE.exec(text);
      if (match) {
        settled = true;
        resolve({ proc, port: Number(match[1]) });
      }
    };
    proc.stdout.on("data", onData);
    proc.stderr.on("data", onData);

    proc.on("error", (err) => {
      if (!settled) {
        settled = true;
        reject(err);
      }
    });
    proc.on("close", (code) => {
      if (!settled) {
        settled = true;
        reject(new Error(`processo R encerrou (código ${code}) antes da porta responder:\n${tail.join("\n")}`));
      }
    });
  });
}
