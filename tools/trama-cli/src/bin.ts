import { Command } from "commander";
import { ensureInstalled } from "./commands/install.js";
import { addCollection } from "./commands/add.js";
import { openProject } from "./commands/open.js";
import { createCommand } from "./commands/create.js";
import { updateCommand } from "./commands/update.js";
import { isProjectDir } from "./project.js";
import { printMascote } from "./mascote.js";

const program = new Command();

program
  .name("trama")
  .description("Instala e roda o trama sem precisar já ter R instalado.")
  .version("0.2.4");

// Envolve o handler de cada comando para transformar erros/rejeições não
// tratadas em uma mensagem de erro limpa, em vez de um stack trace cru
// (o commander não captura automaticamente exceções em .action() async).
function runAction<Args extends unknown[]>(
  fn: (...args: Args) => Promise<void>,
): (...args: Args) => Promise<void> {
  return async (...args: Args) => {
    try {
      await fn(...args);
    } catch (err) {
      console.error(`Erro: ${err instanceof Error ? err.message : err}`);
      process.exit(1);
    }
  };
}

program
  .command("install")
  .description("Baixa o R portátil e instala o núcleo do trama")
  .action(
    runAction(async () => {
      printMascote();
      await ensureInstalled((msg) => console.log(msg));
      console.log("Pronto.");
    }),
  );

program
  .command("create <nome>")
  .description("Cria um novo projeto e instala as coleções escolhidas")
  .action(runAction(async (nome: string) => {
    await createCommand(nome);
  }));

program
  .command("add <colecao>")
  .description("Instala uma coleção adicional (ex: trama.ml)")
  .action(runAction(async (colecao: string) => {
    await addCollection(colecao, (msg) => console.log(msg));
    console.log("Pronto.");
  }));

program
  .command("update")
  .description("Atualiza o núcleo do trama e as coleções instaladas para a versão mais recente")
  .action(
    runAction(async () => {
      await updateCommand((msg) => console.log(msg));
      console.log("Pronto.");
    }),
  );

program
  .command("open")
  .description("Abre o editor no projeto da pasta atual")
  .action(
    runAction(async () => {
      if (!isProjectDir(process.cwd())) {
        console.error("Essa pasta não parece um projeto trama (sem trama.json).");
        process.exit(1);
      }
      await openProject(process.cwd());
    }),
  );

program.parse();
