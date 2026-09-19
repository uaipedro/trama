import { Command } from "commander";
import { ensureInstalled } from "./commands/install.js";
import { addCollection } from "./commands/add.js";
import { openProject } from "./commands/open.js";
import { createCommand } from "./commands/create.js";
import { isProjectDir } from "./project.js";

const program = new Command();

program
  .name("trama")
  .description("Instala e roda o trama sem precisar já ter R instalado.")
  .version("0.1.0");

program
  .command("install")
  .description("Baixa o R portátil e instala o núcleo do trama")
  .action(async () => {
    await ensureInstalled((msg) => console.log(msg));
    console.log("Pronto.");
  });

program
  .command("create <nome>")
  .description("Cria um novo projeto e instala as coleções escolhidas")
  .action(async (nome: string) => {
    await createCommand(nome);
  });

program
  .command("add <colecao>")
  .description("Instala uma coleção adicional (ex: trama.ml)")
  .action(async (colecao: string) => {
    await addCollection(colecao, (msg) => console.log(msg));
    console.log("Pronto.");
  });

program
  .command("open")
  .description("Abre o editor no projeto da pasta atual")
  .action(async () => {
    if (!isProjectDir(process.cwd())) {
      console.error("Essa pasta não parece um projeto trama (sem flows/main.json).");
      process.exit(1);
    }
    await openProject(process.cwd());
  });

program.parse();
