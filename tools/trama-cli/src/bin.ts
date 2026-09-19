#!/usr/bin/env node
import { Command } from "commander";
import { ensureInstalled } from "./commands/install.js";

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

program.parse();
