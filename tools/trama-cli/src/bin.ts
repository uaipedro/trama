import { Command } from "commander";

const program = new Command();

program
  .name("trama")
  .description("Instala e roda o trama sem precisar já ter R instalado.")
  .version("0.1.0");

program.parse();
