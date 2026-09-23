import * as clack from "@clack/prompts";
import { createProject } from "../project.js";
import { ensureInstalled, CORE_PACKAGE_NAMES } from "./install.js";
import { addCollection } from "./add.js";
import { openProject } from "./open.js";

const AVAILABLE_COLLECTIONS = [
  { value: "trama.ml", label: "Machine learning" },
  { value: "trama.series", label: "Séries temporais" },
  { value: "trama.models", label: "Modelos estatísticos" },
  { value: "trama.multi", label: "Multivariada" },
  { value: "trama.sampling", label: "Amostragem" },
];

export async function createCommand(name: string): Promise<void> {
  clack.intro(`Criando projeto trama: ${name}`);

  await ensureInstalled((msg) => clack.log.step(msg));

  const selected = await clack.multiselect({
    message: "Quais coleções instalar? (espaço pra marcar, enter pra confirmar)",
    options: AVAILABLE_COLLECTIONS,
    required: false,
  });
  if (clack.isCancel(selected)) {
    clack.cancel("Cancelado.");
    process.exit(1);
  }

  // trama (o núcleo) não é coleção; trama.data e trama.view são.
  const collections = [...CORE_PACKAGE_NAMES.filter((p) => p !== "trama"), ...(selected as string[])];
  const dir = createProject(process.cwd(), name, collections);

  for (const collection of selected as string[]) {
    await addCollection(collection, (msg) => clack.log.step(msg));
  }

  clack.outro("Abrindo o editor...");
  await openProject(dir);
}
