// Texto colado que é um link de dados: vira um bloco de leitura com o link
// preenchido. Módulo puro pelo mesmo motivo de `colunas.js`.
//
// Só um link sozinho (com espaço em volta, no máximo): uma frase que contém
// um link não é um pedido de ler dados, e colar texto qualquer no canvas não
// pode criar bloco.
export function linkDeDados(texto) {
  const t = String(texto || "").trim();
  if (!/^https?:\/\/\S+$/i.test(t)) return null;
  try { new URL(t); } catch { return null; }
  return t;
}
