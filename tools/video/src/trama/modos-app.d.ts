// Tipos de `modos-app.js`, que é CÓPIA de `inst/www/modos.js` (trazida por
// `scripts/sincronizar.sh`). O vídeo decide o que o card mostra com as mesmas
// funções que o editor usa — não com uma releitura delas.
export type Modo = "mini" | "params" | "preview" | "completo";
export const MODOS: Modo[];
export const MODO_PADRAO: Modo;
export function mostraPreview(m: Modo): boolean;
export function mostraParams(m: Modo): boolean;
export function precisaPainel(m: Modo): boolean;
