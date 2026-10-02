// Entrada do bundle do editor SQL. Remontar: `npm install && npm run build` nesta pasta.
// Exporta só o que `collections/trama.sql/inst/trama/index.js` e o teste usam.
export { EditorView, keymap, placeholder } from "@codemirror/view";
export { EditorState, Compartment, Prec } from "@codemirror/state";
export { basicSetup } from "codemirror";
export { sql, PostgreSQL, StandardSQL } from "@codemirror/lang-sql";
export { linter, lintGutter, setDiagnostics } from "@codemirror/lint";
export { autocompletion } from "@codemirror/autocomplete";
export { format as formatarSql } from "sql-formatter";
import pg from "node-sql-parser/build/postgresql.js";
export const Parser = pg.Parser || (pg.default && pg.default.Parser) || pg;
