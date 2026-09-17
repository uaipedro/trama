// tests/js/editor-streamcontrols.test.mjs
//
// `editor.js` não pode ser importado por `node --test`: ele carrega React,
// ReactDOM e xyflow pelos bundles de `inst/www/vendor/`, construídos para o
// importmap do NAVEGADOR (um único registro de módulos compartilhado entre
// "react" e "react-dom"). Sob Node, um especificador nu como "react" só
// resolve com um loader ESM próprio — e mesmo resolvendo, os bundles fazem
// `require("react")` internamente (fallback do esbuild para import externo);
// como esses arquivos não declaram "type":"module", o Node os trata como
// ambíguos e injeta um `require` sintético que reinstancia o módulo por um
// caminho DIFERENTE do `import` estático, duplicando o estado interno do
// React. O resultado, verificado manualmente, é um crash dentro do próprio
// react-dom.js ("Cannot read properties of undefined (reading
// 'ReactCurrentBatchConfig')") — não um erro do NOSSO código. Renderizar de
// verdade só é viável num browser real (é o que a Tarefa B1 fez, à mão, via
// claude-in-chrome) ou atrás de um bundler/DOM completo (Playwright,
// jsdom+webpack) — investimento bem maior que um "smoke test".
//
// O que ESTE arquivo garante, sem precisar executar React nenhum: que
// `StreamControls` (a) chama `contagemDoPasso(...)` — a função pura já
// testada em `params.test.mjs` — e (b) não referencia `passo` como
// identificador solto (nem parâmetro, nem `const`/`let` local). É exatamente
// a wiring que quebrou em produção: a função pura estava certa, a chamada é
// que não existia. Um teste de texto/fonte é frágil a refactors de nome, mas
// é honesto sobre o que pode e o que não pode ser verificado aqui — e pinça
// a MESMA classe de regressão (variável de escopo errado em uso condicional
// raro) que os 3.544 testes anteriores não pegaram.
import { test } from "node:test";
import assert from "node:assert/strict";
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

const aqui = path.dirname(fileURLToPath(import.meta.url));
const fonte = fs.readFileSync(path.join(aqui, "../../inst/www/editor.js"), "utf8");

function corpoDe(nomeDaFuncao) {
  const m = new RegExp(`function ${nomeDaFuncao}\\(([^)]*)\\)\\s*\\{`).exec(fonte);
  assert.ok(m, `função ${nomeDaFuncao} não encontrada em editor.js`);
  let i = m.index + m[0].length;
  let profundidade = 1;
  const inicio = i;
  while (profundidade > 0 && i < fonte.length) {
    if (fonte[i] === "{") profundidade++;
    else if (fonte[i] === "}") profundidade--;
    i++;
  }
  return { params: m[1], corpo: fonte.slice(inicio, i - 1) };
}

test("StreamControls chama contagemDoPasso() com progress?.message", () => {
  const { corpo } = corpoDe("StreamControls");
  assert.match(corpo, /contagemDoPasso\(\s*progress\?\.message\s*\)/,
    "o contador do card da fonte tem que vir de contagemDoPasso(progress?.message), " +
    "a mesma extração que a barra de progresso comum já faz (Preview, acima)");
});

// Remove comentários de linha e literais de string/template ANTES de procurar
// o identificador — "um passo" (rótulo de botão) e o próprio comentário que
// explica o bug não podem contar como uso da variável.
function semStringsNemComentarios(js) {
  return js
    .replace(/\/\/[^\n]*/g, "")
    .replace(/`(?:\\.|[^`\\])*`/g, "``")
    .replace(/"(?:\\.|[^"\\])*"/g, '""')
    .replace(/'(?:\\.|[^'\\])*'/g, "''");
}

test("StreamControls não referencia `passo` — essa variável é local a App(), não existe aqui", () => {
  const { params, corpo } = corpoDe("StreamControls");
  assert.doesNotMatch(params, /\bpasso\b/);
  const codigo = semStringsNemComentarios(corpo);
  assert.doesNotMatch(codigo, /\bpasso\b/,
    "`passo` não é parâmetro nem const/let local de StreamControls — é a " +
    "variável do carrossel de apresentação em App() (~linha 1816). " +
    "Referenciá-la aqui é ReferenceError em produção (módulo ES, strict mode) " +
    "assim que uma região entra em `running`.");
});
