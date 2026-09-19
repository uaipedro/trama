// tests/js/organizar.test.mjs — Task 5.1: "Organizar leva a nota, sem
// arranjá-la" (nota nunca é arranjada pelo dagre; anda com o bloco que a
// contém, sem escalar).
//
// DECISÃO DE IMPORT (investigada antes de escrever qualquer teste, como
// pedido): `organizar` mora em `inst/www/frames.js`, e ESSE ARQUIVO NÃO
// IMPORTA SOB `node --test` NESTE REPOSITÓRIO — confirmado por dois
// experimentos:
//
//   1. `node -e "import('./inst/www/frames.js')"` falha em
//      `Cannot find package 'react'`: `frames.js` importa React, @xyflow/react
//      e o especificador nu "trama" pelo importmap do navegador, que o Node
//      não tem como resolver.
//   2. Mesmo se isso não existisse, `organizar` chama `dagrePos`, que importa
//      `@dagrejs/dagre` — e ESSE import TAMBÉM falha sob Node aqui: não há
//      `node_modules` na raiz do repo nem um `package.json` que descreva a
//      dependência (o único `node_modules` com `@dagrejs/dagre` mora em
//      `tools/vendor/`, usado só para gerar o bundle vendorizado do
//      navegador). Confirmado isolando o import: `node <arquivo-em-inst/www>`
//      com só `import dagre from "@dagrejs/dagre"` já falha em
//      `ERR_MODULE_NOT_FOUND`, em qualquer diretório do projeto.
//
// Ou seja: ao contrário do que a Task descreve como hipótese mais provável
// ("dagre é pacote npm real, deve importar bem"), NENHUMA das duas partes de
// `organizar` (React de um lado, dagre do outro) roda sob `node --test` neste
// repo, hoje. Mover `organizar` inteira para `geometria.js` (a opção B da
// Task) NÃO resolveria o problema do dagre — só mudaria de arquivo o mesmo
// `ERR_MODULE_NOT_FOUND` — e transformaria `geometria.js` (hoje zero
// dependência externa, doc do próprio arquivo: "é o que o node --test
// consegue importar") numa dependente real do pacote `dagre`, quebrando TODOS
// os testes existentes que o importam. Essa não é a "razão concreta" que a
// Task pede para tocar `geometria.js` além do combinado — é o oposto: uma
// razão concreta para NÃO mover `organizar` inteira para lá.
//
// A solução adotada (ver `inst/www/geometria.js`, função `notasComFrame`):
// só a metade PURA da regra da nota — "dado o retângulo original de cada nó
// e a posição nova de cada frame que o dagre moveu, quanto cada nota anda" —
// foi extraída para `geometria.js`, que não precisa de dagre nenhum pra isso
// (ela roda DEPOIS que o dagre já decidiu tudo). `organizar`, em `frames.js`,
// só chama essa função no fim: `out.notes = notasComFrame(nodes, rect,
// out.frames)`. É a mesma lógica que valeria dentro de `organizar`, só que
// testável sob Node sem precisar do dagre de verdade.
//
// Os testes abaixo cobrem os três cenários obrigatórios do plano, mas na
// função pura `notasComFrame` (testada em detalhe também em
// `geometria.test.mjs`), simulando com `framesNovos` escrito à mão o que um
// dagre real teria decidido — exatamente o que `organizar` passaria pra ela.
// Os testes 1 e 2 (delta exato e nota solta) são exercícios completos da
// regra de negócio; o teste 3 (nota não entra no dagre) é uma checagem
// ESTRUTURAL do código de `organizar`, por leitura de fonte: como nenhum
// teste sob Node consegue EXECUTAR `organizar` de verdade neste repo, a
// garantia de que notas nunca viram nó do dagre não pode vir de rodar a
// função duas vezes (com e sem a nota) e comparar `cards`/`frames` — só pode
// vir de garantir, por inspeção do texto fonte, que nenhuma das listas que
// alimentam `dagrePos` em `organizar` inclui nó do tipo "trNota". Isso já era
// verdade ANTES desta task (nenhuma delas nunca filtrou por "trNota") e
// continua sendo depois: esta task só ACRESCENTOU `out.notes` no fim, sem
// tocar em nenhuma dessas listas.
import { test } from "node:test";
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { rectOf, notasComFrame } from "../../inst/www/geometria.js";

const frame = (id, x, y, w, hh) => ({ id, type: "trFrame", position: { x, y }, width: w, height: hh, data: {} });
const card = (id, x, y) => ({ id, type: "ndNode", position: { x, y }, measured: { width: 240, height: 190 }, data: {} });
const nota = (id, x, y, w, hh) => ({ id, type: "trNota", position: { x, y }, width: w, height: hh, data: {} });

test("frames.js não importa sob node --test (confere a premissa da decisão acima)", async () => {
  // Import ESM de verdade: se um dia alguém tornar `frames.js` importável sob
  // Node (por exemplo trocando o especificador de dagre por um caminho
  // relativo ao vendorizado), este teste passa a falhar — sinal de que a
  // decisão acima merece revisão, e `organizar` poderia então ser testada
  // direto, como o plano original pedia.
  await assert.rejects(
    () => import("../../inst/www/frames.js"),
    (e) => e.code === "ERR_MODULE_NOT_FOUND",
  );
});

test("Organizar: frame com dois cards e uma nota — a nota anda o delta EXATO do frame (dagre simulado)", () => {
  // Cenário: um frame com dois cards (o dagre real os arruma por dentro e o
  // frame cresce/anda) e uma nota dentro dele. `framesNovos` é o que
  // `organizar` teria em `out.frames[frame.id]` depois do dagre — aqui
  // escrito à mão, no lugar de rodar o dagre de verdade (indisponível sob
  // Node neste repo, ver decisão acima).
  const f = frame("F", 0, 0, 1200, 900);
  const c1 = card("c1", 100, 100);
  const c2 = card("c2", 800, 600);
  const n = nota("nota", 200, 700, 300, 150);
  const nodes = [f, c1, c2, n];
  const rect = Object.fromEntries(nodes.map((x) => [x.id, rectOf(x)]));
  // O dagre real arruma c1/c2 por dentro (não testado aqui, é a parte com
  // dagre) e o frame anda: simulamos o frame tendo andado (300, -150).
  const framesNovos = { F: { x: 300, y: -150, w: 1200, h: 900 } };
  const out = notasComFrame(nodes, rect, framesNovos);
  const deltaFrameX = framesNovos.F.x - rect.F.x, deltaFrameY = framesNovos.F.y - rect.F.y;
  assert.equal(out.nota.x - n.position.x, deltaFrameX);
  assert.equal(out.nota.y - n.position.y, deltaFrameY);
  // Nem c1 nem c2 entram no resultado de `notasComFrame`: ela só sabe de nota.
  assert.deepEqual(Object.keys(out), ["nota"]);
});

test("Organizar: nota solta fora de qualquer frame não é devolvida em out.notes (fica onde está)", () => {
  // Escolha documentada (o plano pede pra escolher uma das duas e testar
  // exatamente essa): nota solta NÃO aparece em `out.notes`. É o mesmo
  // contrato de `out.cards`/`out.frames`: só entra quem mudou de posição, e
  // "sem chave" é o sinal de "sem mudança nenhuma" que `organizarTudo`
  // (editor.js) usa pra não emitir op.
  const f = frame("F", 0, 0, 1000, 1000);
  const solta = nota("solta", 5000, 5000, 100, 60);
  const nodes = [f, solta];
  const rect = Object.fromEntries(nodes.map((x) => [x.id, rectOf(x)]));
  const out = notasComFrame(nodes, rect, { F: { x: 400, y: 400, w: 1000, h: 1000 } });
  assert.deepEqual(out, {});
});

test("Organizar: a nota não entra no dagre (checagem estrutural do código-fonte de frames.js)", () => {
  const caminho = fileURLToPath(new URL("../../inst/www/frames.js", import.meta.url));
  const src = readFileSync(caminho, "utf8");
  const inicio = src.indexOf("export function organizar(");
  const fim = src.indexOf("\n}\n", inicio); // fecha no `return out;\n}` da função
  const organizarSrc = src.slice(inicio, fim);
  // `cards`, entrada de todo dagre interno de bloco, é só "ndNode".
  assert.match(organizarSrc, /const cards = nodes\.filter\(\(n\) => n\.type === "ndNode"\);/);
  // Nenhuma linha de `organizar` filtra ou referencia "trNota" — a nota
  // literalmente não é mencionada em lugar nenhum da função além da chamada
  // final a `notasComFrame`, que só ela sabe o que é nota.
  const semAChamadaFinal = organizarSrc.slice(0, organizarSrc.indexOf("notasComFrame"));
  assert.ok(!semAChamadaFinal.includes("trNota"),
    "nenhuma etapa de organizar (dagre interno, prancheta, dagre externo) deve mencionar trNota antes de notasComFrame");
});
