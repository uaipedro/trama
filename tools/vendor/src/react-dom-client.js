// Import ESTÁTICO só pela ORDEM: garante que `react-dom.js` (marcado
// external abaixo) já rodou — e já registrou `globalThis.__tr_modules["react-dom"]`
// — antes deste módulo avaliar seu próprio corpo. Sem isto, a ordem de
// carregamento dependeria de quem MAIS importa "react-dom" em algum lugar
// do grafo (hoje é @xyflow/react, via `createPortal`) chegar primeiro — um
// acidente de ordem de `import`, não uma garantia.
import "react-dom";

// Caminho de ARQUIVO, não o especificador nu "react-dom/client": com o nu,
// `--external:react-dom` bate também em "react-dom/client" (mesmo prefixo) e
// o esbuild trata o PRÓPRIO módulo que este comando gera como externo —
// autoimportação circular, saída de 31 bytes nunca resolvida. Pelo caminho
// de arquivo, o esbuild resolve o pacote de verdade e só "react-dom" (bare,
// referenciado LÁ DENTRO) fica externo — que é o que faz este arquivo
// delegar pro MESMO `react-dom.js` do vendor, em vez de duplicar o
// reconciler inteiro (portais e sistema de eventos dependem de ser um só).
//
// `export *` a partir de caminho de arquivo não sintetiza export ESM (só
// funciona por nome de pacote) — nomear explicitamente é a API pública real
// de `react-dom/client`.
import { createRoot, hydrateRoot } from "../node_modules/react-dom/client.js";
export { createRoot, hydrateRoot };
