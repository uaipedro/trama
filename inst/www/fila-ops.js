// inst/www/fila-ops.js — de que revisão parte a próxima op.
//
// O servidor aplica as ops na ordem em que chegam e cada op aplicada sobe a
// revisão em exatamente 1 (`.tr_apply`, R/document.R). Antes, o front mandava
// toda op com a última revisão CONFIRMADA: duas ops seguidas antes do eco
// (trocar um Segmented e logo um Toggle; soltar um card logo após um
// set_param) saíam com a mesma `base_rev`, e a segunda voltava recusada como
// `stale_rev` — ressincronização e banner por um gesto legítimo. Com o
// executor ocupado o R demora a ler o input e essa janela cresce.
//
// A revisão de partida é a confirmada mais as ops ainda em voo: cada uma
// delas, se aplicada, sobe um. Se uma for recusada, as de trás (que contavam
// com ela) também serão — o servidor diz isso explicitamente, uma a uma — e a
// contagem recomeça da revisão que a recusa informa. Nada fica preso: um eco
// perdido infla a conta só até a próxima recusa.
//
// Puro, sem React nem Shiny, pra rodar sob `node --test`.

export function criarFilaOps(rev = 0) {
  let confirmada = rev;
  let emVoo = [];
  return {
    // Revisão com que a próxima op deve sair.
    base: () => confirmada + emVoo.length,
    // A op de `seq` acabou de sair (com `base()` lido antes).
    enviada(seq) { if (seq != null) emVoo.push(seq); },
    // Eco `op_applied`. Também chega para ops que não são deste cliente
    // (agente, template inserido pelo servidor): só a revisão anda.
    aplicada(seq, novaRev) {
      emVoo = emVoo.filter((s) => s !== seq);
      if (novaRev != null) confirmada = novaRev;
    },
    // Eco `op_rejected`: o que estava em voo foi pensado para um documento
    // que não existe; as recusas dessas ops ainda vão chegar, e são inócuas.
    recusada(seq, novaRev) {
      emVoo = [];
      if (novaRev != null) confirmada = novaRev;
    },
    // Documento inteiro (eco estrutural, undo, troca de projeto). Não esvazia
    // o voo: o documento de uma op estrutural chega logo depois do próprio
    // `op_applied`, com ops seguintes possivelmente ainda a caminho.
    documento(novaRev) { confirmada = novaRev || 0; },
    emVoo: () => emVoo.length,
  };
}
