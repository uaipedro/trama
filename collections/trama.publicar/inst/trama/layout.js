// Regras do layout da página do `publicar`, puras (sem React): ler o param,
// o layout automático, mover, tirar e pôr blocos. O widget (index.js) só
// desenha. Teste em tests/js/publicar-layout.test.mjs.
//
// O param é texto JSON: lista de blocos `{tipo: "cabecalho"}`, `{tipo:
// "texto", texto}`, `{tipo: "item", id}` e `{tipo: "fluxo"}`. Vazio é o
// automático, que o R monta igual (`tr_publicar_layout()`): cabeçalho, cada
// item na ordem dos fios, diagrama. O `id` do item é o nome que a porta
// `named` dá à origem (o nó, ou `nó.saída`).

export function automatico(fontes) {
  return [{ tipo: "cabecalho" }, ...(fontes || []).map((f) => ({ tipo: "item", id: f.nome })), { tipo: "fluxo" }];
}

// `{ blocos, automatico, erro }`. Texto inválido não apaga nada: o widget
// mostra o erro e oferece voltar ao automático.
export function ler(valor, fontes) {
  const txt = String(valor ?? "").trim();
  if (!txt) return { blocos: automatico(fontes), automatico: true, erro: null };
  try {
    const b = JSON.parse(txt);
    if (!Array.isArray(b)) return { blocos: automatico(fontes), automatico: true, erro: "o layout não é uma lista" };
    return { blocos: b.filter((x) => x && typeof x === "object"), automatico: false, erro: null };
  } catch (e) {
    return { blocos: automatico(fontes), automatico: true, erro: `JSON inválido: ${e.message}` };
  }
}

export const gravar = (blocos) => JSON.stringify(blocos);

export function mover(blocos, i, delta) {
  const j = i + delta;
  if (i < 0 || i >= blocos.length || j < 0 || j >= blocos.length) return blocos;
  const out = blocos.slice();
  [out[i], out[j]] = [out[j], out[i]];
  return out;
}

export const tirar = (blocos, i) => blocos.filter((_, k) => k !== i);

export function por(blocos, bloco, depois = blocos.length - 1) {
  const out = blocos.slice();
  out.splice(Math.min(depois + 1, out.length), 0, bloco);
  return out;
}

export const trocar = (blocos, i, campos) => blocos.map((b, k) => (k === i ? { ...b, ...campos } : b));

// Itens ligados que o layout não mostra (vão só para o pacote) e itens do
// layout que não estão mais ligados (o R avisa e pula).
export function diferencas(blocos, fontes) {
  const ligados = new Set((fontes || []).map((f) => f.nome));
  const usados = new Set(blocos.filter((b) => b.tipo === "item").map((b) => b.id));
  return {
    fora: (fontes || []).filter((f) => !usados.has(f.nome)),
    soltos: [...usados].filter((id) => !ligados.has(id)),
  };
}

export function rotuloDoBloco(b, fontes) {
  if (b.tipo === "cabecalho") return "Cabeçalho (título, autores, citação)";
  if (b.tipo === "fluxo") return "Diagrama do fluxo";
  if (b.tipo === "texto") return "Texto";
  const f = (fontes || []).find((x) => x.nome === b.id);
  return f ? `${f.rotulo}` : `${b.id} (não ligado)`;
}
