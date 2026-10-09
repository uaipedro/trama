// Galeria: lógica pura (sem React, sem DOM), testada em Node. A interface
// fica em `galeria-ui.js`. A galeria é um rolo com as imagens da análise;
// cada item é um card gerador (`node`) cuja saída é uma imagem.

// Pertencimento na galeria, como o cliente o vê. A marca explícita do card
// (`tr_galeria` no nó: TRUE/FALSE) vence. Sem marca, vale o padrão: o card
// entra se `imagensPadrao` (a opção da coleção/projeto) e o renderer for
// a imagem.
// `visual`: o bloco declara `galeria = TRUE` no catálogo (blocos de
// visualização). Só eles entram sem marca; o resto é opt-in.
export function naGaleria(marca, renderer, imagensPadrao, visual = true) {
  if (marca === true || marca === false) return marca;
  return !!imagensPadrao && !!visual && renderer === "trama/image";
}

// Op de alternar o botão "galeria" de um card. O cliente pede o oposto do
// que vê (ou `quer`, quando o botão é explícito). Se o pedido coincide com o
// padrão, a op manda `valor: null`: a marca some e o card volta a seguir a
// regra. Caso contrário, grava true/false.
export function opAlternarGaleria({ node, marca, renderer, imagensPadrao, visual = true, quer }) {
  const padrao = naGaleria(undefined, renderer, imagensPadrao, visual);
  const desejado = typeof quer === "boolean" ? quer : !naGaleria(marca, renderer, imagensPadrao, visual);
  return { op: "set_galeria", node, valor: desejado === padrao ? null : desejado };
}

// Índice circular no lightbox: passa de último para primeiro e vice-versa.
// Sem itens devolve 0 (quem chama não deve renderizar nada nesse caso).
export function navegar(indice, total, passo) {
  if (!(total > 0)) return 0;
  return (((indice + passo) % total) + total) % total;
}

// Cache-busting: a versão do nó entra na URL, então um re-render do card
// troca a imagem sem o navegador reaproveitar a antiga. Se a URL já tem
// query, usa `&`.
export function urlComVersao(url, versao) {
  if (url == null || url === "") return "";
  if (versao == null || versao === "") return url;
  const sep = String(url).includes("?") ? "&" : "?";
  return `${url}${sep}v=${encodeURIComponent(String(versao))}`;
}

export const DICA_GALERIA_VAZIA = "Marque um card com ☆ para trazê-lo à galeria";
