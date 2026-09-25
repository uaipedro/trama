// Navegação por teclado do popover "próximo bloco". Separada de `proximo.js`
// porque aquele importa React e os testes em Node não carregam React.
//
// `i` é o índice em foco (-1 = nenhum), `n` o total de itens em ordem de
// leitura. Direita/Esquerda andam 1; Baixo/Cima andam `colunas` (1 quando não
// se sabe quantas cabem na linha). Tudo dá a volta nas pontas.
export function moverFoco(i, n, tecla, colunas = 1) {
  if (n <= 0) return -1;
  const passo = { ArrowRight: 1, ArrowLeft: -1,
                  ArrowDown: Math.max(1, colunas), ArrowUp: -Math.max(1, colunas) }[tecla];
  if (passo === undefined) return i;
  if (i < 0 || i >= n) return passo > 0 ? 0 : n - 1;
  return (((i + passo) % n) + n) % n;
}

// Altura típica do card recém-criado, pelo modo com que ele entra: ainda não
// foi medido, e supor baixo demais faz o bloco nascer por cima do vizinho.
export const alturaNova = (modo) => (modo === "completo" ? 360 : modo === "mini" ? 80 : 220);

// Primeiro `y` livre para o retângulo `q`, descendo a partir de `q.y`: a cada
// card que bate (retângulos com `margem` de folga), o candidato vai para logo
// abaixo dele. Nunca sobe e nunca pula um vão que caiba.
export function primeiroVao(caixas, q, margem = 30) {
  let y = q.y;
  const bate = (c) => q.x < c.x + c.w + margem && c.x < q.x + q.w + margem &&
    y < c.y + c.h + margem && c.y < y + q.h + margem;
  for (let i = 0, c; i < 200 && (c = caixas.find(bate)); i++) y = c.y + c.h + margem;
  return y;
}

// Vão mais perto de `q.y`: procura descendo e subindo (mesma regra de
// `primeiroVao`) e fica com o menor deslocamento; empate desce. Sem vão em
// nenhum sentido (200 passos), volta ao `q.y` original.
export function vaoPerto(caixas, q, margem = 30) {
  const bate = (y) => (c) => q.x < c.x + c.w + margem && c.x < q.x + q.w + margem &&
    y < c.y + c.h + margem && c.y < y + q.h + margem;
  const anda = (passo) => {
    let y = q.y;
    for (let i = 0, c; i < 200; i++) {
      if (!(c = caixas.find(bate(y)))) return y;
      y = passo(c);
    }
    return null;
  };
  const desce = anda((c) => c.y + c.h + margem);
  const sobe = anda((c) => c.y - q.h - margem);
  if (desce == null && sobe == null) return q.y;
  if (sobe == null) return desce;
  if (desce == null) return sobe;
  return Math.abs(sobe - q.y) < Math.abs(desce - q.y) ? sobe : desce;
}
