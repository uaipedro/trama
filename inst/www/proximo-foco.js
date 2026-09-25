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
