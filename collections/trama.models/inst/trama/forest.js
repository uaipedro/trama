// Escala do mini-forest do card `models/fit` (lógica pura, sem DOM):
// testada em tests/js/forest-models.test.mjs.
//
// Recebe os pontos `{termo, est, li, ls}` e a largura útil `W`; devolve a
// posição x de cada um e do zero. O domínio sempre inclui o zero (a linha de
// referência é o que se lê: o IC cruza ou não), com 5% de folga dos lados.
export function escalaForest(pontos, W) {
  const ps = (pontos || []).filter((p) => [p.est, p.li, p.ls].every(Number.isFinite));
  if (!ps.length) return null;
  let lo = Math.min(0, ...ps.map((p) => p.li));
  let hi = Math.max(0, ...ps.map((p) => p.ls));
  if (lo === hi) { lo -= 1; hi += 1; }
  const folga = (hi - lo) * 0.05;
  lo -= folga; hi += folga;
  const x = (v) => ((v - lo) / (hi - lo)) * W;
  return {
    zero: x(0),
    pontos: ps.map((p) => ({ ...p, x: x(p.est), xli: x(p.li), xls: x(p.ls),
                             cruza: p.li <= 0 && p.ls >= 0 })),
  };
}
