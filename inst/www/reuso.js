// inst/www/reuso.js — devolver o MESMO objeto quando nada mudou.
//
// O editor redecora todos os nós a cada `tick` (um por rajada de eventos de
// execução): cada card ganha um `data` novo, e o React Flow re-renderiza todos
// eles, inclusive tabelas e gráficos das coleções, por causa de um evento que
// tocou um card só. Aqui o resultado novo é comparado com o anterior, por id,
// e o objeto antigo volta quando o conteúdo é o mesmo. Com `React.memo` no
// card, isso corta o re-render de quem não mudou.
//
// A comparação é rasa com UM nível a mais: `data` é remontado com spreads
// (`params: {...}`, `sugeridos: [...]`), então dois objetos/arrays simples com
// os mesmos itens contam como iguais. Mais fundo que isso é identidade — quem
// muta objeto no lugar (em vez de trocar) fica sem re-render, e é por isso
// que esse padrão não pode voltar ao editor.

const simples = (x) => x !== null && typeof x === "object" &&
  (Array.isArray(x) || Object.getPrototypeOf(x) === Object.prototype);

function raso(a, b) {
  if (a === b) return true;
  if (!simples(a) || !simples(b) || Array.isArray(a) !== Array.isArray(b)) return false;
  const ka = Object.keys(a), kb = Object.keys(b);
  if (ka.length !== kb.length) return false;
  return ka.every((k) => Object.prototype.hasOwnProperty.call(b, k) && a[k] === b[k]);
}

export function dadosIguais(a, b) {
  if (a === b) return true;
  if (!a || !b) return false;
  const ka = Object.keys(a), kb = Object.keys(b);
  if (ka.length !== kb.length) return false;
  return ka.every((k) => Object.prototype.hasOwnProperty.call(b, k) && raso(a[k], b[k]));
}

// `anteriores`: Map id → nó devolvido na rodada passada. Devolve os nós (com
// os objetos antigos onde deu) e o Map para a próxima rodada.
export function reusarNos(anteriores, novos) {
  const mapa = new Map();
  const nos = novos.map((n) => {
    const p = anteriores && anteriores.get(n.id);
    let out = n;
    if (p && dadosIguais(p.data, n.data)) {
      const ks = Object.keys(n);
      const resto = ks.length === Object.keys(p).length &&
        ks.every((k) => k === "data" || p[k] === n[k]);
      out = resto ? p : { ...n, data: p.data };
    }
    mapa.set(n.id, out);
    return out;
  });
  return { nos, mapa };
}
