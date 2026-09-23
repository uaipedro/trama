// src/trama/estado.ts — o estado de um card NUM QUADRO.
//
// O card é função pura de `(nó, quadro)`: nada de estado React, nada de efeito.
// É o que deixa um render quadro a quadro determinístico, e o que permite a
// aresta, a câmera e o painel de parâmetros perguntarem "onde está o card
// agora" sem conversar com ele.
import { interpolate } from "remotion";
import { tema } from "../theme";
import { geometria, type Geometria } from "./metricas";
import type { Modo } from "./modos-app.js";
import { MODO_PADRAO } from "./modos-app.js";
import { valorParam, type NoFluxo, type Param, type Resultado } from "./tipos";

// Duração da troca de modo. Curta: no app é instantânea, e o vídeo só dá tempo
// de o olho acompanhar o card mudando de tamanho.
export const TROCA_MODO = 14;

const clamp = { extrapolateLeft: "clamp", extrapolateRight: "clamp" } as const;

export function modoEm(no: NoFluxo, q: number): { de: Modo; para: Modo; t: number } {
  let atual: Modo = no.modo ?? MODO_PADRAO;
  for (const m of no.modos ?? []) {
    if (q < m.em) break;
    const t = interpolate(q, [m.em, m.em + TROCA_MODO], [0, 1], { ...clamp, easing: tema.ease.ambos });
    if (t < 1) return { de: atual, para: m.modo, t };
    atual = m.modo;
  }
  return { de: atual, para: atual, t: 1 };
}

// O modo que "vale" pra decidir o que o card é (painel aberto ou não, bolinha
// do mini): o de destino já na metade da troca.
export function modoVigente(no: NoFluxo, q: number): Modo {
  const { de, para, t } = modoEm(no, q);
  return t < 0.5 ? de : para;
}

const lerp = (a: number, b: number, t: number) => a + (b - a) * t;

export function rotuloDe(no: NoFluxo): string {
  return no.rotulo ?? no.spec.rotulo;
}

export function geometriaDoModo(no: NoFluxo, modo: Modo): Geometria {
  return geometria(no.spec, modo, rotuloDe(no), no.tamanho);
}

// Durante a troca, largura, altura e âncora das portas interpolam juntas — a
// aresta segue a porta enquanto o card cresce.
export function geometriaEm(no: NoFluxo, q: number): Geometria {
  const { de, para, t } = modoEm(no, q);
  const a = geometriaDoModo(no, de);
  if (t >= 1) return geometriaDoModo(no, para);
  const b = geometriaDoModo(no, para);
  return {
    w: lerp(a.w, b.w, t),
    h: lerp(a.h, b.h, t),
    portas: (lado, i) => lerp(a.portas(lado, i), b.portas(lado, i), t),
  };
}

// Execução corrente: a primeira (`entra` → `resulta`) ou uma das `rodadas`.
export function execucaoEm(no: NoFluxo, q: number) {
  let resultado: Resultado = no.resultado;
  let desde = no.resulta;
  if (q < no.resulta) return { rodando: true, de: no.entra, ate: no.resulta, resultado, desde };
  for (const r of no.rodadas ?? []) {
    if (q < r.de) break;
    if (q < r.ate) return { rodando: true, de: r.de, ate: r.ate, resultado, desde };
    resultado = r.resultado ?? resultado;
    desde = r.ate;
  }
  return { rodando: false, de: 0, ate: 0, resultado, desde };
}

// Valor do param no quadro: a última troca que já começou. Campo de texto sai
// caractere a caractere; enum troca de uma vez. `digitando` acende a borda.
export function valorEm(no: NoFluxo, param: Param, q: number, atrasoInicial?: number) {
  let valor = valorParam(no, param);
  let digitaDe = atrasoInicial;
  for (const t of no.trocas ?? []) {
    if (t.param !== param.nome || q < t.em) continue;
    valor = t.valor;
    digitaDe = t.em;
  }
  const texto = param.tipo === "campo" || param.tipo === "expr" || param.tipo === "numero";
  if (!texto || digitaDe === undefined) return { valor, digitando: false };
  if (q < digitaDe) return { valor: "", digitando: false };
  const n = Math.floor((q - digitaDe) / 1.15);
  return { valor: valor.slice(0, n), digitando: n < valor.length };
}

// Intensidade do anel de destaque (0–1): sobe rápido, pulsa, some.
export function destaqueEm(no: NoFluxo, alvo: string, q: number): number {
  for (const d of no.destaques ?? []) {
    if (d.alvo !== alvo || q < d.de - 4 || q > d.ate + 8) continue;
    const entra = interpolate(q, [d.de - 4, d.de + 4], [0, 1], { ...clamp, easing: tema.ease.saida });
    const sai = interpolate(q, [d.ate, d.ate + 8], [1, 0], { ...clamp, easing: tema.ease.entrada });
    const pulso = 0.75 + 0.25 * Math.sin((q - d.de) / 4);
    return entra * sai * pulso;
  }
  return 0;
}

// Anel de destaque: a cor-herói, por fora do elemento, sem mexer no layout.
export function anel(i: number): React.CSSProperties | undefined {
  if (i <= 0) return undefined;
  return {
    boxShadow: `0 0 0 ${2 + i}px ${tema.cor.destaque}, 0 0 ${18 * i}px ${tema.cor.brilho}`,
    borderRadius: 6,
    position: "relative",
    zIndex: 2,
  };
}
