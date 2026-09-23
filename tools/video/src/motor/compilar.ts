// src/motor/compilar.ts — do roteiro à linha do tempo.
//
// Um passe só, em ordem: cada plano ganha um início e uma duração, e deixa
// marcas nos nós (entra, trocas, rodadas, destaques), na câmera, nas legendas
// e na trilha. O que sai daqui é o mesmo `NoFluxo`/`ArestaFluxo` que os vídeos
// escritos à mão usam — o canvas não sabe que houve roteiro.
//
// As regras de TEMPO e ENQUADRAMENTO moram aqui, e só aqui. É o que impede um
// roteiro de errar ritmo ou tirar o foco do centro: ele não tem como dizer
// "quadro 212" nem "câmera em x=830".
import { geometriaDoModo } from "../trama/estado";
import type { Modo } from "../trama/modos-app.js";
import { MODO_PADRAO } from "../trama/modos-app.js";
import type { ArestaFluxo, NoFluxo } from "../trama/tipos";
import type { Plano, Roteiro } from "./roteiro";

// --- O quadro -----------------------------------------------------------------
// 4:3, com a zona segura no MEIO: a faixa 9:16 que sobrevive ao recorte
// vertical (1080 de altura × 1080·9/16 = 607,5 de largura). Tudo o que importa
// — o card em foco, a legenda — fica dentro dela; o resto é contexto.
export const QUADRO = { w: 1440, h: 1080, fps: 30 };
export const SEGURA = { w: 608, h: 1080, x: (1440 - 608) / 2 };

export type Camera = { cx: number; cy: number; s: number };
export type Marca = { q: number; cam: Camera };
export type LegendaMarcada = { texto: string; de: number; ate: number; destacar: string[] };
export type Som = { arquivo: string; em: number; volume: number };

export type Filme = {
  id: string;
  duracao: number;
  nos: NoFluxo[];
  arestas: ArestaFluxo[];
  cameras: Marca[];
  // Card selecionado em cada trecho (é ele que tem borda, brilho, e cujo
  // painel de parâmetros abre quando o modo esconde os parâmetros).
  selecao: { q: number; id: string | null }[];
  legendas: LegendaMarcada[];
  sons: Som[];
  stills: { nome: string; q: number }[];
};

// Durações padrão, em quadros. Ajustadas vendo o render, não na teoria.
const DUR: Record<Plano["faz"], number> = {
  entra: 54,
  liga: 40,
  param: 0, // calculada: depende do tamanho do valor
  modo: 48,
  foco: 45,
  geral: 66,
  espera: 30,
  still: 0,
};
// A câmera anda primeiro e a coisa acontece depois que ela chega perto.
export const DESLOCA = 22;
const CHEGA = 14;
const RODA = 30;
const RODA_DE_NOVO = 24;
const POR_CHAR = 1.15;

// Card em foco: cabe na zona segura com folga, e acima da faixa da legenda.
function enquadraUm(x: number, y: number, w: number, h: number): Camera {
  const s = Math.min(2.3, (SEGURA.w - 90) / w, (QUADRO.h - 330) / h);
  // O card sobe um pouco acima do centro pra legenda ter onde pousar.
  return { cx: x + w / 2, cy: y + h / 2 + 70 / s, s };
}

// `seguro`: o grupo inteiro cabe na zona segura (a ligação entre dois cards é
// o assunto, e tem que sobreviver ao recorte vertical). Sem ele, usa o quadro
// todo — é o plano geral, que no vertical perde as pontas.
function enquadraVarios(r: { x1: number; y1: number; x2: number; y2: number }, seguro = false): Camera {
  const w = r.x2 - r.x1, h = r.y2 - r.y1;
  const largura = seguro ? SEGURA.w - 60 : QUADRO.w - 180;
  const s = Math.min(2, largura / w, (QUADRO.h - 330) / h);
  return { cx: (r.x1 + r.x2) / 2, cy: (r.y1 + r.y2) / 2 + 70 / s, s };
}

// Tempo de leitura: 0,3s por palavra + 1s, e nunca menos que o plano.
function leitura(texto: string): number {
  return Math.round(texto.split(/\s+/).length * 9 + 30);
}

export function compilar(roteiro: Roteiro): Filme {
  const nos = new Map<string, NoFluxo>();
  const modoAtual = new Map<string, Modo>();
  const arestas: ArestaFluxo[] = [];
  const cameras: Marca[] = [];
  const selecao: Filme["selecao"] = [];
  const legendas: LegendaMarcada[] = [];
  const sons: Som[] = [];
  const stills: Filme["stills"] = [];

  const bloco = (id: string) => {
    const b = roteiro.blocos[id];
    if (!b) throw new Error(`[roteiro ${roteiro.id}] bloco desconhecido: "${id}"`);
    return b;
  };
  const no = (id: string) => {
    const n = nos.get(id);
    if (!n) throw new Error(`[roteiro ${roteiro.id}] "${id}" usado antes de entrar`);
    return n;
  };
  const retDe = (id: string) => {
    const n = nos.get(id)!;
    const g = geometriaDoModo(n, modoAtual.get(id) ?? MODO_PADRAO);
    return { x1: n.x, y1: n.y, x2: n.x + g.w, y2: n.y + g.h };
  };
  const uniao = (ids: string[]) =>
    ids.map(retDe).reduce((a, b) => ({
      x1: Math.min(a.x1, b.x1), y1: Math.min(a.y1, b.y1),
      x2: Math.max(a.x2, b.x2), y2: Math.max(a.y2, b.y2),
    }));

  // Move a câmera se o alvo é outro; devolve se houve deslocamento (quem
  // chama espera ela chegar).
  const camera = (q: number, cam: Camera): boolean => {
    const ult = cameras[cameras.length - 1]?.cam;
    const longe = !ult ||
      Math.hypot((ult.cx - cam.cx) * ult.s, (ult.cy - cam.cy) * ult.s) > 30 ||
      Math.abs(ult.s - cam.s) / ult.s > 0.08;
    if (!longe) return false;
    cameras.push({ q, cam });
    if (ult) sons.push({ arquivo: "whoosh", em: q + 4, volume: 0.3 });
    return !!ult;
  };
  const focaNo = (q: number, id: string) => {
    const r = retDe(id);
    return camera(q, enquadraUm(r.x1, r.y1, r.x2 - r.x1, r.y2 - r.y1));
  };
  const seleciona = (q: number, id: string | null) => {
    if (selecao[selecao.length - 1]?.id !== id) selecao.push({ q, id });
  };

  let t = 0;
  let inicioAnterior = 0;
  for (const p of roteiro.planos) {
    const q = p.junto ? inicioAnterior : t;
    let dur = p.dur ?? DUR[p.faz];

    switch (p.faz) {
      case "entra": {
        const b = bloco(p.bloco);
        const n: NoFluxo = {
          id: p.bloco, spec: b.spec, x: b.x, y: b.y, rotulo: b.rotulo, resultado: b.resultado,
          tamanho: b.tamanho, vista: b.vista, params: b.params, duracao: b.duracao,
          modo: b.modo, entra: 0, resulta: 0, modos: [], trocas: [], rodadas: [], destaques: [],
        };
        nos.set(p.bloco, n);
        modoAtual.set(p.bloco, b.modo ?? MODO_PADRAO);
        const moveu = focaNo(q, p.bloco);
        n.entra = q + (moveu ? CHEGA : 0);
        n.resulta = n.entra + RODA;
        seleciona(n.entra, p.bloco);
        sons.push({ arquivo: "pop", em: n.entra, volume: 0.46 });
        sons.push({ arquivo: "pop-alto", em: n.resulta, volume: 0.28 });
        break;
      }
      case "liga": {
        no(p.de); no(p.para);
        const moveu = camera(q, enquadraVarios(uniao([p.de, p.para]), true));
        arestas.push({ de: p.de, paraNo: p.para, paraPorta: p.porta ?? 0, desenha: q + (moveu ? CHEGA : 4) });
        seleciona(q, p.para);
        break;
      }
      case "param": {
        const n = no(p.bloco);
        const param = n.spec.params.find((x) => x.nome === p.param);
        if (!param) throw new Error(`[roteiro ${roteiro.id}] "${p.bloco}" não tem o param "${p.param}"`);
        const moveu = focaNo(q, p.bloco);
        seleciona(q, p.bloco);
        const em = q + (moveu ? CHEGA : 0) + 10;
        const texto = param.tipo === "campo" || param.tipo === "expr" || param.tipo === "numero";
        const digita = texto ? Math.ceil(p.valor.length * POR_CHAR) : 0;
        n.destaques!.push({ de: em - 8, ate: em + digita + 10, alvo: "param:" + p.param });
        n.trocas!.push({ em, param: p.param, valor: p.valor });
        const de = em + digita + 6;
        n.rodadas!.push({ de, ate: de + RODA_DE_NOVO, resultado: p.resultado });
        sons.push({ arquivo: "tick", em, volume: 0.3 });
        sons.push({ arquivo: "pop-alto", em: de + RODA_DE_NOVO, volume: 0.26 });
        dur = p.dur ?? de + RODA_DE_NOVO + 20 - q;
        break;
      }
      case "modo": {
        const n = no(p.bloco);
        seleciona(q, p.bloco);
        const moveu = focaNo(q, p.bloco);
        const em = q + (moveu ? CHEGA : 0) + 16;
        n.destaques!.push({ de: em - 14, ate: em + 4, alvo: "modo" });
        n.modos!.push({ em, modo: p.modo });
        modoAtual.set(p.bloco, p.modo);
        // A câmera reenquadra o card no tamanho NOVO, junto com a troca.
        focaNo(em, p.bloco);
        sons.push({ arquivo: "tick", em, volume: 0.34 });
        break;
      }
      case "foco": {
        const ids = p.blocos ?? (p.bloco ? [p.bloco] : []);
        ids.forEach(no);
        if (ids.length === 1) { focaNo(q, ids[0]); seleciona(q, ids[0]); }
        else if (ids.length) camera(q, enquadraVarios(uniao(ids)));
        break;
      }
      case "geral": {
        const ids = [...nos.keys()];
        if (ids.length) camera(q, enquadraVarios(uniao(ids)));
        seleciona(q, null);
        break;
      }
      case "espera":
        break;
      case "still":
        stills.push({ nome: p.nome, q: Math.max(0, q - 1) });
        break;
    }

    if (p.legenda) {
      const ate = q + Math.max(dur, leitura(p.legenda));
      legendas.push({ texto: p.legenda, de: q + 6, ate, destacar: p.destacar ?? [] });
      dur = Math.max(dur, ate - q);
    }
    inicioAnterior = q;
    t = Math.max(t, q + dur);
  }

  // Fim: meio segundo de respiro depois do último plano.
  const duracao = Math.max(1, t + 15);
  if (!cameras.length) cameras.push({ q: 0, cam: { cx: 0, cy: 0, s: 1 } });
  return { id: roteiro.id, duracao, nos: [...nos.values()], arestas, cameras, selecao, legendas, sons, stills };
}
