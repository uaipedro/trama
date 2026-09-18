// Sintetiza o kit de som do vídeo em WAV 16-bit mono, 44.1kHz. Sem download e
// sem dependência: o resultado é determinístico, então o vídeo renderiza igual
// em qualquer máquina que tenha só o Node.
import { writeFileSync, mkdirSync } from "node:fs";

const SR = 44100;
const SAIDA = "public/sfx";

function wav(amostras) {
  const n = amostras.length;
  const buf = Buffer.alloc(44 + n * 2);
  buf.write("RIFF", 0); buf.writeUInt32LE(36 + n * 2, 4); buf.write("WAVE", 8);
  buf.write("fmt ", 12); buf.writeUInt32LE(16, 16); buf.writeUInt16LE(1, 20);
  buf.writeUInt16LE(1, 22); buf.writeUInt32LE(SR, 24);
  buf.writeUInt32LE(SR * 2, 28); buf.writeUInt16LE(2, 32); buf.writeUInt16LE(16, 34);
  buf.write("data", 36); buf.writeUInt32LE(n * 2, 40);
  for (let i = 0; i < n; i++) {
    const v = Math.max(-1, Math.min(1, amostras[i]));
    buf.writeInt16LE(Math.round(v * 32000), 44 + i * 2);
  }
  return buf;
}
const dur = (s) => new Float32Array(Math.round(s * SR));
// Ruído com semente fixa: `Math.random()` daria um arquivo diferente por rodada,
// e "o mesmo código gera o mesmo vídeo" vale mais que a variação.
let semente = 12345;
const ruido = () => {
  semente = (semente * 1103515245 + 12345) & 0x7fffffff;
  return (semente / 0x3fffffff) - 1;
};
// Passa-baixa de um polo: transforma ruído branco em ar, que é o que faz um
// whoosh soar como movimento e não como estática.
function passaBaixa(x, corte) {
  const a = Math.exp(-2 * Math.PI * corte / SR);
  let y = 0;
  return x.map((v) => (y = v * (1 - a) + y * a));
}

// whoosh — ruído com passa-baixa varrendo pra cima e envelope em sino.
function whoosh(segundos = 0.42) {
  const x = dur(segundos);
  for (let i = 0; i < x.length; i++) x[i] = ruido();
  const y = dur(segundos);
  let estado = 0;
  for (let i = 0; i < x.length; i++) {
    const t = i / x.length;
    const corte = 240 + 3600 * t * t;
    const a = Math.exp(-2 * Math.PI * corte / SR);
    estado = x[i] * (1 - a) + estado * a;
    const env = Math.sin(Math.PI * t) ** 1.6;
    y[i] = estado * env * 2.2;
  }
  return y;
}
// pop — seno que despenca de frequência: é o som de uma coisa ASSENTANDO.
function pop(f0 = 880, f1 = 180, segundos = 0.14) {
  const x = dur(segundos);
  let fase = 0;
  for (let i = 0; i < x.length; i++) {
    const t = i / x.length;
    const f = f0 * Math.pow(f1 / f0, t);
    fase += 2 * Math.PI * f / SR;
    x[i] = Math.sin(fase) * Math.exp(-5 * t) * 0.55;
  }
  return x;
}
// thump — o corte de cena. Fundamental baixa com um estalo curto em cima, pra
// ter ataque em caixa de som pequena que não reproduz 55Hz.
function thump(segundos = 0.45) {
  const x = dur(segundos);
  let fase = 0;
  for (let i = 0; i < x.length; i++) {
    const t = i / x.length;
    const f = 110 * Math.pow(0.42, t);
    fase += 2 * Math.PI * f / SR;
    const estalo = i < SR * 0.012 ? ruido() * 0.3 * (1 - i / (SR * 0.012)) : 0;
    x[i] = (Math.sin(fase) * Math.exp(-6 * t) * 0.85) + estalo;
  }
  return x;
}
// tick — clique de digitação/parâmetro. Curtíssimo, senão vira metralhadora.
function tick() {
  const x = dur(0.035);
  for (let i = 0; i < x.length; i++) {
    const t = i / x.length;
    x[i] = (ruido() * 0.5 + Math.sin(2 * Math.PI * 2100 * i / SR) * 0.5)
           * Math.exp(-28 * t) * 0.32;
  }
  return x;
}
// riser — entra no corte. Ruído filtrado subindo, com ganho crescente.
function riser(segundos = 0.8) {
  const x = dur(segundos);
  let estado = 0;
  for (let i = 0; i < x.length; i++) {
    const t = i / x.length;
    const corte = 300 + 5200 * t ** 2.2;
    const a = Math.exp(-2 * Math.PI * corte / SR);
    estado = ruido() * (1 - a) + estado * a;
    x[i] = estado * (t ** 1.8) * 2.6;
  }
  return x;
}
// pad — a cama. Senos desafinados em quinta, batimento lento, e fade nas duas
// pontas pra emendar em laço sem clique.
function pad(segundos = 10) {
  const x = dur(segundos);
  const base = 73.42; // Ré2 — grave o bastante pra não brigar com nada em cima.
  const vozes = [1, 1.002, 1.5, 1.4985, 2, 3.002];
  for (let i = 0; i < x.length; i++) {
    const t = i / SR;
    let v = 0;
    for (let k = 0; k < vozes.length; k++) {
      v += Math.sin(2 * Math.PI * base * vozes[k] * t) / (k + 1.6);
    }
    const respiro = 0.72 + 0.28 * Math.sin(2 * Math.PI * t / 7.5);
    const borda = Math.min(1, t / 1.2, (segundos - t) / 1.2);
    x[i] = v * 0.16 * respiro * borda;
  }
  return x;
}

mkdirSync(SAIDA, { recursive: true });
const kit = { whoosh: whoosh(), pop: pop(), "pop-alto": pop(1180, 260, 0.12),
              thump: thump(), tick: tick(), riser: riser(), pad: pad() };
for (const [nome, amostras] of Object.entries(kit)) {
  writeFileSync(`${SAIDA}/${nome}.wav`, wav(amostras));
  console.log(`${nome}.wav  ${(amostras.length / SR).toFixed(2)}s`);
}
