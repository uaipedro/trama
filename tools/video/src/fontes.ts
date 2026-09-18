// src/fontes.ts — carrega as faces do disco do projeto, não do sistema.
//
// `delayRender` é obrigatório: sem ele o Chromium fotografa o quadro antes da
// face chegar, e o render sai com a fonte de fallback em alguns quadros e a
// certa em outros — o defeito mais difícil de pegar numa inspeção por
// amostragem, porque depende de qual quadro você extraiu.
import { continueRender, delayRender, staticFile } from "remotion";

const FACES = [
  { familia: "Inter Display", arquivo: "InterDisplay-ExtraBold.otf", peso: "800" },
  { familia: "Inter Display", arquivo: "InterDisplay-SemiBold.otf", peso: "600" },
  { familia: "Inter", arquivo: "Inter-Medium.otf", peso: "500" },
  { familia: "Inter", arquivo: "Inter-SemiBold.otf", peso: "600" },
  { familia: "Noto Sans Mono", arquivo: "NotoSansMono-Regular.ttf", peso: "400" },
  { familia: "Noto Sans Mono", arquivo: "NotoSansMono-Medium.ttf", peso: "500" },
];

let carregado = false;

export function carregarFontes() {
  if (carregado) return;
  carregado = true;
  const espera = delayRender("carregando as fontes");
  Promise.all(
    FACES.map(({ familia, arquivo, peso }) => {
      const face = new FontFace(familia, `url(${staticFile("fonts/" + arquivo)})`, {
        weight: peso,
      });
      // O elenco existe porque a `lib.dom` desta versão do TypeScript declara
      // `document.fonts` sem o `add` — que o navegador tem desde sempre.
      return face
        .load()
        .then((f) => (document.fonts as unknown as Set<FontFace>).add(f));
    }),
  )
    .then(() => continueRender(espera))
    // Falhar ALTO: fonte que não carrega em silêncio vira vídeo entregue com a
    // tipografia errada, que é exatamente o que não pode passar batido.
    .catch((e) => {
      console.error("[trama-video] fonte não carregou:", e);
      continueRender(espera);
    });
}
