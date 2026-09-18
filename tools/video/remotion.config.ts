import { Config } from "@remotion/cli/config";

Config.setVideoImageFormat("jpeg");
// O vídeo empilha blur, mix-blend e sombras; sem isto o Chromium headless cai
// no rasterizador de software e o render fica ordens de grandeza mais lento.
Config.setChromiumOpenGlRenderer("angle");
Config.setOverwriteOutput(true);
