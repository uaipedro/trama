/**
 * A lhama de óculos do trama em ASCII puro (sem Unicode, pra não virar lixo
 * no console do Windows). Aparece no `trama install`: o postinstall do npm
 * teria a saída engolida (npm >= 7 esconde a saída de scripts de instalação).
 */
export const MASCOTE = String.raw`
        /\    /\
       /  \__/  \
       |  ,  ,  |
     .-(o)----(o)-.
       |  .--.  |
        \ '--' /
         \ \/ /
          |  |
       ___|  |___
       \  \  /  /
        \  \/  /
         '.  .'
          |  |
          |  |    t r a m a
`;

/** Imprime o mascote em verde-azulado quando o terminal suporta cor. */
export function printMascote(out: NodeJS.WriteStream = process.stdout): void {
  const cor = out.isTTY && !process.env.NO_COLOR;
  out.write(cor ? `\x1b[36m${MASCOTE}\x1b[0m\n` : `${MASCOTE}\n`);
}
