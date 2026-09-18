#!/bin/sh
# Normaliza a loudness do vídeo renderizado e copia o vídeo sem recodificar.
#
# POR QUE ISTO NÃO SE RESOLVE NOS GANHOS DA COMPOSIÇÃO: a trilha é esparsa —
# uma cama baixa e golpes curtos. A loudness integrada da EBU R128 é GATEADA:
# ela mede os trechos altos e descarta o que está muito abaixo deles. Subir a
# cama 7 dB, portanto, quase não move o integrado (medido: −21,9 → −22,1 LUFS),
# e subir os golpes junto estoura o pico, que já estava a 2,8 dB do teto. Quem
# resolve isso é um limitador no fim da cadeia, e não um ganho no começo.
#
# Duas passagens porque `loudnorm` numa só opera em modo dinâmico e erra o alvo
# em material com faixa dinâmica larga: a primeira MEDE, a segunda corrige com
# os números medidos.
set -e
cd "$(dirname "$0")/.."

ENTRADA="${1:-out/bruto.mp4}"
SAIDA="${2:-out/trama-demo.mp4}"
# −16 LUFS com teto de pico real em −1 dBTP: é o que as plataformas de vídeo
# esperam receber. Elas normalizam de novo do lado delas, e entregar mais alto
# que isso só garante que a redução aconteça lá, sem controle nenhum daqui.
ALVO_I=-16
ALVO_TP=-1.0
ALVO_LRA=11

[ -f "$ENTRADA" ] || { echo "não achei $ENTRADA — rode o render antes." >&2; exit 1; }

echo "medindo $ENTRADA…"
MEDIDA=$(ffmpeg -hide_banner -nostats -i "$ENTRADA" -map 0:a \
  -af "loudnorm=I=$ALVO_I:TP=$ALVO_TP:LRA=$ALVO_LRA:print_format=json" \
  -f null /dev/null 2>&1 | sed -n '/^{/,/^}/p')

campo() { echo "$MEDIDA" | sed -n "s/.*\"$1\" *: *\"\{0,1\}\([^\",]*\)\"\{0,1\}.*/\1/p"; }
I=$(campo input_i); TP=$(campo input_tp); LRA=$(campo input_lra)
LIMIAR=$(campo input_thresh); OFFSET=$(campo target_offset)

[ -n "$I" ] || { echo "a medição falhou; saída do ffmpeg não trouxe o JSON." >&2; exit 1; }
echo "entrada: I=$I LUFS  TP=$TP dBFS  LRA=$LRA LU"

ffmpeg -hide_banner -loglevel error -y -i "$ENTRADA" \
  -map 0:v -map 0:a -c:v copy \
  -af "loudnorm=I=$ALVO_I:TP=$ALVO_TP:LRA=$ALVO_LRA:measured_I=$I:measured_TP=$TP:measured_LRA=$LRA:measured_thresh=$LIMIAR:offset=$OFFSET:linear=true,alimiter=limit=0.891:level=false" \
  -c:a aac -b:a 192k "$SAIDA"

echo "gravado $SAIDA:"
ffmpeg -hide_banner -nostats -i "$SAIDA" -map 0:a -af ebur128=peak=true -f null /dev/null 2>&1 |
  grep -E '    I: |    Peak: |    LRA: '
