#!/bin/sh
# Masteriza um loop da home do site pro peso que a página pede: h264, sem
# áudio, `faststart` (o navegador começa a tocar antes do arquivo inteiro
# chegar) e até 1,5 MB — a home carrega os dois de cara, então cada byte a mais
# aqui é tempo de carregamento pra quem abre a página.
#
# POR QUE UM LAÇO DE CRF: `-fs` limita o tamanho cortando o arquivo no meio (um
# vídeo truncado, não mais leve), e duas passagens com bitrate-alvo preciso
# pedem medir a complexidade da cena primeiro. Mais simples e só um pouco menos
# preciso: sobe o CRF até o arquivo caber, e para no primeiro que cabe — cada
# +1 de CRF perde pouca qualidade perceptível num vídeo de interface (poucas
# cores, muita área parada).
set -e
cd "$(dirname "$0")/.."

BRUTO="$1"
SAIDA_MP4="$2"
SAIDA_JPG="$3"
# Instante do quadro do pôster, em segundos.
QUADRO_POSTER="${4:-0}"
LIMITE_BYTES=$((1500 * 1024))

[ -f "$BRUTO" ] || { echo "não achei $BRUTO — rode o render antes." >&2; exit 1; }

echo "codificando $SAIDA_MP4 a partir de $BRUTO…"
CRF=20
TMP="$(mktemp).mp4"
while [ "$CRF" -le 34 ]; do
  ffmpeg -hide_banner -loglevel error -y -i "$BRUTO" \
    -an -c:v libx264 -preset slow -crf "$CRF" -pix_fmt yuv420p \
    -movflags +faststart "$TMP"
  TAM=$(stat -c%s "$TMP")
  echo "  crf=$CRF -> $((TAM / 1024)) KiB"
  if [ "$TAM" -le "$LIMITE_BYTES" ]; then
    break
  fi
  CRF=$((CRF + 2))
done

if [ "$TAM" -gt "$LIMITE_BYTES" ]; then
  echo "aviso: $SAIDA_MP4 ficou com $((TAM / 1024)) KiB mesmo em crf=$CRF." >&2
fi

mv "$TMP" "$SAIDA_MP4"
echo "gravado $SAIDA_MP4 ($((TAM / 1024)) KiB, crf=$CRF)"

ffmpeg -hide_banner -loglevel error -y -ss "$QUADRO_POSTER" -i "$BRUTO" \
  -frames:v 1 -q:v 5 "$SAIDA_JPG"
echo "gravado $SAIDA_JPG"
