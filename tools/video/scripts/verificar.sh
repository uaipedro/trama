#!/bin/sh
# Extrai uma folha de contato do vídeo PRONTO e mede o áudio.
#
# Existe porque render sem inspeção é o defeito mais comum deste tipo de
# trabalho: o código compila, o mp4 sai, e só ao assistir se descobre que uma
# legenda cobria um card ou que um campo estourou a largura. Os quadros saem do
# mp4, e não do renderizador, então a codificação também é verificada.
set -e
cd "$(dirname "$0")/.."

VIDEO="${1:-out/trama-demo.mp4}"
[ -f "$VIDEO" ] || { echo "não achei $VIDEO — rode o render antes." >&2; exit 1; }

NOME=$(basename "$VIDEO" .mp4)
DEST=out/check-$NOME
rm -rf "$DEST"
mkdir -p "$DEST"

DUR=$(ffprobe -v error -show_entries format=duration -of csv=p=0 "$VIDEO")
echo "vídeo: $VIDEO  ${DUR}s"

# 16 quadros distribuídos: o suficiente para pegar cada cena e cada transição
# sem virar uma folha ilegível.
N=16
i=0
while [ "$i" -lt "$N" ]; do
  T=$(awk "BEGIN{printf \"%.2f\", $DUR*($i+0.5)/$N}")
  ffmpeg -v error -ss "$T" -i "$VIDEO" -frames:v 1 \
    -vf "scale=480:-1,drawtext=text='${T}s':x=8:y=8:fontsize=26:fontcolor=yellow:box=1:boxcolor=black@0.75" \
    "$DEST/q_$(printf %02d $i).png" -y
  i=$((i + 1))
done

ffmpeg -v error $(for f in "$DEST"/q_*.png; do printf -- "-i %s " "$f"; done) \
  -filter_complex "xstack=inputs=$N:layout=0_0|w0_0|w0+w1_0|w0+w1+w2_0|0_h0|w0_h0|w0+w1_h0|w0+w1+w2_h0|0_h0+h4|w0_h0+h4|w0+w1_h0+h4|w0+w1+w2_h0+h4|0_h0+h4+h8|w0_h0+h4+h8|w0+w1_h0+h4+h8|w0+w1+w2_h0+h4+h8" \
  "out/folha-$NOME.png" -y

echo "folha: out/folha-$NOME.png (quadros soltos em $DEST/)"
echo
echo "loudness:"
# `sed` antes do `grep` porque o ebur128 escreve uma linha por quadro, e cada
# uma delas também traz um campo `I:` — sem cortar no "Summary:" a medição final
# sai afogada em trezentas linhas de progresso.
ffmpeg -hide_banner -nostats -i "$VIDEO" -map 0:a -af ebur128=peak=true -f null /dev/null 2>&1 |
  sed -n '/Summary:/,$p' | grep -E '    I: |    LRA: |    Peak: '
echo
echo "OLHE a folha de contato antes de entregar. Procure, nesta ordem:"
echo "  - legenda ou painel cobrindo um card;"
echo "  - texto ou card cortado na borda do quadro;"
echo "  - campo estourando a largura do card;"
echo "  - tabela com linha cortada ao meio na borda do preview;"
echo "  - elemento visível antes de entrar ou depois de sair."
