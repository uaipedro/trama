#!/bin/sh
# Traz do pacote o que o vídeo COPIA em vez de reescrever: as folhas de estilo
# do editor e da coleção `data`, a marca e os ícones. Rode depois de mexer no
# front do trama — senão o vídeo segue mostrando a aparência antiga, que é um
# defeito que não aparece lendo só o código do vídeo.
set -e
cd "$(dirname "$0")/.."
cp ../../inst/www/trama.css src/trama-app.css
cp ../../collections/trama.data/inst/trama/data.css src/trama-data.css
cp ../../collections/trama.models/inst/trama/models.css src/trama-models.css
cp ../../inst/www/marca.svg public/marca.svg
cp ../../inst/www/vendor/lucide.svg public/lucide.svg
cp ../../inst/www/modos.js src/trama/modos-app.js
node scripts/icones.mjs
echo "sincronizado com inst/www, trama.data e trama.models."
