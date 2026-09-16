#!/bin/sh
# Gera os PNGs do favicon a partir dos SVGs da marca (inst/www/marca*.svg).
#
# Por que Chrome headless e não rsvg-convert/ImageMagick: o rasterizador passa
# a ser o MESMO motor que exibe a marca no navegador, então o PNG e o SVG não
# divergem. Um conversor externo reinterpreta o desenho por conta própria
# (arredondamento de traço, hinting) e o favicon deixaria de bater com o que
# o app mostra.
#
# Precisa também de python3, só com a stdlib: a captura sai maior que o
# pedido (ver mais abaixo) e o recorte para o tamanho final é feito em Python,
# lendo e reescrevendo o PNG RGBA com zlib + struct.
#
# Por que --default-background-color=00000000: sem esse flag o Chrome pinta o
# canvas de branco antes de desenhar e o PNG sai com fundo opaco. Os dois
# últimos zeros são o alfa — 00000000 é RGBA totalmente transparente.
#
# Por que um SVG temporário como quadro, e não uma página HTML: num HTML o
# Chrome dimensiona o <svg> como elemento substituído e IGNORA a altura que o
# CSS pede, encaixotando o desenho num quadrado e cortando o resto (visto na
# prática: 398x399 no lugar de 398x461, com a ponta de baixo do hexágono
# decepada). Vale para SVG embutido e dentro de <img>. Num arquivo .svg
# renderizado DIRETO quem manda no enquadramento é o viewBox, que é exato —
# então o quadro quadrado é montado aqui como um SVG que envolve o corpo do
# original num <g transform>.
#
# Por que capturar com folga e recortar: neste Chrome (145) o --window-size
# vale para a JANELA, e o viewport fica 88 px mais baixo — a captura sai com
# uma faixa transparente embaixo e o desenho cortado. Como a altura perdida
# depende da versão, pedimos uma janela bem mais alta e recortamos o canto
# superior esquerdo no tamanho certo, o que funciona para qualquer folga
# menor que FOLGA.
#
# Uso:  tools/marca/rasterizar.sh        (ou CHROME=/caminho/do/chrome ...)
set -e

AQUI=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
RAIZ=$(CDPATH= cd -- "$AQUI/../.." && pwd)
WWW="$RAIZ/inst/www"
CHROME=${CHROME:-google-chrome}
FOLGA=300

# As duas dependências são verificadas aqui porque as falhas delas são mudas:
# a chamada do Chrome silencia stderr (para não poluir a saída com os avisos
# do headless) e engoliria junto o "not found", e o python3 ausente só
# apareceria no meio do recorte. Sem isso o script sai 127 sem dizer nada.
command -v "$CHROME" >/dev/null 2>&1 || {
  echo "rasterizar: '$CHROME' não encontrado. Instale o Google Chrome ou use CHROME=/caminho/do/chrome" >&2
  exit 1
}
command -v python3 >/dev/null 2>&1 || {
  echo "rasterizar: python3 não encontrado — é o que recorta a captura do Chrome" >&2
  exit 1
}

TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

# renderizar SVG LADO FRACAO SAIDA
#   LADO   = aresta do PNG quadrado
#   FRACAO = quanto do lado o desenho ocupa (o resto é respiro transparente)
renderizar() {
  svg=$1
  lado=$2
  fracao=$3
  saida=$4

  # O quadro é sempre quadrado (ícone de app), mas marca.svg é 173×200.
  # Escalar pelo maior eixo do viewBox mantém a proporção — nada de hexágono
  # esticado — e centrar deixa o respiro nas laterais.
  vb=$(sed -n 's/.*viewBox="\([^"]*\)".*/\1/p' "$svg" | head -1)
  geo=$(echo "$vb" | awk -v lado="$lado" -v f="$fracao" '{
    maior = ($3 > $4) ? $3 : $4
    escala = lado * f / maior
    printf "%.5f %.4f %.4f", escala, (lado - $3 * escala) / 2, (lado - $4 * escala) / 2
  }')
  escala=$(echo "$geo" | cut -d' ' -f1)
  desloca_x=$(echo "$geo" | cut -d' ' -f2)
  desloca_y=$(echo "$geo" | cut -d' ' -f3)

  # `sed '1d;$d'` tira a linha do <svg> raiz e a do </svg>: sobra o corpo do
  # desenho, que entra no <g> já posicionado. Nenhuma cor é injetada aqui: os
  # SVGs da marca já trazem TODAS as cores fixas, inclusive a do contorno, para
  # que o PNG saia idêntico ao que o navegador mostra — e é justamente por isso
  # que não há mais um `style="color:..."` na raiz do quadro. A premissa é que
  # os SVGs da marca tenham a tag de abertura inteira na primeira linha e o
  # </svg> na última — vale hoje e vale para quem editar à mão, mas um
  # formatador que quebrasse a raiz em duas linhas produziria XML malformado
  # sem erro visível aqui.
  quadro="$TMP/quadro.svg"
  {
    printf '<svg xmlns="http://www.w3.org/2000/svg" width="%s" height="%s" viewBox="0 0 %s %s" fill="none">\n' \
      "$lado" "$lado" "$lado" "$lado"
    printf '<g transform="translate(%s %s) scale(%s)">\n' "$desloca_x" "$desloca_y" "$escala"
    sed '1d;$d' "$svg"
    printf '</g>\n</svg>\n'
  } > "$quadro"

  bruto="$TMP/bruto.png"
  "$CHROME" --headless \
    --disable-gpu \
    --hide-scrollbars \
    --force-device-scale-factor=1 \
    --default-background-color=00000000 \
    --window-size="$lado,$((lado + FOLGA))" \
    --screenshot="$bruto" \
    "file://$quadro" >/dev/null 2>&1

  # O Chrome pode sair 0 sem escrever nada (perfil bloqueado, sandbox, display
  # ausente). Sem esta checagem o erro só apareceria como FileNotFoundError do
  # open() lá no recorte, apontando para o lugar errado.
  [ -s "$bruto" ] || {
    echo "rasterizar: o Chrome não produziu $bruto" >&2
    exit 1
  }

  recortar "$bruto" "$lado" "$saida"
  echo "gerado $saida (${lado}x${lado})"
}

# recortar ENTRADA LADO SAIDA — corta o canto superior esquerdo LADOxLADO.
# Em Python porque a máquina não tem nenhum utilitário de imagem; a stdlib
# (zlib + struct) dá conta de ler e reescrever PNG RGBA sem dependência nova.
recortar() {
  python3 - "$1" "$2" "$3" <<'FIM'
import struct, sys, zlib

entrada, lado, saida = sys.argv[1], int(sys.argv[2]), sys.argv[3]
bruto = open(entrada, 'rb').read()

# varre os pedaços do PNG: só interessam o cabeçalho e os dados
i, dados = 8, b''
while i < len(bruto):
    tam = struct.unpack('>I', bruto[i:i + 4])[0]
    tipo = bruto[i + 4:i + 8]
    if tipo == b'IHDR':
        larg, alt, prof, cor = struct.unpack('>IIBB', bruto[i + 8:i + 18])
    elif tipo == b'IDAT':
        dados += bruto[i + 8:i + 8 + tam]
    i += 12 + tam
if (prof, cor) != (8, 6):
    raise SystemExit('esperava PNG RGBA de 8 bits, veio profundidade %d tipo %d' % (prof, cor))
if larg < lado or alt < lado:
    raise SystemExit('captura %dx%d menor que o recorte de %d' % (larg, alt, lado))

# desfaz os filtros por linha (o PNG guarda cada linha como diferença da
# anterior ou do pixel à esquerda; sem desfazer não dá para recortar)
cru = zlib.decompress(dados)
passo, bpp = larg * 4, 4
anterior, linhas, p = bytearray(passo), [], 0
for _ in range(alt):
    filtro, p = cru[p], p + 1
    linha = bytearray(cru[p:p + passo]); p += passo
    for x in range(passo):
        esq = linha[x - bpp] if x >= bpp else 0
        cima = anterior[x]
        diag = anterior[x - bpp] if x >= bpp else 0
        if filtro == 1:
            linha[x] = (linha[x] + esq) & 255
        elif filtro == 2:
            linha[x] = (linha[x] + cima) & 255
        elif filtro == 3:
            linha[x] = (linha[x] + (esq + cima) // 2) & 255
        elif filtro == 4:
            p0 = esq + cima - diag
            de, dc, dd = abs(p0 - esq), abs(p0 - cima), abs(p0 - diag)
            prev = esq if (de <= dc and de <= dd) else (cima if dc <= dd else diag)
            linha[x] = (linha[x] + prev) & 255
    linhas.append(bytes(linha))
    anterior = linha

# reescreve sem filtro (0): o arquivo é pequeno e assim o código fica curto
corpo = b''.join(b'\x00' + l[:lado * 4] for l in linhas[:lado])

def pedaco(tipo, dado):
    return (struct.pack('>I', len(dado)) + tipo + dado
            + struct.pack('>I', zlib.crc32(tipo + dado) & 0xffffffff))

with open(saida, 'wb') as f:
    f.write(b'\x89PNG\r\n\x1a\n')
    f.write(pedaco(b'IHDR', struct.pack('>IIBBBBB', lado, lado, 8, 6, 0, 0, 0)))
    f.write(pedaco(b'IDAT', zlib.compress(corpo, 9)))
    f.write(pedaco(b'IEND', b''))
FIM
}

# 32 px sai da marca-min: viewBox quadrado e desenhada para essa escala, então
# ocupa o quadro inteiro (fração 1) — não sobra faixa vazia.
renderizar "$WWW/marca-min.svg" 32 1 "$WWW/favicon-32.png"

# 512 e 180 saem da marca cheia. Fração 0,9 porque ícone de app leva margem:
# colado na borda ele encosta no recorte arredondado que o sistema aplica.
renderizar "$WWW/marca.svg" 512 0.9 "$WWW/icon.png"
renderizar "$WWW/marca.svg" 180 0.9 "$WWW/icon-180.png"
