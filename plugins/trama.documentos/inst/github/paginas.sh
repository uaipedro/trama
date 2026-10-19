#!/usr/bin/env bash
# Gerado pelo trama.documentos: acumula as páginas na branch `paginas`.
# /vX.Y.Z/ é imutável (versão já publicada faz falhar), /ultimo/ é a mais
# nova, e a raiz leva para /ultimo/. O site do Pages é a branch inteira, que
# o próximo passo empacota: o deploy substitui o site, então ele precisa
# carregar todas as versões.
set -euo pipefail
V="$GITHUB_REF_NAME"
git config user.name "github-actions[bot]"
git config user.email "41898282+github-actions[bot]@users.noreply.github.com"
if git ls-remote --exit-code --heads origin paginas > /dev/null; then
  git fetch origin paginas:paginas
  git worktree add _paginas paginas
else
  git worktree add --orphan -b paginas _paginas
fi
if [ -e "_paginas/$V" ]; then
  echo "A versão $V já está publicada: versão publicada não muda." >&2
  exit 1
fi
cp -r _build "_paginas/$V"
rm -rf _paginas/ultimo
cp -r _build _paginas/ultimo
touch _paginas/.nojekyll
cat > _paginas/index.html <<HTML
<!doctype html>
<meta charset="utf-8">
<meta http-equiv="refresh" content="0; url=ultimo/">
<title>Redirecionando</title>
<p><a href="ultimo/">Versão mais nova</a></p>
HTML
cd _paginas
git add -A
git commit -m "páginas: $V"
git push origin paginas
cd ..
rm -rf _site
cp -r _paginas _site
rm -rf _site/.git
