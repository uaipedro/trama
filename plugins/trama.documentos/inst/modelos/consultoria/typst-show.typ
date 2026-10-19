// Liga o cabeçalho YAML do .qmd ao modelo Consultoria (typst-template.typ).
#show: doc => consultoria(
$if(title)$
  title: [$title$],
$endif$
$if(subtitle)$
  subtitle: [$subtitle$],
$endif$
$if(by-author)$
  authors: (
$for(by-author)$
$if(it.name.literal)$
    (name: [$it.name.literal$],),
$endif$
$endfor$
  ),
$endif$
$if(date)$
  date: [$date$],
$endif$
$if(cliente)$
  cliente: [$cliente$],
$endif$
  capa: $if(capa)$true$else$false$endif$,
  cor: brand-color.at("primary", default: rgb("#003366")),
  cor-clara: brand-color.at("secondary", default: rgb("#E6EFF6")),
$if(brand.typography.base.family)$
  font: $brand.typography.base.family$ + ("Liberation Sans", "Arial"),
$endif$
$if(fontsize)$
  fontsize: $fontsize$,
$endif$
$if(toc)$
  toc: $toc$,
$endif$
$if(toc-title)$
  toc_title: [$toc-title$],
$endif$
  doc,
)
