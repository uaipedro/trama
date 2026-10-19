// Liga os campos do cabeçalho YAML do .qmd ao modelo ABNT (typst-template.typ).
// Sem `titulo` e `autor`, usa o `title` e o `author` do Quarto (os que o trama
// preenche com o perfil ao criar o documento).
#show: doc => abnt(
  instituicao: ($for(instituicao)$[$instituicao$],$endfor$),
$if(titulo)$
  titulo: [$titulo$],
$elseif(title)$
  titulo: [$title$],
$endif$
$if(subtitulo)$
  subtitulo: [$subtitulo$],
$elseif(subtitle)$
  subtitulo: [$subtitle$],
$endif$
$if(titulo-en)$
  titulo-en: [$titulo-en$],
$endif$
$if(autor)$
  autor: [$autor$],
$elseif(by-author)$
  autor: [$for(by-author)$$it.name.literal$$sep$; $endfor$],
$endif$
$if(orientador)$
  orientador: [$orientador$],
$endif$
$if(coorientador)$
  coorientador: [$coorientador$],
$endif$
$if(natureza)$
  natureza: [$natureza$],
$endif$
$if(local)$
  local: [$local$],
$endif$
$if(ano)$
  ano: [$ano$],
$endif$
$if(agradecimentos)$
  agradecimentos: [$agradecimentos$],
$endif$
$if(resumo)$
  resumo: [$resumo$],
$endif$
$if(palavras-chave)$
  palavras-chave: [$for(palavras-chave)$$palavras-chave$$sep$; $endfor$.],
$endif$
$if(resumo-en)$
  resumo-en: [$resumo-en$],
$endif$
$if(keywords)$
  keywords: [$for(keywords)$$keywords$$sep$; $endfor$.],
$endif$
  capa: $if(capa)$true$else$false$endif$,
  folha-rosto: $if(folha-rosto)$true$else$false$endif$,
  ficha-catalografica: $if(ficha-catalografica)$true$else$false$endif$,
  folha-aprovacao: $if(folha-aprovacao)$true$else$false$endif$,
  sumario: $if(sumario)$true$else$false$endif$,
  lista-tabelas: $if(lista-tabelas)$true$else$false$endif$,
  lista-figuras: $if(lista-figuras)$true$else$false$endif$,
$if(margem-esquerda)$
  margem-esquerda: $margem-esquerda$,
$endif$
$if(margem-superior)$
  margem-superior: $margem-superior$,
$endif$
$if(margem-direita)$
  margem-direita: $margem-direita$,
$endif$
$if(margem-inferior)$
  margem-inferior: $margem-inferior$,
$endif$
  doc,
)
