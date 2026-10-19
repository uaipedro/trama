// Modelo UFLA (ABNT) do trama: saída PDF via Typst.
// Segue a ABNT NBR 14724: A4, Arial 12, entrelinha 1,5, margens esquerda e
// superior 3 cm, direita e inferior 2 cm. Adaptado do modelo ABNT em Quarto
// do kit "pedro-e-pedro" (ver ORIGEM.md). Confira com o manual de
// normalização da Biblioteca da UFLA o que o seu programa pede a mais.
// Não é preciso mexer aqui: tudo se ajusta pelo cabeçalho YAML do .qmd.

#let abnt(
  instituicao: (),
  titulo: [],
  subtitulo: none,
  titulo-en: none,
  autor: [],
  orientador: none,
  coorientador: none,
  natureza: none,
  local: [],
  ano: [],
  agradecimentos: none,
  resumo: none,
  palavras-chave: none,
  resumo-en: none,
  keywords: none,
  capa: true,
  folha-rosto: true,
  ficha-catalografica: false,
  folha-aprovacao: false,
  sumario: true,
  lista-tabelas: false,
  lista-figuras: false,
  margem-esquerda: 3cm,
  margem-superior: 3cm,
  margem-direita: 2cm,
  margem-inferior: 2cm,
  fonte: ("Arial", "Liberation Sans"),
  doc,
) = {
  // ---------- Configuração geral ----------
  set document(title: titulo)
  set page(
    paper: "a4",
    margin: (
      left: margem-esquerda,
      top: margem-superior,
      right: margem-direita,
      bottom: margem-inferior,
    ),
    numbering: none,
    number-align: top + right,
  )
  set text(font: fonte, size: 12pt, lang: "pt", region: "br", hyphenate: false)
  // Entrelinha 1,5 (equivalente ao "1,5 linha" do Word para Arial 12).
  set par(justify: true, leading: 1em, spacing: 1em,
          first-line-indent: (amount: 1.25cm, all: true))

  // Títulos: 1 INTRODUÇÃO (negrito, maiúsculas), 1.1 Seção (negrito), 1.1.1 (normal).
  set heading(numbering: "1.1.1")
  show heading: set text(size: 12pt)
  show heading: set block(above: 1.5em, below: 1.5em)
  show heading.where(level: 1): it => {
    if it.at("label", default: none) != <sem-quebra> { pagebreak(weak: true) }
    let alinhamento = if it.numbering == none { center } else { left }
    align(alinhamento, upper(strong(it)))
  }
  show heading.where(level: 2): set text(weight: "bold")
  show heading.where(level: 3): set text(weight: "regular")

  // Ilustrações e tabelas: título em cima ("Figura 1 – ..."), texto menor.
  show figure.caption: set text(size: 10pt)
  show figure.caption: set par(justify: false, leading: 0.5em, first-line-indent: 0pt)
  show figure.where(kind: "quarto-float-fig"): set figure.caption(position: top)
  show figure.where(kind: "quarto-float-tbl"): set figure.caption(position: top)
  set figure.caption(separator: [ – ])

  // Tabelas no padrão IBGE: sem linhas verticais, só traços horizontais.
  set table(
    stroke: (_, y) => (top: if y == 0 { 1pt } else if y == 1 { 0.5pt } else { 0pt }),
    inset: (x: 6pt, y: 4pt),
  )
  show table: set text(size: 10pt)
  show table: set par(justify: false, leading: 0.5em, first-line-indent: 0pt)
  show table: it => block(stroke: (bottom: 1pt), it)

  // Notas de rodapé: espaço simples, fonte 10.
  show footnote.entry: set text(size: 10pt)
  show footnote.entry: set par(leading: 0.5em, first-line-indent: 0pt)

  // Sumário e índices: sem recuo, com linha pontilhada até a página.
  show outline.entry: set block(above: 0.9em)
  show outline.entry.where(level: 1): it => strong(it)

  // ---------- Blocos auxiliares ----------
  let simples(corpo) = {
    set par(leading: 0.5em, spacing: 0.8em, first-line-indent: 0pt)
    corpo
  }
  let centro(corpo) = {
    set par(leading: 0.5em, spacing: 0.8em, first-line-indent: 0pt, justify: false)
    align(center, corpo)
  }
  let cabecalho-instituicao() = centro(strong(upper(
    instituicao.join(linebreak()))))
  let titulo-centro() = centro({
    strong(upper(titulo))
    if subtitulo != none { [: ]; strong(subtitulo) }
  })
  let titulo-sem-numero(texto, no-sumario: false) = heading(
    level: 1, numbering: none, outlined: no-sumario, texto)

  // ---------- Capa ----------
  if capa {
    cabecalho-instituicao()
    v(1fr)
    titulo-centro()
    v(1fr)
    align(center, strong(upper(autor)))
    v(1fr)
    centro(strong[#local \ #ano])
    pagebreak()
  }

  // ---------- Folha de rosto ----------
  if folha-rosto {
    cabecalho-instituicao()
    v(1fr)
    titulo-centro()
    v(2em)
    align(center, strong(upper(autor)))
    v(2em)
    if orientador != none {
      align(right, simples(upper[
        Orientador(a): #orientador
        #if coorientador != none [\ Coorientador(a): #coorientador]
      ]))
    }
    v(2em)
    if natureza != none {
      pad(left: 8cm, simples(text(size: 11pt, natureza)))
    }
    v(1fr)
    centro[#local \ #ano]
    pagebreak()
  }

  // ---------- Ficha catalográfica (verso da folha de rosto) ----------
  if ficha-catalografica {
    centro[
      FICHA CATALOGRÁFICA \
      *Confeccionar com a biblioteca e inserir após a defesa*
    ]
    pagebreak()
  }

  // ---------- Folha de aprovação ----------
  if folha-aprovacao {
    centro[
      FOLHA DE APROVAÇÃO \
      *Página em branco: inserir após a defesa a folha de aprovação fornecida
      pelo programa.*
    ]
    pagebreak()
  }

  // ---------- Agradecimentos ----------
  if agradecimentos != none {
    titulo-sem-numero[AGRADECIMENTOS]
    agradecimentos
    pagebreak()
  }

  // ---------- Sumário ----------
  if sumario {
    align(center, strong[SUMÁRIO])
    v(1em)
    align(right, text(size: 11pt)[Página])
    outline(title: none, indent: 0pt, depth: 3)
    pagebreak()
  }

  // A partir daqui, numeração em algarismos romanos (i, ii, iii...).
  set page(numbering: "i")
  counter(page).update(1)

  if lista-tabelas {
    titulo-sem-numero(no-sumario: true)[ÍNDICE DE TABELAS]
    outline(title: none, target: figure.where(kind: "quarto-float-tbl"))
  }
  if lista-figuras {
    titulo-sem-numero(no-sumario: true)[ÍNDICE DE FIGURAS]
    outline(title: none, target: figure.where(kind: "quarto-float-fig"))
  }

  // ---------- Resumo e abstract ----------
  let pagina-resumo(tit, nomes, rotulo, texto, palavras, rotulo-palavras) = {
    pagebreak(weak: true)
    simples({
      set par(justify: false)
      strong(upper(tit))
      v(1em)
      strong(nomes)
    })
    [#heading(level: 1, numbering: none, rotulo) <sem-quebra>]
    {
      set par(first-line-indent: 0pt)
      texto
      v(1em)
      [*#rotulo-palavras:* #palavras]
    }
  }
  if resumo != none {
    pagina-resumo(
      titulo,
      [Autor(a): #upper(autor)
       #if orientador != none [\ Orientador(a): #upper(orientador)]
       #if coorientador != none [\ Coorientador(a): #upper(coorientador)]],
      [RESUMO], resumo, palavras-chave, "Palavras-chave",
    )
  }
  if resumo-en != none {
    pagina-resumo(
      if titulo-en != none { titulo-en } else { titulo },
      [Author: #upper(autor)
       #if orientador != none [\ Adviser: #upper(orientador)]
       #if coorientador != none [\ Co-adviser: #upper(coorientador)]],
      [ABSTRACT], resumo-en, keywords, "Keywords",
    )
  }

  // ---------- Texto principal: numeração arábica a partir de 1 ----------
  pagebreak(weak: true)
  set page(numbering: "1")
  counter(page).update(1)
  doc
}
