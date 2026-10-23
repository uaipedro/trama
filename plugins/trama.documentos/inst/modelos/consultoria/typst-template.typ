// Modelo Consultoria do trama: relatório com capa na cor da marca.
// As cores vêm do _brand.yml (primary, secondary); o resto se ajusta pelo
// cabeçalho YAML do .qmd (title, subtitle, author, date, cliente, capa).

// Cores de reserva: o Quarto define `brand-color` depois deste arquivo quando
// o _brand.yml tem cores, e essa definição vence esta.
#let brand-color = (primary: rgb("#003366"), secondary: rgb("#E6EFF6"))

#let consultoria(
  title: none,
  subtitle: none,
  authors: (),
  date: none,
  cliente: none,
  capa: true,
  cor: rgb("#003366"),
  cor-clara: rgb("#E6EFF6"),
  font: ("Inter", "Liberation Sans", "Arial"),
  fontsize: 10.5pt,
  toc: false,
  toc_title: none,
  doc,
) = {
  set document(title: title)
  set text(font: font, size: fontsize, lang: "pt", region: "br")
  set par(justify: true, leading: 0.75em, spacing: 1.1em)
  set heading(numbering: "1.1")
  show heading: set text(fill: cor)
  show heading.where(level: 1): set block(above: 1.8em, below: 1em)
  show heading.where(level: 1): set text(size: 1.4em)
  show link: set text(fill: cor)
  show figure.caption: set text(size: 0.9em)
  set table(
    stroke: (_, y) => (top: if y == 0 { 1pt + cor } else if y == 1 { 0.5pt + cor } else { 0pt }, bottom: 0.5pt + cor-clara),
    fill: (_, y) => if y == 0 { cor-clara },
    inset: (x: 6pt, y: 5pt),
  )

  let autores = authors.map(a => a.name).join(", ")

  // ---------- Capa ----------
  if capa {
    page(margin: 0pt, numbering: none, {
      block(width: 100%, height: 62%, fill: cor, inset: (x: 2.5cm, top: 3cm, bottom: 2cm), {
        set par(justify: false)
        set text(fill: white)
        if cliente != none { text(size: 11pt, tracking: 0.08em, upper(cliente)); v(1.2em) }
        text(size: 28pt, weight: "bold", title)
        if subtitle != none { v(0.6em); text(size: 15pt, subtitle) }
      })
      block(width: 100%, inset: (x: 2.5cm, top: 1.5cm), {
        set par(justify: false)
        if autores != "" { text(size: 12pt, weight: "bold", autores); linebreak() }
        if date != none { text(size: 11pt, fill: luma(90), date) }
      })
    })
  }

  // ---------- Corpo ----------
  set page(
    paper: "a4",
    margin: (x: 2.5cm, top: 2.8cm, bottom: 2.5cm),
    numbering: "1",
    header: context {
      set text(size: 8.5pt, fill: luma(110))
      block(width: 80%, [#title#if cliente != none [ · #cliente]])
      v(-0.4em)
      line(length: 100%, stroke: 0.6pt + cor)
    },
    footer: context {
      set text(size: 8.5pt, fill: luma(110))
      align(right, counter(page).display("1"))
    },
  )
  counter(page).update(1)
  if not capa {
    block(below: 1.5em, {
      text(size: 22pt, weight: "bold", fill: cor, title)
      if subtitle != none { linebreak(); text(size: 13pt, subtitle) }
      if autores != "" or date != none { v(0.4em); text(size: 10pt, fill: luma(90))[#autores #if date != none [· #date]] }
    })
  }
  if toc {
    outline(title: toc_title, depth: 2)
    pagebreak()
  }
  doc
}
