-- Filtro do modelo UFLA (ABNT) do trama, adaptado do modelo ABNT em Quarto
-- do kit "pedro-e-pedro" (ver ORIGEM.md).
-- 1. No Word (.docx), monta capa, folha de rosto, sumário, resumo e abstract a
--    partir do cabeçalho YAML (no PDF quem faz isso é o typst-template.typ).
-- 2. Em qualquer formato, formata blocos ::: {.fonte} (a linha "Fonte: ..."
--    abaixo de figuras) e ::: {.fonte-tabela} (a mesma linha abaixo de
--    tabelas; no Word ela traz o traço que fecha a tabela) e as referências.

local stringify = pandoc.utils.stringify

local function eh_docx() return FORMAT:match("docx") ~= nil end
local function eh_typst() return FORMAT:match("typst") ~= nil end

-- Converte um campo do YAML em lista de blocos.
local function blocos(valor)
  if valor == nil then return pandoc.Blocks({}) end
  local tipo = pandoc.utils.type(valor)
  if tipo == "Blocks" then return valor end
  if tipo == "Inlines" then return pandoc.Blocks({ pandoc.Para(valor) }) end
  return pandoc.Blocks({ pandoc.Para(pandoc.Str(stringify(valor))) })
end

local function com_estilo(estilo, conteudo)
  return pandoc.Div(conteudo, { ["custom-style"] = estilo })
end

local function paragrafo(estilo, texto)
  return com_estilo(estilo, { pandoc.Para({ pandoc.Str(texto) }) })
end

local function quebra_pagina()
  return pandoc.RawBlock("openxml", '<w:p><w:r><w:br w:type="page"/></w:r></w:p>')
end

-- Autor: `autor` do YAML ou, sem ele, o `author` do Quarto (o do perfil).
local function nome_autor(meta)
  if meta.autor then return stringify(meta.autor) end
  local lista = meta["by-author"] or meta.author
  if lista == nil then return "" end
  if pandoc.utils.type(lista) ~= "List" then lista = { lista } end
  local nomes = {}
  for _, a in ipairs(lista) do
    local n = a
    if type(a) == "table" and a.name ~= nil then
      n = a.name
      if type(n) == "table" and n.literal ~= nil then n = n.literal end
    end
    table.insert(nomes, stringify(n))
  end
  return table.concat(nomes, "; ")
end

local function lista_texto(valor)
  if valor == nil then return "" end
  local itens = {}
  for _, item in ipairs(valor) do table.insert(itens, stringify(item)) end
  return table.concat(itens, "; ") .. "."
end

local function campo_sumario()
  return pandoc.RawBlock("openxml", [[
<w:p><w:r><w:fldChar w:fldCharType="begin" w:dirty="true"/></w:r><w:r><w:instrText xml:space="preserve"> TOC \o "1-3" \h \z \u </w:instrText></w:r><w:r><w:fldChar w:fldCharType="separate"/></w:r><w:r><w:t>Clique com o botão direito aqui e escolha "Atualizar campo" para gerar o sumário.</w:t></w:r><w:r><w:fldChar w:fldCharType="end"/></w:r></w:p>]])
end

local function elementos_pre_textuais(meta)
  local b = pandoc.Blocks({})
  local function add(x) b:insert(x) end
  local instituicao = {}
  for _, linha in ipairs(meta.instituicao or {}) do
    table.insert(instituicao, stringify(linha))
  end
  local titulo = stringify(meta.titulo or meta.title or "")
  local subtitulo = meta.subtitulo or meta.subtitle
  if subtitulo then titulo = titulo .. ": " .. stringify(subtitulo) end
  local autor = nome_autor(meta)
  local local_ano = { stringify(meta["local"] or ""), stringify(meta.ano or "") }

  local function cabecalho()
    for _, linha in ipairs(instituicao) do add(paragrafo("Capa", linha)) end
  end

  if meta.capa then
    cabecalho()
    add(paragrafo("Capa Titulo", titulo))
    add(paragrafo("Capa Autor", autor))
    add(paragrafo("Capa Local", local_ano[1]))
    add(paragrafo("Capa", local_ano[2]))
    add(quebra_pagina())
  end

  if meta["folha-rosto"] then
    cabecalho()
    add(paragrafo("Rosto Titulo", titulo))
    add(paragrafo("Rosto Autor", autor))
    if meta.orientador then
      add(paragrafo("Rosto Orientador", "Orientador(a): " .. stringify(meta.orientador)))
    end
    if meta.coorientador then
      add(paragrafo("Rosto Orientador", "Coorientador(a): " .. stringify(meta.coorientador)))
    end
    if meta.natureza then add(com_estilo("Rosto Natureza", blocos(meta.natureza))) end
    add(paragrafo("Rosto Local", local_ano[1]))
    add(paragrafo("Centro", local_ano[2]))
    add(quebra_pagina())
  end

  if meta["ficha-catalografica"] then
    add(paragrafo("Centro", "FICHA CATALOGRÁFICA"))
    add(paragrafo("Centro", "Confeccionar com a biblioteca e inserir após a defesa."))
    add(quebra_pagina())
  end

  if meta["folha-aprovacao"] then
    add(paragrafo("Centro", "FOLHA DE APROVAÇÃO"))
    add(paragrafo("Centro", "Página em branco: inserir após a defesa a folha de aprovação fornecida pelo programa."))
    add(quebra_pagina())
  end

  if meta.agradecimentos then
    add(paragrafo("Titulo sem numero", "AGRADECIMENTOS"))
    for _, x in ipairs(blocos(meta.agradecimentos)) do add(x) end
    add(quebra_pagina())
  end

  if meta.sumario then
    add(paragrafo("Titulo sem numero", "SUMÁRIO"))
    add(campo_sumario())
    add(quebra_pagina())
  end

  local function pagina_resumo(tit, nomes, rotulo, texto, rotulo_palavras, palavras)
    add(paragrafo("Resumo Titulo", tit))
    for _, n in ipairs(nomes) do add(paragrafo("Resumo Nomes", n)) end
    add(paragrafo("Titulo pre-textual", rotulo))
    add(com_estilo("Resumo", blocos(texto)))
    add(com_estilo("Resumo", { pandoc.Para({
      pandoc.Strong({ pandoc.Str(rotulo_palavras .. ":") }), pandoc.Space(),
      pandoc.Str(lista_texto(palavras)) }) }))
    add(quebra_pagina())
  end

  local orientador = stringify(meta.orientador or "")
  if meta.resumo then
    local nomes = { "Autor(a): " .. autor }
    if meta.orientador then table.insert(nomes, "Orientador(a): " .. orientador) end
    if meta.coorientador then table.insert(nomes, "Coorientador(a): " .. stringify(meta.coorientador)) end
    pagina_resumo(titulo, nomes, "RESUMO", meta.resumo, "Palavras-chave", meta["palavras-chave"])
  end
  if meta["resumo-en"] then
    local nomes = { "Author: " .. autor }
    if meta.orientador then table.insert(nomes, "Adviser: " .. orientador) end
    if meta.coorientador then table.insert(nomes, "Co-adviser: " .. stringify(meta.coorientador)) end
    pagina_resumo(stringify(meta["titulo-en"] or titulo), nomes, "ABSTRACT",
      meta["resumo-en"], "Keywords", meta.keywords)
  end
  return b
end

-- No PDF, títulos sem número (Declaração de IA, Apêndice...) viram títulos
-- Typst diretos, para começarem em página nova como os demais.
-- No Word, numera os títulos no formato ABNT ("1 INTRODUÇÃO", "1.1 Seção"),
-- sem o ponto que o Word colocaria depois do número.
local contadores = { 0, 0, 0, 0, 0, 0 }

function Header(el)
  -- Títulos sem número (Referências, Apêndice...) ficam centralizados, como na ABNT.
  if eh_docx() and el.classes:includes("unnumbered") and el.level == 1 then
    return { quebra_pagina(), com_estilo("Titulo pre-textual", { pandoc.Para(el.content) }) }
  end
  if eh_docx() and not el.classes:includes("unnumbered") then
    contadores[el.level] = contadores[el.level] + 1
    for i = el.level + 1, #contadores do contadores[i] = 0 end
    local partes = {}
    for i = 1, el.level do table.insert(partes, tostring(contadores[i])) end
    el.content:insert(1, pandoc.Space())
    el.content:insert(1, pandoc.Str(table.concat(partes, ".")))
    return el
  end
  if eh_typst() and el.classes:includes("unnumbered") then
    local texto = pandoc.write(pandoc.Pandoc({ pandoc.Plain(el.content) }), "typst")
    texto = texto:gsub("%s+$", "")
    return pandoc.RawBlock("typst",
      "#heading(level: " .. el.level .. ", numbering: none)[" .. texto .. "]")
  end
end

function Div(el)
  local tabela = el.classes:includes("fonte-tabela")
  if el.classes:includes("fonte") or tabela then
    if eh_docx() then return com_estilo(tabela and "Fonte tabela" or "Fonte", el.content) end
    if eh_typst() then
      return {
        pandoc.RawBlock("typst", "#block(above: 0.4em)[#set text(size: 10pt); #set par(first-line-indent: 0pt, leading: 0.5em)"),
        pandoc.Div(el.content),
        pandoc.RawBlock("typst", "]"),
      }
    end
  end
  -- Referências: espaço simples, alinhadas à esquerda, separadas por uma linha.
  if el.identifier == "refs" and eh_typst() then
    return {
      pandoc.RawBlock("typst", "#block[#set par(first-line-indent: 0pt, justify: false, leading: 0.5em, spacing: 1.5em)"),
      el,
      pandoc.RawBlock("typst", "]"),
    }
  end
end

function Pandoc(doc)
  if eh_docx() then
    local novos = elementos_pre_textuais(doc.meta)
    novos:extend(doc.blocks)
    doc.blocks = novos
    -- A capa já traz título e autor: sem isto o Word repetiria o bloco de
    -- título do Pandoc no alto da primeira página.
    for _, k in ipairs({ "title", "subtitle", "author", "by-author", "authors", "date" }) do
      doc.meta[k] = nil
    end
  end
  return doc
end
