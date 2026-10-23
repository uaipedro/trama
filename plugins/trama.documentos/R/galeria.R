# A galeria de modelos: ler, validar, criar, editar, duplicar, importar,
# exportar e remover.
#
# Um modelo é uma pasta-extensão Quarto (ver `R/modelos.R`). O que o trama
# precisa saber e o Quarto não lê mora em `trama-modelo.yml`, ao lado:
#
#   descricao: Relatório técnico com capa e sumário.
#   perfil: aluno-ufla            # perfil de autor que o modelo sugere
#   formatos_desligados:          # configuração guardada de formato desligado
#     docx: {reference-doc: referencia.docx}
#
# Editar mexe só nos campos que dá para mudar sem saber Quarto (título,
# descrição, formatos, cores, fontes, logo, capa, CSL, perfil); quem sabe
# Quarto edita os arquivos da pasta direto. Os embutidos não são editados no
# lugar: editar um embutido cria a cópia do usuário com o mesmo nome, que tem
# precedência; remover essa cópia volta ao embutido.
#
# Modelo inválido não quebra nada: a galeria o lista com o problema, e criar
# documento com ele dá erro dizendo o quê consertar.

.TD_META_MODELO <- "trama-modelo.yml"
.TD_FORMATOS_MODELO <- c("html", "typst", "docx", "revealjs", "dashboard")
.TD_CORES <- c(primaria = "primary", secundaria = "secondary", destaque = "tertiary",
               fundo = "background", texto = "foreground")
.TD_FONTES <- c(texto = "base", titulos = "headings", codigo = "monospace")
# Opções de formato que apontam para arquivo da pasta do modelo.
.TD_OPCOES_ARQUIVO <- c("css", "reference-doc", "template", "template-partials", "csl", "filters",
                        "include-in-header", "include-before-body", "include-after-body", "theme",
                        "bibliography")

#' Um modelo de documento, em detalhe
#'
#' O que a galeria mostra e edita: título, descrição, formatos, cores,
#' fontes, logo, capa, CSL e o perfil sugerido, mais se o modelo é válido.
#'
#' @param nome Nome do modelo (a pasta), como em [tr_documento_modelos()].
#' @return Lista com `nome`, `titulo`, `descricao`, `formatos`,
#'   `formatos_desligados`, `origem` (`"embutido"` ou `"usuario"`),
#'   `sobre_embutido` (cópia do usuário de um embutido), `caminho`, `perfil`,
#'   `cores` e `fontes` (listas nomeadas como em
#'   [tr_documento_modelo_editar()]), `logo` (caminho ou `NULL`), `capa`
#'   (`TRUE`, `FALSE` ou `NULL` se o modelo não diz), `csl` (nome do arquivo
#'   ou `NULL`), `valido` e `problemas`.
#' @export
tr_documento_modelo <- function(nome) {
  d <- .td_modelo_dir(nome)
  m <- .td_modelo_ler(d)
  brand <- .td_brand_ler(d)
  comum <- m$ext$contributes$formats$common %||% list()
  logo <- .td_brand_logo(brand)
  c(m[c("nome", "titulo", "descricao", "formatos")],
    list(formatos_desligados = names(m$meta$formatos_desligados %||% list()),
         origem = .td_origem(d), sobre_embutido = .td_sobre_embutido(d), caminho = d,
         perfil = m$perfil,
         cores = .td_brand_cores(brand), fontes = .td_brand_fontes(brand),
         logo = if (!is.null(logo)) file.path(d, logo),
         capa = if (is.logical(comum$capa)) comum$capa,
         csl = comum$csl,
         valido = !length(m$problemas), problemas = m$problemas))
}

#' Cria, edita, duplica e remove modelos do usuário
#'
#' Os modelos do usuário moram em `tools::R_user_dir("trama", "data")/modelos/`.
#' `tr_documento_modelo_criar()` parte de um modelo existente (`base`) e
#' aplica as edições; `tr_documento_modelo_duplicar()` copia tal como está;
#' `tr_documento_modelo_editar()` muda os campos dados (os demais ficam);
#' `tr_documento_modelo_remover()` apaga um modelo do usuário (o embutido de
#' mesmo nome, se houver, volta a valer).
#'
#' Editar um embutido cria a cópia do usuário com o mesmo nome, que passa a
#' valer em documentos novos. Documento já criado leva a cópia do modelo de
#' quando foi criado: mudar o modelo não muda documento existente.
#'
#' @param nome Nome do modelo: letras minúsculas, dígitos e `-` (é a pasta e o
#'   prefixo do formato no Quarto, `<nome>-html`).
#' @param titulo Nome de exibição na galeria (`"UFLA (ABNT)"`).
#' @param descricao Uma frase sobre o modelo.
#' @param formatos Formatos habilitados, entre `"html"`, `"typst"` (PDF),
#'   `"docx"`, `"revealjs"` (slides) e `"dashboard"`. Desligar guarda a
#'   configuração do formato; religar a traz de volta.
#' @param cores Lista ou vetor nomeado de cores `#rrggbb`: `primaria`,
#'   `secundaria`, `destaque`, `fundo`, `texto` (vão para o `_brand.yml`).
#'   Os gráficos do documento usam estas cores num tema claro.
#' @param fontes Lista ou vetor nomeado de famílias: `texto`, `titulos`,
#'   `codigo`. A fonte precisa estar instalada para o PDF.
#' @param logo Caminho de uma imagem (png, svg, jpg), copiada para o modelo;
#'   `FALSE` tira o logo.
#' @param capa `TRUE` ou `FALSE`: capa nos modelos que têm (UFLA,
#'   Consultoria). Vai como metadado `capa` do formato.
#' @param csl Caminho de um estilo de citação `.csl`, copiado para o modelo;
#'   `FALSE` volta ao estilo padrão do Quarto.
#' @param perfil Perfil de autor que o modelo sugere
#'   ([tr_documento_perfis()]); `FALSE` tira a sugestão.
#' @param base Modelo de onde `tr_documento_modelo_criar()` parte.
#' @param modelo Modelo a duplicar.
#' @return Invisível: o modelo, como [tr_documento_modelo()]; `remover`
#'   devolve o nome.
#' @export
tr_documento_modelo_criar <- function(nome, titulo = NULL, base = "basico", descricao = NULL, formatos = NULL,
                                      cores = NULL, fontes = NULL, logo = NULL, capa = NULL, csl = NULL,
                                      perfil = NULL) {
  tr_documento_modelo_duplicar(base, nome, titulo = titulo %||% nome)
  tr_documento_modelo_editar(nome, descricao = descricao, formatos = formatos, cores = cores, fontes = fontes,
                             logo = logo, capa = capa, csl = csl, perfil = perfil)
}

#' @rdname tr_documento_modelo_criar
#' @export
tr_documento_modelo_duplicar <- function(modelo, nome, titulo = NULL) {
  .td_nome_modelo(nome)
  origem <- .td_modelo_dir(modelo)
  destino <- .td_dir_usuario("modelos", nome)
  if (file.exists(file.path(destino, "_extension.yml"))) {
    .td_erro(sprintf("J\u{E1} existe o modelo do usu\u{E1}rio '%s' (%s).", nome, destino),
             c("tr_documento_erro_modelo_existe", "tr_documento_erro_modelo"))
  }
  .td_instalar_pasta(origem, destino)
  ext <- .td_ext_ler(destino)
  ext$title <- titulo %||% if (identical(nome, basename(origem))) ext$title else
    sprintf("%s (c\u{F3}pia)", ext$title %||% basename(origem))
  .td_yaml_gravar(ext, file.path(destino, "_extension.yml"))
  invisible(tr_documento_modelo(nome))
}

#' @rdname tr_documento_modelo_criar
#' @export
tr_documento_modelo_editar <- function(nome, titulo = NULL, descricao = NULL, formatos = NULL, cores = NULL,
                                       fontes = NULL, logo = NULL, capa = NULL, csl = NULL, perfil = NULL) {
  ruim <- function(msg) .td_erro(msg, "tr_documento_erro_modelo")
  # Confere tudo antes de escrever qualquer coisa: edição recusada não deixa
  # modelo pela metade nem cópia de embutido à toa.
  if (!is.null(formatos)) {
    formatos <- unique(as.character(formatos))
    if (!length(formatos)) ruim("O modelo precisa de pelo menos um formato.")
    # Formato que o modelo já tem (pdf, beamer de uma extensão importada) fica;
    # novo, só os que a galeria sabe configurar.
    atuais <- tryCatch(.td_modelo_ler(.td_modelo_dir(nome)), error = function(e) list())
    fora <- setdiff(formatos, c(.TD_FORMATOS_MODELO, atuais$formatos, names(atuais$meta$formatos_desligados)))
    if (length(fora)) ruim(sprintf("Formato '%s' desconhecido: use %s.", fora[[1]], paste(.TD_FORMATOS_MODELO, collapse = ", ")))
  }
  cores <- .td_conferir_nomeados(cores, names(.TD_CORES), "cores", function(v) {
    if (!grepl("^#[0-9A-Fa-f]{6}$", v)) ruim(sprintf("Cor '%s' inv\u{E1}lida: use #rrggbb.", v))
  })
  fontes <- .td_conferir_nomeados(fontes, names(.TD_FONTES), "fontes", function(v) {
    if (!nzchar(trimws(v)) || grepl("[\"'{}\n]", v)) ruim(sprintf("Fonte '%s' inv\u{E1}lida.", v))
  })
  if (!is.null(logo) && !isFALSE(logo)) {
    if (!is.character(logo) || length(logo) != 1L || !file.exists(logo)) ruim(sprintf("Logo '%s' n\u{E3}o existe.", logo))
    if (!tolower(tools::file_ext(logo)) %in% c("png", "svg", "jpg", "jpeg")) ruim("O logo \u{E9} png, svg ou jpg.")
  }
  if (!is.null(capa) && !(is.logical(capa) && length(capa) == 1L && !is.na(capa))) ruim("'capa' \u{E9} TRUE ou FALSE.")
  if (!is.null(csl) && !isFALSE(csl)) {
    if (!is.character(csl) || length(csl) != 1L || !file.exists(csl)) ruim(sprintf("CSL '%s' n\u{E3}o existe.", csl))
    if (!identical(tolower(tools::file_ext(csl)), "csl")) ruim("O estilo de cita\u{E7}\u{E3}o \u{E9} um arquivo .csl.")
    if (!any(grepl("<style", readLines(csl, n = 20L, warn = FALSE), fixed = TRUE))) {
      ruim(sprintf("'%s' n\u{E3}o parece um estilo CSL (falta <style>).", basename(csl)))
    }
  }
  if (!is.null(perfil) && !isFALSE(perfil) &&
      !(is.character(perfil) && length(perfil) == 1L && grepl("^[A-Za-z0-9][A-Za-z0-9_-]*$", perfil))) {
    ruim("'perfil' \u{E9} o nome de um perfil de autor (letras, d\u{ED}gitos, '-', '_').")
  }

  d <- .td_modelo_dir(nome)
  if (!identical(.td_origem(d), "usuario")) {
    tr_documento_modelo_duplicar(nome, nome)
    d <- .td_dir_usuario("modelos", nome)
  }
  ext <- .td_ext_ler(d)
  meta <- .td_meta_ler(d)
  brand <- .td_brand_ler(d)
  if (!is.null(titulo)) ext$title <- as.character(titulo)
  if (!is.null(descricao)) meta$descricao <- if (nzchar(descricao)) as.character(descricao)
  if (!is.null(perfil)) meta$perfil <- if (!isFALSE(perfil)) perfil
  if (!is.null(formatos)) {
    fmts <- ext$contributes$formats %||% list()
    desl <- meta$formatos_desligados %||% list()
    for (f in setdiff(setdiff(names(fmts), "common"), formatos)) {
      desl[f] <- list(.td_mapa(fmts[[f]]))
      fmts[[f]] <- NULL
    }
    for (f in setdiff(formatos, names(fmts))) {
      fmts[f] <- list(.td_mapa(desl[[f]] %||% .td_formato_padrao(f)))
      desl[[f]] <- NULL
    }
    # Ordem pedida, `common` na frente.
    ext$contributes$formats <- fmts[intersect(c("common", formatos), names(fmts))]
    meta$formatos_desligados <- if (length(desl)) desl
  }
  comum <- ext$contributes$formats$common %||% list()
  if (!is.null(capa)) comum$capa <- capa
  if (!is.null(csl)) {
    if (isFALSE(csl)) comum$csl <- NULL else {
      .td_copiar_arquivo(csl, file.path(d, basename(csl)))
      comum$csl <- basename(csl)
    }
  }
  ext$contributes$formats$common <- if (length(comum)) comum
  # Citações do PDF pelo Pandoc: com o CSL da extensão, o Typst recebe o
  # caminho escapado ("\_extensions/...") e não acha o arquivo. Vale também
  # quando o PDF é ligado depois do CSL.
  if (!is.null(comum$csl) && !is.null(ext$contributes$formats$typst)) {
    ext$contributes$formats$typst <- .td_mapa(ext$contributes$formats$typst)
    ext$contributes$formats$typst$citeproc <- TRUE
  }
  if (length(cores)) {
    brand$color <- brand$color %||% list()
    if (!is.list(brand$color)) brand$color <- list()
    for (k in names(cores)) {
      chave <- .TD_CORES[[k]]
      atual <- brand$color[[chave]]
      # A cor era um nome da paleta (`primary: azul-yucca`): muda a paleta, e
      # tudo que usava esse nome (cor dos títulos, por exemplo) acompanha.
      if (is.character(atual) && length(atual) == 1L && is.list(brand$color$palette) && !is.null(brand$color$palette[[atual]])) {
        brand$color$palette[[atual]] <- cores[[k]]
      } else {
        brand$color[[chave]] <- cores[[k]]
      }
    }
  }
  if (length(fontes)) {
    if (!is.list(brand$typography)) brand$typography <- list()
    for (k in names(fontes)) brand$typography[[.TD_FONTES[[k]]]] <- list(family = fontes[[k]])
  }
  if (!is.null(logo)) {
    velho <- .td_brand_logo(brand)
    if (isFALSE(logo)) brand$logo <- NULL else {
      arq <- paste0("logo.", tolower(tools::file_ext(logo)))
      .td_copiar_arquivo(logo, file.path(d, arq))
      brand$logo <- list(medium = arq)
    }
    if (!is.null(velho) && !identical(velho, .td_brand_logo(brand)) && grepl("^logo\\.", velho)) {
      unlink(file.path(d, velho))
    }
  }
  .td_yaml_gravar(ext, file.path(d, "_extension.yml"))
  if (length(brand)) .td_yaml_gravar(brand, file.path(d, "_brand.yml"))
  meta <- Filter(Negate(is.null), meta)
  if (length(meta)) .td_yaml_gravar(meta, file.path(d, .TD_META_MODELO)) else unlink(file.path(d, .TD_META_MODELO))
  invisible(tr_documento_modelo(nome))
}

#' @rdname tr_documento_modelo_criar
#' @export
tr_documento_modelo_remover <- function(nome) {
  d <- .td_dir_usuario("modelos", nome)
  if (!.td_nome_valido(nome) || !file.exists(file.path(d, "_extension.yml"))) {
    if (nzchar(system.file("modelos", nome, package = "trama.documentos"))) {
      .td_erro(sprintf("'%s' \u{E9} embutido: n\u{E3}o se remove (duplique e edite a c\u{F3}pia).", nome),
               "tr_documento_erro_modelo")
    }
    .td_erro(sprintf("N\u{E3}o h\u{E1} modelo do usu\u{E1}rio '%s'.", nome), "tr_documento_erro_modelo")
  }
  unlink(d, recursive = TRUE)
  invisible(nome)
}

#' Exporta e importa um modelo como zip
#'
#' O zip leva a pasta do modelo (`<nome>/_extension.yml`, …). Importar aceita
#' esse formato, a pasta solta na raiz do zip e o formato das extensões Quarto
#' publicadas (`_extensions/<nome>/` com o `template.qmd` na raiz). O modelo é
#' conferido antes de entrar: zip inválido não deixa nada na galeria.
#'
#' @param nome Nome do modelo. Na importação, `NULL` usa o da pasta do zip.
#' @param arquivo Caminho do zip. Na exportação, `NULL` grava `<nome>.zip` na
#'   pasta atual.
#' @param substituir Importar por cima de um modelo do usuário de mesmo nome.
#' @return O caminho do zip (exportar) ou, invisível, o modelo importado
#'   ([tr_documento_modelo()]).
#' @export
tr_documento_modelo_exportar <- function(nome, arquivo = NULL) {
  d <- .td_modelo_dir(nome)
  arquivo <- arquivo %||% file.path(getwd(), paste0(nome, ".zip"))
  if (!grepl("\\.zip$", arquivo, ignore.case = TRUE)) arquivo <- paste0(arquivo, ".zip")
  base <- tempfile("modelo-zip-")
  dir.create(base)
  on.exit(unlink(base, recursive = TRUE))
  .td_copiar_pasta(d, file.path(base, nome))
  if (file.exists(arquivo)) unlink(arquivo)
  zip::zip(normalizePath(arquivo, mustWork = FALSE), nome, root = base, mode = "cherry-pick")
  normalizePath(arquivo)
}

#' @rdname tr_documento_modelo_exportar
#' @export
tr_documento_modelo_importar <- function(arquivo, nome = NULL, substituir = FALSE) {
  ruim <- function(msg) .td_erro(msg, "tr_documento_erro_modelo")
  if (!is.character(arquivo) || length(arquivo) != 1L || !file.exists(arquivo)) ruim(sprintf("N\u{E3}o h\u{E1} o arquivo '%s'.", arquivo))
  lista <- tryCatch(utils::unzip(arquivo, list = TRUE), error = function(e) NULL, warning = function(w) NULL)
  if (is.null(lista)) ruim(sprintf("'%s' n\u{E3}o \u{E9} um zip.", basename(arquivo)))
  if (any(grepl("^(/|[A-Za-z]:)|(^|/)\\.\\.(/|$)|\\\\", lista$Name))) ruim("O zip tem caminhos que saem da pasta: recusado.")
  if (sum(as.numeric(lista$Length)) > .TD_MAX_ZIP) ruim("O zip descompactado passa de 100 MB: grande demais para um modelo.")
  tmp <- tempfile("modelo-importar-")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  # O unzip interno do R não recria links simbólicos (o Info-ZIP do sistema
  # recria, e um link para ~/.ssh iria parar no documento publicado). E,
  # mesmo assim, link que aparecer é recusado.
  utils::unzip(arquivo, exdir = tmp, unzip = "internal")
  if (length(.td_links(tmp))) ruim("O zip tem links simb\u{F3}licos: recusado.")
  exts <- list.files(tmp, pattern = "^_extension\\.ya?ml$", recursive = TRUE, all.files = TRUE)
  # Até `<repo>-main/_extensions/<org>/<nome>/_extension.yml`, o zip que o
  # GitHub baixa de uma extensão publicada.
  exts <- exts[lengths(strsplit(exts, "/")) <= 5L]
  if (length(exts) != 1L) {
    ruim(if (!length(exts)) "O zip n\u{E3}o tem _extension.yml: n\u{E3}o \u{E9} um modelo." else
      "O zip tem mais de um _extension.yml: importe um modelo por vez.")
  }
  raiz <- dirname(file.path(tmp, exts))
  if (basename(exts) == "_extension.yaml") file.rename(file.path(tmp, exts), file.path(raiz, "_extension.yml"))
  if (!file.exists(file.path(raiz, "template.qmd"))) {
    # Extensão Quarto publicada: o esqueleto fica na raiz do repositório,
    # acima de `_extensions/`. Vale o mais próximo.
    acima <- character()
    d <- raiz
    while (!identical(normalizePath(d), normalizePath(tmp))) {
      d <- dirname(d)
      acima <- c(acima, file.path(d, "template.qmd"))
    }
    solto <- acima[file.exists(acima)]
    if (length(solto)) file.copy(solto[[1]], raiz) else writeLines(c("---", "title: Documento", "---", ""), file.path(raiz, "template.qmd"))
  }
  nome <- nome %||% .td_nome_limpo(
    if (identical(normalizePath(raiz), normalizePath(tmp))) tools::file_path_sans_ext(basename(arquivo)) else basename(raiz))
  .td_nome_modelo(nome)
  probs <- .td_modelo_problemas(raiz)
  if (length(probs)) ruim(paste(c(sprintf("O modelo '%s' tem problemas e n\u{E3}o foi importado:", nome), paste("-", probs)), collapse = "\n"))
  destino <- .td_dir_usuario("modelos", nome)
  if (file.exists(file.path(destino, "_extension.yml")) && !isTRUE(substituir)) {
    .td_erro(sprintf("J\u{E1} existe o modelo '%s'. Use substituir = TRUE ou outro nome.", nome),
             c("tr_documento_erro_modelo_existe", "tr_documento_erro_modelo"))
  }
  .td_instalar_pasta(raiz, destino)
  invisible(tr_documento_modelo(nome))
}

# ---- leitura e validação -------------------------------------------------------------

#' Pasta do modelo: a do usuário vence a embutida.
#' @noRd
.td_modelo_dir <- function(nome) {
  if (.td_nome_valido(nome)) {
    for (d in c(.td_dir_usuario("modelos", nome), system.file("modelos", nome, package = "trama.documentos"))) {
      if (nzchar(d) && file.exists(file.path(d, "_extension.yml"))) return(d)
    }
  }
  .td_erro(sprintf("N\u{E3}o h\u{E1} modelo '%s'. Modelos: %s.", nome, paste(tr_documento_modelos()$nome, collapse = ", ")),
           "tr_documento_erro_modelo")
}

.td_origem <- function(d) if (startsWith(normalizePath(d), normalizePath(.td_dir_usuario(), mustWork = FALSE))) "usuario" else "embutido"

.td_sobre_embutido <- function(d) {
  identical(.td_origem(d), "usuario") && nzchar(system.file("modelos", basename(d), package = "trama.documentos"))
}

# Nome de formato do Quarto não serve de nome de modelo: `dashboard-dashboard`
# o Quarto não resolve como formato de extensão (por isso o embutido é
# `painel`).
.TD_NOMES_RESERVADOS <- c("html", "pdf", "docx", "typst", "revealjs", "dashboard", "beamer", "pptx", "latex",
                          "odt", "epub", "gfm", "markdown", "commonmark", "jats", "ipynb", "rtf", "context")

.td_nome_valido <- function(nome) {
  is.character(nome) && length(nome) == 1L && !is.na(nome) && grepl("^[a-z0-9][a-z0-9-]*$", nome) &&
    !nome %in% .TD_NOMES_RESERVADOS
}

.td_nome_modelo <- function(nome) {
  if (!.td_nome_valido(nome)) {
    .td_erro(sprintf(paste("Nome de modelo '%s' inv\u{E1}lido: letras min\u{FA}sculas, d\u{ED}gitos e '-' (ex.: 'meu-relatorio'),",
                           "e n\u{E3}o o nome de um formato do Quarto (html, docx, dashboard...)."),
                     paste(nome, collapse = " ")), "tr_documento_erro_modelo")
  }
  invisible(nome)
}

.td_nome_limpo <- function(x) {
  x <- tolower(trama::tr_sem_acento(x))
  x <- gsub("^-+|-+$", "", gsub("[^a-z0-9]+", "-", x))
  if (nzchar(x)) x else "modelo"
}

#' O que dá para ler de um modelo, sem errar: modelo quebrado volta com
#' `problemas`.
#' @noRd
.td_modelo_ler <- function(d) {
  # Antes do tryCatch: se `d` é uma chamada que erra (modelo que não existe),
  # o erro sobe em vez de ser engolido como "YAML ilegível".
  force(d)
  problemas <- tryCatch(.td_modelo_problemas(d), error = function(e) {
    sprintf("o modelo n\u{E3}o p\u{F4}de ser lido (%s)", conditionMessage(e))
  })
  ext <- tryCatch(.td_ext_ler(d), error = function(e) list())
  meta <- tryCatch(.td_meta_ler(d), error = function(e) list())
  formatos <- setdiff(names(ext$contributes$formats), "common")
  list(nome = basename(d), dir = d, titulo = as.character(ext$title %||% basename(d))[[1]],
       descricao = if (is.character(meta$descricao)) meta$descricao[[1]] else NA_character_,
       perfil = if (is.character(meta$perfil)) meta$perfil[[1]],
       formatos = formatos %||% character(), ext = ext, meta = meta, problemas = problemas)
}

#' O que impede o modelo de gerar documento, em frases para quem vai
#' consertar. Vazio: válido.
#' @noRd
.td_modelo_problemas <- function(d) {
  p <- character()
  ler <- function(arq) tryCatch(list(ok = yaml::read_yaml(file.path(d, arq))), error = function(e) {
    p <<- c(p, sprintf("%s n\u{E3}o \u{E9} YAML v\u{E1}lido: %s", arq, conditionMessage(e)))
    NULL
  })
  if (!file.exists(file.path(d, "_extension.yml"))) return("falta o _extension.yml")
  ext <- ler("_extension.yml")$ok
  if (!is.null(ext)) {
    fmts <- if (is.list(ext)) ext$contributes$formats
    if (!is.list(fmts) || !length(setdiff(names(fmts), "common"))) {
      p <- c(p, "o _extension.yml n\u{E3}o declara formatos (contributes: formats:)")
    } else {
      for (f in names(fmts)) {
        # `html: default` o Quarto aceita.
        if (identical(fmts[[f]], "default")) next
        if (!is.null(fmts[[f]]) && !is.list(fmts[[f]])) {
          p <- c(p, sprintf("o formato '%s' do _extension.yml n\u{E3}o \u{E9} uma lista de op\u{E7}\u{F5}es", f))
          next
        }
        for (op in intersect(names(fmts[[f]]), .TD_OPCOES_ARQUIVO)) {
          v <- fmts[[f]][[op]]
          if (!is.list(v) || !is.null(names(v))) v <- list(v)
          for (a in v) {
            # `{path: x.lua, at: pre}` aponta o arquivo em `path`; `{text: ...}` não é arquivo.
            if (is.list(a)) a <- a$path
            if (!is.character(a) || length(a) != 1L || grepl("^(https?:|\\$|~)", a)) next
            if (op == "filters" && a %in% c("quarto", "citeproc")) next
            # Temas de nome (`theme: cosmo`) não são arquivo.
            if (op == "theme" && !grepl("\\.(s?css)$", a)) next
            if (!file.exists(file.path(d, a))) p <- c(p, sprintf("'%s' (%s, formato %s) n\u{E3}o est\u{E1} na pasta do modelo", a, op, f))
          }
        }
      }
    }
  }
  if (file.exists(file.path(d, "_brand.yml"))) {
    brand <- ler("_brand.yml")$ok
    if (!is.null(brand) && !is.list(brand)) p <- c(p, "o _brand.yml n\u{E3}o \u{E9} um mapa (chave: valor)")
    if (is.list(brand) && !is.null(brand$color) && !is.list(brand$color)) p <- c(p, "'color' do _brand.yml n\u{E3}o \u{E9} um mapa")
    if (is.list(brand) && is.list(brand$color)) {
      pal <- if (is.list(brand$color$palette)) names(brand$color$palette)
      for (k in setdiff(names(brand$color), "palette")) {
        v <- brand$color[[k]]
        if (!is.character(v) || length(v) != 1L || !(grepl("^#([0-9A-Fa-f]{3}|[0-9A-Fa-f]{6})$", v) || v %in% pal || grepl("^[a-z]+$", v))) {
          p <- c(p, sprintf("cor '%s' do _brand.yml inv\u{E1}lida", k))
        }
      }
      logo <- .td_brand_logo(brand)
      if (!is.null(logo) && !file.exists(file.path(d, logo))) p <- c(p, sprintf("o logo '%s' n\u{E3}o est\u{E1} na pasta do modelo", logo))
    }
  }
  if (!file.exists(file.path(d, "template.qmd"))) {
    p <- c(p, "falta o template.qmd (o esqueleto do documento)")
  } else {
    l <- readLines(file.path(d, "template.qmd"), warn = FALSE, encoding = "UTF-8")
    f <- .td_fim_yaml(l)
    if (f >= 2L && is.null(tryCatch(yaml::yaml.load(paste(l[2:(f - 1L)], collapse = "\n")) %||% list(), error = function(e) NULL))) {
      p <- c(p, "o YAML do template.qmd n\u{E3}o \u{E9} v\u{E1}lido")
    }
  }
  if (file.exists(file.path(d, .TD_META_MODELO))) {
    meta <- ler(.TD_META_MODELO)$ok
    if (!is.null(meta$perfil) && !(is.character(meta$perfil) && length(meta$perfil) == 1L)) {
      p <- c(p, sprintf("'perfil' em %s \u{E9} o nome de um perfil", .TD_META_MODELO))
    }
  }
  p
}

#' YAML lido com lista que continua lista: o `read_yaml` simplifica
#' `filters: [abnt.lua]` para um texto, que regravado vira escalar, e o
#' Quarto quebra (`"abnt.lua".flatMap`).
#' @noRd
.td_yaml_ler <- function(arquivo) yaml::read_yaml(arquivo, handlers = list(seq = function(x) as.list(x)))

.td_ext_ler <- function(d) {
  ext <- .td_yaml_ler(file.path(d, "_extension.yml"))
  if (!is.list(ext)) ext <- list()
  ext
}

.td_meta_ler <- function(d) {
  f <- file.path(d, .TD_META_MODELO)
  m <- if (file.exists(f)) .td_yaml_ler(f) else list()
  if (is.list(m)) m else list()
}

.td_brand_ler <- function(d) {
  f <- file.path(d, "_brand.yml")
  b <- if (file.exists(f)) tryCatch(.td_yaml_ler(f), error = function(e) list()) else list()
  if (is.list(b)) b else list()
}

#' Arquivo do logo no `_brand.yml` (forma curta, `medium`, `small`, `large`
#' ou a primeira de `images`).
#' @noRd
.td_brand_logo <- function(brand) {
  l <- if (is.list(brand)) brand$logo
  if (is.null(l)) return(NULL)
  caminho <- function(x) {
    if (is.list(x)) x <- x$path
    if (is.character(x) && length(x) == 1L) x
  }
  if (is.character(l)) return(l[[1]])
  if (!is.list(l)) return(NULL)
  imagens <- if (is.list(l$images)) l$images else list()
  for (k in c("medium", "large", "small")) {
    v <- l[[k]]
    if (is.list(v)) v <- v$light %||% v$path
    if (is.character(v) && length(v) == 1L) return(caminho(imagens[[v]]) %||% v)
  }
  for (im in imagens) if (!is.null(caminho(im))) return(caminho(im))
  NULL
}

#' Cor do `_brand.yml` já resolvida (nome da paleta vira o `#rrggbb`).
#' @noRd
.td_brand_cor <- function(brand, chave) {
  if (!is.list(brand) || !is.list(brand$color)) return(NULL)
  v <- brand$color[[chave]]
  pal <- if (is.list(brand$color$palette)) brand$color$palette else list()
  for (i in 1:3) if (is.character(v) && length(v) == 1L && !is.null(pal[[v]])) v <- pal[[v]]
  if (!is.character(v) || length(v) != 1L) return(NULL)
  if (grepl("^#[0-9A-Fa-f]{3}$", v)) v <- paste0("#", paste(rep(strsplit(substring(v, 2), "")[[1]], each = 2), collapse = ""))
  if (grepl("^#[0-9A-Fa-f]{6}$", v)) return(tolower(v))
  switch(v, white = "#ffffff", black = "#000000", NULL)
}

.td_brand_cores <- function(brand) {
  Filter(Negate(is.null), lapply(.TD_CORES, function(k) .td_brand_cor(brand, k)))
}

.td_brand_fontes <- function(brand) {
  if (!is.list(brand) || !is.list(brand$typography)) return(list())
  Filter(Negate(is.null), lapply(.TD_FONTES, function(k) {
    v <- brand$typography[[k]]
    if (is.list(v)) v <- v$family
    if (is.character(v)) v[[1]]
  }))
}

# ---- escrita -------------------------------------------------------------------------

.TD_MAX_ZIP <- 100 * 1024^2

#' Links simbólicos dentro da pasta (caminhos relativos), achados nível a
#' nível sem entrar em pasta que é link.
#' @noRd
.td_links <- function(pasta) {
  achados <- character()
  fila <- ""
  while (length(fila)) {
    rel <- fila[[1]]
    fila <- fila[-1]
    for (n in list.files(file.path(pasta, rel), all.files = TRUE, no.. = TRUE)) {
      r <- if (nzchar(rel)) file.path(rel, n) else n
      if (nzchar(Sys.readlink(file.path(pasta, r)))) {
        achados <- c(achados, r)
      } else if (dir.exists(file.path(pasta, r))) {
        fila <- c(fila, r)
      }
    }
  }
  achados
}

#' Instala uma pasta de modelo: copia ao lado e troca com renomeação, para
#' cópia interrompida não deixar pasta pela metade.
#' @noRd
.td_instalar_pasta <- function(de, destino) {
  dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
  nova <- tempfile(paste0(".", basename(destino), "-"), tmpdir = dirname(destino))
  .td_copiar_pasta(de, nova)
  if (dir.exists(destino)) unlink(destino, recursive = TRUE)
  if (!file.rename(nova, destino)) {
    unlink(nova, recursive = TRUE)
    .td_erro(sprintf("N\u{E3}o foi poss\u{ED}vel gravar o modelo em '%s'.", destino), "tr_documento_erro_modelo")
  }
  invisible(destino)
}

#' Copia um arquivo, sem errar se é o mesmo arquivo.
#' @noRd
.td_copiar_arquivo <- function(de, para) {
  if (file.exists(para) && identical(normalizePath(de), normalizePath(para))) return(invisible(para))
  if (!file.copy(de, para, overwrite = TRUE)) .td_erro(sprintf("N\u{E3}o foi poss\u{ED}vel copiar '%s'.", de), "tr_documento_erro_modelo")
  invisible(para)
}

#' Mapa YAML: lista vazia ou `default` vira `{}` (e não `[]`).
#' @noRd
.td_mapa <- function(x) {
  if (is.null(x) || identical(x, "default") || !length(x)) return(structure(list(), names = character()))
  x
}

#' YAML que o Quarto lê igual: lógico sai `true`/`false` (o `yes`/`no` do
#' pacote yaml é texto para o Quarto).
#' @noRd
.td_yaml_texto <- function(x) {
  logico <- function(v) structure(ifelse(v, "true", "false"), class = "verbatim")
  sub("\n$", "", yaml::as.yaml(x, indent.mapping.sequence = TRUE, handlers = list(logical = logico)))
}

.td_yaml_gravar <- function(x, arquivo) {
  writeLines(enc2utf8(.td_yaml_texto(x)), arquivo, useBytes = TRUE)
  invisible(arquivo)
}

#' Copia a pasta sem seguir link simbólico (link fica de fora).
#' @noRd
.td_copiar_pasta <- function(de, para) {
  dir.create(para, recursive = TRUE, showWarnings = FALSE)
  links <- .td_links(de)
  for (a in setdiff(list.files(de, all.files = TRUE, no.. = TRUE, recursive = TRUE), links)) {
    if (any(startsWith(a, paste0(links, "/")))) next
    dir.create(dirname(file.path(para, a)), recursive = TRUE, showWarnings = FALSE)
    file.copy(file.path(de, a), file.path(para, a), overwrite = TRUE)
  }
  invisible(para)
}

.td_conferir_nomeados <- function(x, nomes, campo, conferir) {
  if (is.null(x)) return(NULL)
  x <- as.list(x)
  if (!length(x) || is.null(names(x)) || !all(names(x) %in% nomes)) {
    .td_erro(sprintf("'%s' \u{E9} lista nomeada com %s.", campo, paste(nomes, collapse = ", ")), "tr_documento_erro_modelo")
  }
  for (v in x) {
    if (!is.character(v) || length(v) != 1L || is.na(v)) .td_erro(sprintf("Valor de '%s' \u{E9} texto.", campo), "tr_documento_erro_modelo")
    conferir(v)
  }
  x
}

#' Configuração de partida de um formato religado ou novo.
#' @noRd
.td_formato_padrao <- function(f) {
  switch(f,
    html = list(toc = TRUE, `toc-title` = "Sum\u{E1}rio", `embed-resources` = FALSE),
    typst = list(papersize = "a4", fontsize = "11pt", margin = list(x = "2.5cm", y = "2.5cm")),
    docx = list(toc = FALSE),
    revealjs = list(`slide-number` = TRUE),
    dashboard = list(`embed-resources` = FALSE),
    list())
}
