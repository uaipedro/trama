# Proveniência das linhas: o isolamento do teste imposto por construção.
#
# O `ml/split` marca cada saída com o atributo `tr_ml_origem` = list(papel =
# "treino" | "teste", divisao = <hash>, colunas, teste). Escolhemos atributo, e
# não coluna oculta, porque é o menos invasivo que sobrevive a tudo que o fluxo
# faz com a tabela: o tipo `data/table` guarda em RDS (atributos preservados na
# ida e volta do store e na releitura do cache), e subconjunto, `dplyr::filter`
# e `mutate` reconstroem a tabela copiando os atributos. Uma coluna apareceria
# no card, na exportação e em `cols` vazio; o atributo não muda os dados.
#
# Impressões digitais por linha. A marca de papel sozinha se perde quando a
# tabela é montada de novo (juntar treino e teste fica com o atributo da
# primeira; tirar o atributo; ajustar antes de dividir). Por isso a marca
# também leva a impressão de cada linha do teste: o xxHash64 (`cli`) do
# conteúdo da linha nas `colunas` da divisão (todas as colunas da entrada, em
# ordem de nome; números por seu valor binário exato em `%a`, inteiro e real
# iguais; fator como texto), guardada como multiconjunto — cada impressão
# distinta do teste com `n_teste` (cópias no teste) e `n_treino` (cópias
# idênticas que o sorteio pôs no treino). Contar cópias é o que separa uma
# linha repetida legítima (a mesma medida nos dois lados, como as linhas 102 e
# 143 do iris) de uma linha do teste que voltou ao treino. Todo modelo guarda
# o multiconjunto das impressões das suas linhas de treino, qualquer que seja
# a marca da entrada (`treino_impressoes`).
#
# Custo (medido em 100 mil linhas, 3 colunas, notebook 2024): ~0,5 s para
# formatar e ~0,2 s para o hash vetorizado (`rlang::hash` linha a linha levava
# 2,6 s e foi descartado); cada impressão distinta ocupa ~88 bytes (texto de 16
# hexadecimais), ~9 MB por 100 mil linhas. O custo é pago no `ml/split`, uma
# vez por ajuste final e na checagem de ajuste, previsão e avaliação; os folds
# internos de `ml/tune` e `ml/nested_cv` não recalculam.
#
# Regras:
# - ajustar (`ml/<modelo>`, `ml/tune`, `ml/nested_cv`) ou dividir de novo com
#   linhas "teste" é erro `tr_ml_error_test_leak`; também quando a tabela,
#   marcada por uma divisão, tem mais cópias de uma impressão do teste do que
#   essa divisão pôs no treino (treino e teste juntados, B2);
# - `models/predict` (via `tr_models_predict_raw.tr_ml_fit`, na integração
#   9.1b; antes `ml/predict`) recusa (`tr_ml_error_test_leak`) prever linhas do teste com um
#   modelo que as viu no ajuste: nas colunas da divisão, o modelo tem mais
#   cópias da impressão do que a divisão pôs no treino (ajuste na tabela
#   inteira ou numa cópia sem marca do teste, B1). Se o modelo foi ajustado sem
#   marca e com outras colunas, compara nas colunas do modelo, por presença;
# - `models/predict` propaga a marca e recusa prever o teste de OUTRA divisão com um
#   modelo ajustado no treino de uma divisão marcada: `tr_ml_error_split_mismatch`;
# - avaliar (`models/evaluate`, `models/confusion`, `models/roc`,
#   `models/pr_curve`, antes os `ml/*`, pelo `.tr_models_checar_avaliacao`) linhas
#   "treino", ou uma tabela marcada como teste com linhas que não são do teste
#   (previsões do treino juntadas às do teste, ou o teste repetido, B4), é erro
#   `tr_ml_error_train_eval`, salvo `permitir_treino = TRUE`, que devolve o
#   resultado com a nota de otimismo e um aviso.
#
# O que a marca NÃO cobre (B3), por construção: o atributo só viaja quando a
# operação copia os atributos da tabela que o leva, e a impressão só confere
# quando as colunas da divisão estão presentes com os mesmos valores.
# - juntar pela direita: `tr_join(outra, teste)`/`left_join(outra, teste)`
#   ficam com o atributo de `outra` (sem marca); o resultado é tabela nova;
# - remodelar: `pivot_longer`/`pivot_wider` mudam o que é uma linha e as
#   colunas; a marca some ou a impressão deixa de conferir;
# - recriar à mão: copiar valores para uma tabela nova (`data.frame(...)`,
#   planilha exportada e lida de volta, `as.data.frame` de outra origem);
# - mudar valores ou tirar colunas da divisão (`mutate` que reescreve uma
#   coluna existente, arredondar, converter para texto, `select` que remove
#   uma coluna): as impressões não conferem e a checagem de conteúdo é
#   pulada (a de papel continua);
# - linhas idênticas dentro de uma tabela sem marca e o teste, quando o modelo
#   foi ajustado sem marca e em outras colunas (comparação por presença);
# - dividir fora do `ml/split`.
# Nesses casos os blocos se comportam como sem marca; os pressupostos da
# documentação explicam a disciplina que continua sendo do usuário.

.tr_ml_origem_attr <- "tr_ml_origem"

.tr_ml_marcar <- function(x, papel, divisao, colunas = NULL, teste = NULL) {
  attr(x, .tr_ml_origem_attr) <- list(papel = papel, divisao = divisao,
                                      colunas = colunas, teste = teste)
  x
}

.tr_ml_origem <- function(x) {
  o <- attr(x, .tr_ml_origem_attr, exact = TRUE)
  if (is.list(o) && length(o$papel) == 1L && o$papel %in% c("treino", "teste")) o else NULL
}

# Só papel e divisão, para guardar no modelo sem copiar as impressões.
.tr_ml_origem_curta <- function(x) {
  o <- .tr_ml_origem(x)
  if (is.null(o)) NULL else list(papel = o$papel, divisao = o$divisao)
}

# ---- impressões digitais por linha -----------------------------------------

.tr_ml_colunas_canonicas <- function(nomes) sort(unique(nomes), method = "radix")

# Texto canônico de cada célula: números pelo valor binário exato (`%a`), com
# inteiro e real iguais; datas pelo número subjacente; fator como texto.
.tr_ml_celulas <- function(v) {
  if (inherits(v, "POSIXlt")) v <- as.POSIXct(v)
  if (is.factor(v)) v <- as.character(v)
  u <- unclass(v)
  if (is.list(u)) return(vapply(u, function(e) rlang::hash(e), ""))
  if (is.numeric(u)) {
    s <- sprintf("%a", as.double(u)); s[is.na(u)] <- "\u{1E}NA"; return(s)
  }
  s <- as.character(u); s[is.na(u)] <- "\u{1E}NA"
  paste0("t", s)
}

# xxHash64 de cada linha nas `colunas` (NULL se alguma faltar).
.tr_ml_impressoes <- function(x, colunas) {
  if (is.null(colunas) || !all(colunas %in% names(x))) return(NULL)
  if (!nrow(x)) return(character())
  partes <- lapply(unname(as.list(x)[colunas]), .tr_ml_celulas)
  cli::hash_xxhash64(do.call(paste, c(partes, sep = "\u{1F}")))
}

# Multiconjunto: impressões distintas e suas contagens.
.tr_ml_multiconjunto <- function(f) {
  u <- unique(f)
  list(impressao = u, n = tabulate(match(f, u), length(u)))
}

.tr_ml_contar <- function(mc, f) {
  n <- mc$n[match(f, mc$impressao)]
  n[is.na(n)] <- 0L
  n
}

# Impressões do treino de um modelo: nas colunas da divisão quando a entrada
# é marcada e as tem, senão em todas as colunas da entrada.
# `ja` reaproveita as impressões calculadas na checagem da entrada.
.tr_ml_impressoes_treino <- function(dados, ja = NULL) {
  if (isTRUE(.tr_ml_estado$sem_impressao)) return(NULL)
  o <- .tr_ml_origem(dados)
  colunas <- if (!is.null(o$colunas) && all(o$colunas %in% names(dados))) o$colunas else
    .tr_ml_colunas_canonicas(names(dados))
  f <- if (identical(ja$colunas, colunas)) ja$f else .tr_ml_impressoes(dados, colunas)
  c(list(colunas = colunas), .tr_ml_multiconjunto(f))
}

# Estado interno: os folds de `ml/tune` e `ml/nested_cv` ajustam sem calcular
# impressões (a entrada já foi checada uma vez e o ajuste final as calcula).
.tr_ml_estado <- new.env(parent = emptyenv())

.tr_ml_sem_impressao <- function(code) {
  antes <- .tr_ml_estado$sem_impressao
  .tr_ml_estado$sem_impressao <- TRUE
  on.exit(.tr_ml_estado$sem_impressao <- antes, add = TRUE)
  force(code)
}

# Linhas da tabela que só podem ter vindo do teste da divisão marcada: cópias
# de uma impressão do teste além das que a divisão pôs no treino. NA quando a
# tabela não traz a divisão ou perdeu colunas dela.
.tr_ml_excesso_de_teste <- function(x) {
  o <- .tr_ml_origem(x)
  if (is.null(o$teste)) return(structure(NA_integer_, impressoes = NULL))
  f <- .tr_ml_impressoes(x, o$colunas)
  if (is.null(f)) return(structure(NA_integer_, impressoes = NULL))
  mc <- .tr_ml_multiconjunto(f)
  i <- match(mc$impressao, o$teste$impressao); ok <- !is.na(i)
  structure(as.integer(sum(pmax(0L, mc$n[ok] - o$teste$n_treino[i[ok]]))),
            impressoes = list(colunas = o$colunas, f = f))
}

# Linhas de uma tabela marcada como teste que não são do teste: impressão fora
# do teste ou mais cópias do que o teste tem.
.tr_ml_fora_do_teste <- function(x) {
  o <- .tr_ml_origem(x)
  if (!identical(o$papel, "teste") || is.null(o$teste)) return(0L)
  f <- .tr_ml_impressoes(x, o$colunas)
  if (is.null(f)) return(0L)
  mc <- .tr_ml_multiconjunto(f)
  limite <- o$teste$n_teste[match(mc$impressao, o$teste$impressao)]
  limite[is.na(limite)] <- 0L
  as.integer(sum(pmax(0L, mc$n - limite)))
}

# Linhas do teste marcado que o modelo viu no ajuste.
.tr_ml_teste_no_treino <- function(modelo, dados) {
  o <- .tr_ml_origem(dados); mt <- modelo$treino_impressoes
  if (!identical(o$papel, "teste") || is.null(o$teste) || is.null(mt)) return(0L)
  if (identical(mt$colunas, o$colunas)) {
    f <- .tr_ml_impressoes(dados, o$colunas)
    if (is.null(f)) return(0L)
    u <- unique(f)
    legit <- o$teste$n_treino[match(u, o$teste$impressao)]
    legit[is.na(legit)] <- 0L
    return(as.integer(sum(.tr_ml_contar(mt, u) > legit)))
  }
  # Modelo ajustado sem marca e em outras colunas: compara nas colunas dele,
  # por presença (conservador quando há linhas idênticas).
  if (!is.null(modelo$origem)) return(0L)
  f <- .tr_ml_impressoes(dados, mt$colunas)
  if (is.null(f)) return(0L)
  as.integer(sum(.tr_ml_contar(mt, f) > 0L))
}

.tr_ml_papel <- function(x) .tr_ml_origem(x)$papel %||% NA_character_

# Identificador da divisão: depende só dos dados e das linhas sorteadas, então
# a mesma divisão refeita (ou relida do cache) tem o mesmo id.
.tr_ml_divisao_id <- function(dados, idx) {
  substr(rlang::hash(list(rlang::hash(as.data.frame(dados)), as.integer(idx))), 1L, 16L)
}

.tr_ml_exigir_nao_teste <- function(dados) {
  if (identical(.tr_ml_papel(dados), "teste")) {
    .tr_ml_abort("tr_ml_error_test_leak", paste(
      "Estas linhas s\u{E3}o a sa\u{ED}da teste do `ml/split`: ajustar nelas treina no teste",
      "e invalida toda avalia\u{E7}\u{E3}o. Ligue a sa\u{ED}da treino; o teste vai s\u{F3} ao `models/predict`."))
  }
  excesso <- .tr_ml_excesso_de_teste(dados)
  if (!is.na(excesso) && excesso > 0L) {
    .tr_ml_abort("tr_ml_error_test_leak", paste(
      "%d linha(s) desta tabela s\u{E3}o do teste do `ml/split` (treino e teste foram juntados):",
      "ajustar nelas treina no teste. Use s\u{F3} a sa\u{ED}da treino."), excesso)
  }
  invisible(attr(excesso, "impressoes"))
}

.tr_ml_nota_treino <- paste(
  "Avalia\u{E7}\u{E3}o no treino \u{E9} otimista: as linhas ajustaram o modelo e n\u{E3}o",
  "medem generaliza\u{E7}\u{E3}o. Reporte o desempenho no teste.")

# Devolve a nota (ou "") e recusa linhas de treino sem opt-in.
.tr_ml_checar_avaliacao <- function(dados, permitir_treino) {
  if (length(permitir_treino) != 1L || !is.logical(permitir_treino) || is.na(permitir_treino)) {
    .tr_ml_abort("tr_ml_error_bad_param", "Param 'permitir_treino' deve ser TRUE ou FALSE.")
  }
  papel <- .tr_ml_papel(dados)
  fora <- if (identical(papel, "teste")) .tr_ml_fora_do_teste(dados) else 0L
  if (!identical(papel, "treino") && fora == 0L) return("")
  if (!permitir_treino && fora > 0L) {
    .tr_ml_abort("tr_ml_error_train_eval", paste(
      "%d linha(s) desta tabela marcada como teste n\u{E3}o s\u{E3}o do teste do `ml/split`",
      "(previs\u{F5}es do treino juntadas \u{E0}s do teste, ou o teste repetido): a avalia\u{E7}\u{E3}o",
      "mistura linhas otimistas. Avalie s\u{F3} o teste, ou marque `permitir_treino`."), fora)
  }
  if (!permitir_treino) {
    .tr_ml_abort("tr_ml_error_train_eval", paste(
      "Estas previs\u{F5}es s\u{E3}o das linhas de treino do `ml/split`: a avalia\u{E7}\u{E3}o no treino",
      "\u{E9} otimista. Avalie a sa\u{ED}da teste passada pelo `models/predict`, ou marque",
      "`permitir_treino` para medir o ajuste no treino de prop\u{F3}sito."))
  }
  warning(.tr_ml_nota_treino, call. = FALSE)
  .tr_ml_nota_treino
}

# A tabela que o modelo guarda (`$dados`) serve à cruzada e ao "só modelo" dos
# avaliadores da models: sem a marca, para que prever o próprio treino não
# pareça um teste (integração 9.1b).
.tr_ml_sem_origem <- function(x) {
  attr(x, .tr_ml_origem_attr) <- NULL
  x
}
