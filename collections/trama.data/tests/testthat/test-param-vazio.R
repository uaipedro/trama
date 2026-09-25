# A DOUTRINA do param vazio, travada por varredura — nos moldes de
# `test-help.R`, e pelo mesmo motivo: nó novo nasce reprovado até se conformar.
#
# A doutrina: campo vazio é "DESLIGADO" quando o nó tem um comportamento
# honesto para o vazio (`tr_filter` sem condição não filtra; `tr_select` sem
# colunas passa a tabela adiante); campo vazio é ERRO quando não tem — não
# existe criar coluna sem nome, nem ler a planilha nenhuma de um `.xlsx`.
#
# O critério operacional é o DEFAULT DO SPEC: um param de texto livre que nasce
# preenchido nasce assim porque o nó precisa dele. Quem esvazia esse campo no
# card não está desligando nada — está deixando o card incompleto, e o nó tem
# de dizer isso, culpando o campo, com `tr_data_error_blank_param` em primeira
# ordem (é a classe que o motor grava).
#
# Antes desta varredura a doutrina estava escrita duas vezes e em desacordo, e
# o desacordo tinha custo observável: `tr_mutate(d, "", "valor*2")` devolvia a
# tabela intacta e VERDE enquanto o irmão `tr_group_summarise()` abortava.

# Como chamar cada nó de modo que ele CHEGUE à checagem. Não dá para usar só os
# defaults do spec: `tr_pivot_longer()` com `cols` vazio devolve a tabela antes
# de olhar para `names_to`, e o teste passaria sem testar nada.
args_validos <- function() list(
  "data/read_excel"      = list(path = "planilha.xlsx", sheet = "1"),
  "data/generate"        = list(n = 10L, expr = "x = rnorm(n)"),
  "data/mutate"          = list(dados = df_exemplo(), name = "nova",
                                expr = "valor * 2", by = ""),
  "data/pivot_longer"    = list(dados = df_exemplo(), cols = "valor, qtd",
                                names_to = "nome", values_to = "valor"),
  "data/group_summarise" = list(dados = df_exemplo(), by = "regiao",
                                name = "total", expr = "sum(valor)")
)

# Exceção é DECLARADA, com motivo, nunca silenciosa: cada uma destas é um campo
# que nasce preenchido E tem um comportamento honesto para o vazio.
excecoes <- function() c(
  "data/read_csv:na" =
    paste("célula vazia conta como faltante SEMPRE, então o campo vazio quer",
          "dizer 'nenhuma marca além dela' — que é o piso, não um card incompleto"),
  "data/replace_na:value" =
    paste("campo de preenchimento vazio é 'não preencha': o nó devolve a tabela",
          "como estava, que é exatamente o que a tela mostra")
)

test_that("todo param de texto com default não vazio recusa o branco, classificado", {
  reg <- data_registry()
  validos <- args_validos(); excecao <- excecoes()

  for (n in reg$nodes) {
    # Sem o pacote de Suggests, o nó aborta ANTES da checagem do branco, por
    # `tr_data_error_missing_package` — e não é isso que se está medindo.
    if (identical(n$id, "data/read_excel") && !requireNamespace("readxl", quietly = TRUE)) next

    for (nm in names(n$params)) {
      p <- n$params[[nm]]
      # Texto livre: `expr` e `text`. `cols` e `path` não entram porque nascem
      # vazios; `enum`, `boolean` e `integer` não são digitáveis.
      if (!p$kind %in% c("text", "expr")) next
      if (!is.character(p$default) || !nzchar(trimws(p$default))) next

      chave <- paste0(n$id, ":", nm)
      if (chave %in% names(excecao)) {
        expect_true(nzchar(excecao[[chave]]), info = chave)
        next
      }

      args <- validos[[n$id]]
      # Nó novo com campo assim nasce REPROVADO: ou entra na tabela de chamada,
      # ou é declarado exceção com o motivo. Não há terceira saída silenciosa.
      expect_true(!is.null(args),
                  info = paste0(chave, ": param de texto com default '", p$default,
                                "' e nenhum argumento válido declarado em ",
                                "args_validos() nem motivo em excecoes()"))
      if (is.null(args)) next

      args[[nm]] <- ""
      err <- tryCatch(do.call(n$fn, args), error = identity, warning = identity)
      expect_equal(class(err)[[1]], "tr_data_error_blank_param", info = chave)
      # E culpando o campo certo: o card tem vários, e "campo em branco" sem
      # dizer qual manda o usuário caçar.
      expect_match(conditionMessage(err), nm, fixed = TRUE, info = chave)
    }
  }
})

# A outra metade da doutrina, pelo lado do desligado: estes NÃO podem abortar.
# Sem isto, a varredura acima empurraria a coleção inteira para o "tudo é
# obrigatório", que faria todo card recém-arrastado da paleta nascer vermelho.
test_that("param que nasce vazio no spec continua sendo 'desligado'", {
  d <- df_exemplo()
  expect_identical(tr_filter(d, ""), d)
  expect_identical(tr_select(d, ""), d)
  expect_identical(tr_arrange(d, ""), d)
  expect_identical(tr_convert(d, ""), d)
  expect_identical(tr_replace_na(d, "valor", ""), d)
  # Gravador sem caminho não grava e repassa a tabela adiante.
  expect_identical(tr_write_csv(d, ""), d)
})

# O `na` do `data/read_csv` é exceção declarada acima — e é exceção porque o
# vazio ali tem significado, não porque ninguém olhou. Aqui está o significado.
test_that("read_csv sem marcas de faltante ainda lê a célula vazia como faltante", {
  p <- tempfile(fileext = ".csv"); on.exit(unlink(p), add = TRUE)
  writeLines(c("a,b", "1,x", "2,", "3,NA"), p)
  expect_equal(tr_read_csv(p, na = "")$b, c("x", NA, "NA"))
})
