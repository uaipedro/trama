test_that("a decisão sai do p-valor quando há, e do crítico na cauda de sentido quando não", {
  d <- function(...) .tr_series_teste("T", "h", ..., conclusao_sim = "s", conclusao_nao = "n",
                                      fonte = "—")$decisao_5
  # Com p-valor: comparação direta com 0,05.
  expect_equal(d(0, "z", p_valor = 0.03), "rejeita H0")
  expect_equal(d(0, "z", p_valor = 0.07), "não rejeita H0")
  # Sem p-valor, cauda de baixo (ADF): rejeita abaixo do crítico.
  cv <- c(`10%` = -2.57, `5%` = -2.88, `1%` = -3.46)
  expect_equal(d(-4.12, "t", criticos = cv, sentido = "menor"), "rejeita H0")
  expect_equal(d(-2.00, "t", criticos = cv, sentido = "menor"), "não rejeita H0")
  # Sem p-valor, cauda de cima (KPSS): rejeita ACIMA do crítico. É a
  # inversão que o campo `sentido` existe para carregar.
  kp <- c(`10%` = 0.347, `5%` = 0.463, `1%` = 0.739)
  expect_equal(d(0.60, "eta", criticos = kp, sentido = "maior"), "rejeita H0")
  expect_equal(d(0.20, "eta", criticos = kp, sentido = "maior"), "não rejeita H0")
})

test_that("o construtor monta o registro e escolhe a conclusão pela decisão", {
  t <- .tr_series_teste("ADF", "raiz unitária", -4.12, "t",
                        criticos = c(`10%` = -2.57, `5%` = -2.88, `1%` = -3.46),
                        sentido = "menor", conclusao_sim = "estacionária",
                        conclusao_nao = "não estacionária", fonte = "Dickey & Fuller (1979)")
  expect_s3_class(t, "tr_series_test")
  expect_equal(t$decisao_5, "rejeita H0")
  expect_equal(t$conclusao, "estacionária")
  expect_true(is.na(t$p_valor))
  expect_equal(names(t$criticos), c("10%", "5%", "1%"))
})

test_that("tabela de críticos sem o nível da decisão é erro tipado, não índice cru", {
  # Os críticos do `urca` não têm contrato entre um teste e outro, e os dois
  # casos abaixo são os que de fato chegam: o `ur.za` entrega SEM NOMES e em
  # ordem invertida, o `ur.kpss` entrega quatro colunas. Montar a tabela errado
  # tem de ser card vermelho — devolver NA em silêncio seria pior, porque o
  # `isTRUE()` do construtor viraria isso num "não rejeita H0" confiante.
  base <- list("Zivot-Andrews", "raiz unitária", -4.79, "t",
               sentido = "menor", conclusao_sim = "estacionária com quebra",
               conclusao_nao = "raiz unitária (com quebra)",
               fonte = "Zivot & Andrews (1992)")
  expect_error(do.call(.tr_series_teste, c(base, list(criticos = c(-5.57, -5.08, -4.82)))),
               class = "tr_series_error_bad_criticos")
  expect_error(do.call(.tr_series_teste, c(base, list(criticos = c(`10%` = -4.82)))),
               class = "tr_series_error_bad_criticos")
  # A mensagem nomeia o teste e o que chegou: é o que manda consertar.
  err <- tryCatch(do.call(.tr_series_teste, c(base, list(criticos = c(`10%` = -4.82)))),
                  condition = identity)
  expect_match(conditionMessage(err), "Zivot-Andrews", fixed = TRUE)
  expect_match(conditionMessage(err), "10%", fixed = TRUE)
  # Completa: passa e decide. -4,79 não vence o crítico de 5% (-5,08).
  ok <- do.call(.tr_series_teste,
                c(base, list(criticos = c(`10%` = -4.82, `5%` = -5.08, `1%` = -5.57))))
  expect_equal(ok$decisao_5, "não rejeita H0")
  expect_equal(ok$conclusao, "raiz unitária (com quebra)")
})

test_that("bloco sem p-valor e sem tabela de críticos é erro, e não 'não rejeita'", {
  # A outra porta da mesma mentira: sem fonte de decisão nenhuma, o
  # decisão leria NA e o `isTRUE()` do construtor o
  # transformaria num "não rejeita H0" de aparência confiante. Vale travar
  # agora, antes de treze blocos novos passarem por aqui.
  err <- tryCatch(
    .tr_series_teste("Inventado", "nada", 1.23, "Z",
                     conclusao_sim = "sim", conclusao_nao = "não", fonte = "—"),
    condition = identity)
  expect_s3_class(err, "tr_series_error_bad_criticos")
  expect_match(conditionMessage(err), "Inventado", fixed = TRUE)
})

test_that("ADF: passeio aleatório não é estacionário, a diferença dele é", {
  set.seed(1)
  passeio <- stats::ts(cumsum(stats::rnorm(200)))
  a <- tr_series_adf(passeio)
  expect_s3_class(a, "tr_series_test")
  expect_equal(a$teste, "ADF")
  expect_equal(a$sentido, "menor")
  expect_equal(a$conclusao, "não há evidência contra a raiz unitária")
  # Sem p-valor: o urca só publica tabela, e interpolar seria inventar.
  expect_true(is.na(a$p_valor))
  expect_equal(names(a$criticos), c("10%", "5%", "1%"))
  expect_equal(tr_series_adf(diff(passeio))$conclusao, "estacionária")
})

test_that("ADF recusa faltante e série curta", {
  expect_error(tr_series_adf(datasets::presidents), class = "tr_series_error_missing_values")
  expect_error(tr_series_adf(stats::ts(1:5)), class = "tr_series_error_too_short")
})

test_that("o ADF atravessa o adaptador como uma linha de relatório", {
  tb <- tabela_teste(tr_series_adf(serie_mensal()))
  expect_equal(nrow(tb), 1L)
  expect_equal(tb$teste, "ADF")
  expect_true(is.na(tb$p_valor))
  expect_false(is.na(tb$valor_critico_5))
})

test_that("KPSS rejeita na cauda de CIMA: o passeio cai, o ruído passa", {
  # O par de controle do ADF, lido ao contrário. Se os dois saírem iguais, o
  # `sentido` está errado — e um KPSS com a cauda trocada não é um teste
  # fraco, é um teste que conclui o oposto do que deveria.
  set.seed(1)
  passeio <- stats::ts(cumsum(stats::rnorm(200)))
  k <- tr_series_kpss(passeio)
  expect_s3_class(k, "tr_series_test")
  expect_equal(k$teste, "KPSS")
  expect_equal(k$sentido, "maior")
  expect_equal(k$h0, "a série é estacionária em torno de um nível")
  expect_equal(k$decisao_5, "rejeita H0")
  expect_equal(k$conclusao, "não estacionária")
  expect_true(is.na(k$p_valor))
  # Série estacionária: a estatística fica ABAIXO do crítico e H0 sobrevive.
  b <- tr_series_kpss(diff(passeio))
  expect_equal(b$decisao_5, "não rejeita H0")
  # Não rejeitar não é concluir estacionária: a conclusão diz só que falta
  # evidência contra H0, que é tudo o que um teste que não rejeita pode dizer.
  expect_equal(b$conclusao, "não há evidência contra a estacionariedade")
  expect_equal(tr_series_kpss(passeio, deterministico = "tendência")$h0,
               "a série é estacionária em torno de uma tendência")
  expect_true(b$estatistica < b$criticos[["5%"]])
  expect_true(k$estatistica > k$criticos[["5%"]])
})

test_that("os críticos do KPSS são 10/5/1, e o 1% não é o 2,5% disfarçado", {
  # A tabela do `ur.kpss` tem QUATRO colunas (10pct, 5pct, 2.5pct, 1pct), e
  # não três como a do `ur.df`. Montada por posição, a terceira entrega 0,574
  # — um corte plausível e errado, mais frouxo que o de 1%. Os números vão
  # crus no teste justamente para pegar isso.
  k <- tr_series_kpss(serie_mensal())
  expect_equal(names(k$criticos), c("10%", "5%", "1%"))
  expect_equal(unname(k$criticos), c(0.347, 0.463, 0.739))
  expect_false(isTRUE(all.equal(unname(k$criticos[["1%"]]), 0.574)))
  # Com tendência a tabela é outra (tau, não mu), e continua nomeada.
  kt <- tr_series_kpss(serie_mensal(), "tendência")
  expect_equal(names(kt$criticos), c("10%", "5%", "1%"))
  expect_equal(unname(kt$criticos), c(0.119, 0.146, 0.216))
  expect_match(kt$nota, "tendência")
})

test_that("KPSS recusa faltante e série curta", {
  expect_error(tr_series_kpss(datasets::presidents), class = "tr_series_error_missing_values")
  expect_error(tr_series_kpss(stats::ts(1:5)), class = "tr_series_error_too_short")
  expect_error(tr_series_kpss(serie_mensal(), "quadrática"), class = "tr_series_error_bad_option")
})

test_that("o KPSS atravessa o adaptador como uma linha de relatório", {
  tb <- tabela_teste(tr_series_kpss(serie_mensal()))
  expect_equal(nrow(tb), 1L)
  expect_equal(tb$teste, "KPSS")
  expect_true(is.na(tb$p_valor))
  expect_equal(tb$valor_critico_5, 0.463)
})

test_that("Phillips-Perron: passeio não estacionário, diferença estacionária", {
  set.seed(1)
  passeio <- stats::ts(cumsum(stats::rnorm(200)))
  p <- tr_series_phillips_perron(passeio)
  expect_s3_class(p, "tr_series_test")
  expect_equal(p$teste, "Phillips-Perron")
  expect_equal(p$sentido, "menor")
  expect_equal(p$conclusao, "não há evidência contra a raiz unitária")
  # Decide pelo p-valor: o `stats::PP.test` não publica tabela de críticos.
  expect_false(is.na(p$p_valor))
  expect_null(p$criticos)
  expect_equal(tr_series_phillips_perron(diff(passeio))$conclusao, "estacionária")
})

test_that("o p-valor preso na borda do PP vira ressalva na nota", {
  # `rnorm(300)` é estacionária o bastante para o p-valor bater no piso da
  # tabela (0,01) — e o `PP.test` prende no limite sem dar nem um warning.
  set.seed(2)
  p <- tr_series_phillips_perron(stats::ts(stats::rnorm(300)))
  expect_equal(p$p_valor, 0.01)
  expect_match(p$nota, "truncado")
  expect_match(p$nota, "<= 0,01", fixed = TRUE)
  # A outra borda é 0,99, e não 0,1: o `tablep` do `stats` vai até 0,99. Uma
  # série I(2) chega lá.
  set.seed(9)
  alto <- tr_series_phillips_perron(stats::ts(cumsum(cumsum(stats::rnorm(200)))))
  expect_equal(alto$p_valor, 0.99)
  expect_match(alto$nota, ">= 0,99", fixed = TRUE)
  # Longe das bordas, a ressalva não aparece: nota de aviso que aparece sempre
  # é nota que ninguém lê. O passeio aleatório sai com p interpolado (~0,62),
  # e um corte em 0,1 o marcaria como truncado.
  set.seed(1)
  meio <- tr_series_phillips_perron(stats::ts(cumsum(stats::rnorm(200))))
  expect_true(meio$p_valor > 0.1 && meio$p_valor < 0.99)
  expect_false(grepl("truncado", meio$nota))
})

test_that("Phillips-Perron recusa faltante e série curta", {
  expect_error(tr_series_phillips_perron(datasets::presidents),
               class = "tr_series_error_missing_values")
  expect_error(tr_series_phillips_perron(stats::ts(1:5)), class = "tr_series_error_too_short")
})

test_that("o Phillips-Perron atravessa o adaptador como uma linha de relatório", {
  tb <- tabela_teste(tr_series_phillips_perron(serie_mensal()))
  expect_equal(nrow(tb), 1L)
  expect_equal(tb$teste, "Phillips-Perron")
  expect_false(is.na(tb$p_valor))
  expect_true(is.na(tb$valor_critico_5))
})

# O degrau de NÍVEL: sessenta pontos em torno de zero e sessenta em torno de
# seis, mensais a partir de janeiro de 1950. É estacionária dos dois lados do
# corte — o que existe é o DESLOCAMENTO DA MÉDIA no meio —, e é exatamente o caso
# em que a dissertação diz que o ADF se engana. A quebra está entre a observação
# 60 e a 61 por construção, então a resposta certa é conhecida de antemão.
serie_degrau <- function() {
  set.seed(1)
  stats::ts(c(stats::rnorm(60), stats::rnorm(60) + 6), start = c(1950, 1), frequency = 12)
}

# Os testes abaixo travam a mecânica com defasagens FIXAS (a regra da versão 1,
# trunc((n - 1)^(1/3))). A seleção do geral para o específico, que é o padrão
# desde a versão 2, tem os testes próprios mais adiante.
za_fixa <- function(...) tr_series_zivot_andrews(..., selecao = "fixa")

test_that("cada modelo do Zivot-Andrews traz a SUA tabela de críticos", {
  # A armadilha central deste bloco, posta como asserção. O `z@cval` chega SEM
  # NOMES e na ordem 1%, 5%, 10% — invertida em relação à da casa. Montar a
  # tabela por posição supondo a ordem de sempre trocaria o corte de 1% pelo de
  # 10% e inverteria o veredito sem erro nenhum no caminho.
  #
  # E são TRÊS tabelas, uma por modelo, escolhidas pelo `urca` junto com o
  # `model`. Os três triplos vão escritos aqui para que uma edição futura não
  # possa colapsá-los num só: fixar -5.08 como "o corte de 5%" faria os outros
  # dois modelos passarem por acidente.
  esperado <- list(
    "nível"      = c(`10%` = -4.58, `5%` = -4.80, `1%` = -5.34),
    "inclinação" = c(`10%` = -4.11, `5%` = -4.42, `1%` = -4.93),
    "ambas"      = c(`10%` = -4.82, `5%` = -5.08, `1%` = -5.57))
  for (m in names(esperado)) {
    t <- za_fixa(serie_degrau(), mudanca = m)
    expect_equal(names(t$criticos), c("10%", "5%", "1%"), info = m)
    expect_equal(t$criticos, esperado[[m]], info = m)
    # O 1% é o corte MAIS SEVERO, e é esta comparação que pega a leitura ao
    # contrário: lido invertido, o "1%" seria o valor menos negativo dos três.
    expect_lt(t$criticos[["1%"]], t$criticos[["5%"]])
    expect_lt(t$criticos[["5%"]], t$criticos[["10%"]])
  }
  # E os três são DIFERENTES entre si — a Tabela 3.2 da dissertação publica só a
  # linha do modelo completo, e é de lá que viria a tentação de usar uma só.
  expect_false(isTRUE(all.equal(esperado[["nível"]], esperado[["ambas"]],
                                check.attributes = FALSE)))
})

test_that("no degrau de nível o Zivot-Andrews acha a quebra que o ADF não vê", {
  # A razão de existir do bloco, medida contra o vizinho na MESMA série. O ADF
  # devolve "não estacionária" para uma série que é estacionária dos dois lados
  # de um degrau — é o viés que MARGARIDO (2001) aponta, citado na dissertação —
  # e este teste, estimando a quebra, rejeita com folga.
  x <- serie_degrau()
  adf <- tr_series_adf(x)
  expect_equal(adf$estatistica, -1.565276, tolerance = 1e-5)
  expect_equal(adf$decisao_5, "não rejeita H0")
  expect_equal(adf$conclusao, "não há evidência contra a raiz unitária")

  t <- za_fixa(x, mudanca = "nível")
  expect_s3_class(t, "tr_series_test")
  expect_equal(t$teste, "Zivot-Andrews")
  expect_equal(t$h0, "a série tem raiz unitária, sem quebra")
  expect_equal(t$rotulo_estat, "t")
  expect_equal(t$sentido, "menor")
  expect_equal(t$estatistica, -10.999603, tolerance = 1e-5)
  expect_equal(t$decisao_5, "rejeita H0")
  expect_match(t$fonte, "Zivot & Andrews (1992)", fixed = TRUE)
  # A quebra cai na última observação antes do degrau, que é onde ela foi posta.
  expect_equal(t$extra$quebra, 60L)
  # E o t fica ABAIXO até do corte de 1%: a rejeição não é de borda.
  expect_lt(t$estatistica, t$criticos[["1%"]])

  # O modelo de INCLINAÇÃO não enxerga esta quebra, e não deveria mesmo: um
  # degrau não é uma virada de tendência. Serve de controle de que o parâmetro
  # `mudanca` chega ao `urca` — se os três modelos dessem o mesmo número, os
  # testes de cima passariam e o parâmetro não estaria fazendo nada.
  inc <- za_fixa(x, mudanca = "inclinação")
  expect_equal(inc$estatistica, -2.681320, tolerance = 1e-5)
  expect_equal(inc$decisao_5, "não rejeita H0")
  expect_equal(inc$conclusao, "não há evidência contra a raiz unitária, mesmo admitindo uma quebra")
  expect_equal(inc$extra$quebra, 87L)
})

test_that("o passeio aleatório não rejeita, mesmo com a quebra de graça", {
  # H0 VERDADEIRA: raiz unitária de verdade. Não rejeitar aqui é o acerto, e é o
  # controle do outro lado — um bloco que rejeitasse tudo passaria no teste do
  # degrau e não valeria nada.
  set.seed(5)
  rw <- stats::ts(cumsum(stats::rnorm(120)), start = c(1950, 1), frequency = 12)
  for (m in c("nível", "ambas")) {
    t <- za_fixa(rw, mudanca = m)
    expect_equal(t$decisao_5, "não rejeita H0", info = m)
    expect_equal(t$conclusao,
                 "não há evidência contra a raiz unitária, mesmo admitindo uma quebra", info = m)
    expect_gt(t$estatistica, t$criticos[["5%"]])
    # A posição continua publicada mesmo sem rejeitar: é onde o corte foi MAIS
    # favorável, e nem assim deu.
    expect_equal(t$extra$quebra, 64L, info = m)
  }
  expect_equal(za_fixa(rw, mudanca = "nível")$estatistica,
               -3.909448, tolerance = 1e-5)
  expect_equal(za_fixa(rw, mudanca = "ambas")$estatistica,
               -4.178075, tolerance = 1e-5)
})

test_that("o rótulo da quebra sai do calendário da série", {
  # O índice é o que se confere contra o `ur.za`; o rótulo é o que uma pessoa lê.
  # Numa mensal que começa em janeiro de 1950, a observação 60 é dezembro de 1954
  # — e é isso que tem de chegar ao relatório, não o número 60.
  t <- za_fixa(serie_degrau(), mudanca = "nível")
  expect_equal(t$extra$quando, "1954 dez")
  expect_equal(t$extra$quando, .tr_series_rotulo_em(serie_degrau(), 60))
  expect_equal(t$conclusao, "estacionária, com uma quebra na observação 60 (1954 dez)")
  # Numa série SEM calendário o rótulo seria o próprio índice, e o bloco não o
  # repete entre parênteses — "observação 60 (60)" ensina a ignorar o parêntese
  # justamente onde ele carrega a data. Mesma regra do `series/pettitt`.
  set.seed(1)
  cru <- c(stats::rnorm(60), stats::rnorm(60) + 6)
  s <- za_fixa(cru, mudanca = "nível")
  expect_equal(s$extra$quando, "60")
  expect_equal(s$conclusao, "estacionária, com uma quebra na observação 60")
})

test_that("a quebra do Zivot-Andrews fica dentro da janela de 15% a 85%", {
  # O `urca` varre TODOS os cortes, e neste passeio aleatório o mínimo dele cai
  # na observação 1 — um trecho de uma observação só antes da quebra, data que
  # não se interpreta. O bloco varre só entre 15% e 85%, como o teste publicado
  # de onde vem a tabela de críticos, e a quebra tem de sair dali de dentro.
  set.seed(36)
  x <- cumsum(stats::rnorm(60))
  z <- urca::ur.za(x, model = "both", lag = 3L)
  # Conferido antes de confiar no atalho: o `teststat` do pacote é o t do corte
  # que ele publica, então refazer o mínimo numa janela é o mesmo teste.
  expect_equal(z@tstats[z@bpoint], z@teststat)
  expect_equal(z@bpoint, 1L)

  t <- za_fixa(x)
  lo <- ceiling(0.15 * 60)
  hi <- floor(0.85 * 60)
  expect_gte(t$extra$quebra, lo)
  expect_lte(t$extra$quebra, hi)
  expect_equal(t$extra$quebra, 30L)
  # E a estatística é o mínimo DA JANELA, não o do pacote: menos negativa, porque
  # o corte que o `urca` escolheu ficou de fora.
  expect_equal(t$estatistica, min(z@tstats[lo:hi]))
  expect_gt(t$estatistica, z@teststat)
})

test_that("Zivot-Andrews recusa faltante, série curta e defasagem que não cabe", {
  expect_error(za_fixa(datasets::presidents),
               class = "tr_series_error_missing_values")
  err <- tryCatch(za_fixa(stats::ts(1:19)), condition = identity)
  expect_s3_class(err, "tr_series_error_too_short")
  expect_match(conditionMessage(err), "pelo menos 20", fixed = TRUE)

  # O piso é vinte, e não os doze dos irmãos de categoria, porque este teste
  # gasta mais graus de liberdade (a dummy da quebra, duas no modelo completo) e
  # ainda escolhe o melhor entre n - 1 cortes. O número foi MEDIDO: sob passeio
  # aleatório, onde H0 é verdadeira e toda rejeição é erro, o modelo completo
  # rejeita a 5% em cerca de 68% das amostras com n = 11 e 24% com n = 14, contra
  # 14% com n = 20. Abaixo de vinte o bloco diria "estacionária com quebra" para
  # série com raiz unitária de verdade na maioria das vezes.
  expect_s3_class(za_fixa(stats::ts(stats::rnorm(20))), "tr_series_test")

  # E a defasagem escolhida pelo usuário pode esgotar os graus de liberdade mesmo
  # acima do piso. Sem este guard o `ur.za` morre com um erro CRU do R, sem classe
  # e sem dizer o que fazer. A conta: gl = n - 1 - 2k - fixos, com fixos = 5 no
  # modelo completo, então em n = 20 o maior k que deixa um grau é 6.
  e2 <- tryCatch(za_fixa(stats::ts(stats::rnorm(20)), defasagens = 7L),
                 condition = identity)
  expect_s3_class(e2, "tr_series_error_bad_option")
  expect_match(conditionMessage(e2), "O máximo aqui é 6", fixed = TRUE)
  # E o máximo que ele anuncia REALMENTE roda: um limite que erra por um é pior
  # que limite nenhum, porque manda o usuário para um erro cru.
  expect_s3_class(za_fixa(stats::ts(stats::rnorm(20)), defasagens = 6L),
                  "tr_series_test")

  expect_error(za_fixa(serie_degrau(), mudanca = "oi"),
               class = "tr_series_error_bad_option")
})

test_that("o Zivot-Andrews atravessa o adaptador com a quebra em colunas", {
  t <- za_fixa(serie_degrau())
  tb <- tabela_teste(t)
  expect_s3_class(tb, "tbl_df")
  expect_equal(nrow(tb), 1L)
  expect_equal(tb$teste, "Zivot-Andrews")
  # Sem p-valor, e isso é contrato: o `urca` publica só a tabela, e interpolar um
  # p-valor a partir dela seria inventar precisão que o pacote não dá.
  expect_true(is.na(tb$p_valor))
  # A decisão vem daqui, então a coluna não pode estar vazia.
  expect_equal(tb$valor_critico_5, -5.08)
  expect_equal(tb$extra_quebra, 60L)
  expect_type(tb$extra_quando, "character")
  expect_equal(tb$extra_quando, "1954 dez")
})

test_that("a ressalva de série curta aparece onde vale, e some onde não vale", {
  # O excesso de tamanho não acaba no piso: medido, a taxa de rejeição sob
  # passeio aleatório fica em torno de 10% a 14% até n = 30, e é abaixo de 40 que o
  # excesso pesa mais (acima segue por volta de 7% a 9%). A ressalva vai na nota nessa faixa — e NÃO vai fora
  # dela, porque ressalva que sai sempre é ressalva que se aprende a ignorar.
  curta <- za_fixa(stats::ts(stats::rnorm(25)))
  expect_match(curta$nota, "série curta para este teste", fixed = TRUE)
  expect_false(grepl("série curta para este teste", za_fixa(serie_degrau())$nota,
                     fixed = TRUE))
  # A nota diz o que explica o número: o que quebrou, quantas defasagens e o
  # tamanho dos dois trechos.
  expect_match(za_fixa(serie_degrau())$nota,
               "quebra de nível e inclinação, estimada pelo teste; 4 defasagens; 60 observações antes da quebra e 60 a partir dela",
               fixed = TRUE)
})

test_that("Ljung-Box: o ruído passa, a série com estrutura cai", {
  # O controle nos dois sentidos. Um teste de ruído branco que só sabe dizer
  # "compatível" passa despercebido justamente onde importa — nos resíduos de
  # um modelo que deixou estrutura para trás.
  set.seed(3)
  ruido <- stats::ts(stats::rnorm(200))
  lb <- tr_series_ljung_box(ruido)
  expect_s3_class(lb, "tr_series_test")
  expect_equal(lb$teste, "Ljung-Box")
  expect_equal(lb$h0, "as autocorrelações até a defasagem 10 são nulas")
  expect_equal(lb$sentido, "menor")
  expect_equal(lb$decisao_5, "não rejeita H0")
  expect_equal(lb$conclusao, "não há evidência de autocorrelação")
  expect_false(is.na(lb$p_valor))
  expect_null(lb$criticos)
  expect_equal(tr_series_ljung_box(serie_mensal())$conclusao,
               "há autocorrelação: não é ruído branco")
})

test_that("Box-Pierce: mesma hipótese nula, outra fonte", {
  set.seed(3)
  ruido <- stats::ts(stats::rnorm(200))
  bp <- tr_series_box_pierce(ruido)
  expect_equal(bp$teste, "Box-Pierce")
  expect_equal(bp$h0, "as autocorrelações até a defasagem 10 são nulas")
  expect_equal(bp$conclusao, "não há evidência de autocorrelação")
  expect_match(bp$fonte, "Box & Pierce")
  expect_match(tr_series_ljung_box(ruido)$fonte, "Ljung & Box")
  # Mesma família, correções diferentes: as estatísticas não podem sair
  # idênticas, ou o `type` não está chegando ao `Box.test`.
  expect_false(isTRUE(all.equal(bp$estatistica, tr_series_ljung_box(ruido)$estatistica)))
  expect_equal(tr_series_box_pierce(serie_mensal())$conclusao,
               "há autocorrelação: não é ruído branco")
})

test_that("defasagens = 0 é a regra de Hyndman, e a nota diz quantas foram", {
  # Numa mensal são duas vezes o ciclo: 24. Uma deriva silenciosa nesse padrão
  # mudaria a conclusão de todo mundo que usa o bloco sem tocar no parâmetro.
  lb <- tr_series_ljung_box(serie_mensal())
  expect_match(lb$nota, "^24 defasagens")
  expect_match(lb$nota, "0 graus descontados")
  # Não sazonal: 10, e nunca mais que um quinto da série.
  set.seed(3)
  expect_match(tr_series_ljung_box(stats::ts(stats::rnorm(200)))$nota, "^10 defasagens")
  expect_match(tr_series_ljung_box(stats::ts(stats::rnorm(30)))$nota, "^6 defasagens")
})

test_that("graus chega ao Box.test, e não é parâmetro de enfeite", {
  # Aceitar `graus` e ignorá-lo é o pior defeito possível aqui: o card fica
  # com a mesma cara e o teste passa a aceitar ruído branco onde há estrutura.
  set.seed(3)
  ruido <- stats::ts(stats::rnorm(200))
  sem <- tr_series_ljung_box(ruido)
  com <- tr_series_ljung_box(ruido, graus = 2L)
  expect_false(isTRUE(all.equal(sem$p_valor, com$p_valor)))
  # Descontar graus tira graus de liberdade do qui-quadrado: com a MESMA
  # estatística, o p-valor só pode cair.
  expect_equal(sem$estatistica, com$estatistica)
  expect_true(com$p_valor < sem$p_valor)
  expect_match(com$nota, "2 graus descontados")
  expect_false(isTRUE(all.equal(tr_series_box_pierce(ruido)$p_valor,
                                tr_series_box_pierce(ruido, graus = 2L)$p_valor)))
})

test_that("defasagens que não sobrevivem aos graus descontados são erro", {
  # Sem grau de liberdade nenhum não há teste: melhor card vermelho que um
  # p-valor de fantasia.
  set.seed(3)
  ruido <- stats::ts(stats::rnorm(200))
  expect_error(tr_series_ljung_box(ruido, defasagens = 2L, graus = 2L),
               class = "tr_series_error_bad_option")
  expect_error(tr_series_box_pierce(ruido, defasagens = 2L, graus = 3L),
               class = "tr_series_error_bad_option")
})

test_that("ruído branco recusa série curta demais para as defasagens", {
  expect_error(tr_series_ljung_box(stats::ts(1:5), defasagens = 10L),
               class = "tr_series_error_too_short")
  expect_error(tr_series_box_pierce(stats::ts(1:5), defasagens = 10L),
               class = "tr_series_error_too_short")
})

test_that("o mínimo do ruído branco conta as VÁLIDAS, e não o comprimento com buraco", {
  # Os dois blocos NA-tolerantes contavam o tamanho com `length()`, que inclui o
  # faltante — e a página promete o contrário. Medido antes do conserto, nos dois
  # casos abaixo:
  #
  # - quarenta pontos com seis válidos passavam e saíam com "não rejeita H0",
  #   p = 0,36, calculado sobre UMA defasagem: a regra de Hyndman encolhe a
  #   defasagem junto com a série, e o mínimo `lag + 2` encolhia junto com ela.
  #   Resultado errado com cara de certo, que é o inimigo declarado da coleção;
  # - com UM válido, o `Box.test` devolvia p = NaN e o que chegava ao usuário era
  #   `tr_series_error_bad_criticos` ("nem p-valor nem tabela de críticos") —
  #   verdade sobre o sintoma, mentira sobre a causa, e mandando olhar o lugar
  #   errado.
  set.seed(1)
  x <- stats::ts(stats::rnorm(40), frequency = 1)
  seis <- x; seis[7:40] <- NA
  uma <- x; uma[2:40] <- NA
  for (f in list(tr_series_ljung_box, tr_series_box_pierce)) {
    for (serie in list(seis, uma)) {
      err <- tryCatch(f(serie), condition = identity)
      expect_s3_class(err, "tr_series_error_too_short")
      # A mensagem tem de nomear o TAMANHO, e dizer que a conta é das válidas —
      # é o que manda consertar (recortar, interpolar, ou trazer mais dado). O
      # `válid` sem terminação porque a frase concorda com o número: com um
      # valor só ela sai no singular.
      expect_match(conditionMessage(err), "observaç", fixed = TRUE)
      expect_match(conditionMessage(err), "válid", fixed = TRUE)
      expect_match(conditionMessage(err), "(de 40)", fixed = TRUE)
      expect_match(conditionMessage(err), "pelo menos 12", fixed = TRUE)
    }
  }
  # E o piso não estraga o caso legítimo: série cheia com buraco de sobra passa.
  com_buraco <- x; com_buraco[c(3, 17, 29)] <- NA
  expect_s3_class(tr_series_ljung_box(com_buraco), "tr_series_test")
})

test_that("os dois testes de ruído branco atravessam o adaptador", {
  lb <- tabela_teste(tr_series_ljung_box(serie_mensal()))
  expect_equal(nrow(lb), 1L)
  expect_equal(lb$teste, "Ljung-Box")
  expect_false(is.na(lb$p_valor))
  expect_true(is.na(lb$valor_critico_5))
  bp <- tabela_teste(tr_series_box_pierce(serie_mensal()))
  expect_equal(nrow(bp), 1L)
  expect_equal(bp$teste, "Box-Pierce")
  expect_false(is.na(bp$p_valor))
})

test_that("diferenças necessárias: simples e sazonal, e NA com motivo na anual", {
  n <- tr_series_ndiffs(serie_mensal())
  expect_equal(n$tipo, c("simples", "sazonal"))
  expect_equal(n$diferencas[[2]], 1L)
  a <- tr_series_ndiffs(serie_anual())
  expect_true(is.na(a$diferencas[[2]]))
  expect_match(a$nota[[2]], "ciclo")
})

test_that("os F não mudam com o contraste", {
  # O contraste é posto no FATOR, e não no `contrasts=` do `lm`, para que o
  # reajuste a partir de `fit$model` no F parcial herde a parametrização. Se
  # alguém o mudar de lugar, os F parciais passam a depender do contraste — e
  # nada mais pegaria. Ano incompleto de propósito: com desenho balanceado a
  # igualdade sairia de graça.
  x <- stats::window(datasets::AirPassengers, start = c(1949, 9), end = c(1952, 2))
  a <- tr_series_regression(x, contraste = "soma_zero")
  b <- tr_series_regression(x, contraste = "categoria_base")
  for (f in list(tr_series_f_global, tr_series_f_sazonal, tr_series_f_tendencia)) {
    expect_equal(f(a)$estatistica, f(b)$estatistica, tolerance = 1e-8)
    expect_equal(f(a)$p_valor, f(b)$p_valor, tolerance = 1e-8)
  }
})

test_that("sem sinal, nenhum dos três F rejeita", {
  # O outro sentido do controle: um F que só sabe dizer "rejeita" acharia
  # tendência e sazonalidade em ruído puro, e ninguém repararia.
  set.seed(7)
  ruido <- stats::ts(stats::rnorm(120), frequency = 12, start = c(2010, 1))
  a <- tr_series_regression(ruido)
  for (f in list(tr_series_f_global, tr_series_f_sazonal, tr_series_f_tendencia)) {
    expect_equal(f(a)$decisao_5, "não rejeita H0")
  }
})

test_that("os três F da regressão rejeitam na mensal, cada um no seu bloco", {
  a <- tr_series_regression(serie_mensal())
  g <- tr_series_f_global(a)
  s <- tr_series_f_sazonal(a)
  d <- tr_series_f_tendencia(a)
  for (t in list(g, s, d)) {
    expect_s3_class(t, "tr_series_test")
    expect_equal(t$rotulo_estat, "F")
    expect_equal(t$sentido, "menor")
    expect_equal(t$decisao_5, "rejeita H0")
    expect_true(t$p_valor < 0.05)
    expect_match(t$fonte, "Morettin")
  }
  expect_equal(g$teste, "F global")
  expect_equal(s$teste, "F do bloco sazonal")
  expect_equal(d$teste, "F do bloco de tendência")
  expect_equal(g$conclusao, "o modelo explica parte da série")
  expect_equal(s$conclusao, "há sazonalidade")
  expect_equal(d$conclusao, "há tendência")
  # O reajuste do F parcial tem de derrubar SÓ o bloco pedido: onze dummies na
  # mensal, e `grau` no polinômio. Um numerador com o número errado de graus é
  # um teste de outra hipótese nula, com cara de certo.
  expect_match(s$nota, "11 graus no numerador")
  expect_match(d$nota, "1 grau no numerador")
})

test_that("o bloco que não existe no ajuste é cartão vermelho, e diz qual falta", {
  # Um bloco dedicado não pode omitir a si mesmo: sairia um card verde
  # testando outra coisa.
  sem_saz <- tr_series_regression(serie_mensal(), sazonalidade = FALSE)
  err <- tryCatch(tr_series_f_sazonal(sem_saz), error = identity)
  expect_s3_class(err, "tr_series_error_no_block")
  expect_match(conditionMessage(err), "sazonal")
  sem_tend <- tr_series_regression(serie_mensal(), grau = 0L)
  err2 <- tryCatch(tr_series_f_tendencia(sem_tend), error = identity)
  expect_s3_class(err2, "tr_series_error_no_block")
  expect_match(conditionMessage(err2), "tendência")
  # O F global existe para qualquer ajuste: com um bloco só, é o F daquele bloco.
  expect_equal(tr_series_f_global(sem_saz)$estatistica,
               tr_series_f_tendencia(sem_saz)$estatistica, tolerance = 1e-8)
})

test_that("os três F recusam o que não é regressão", {
  for (f in list(tr_series_f_global, tr_series_f_sazonal, tr_series_f_tendencia)) {
    expect_error(f(serie_mensal()), class = "tr_series_error_not_a_regression")
  }
})

test_that("os três F atravessam o adaptador de tabela", {
  a <- tr_series_regression(serie_mensal())
  for (t in list(tr_series_f_global(a), tr_series_f_sazonal(a), tr_series_f_tendencia(a))) {
    tb <- tabela_teste(t)
    expect_equal(nrow(tb), 1L)
    expect_equal(tb$teste, t$teste)
    expect_false(is.na(tb$p_valor))
  }
})

# Uma reta com ruído: o caso em que a resposta é conhecida de antemão, e os três
# números vão crus porque é assim que se percebe a fórmula mexida.
serie_crescente <- function() {
  set.seed(42)
  as.numeric(1:60 + stats::rnorm(60, sd = 5))
}

test_that("Mann-Kendall mede o S, o Z e o p da reta com ruído", {
  t <- tr_series_mann_kendall(serie_crescente())
  expect_s3_class(t, "tr_series_test")
  expect_equal(t$teste, "Mann-Kendall")
  expect_equal(t$h0, "a série não tem tendência monótona")
  expect_equal(t$rotulo_estat, "Z")
  expect_equal(t$sentido, "menor")
  expect_equal(t$extra$S, 1430)
  expect_equal(t$estatistica, 9.114059, tolerance = 1e-6)
  expect_equal(t$p_valor, 7.935599e-20, tolerance = 1e-6)
  expect_equal(t$decisao_5, "rejeita H0")
  expect_equal(t$conclusao, "há tendência de aumento")
  expect_match(t$fonte, "Mann")
})

test_that("Mann-Kendall bate com o trend::mk.test na mesma série", {
  # A conferência contra o pacote de referência, que é o que separa "a fórmula
  # roda" de "a fórmula é a certa": correção de continuidade e correção de
  # empates são dois lugares onde a nossa versão poderia divergir em silêncio,
  # entregando um p-valor plausível e errado.
  skip_if_not_installed("trend")
  x <- serie_crescente()
  t <- tr_series_mann_kendall(x)
  ref <- trend::mk.test(x)
  expect_equal(t$estatistica, unname(ref$statistic), tolerance = 1e-9)
  expect_equal(t$p_valor, unname(ref$p.value), tolerance = 1e-9)
})

test_that("o sinal de Z vira a conclusão: a série que cai não 'aumenta'", {
  # O controle no outro sentido. Um teste bilateral que só soubesse dizer "há
  # tendência" passaria nos dois casos — e a direção é justamente o que o
  # usuário foi perguntar.
  queda <- rev(serie_crescente())
  t <- tr_series_mann_kendall(queda)
  expect_true(t$estatistica < 0)
  expect_equal(t$estatistica, -tr_series_mann_kendall(serie_crescente())$estatistica,
               tolerance = 1e-9)
  expect_equal(t$decisao_5, "rejeita H0")
  expect_equal(t$conclusao, "há tendência de queda")
  expect_equal(t$extra$S, -1430)
})

test_that("Mann-Kendall não acha tendência em ruído branco", {
  set.seed(7)
  t <- tr_series_mann_kendall(stats::rnorm(100))
  expect_equal(t$decisao_5, "não rejeita H0")
  expect_equal(t$conclusao, "não há evidência de tendência")
  expect_true(t$p_valor > 0.05)
})

test_that("a correção de empates muda a variância, e não é enfeite", {
  # Série arredondada de propósito: é a forma do dado climatológico real
  # (medição com uma casa, corrida de zeros). Sem o desconto dos empates a
  # variância fica maior do que a que S pode ter, e o p-valor sai otimista.
  set.seed(3)
  x <- round(seq(0, 4, length.out = 60) + stats::rnorm(60))
  t <- tr_series_mann_kendall(x)
  expect_match(t$nota, "grupos de empates corrigidos")
  n <- length(x)
  S <- t$extra$S
  # A mesma conta SEM o termo dos empates: é a versão que a fórmula de livro
  # traz quando a série não tem repetição nenhuma.
  v_sem <- n * (n - 1) * (2 * n + 5) / 18
  z_sem <- (S - 1) / sqrt(v_sem)
  p_sem <- 2 * stats::pnorm(-abs(z_sem))
  # A diferença é medida como RAZÃO, e não por `all.equal`: os dois p-valores
  # são da ordem de 1e-11, e abaixo da tolerância o `all.equal` troca a
  # diferença relativa pela ABSOLUTA e chama 2,35e-11 e 6,53e-11 de iguais.
  # Medido: a primeira versão deste teste dizia "os dois diferem" e passava
  # vazia — o mesmo verde falso que ela existe para impedir.
  expect_equal(t$p_valor / p_sem, 0.36, tolerance = 0.02)
  # E o sentido do erro: sem a correção a variância fica MAIOR do que a que S
  # pode ter, o Z sai menor e o p-valor maior — o teste perde poder e deixa de
  # ver a tendência que existe.
  expect_true(abs(t$estatistica) > abs(z_sem))
  expect_true(t$p_valor < p_sem)
})

test_that("Mann-Kendall recusa faltante e série curta", {
  expect_error(tr_series_mann_kendall(datasets::presidents),
               class = "tr_series_error_missing_values")
  err <- tryCatch(tr_series_mann_kendall(stats::ts(1:9)), condition = identity)
  expect_s3_class(err, "tr_series_error_too_short")
  expect_match(conditionMessage(err), "pelo menos 10", fixed = TRUE)
  # Dez é o piso, e não o primeiro recusado.
  expect_s3_class(tr_series_mann_kendall(stats::ts(1:10)), "tr_series_test")
})

test_that("o Mann-Kendall atravessa o adaptador, e leva o S como coluna", {
  tb <- tabela_teste(tr_series_mann_kendall(serie_mensal()))
  expect_equal(nrow(tb), 1L)
  expect_equal(tb$teste, "Mann-Kendall")
  expect_false(is.na(tb$p_valor))
  expect_true(is.na(tb$valor_critico_5))
  expect_true("extra_S" %in% names(tb))
  expect_equal(tb$extra_S, tr_series_mann_kendall(serie_mensal())$extra$S)
})

test_that("Cox-Stuart em terços mede os pares, o Z e o p da reta com ruído", {
  t <- tr_series_cox_stuart(serie_crescente())
  expect_s3_class(t, "tr_series_test")
  expect_equal(t$teste, "Cox-Stuart")
  expect_equal(t$h0, "a série não tem tendência monótona")
  expect_equal(t$rotulo_estat, "Z")
  expect_equal(t$sentido, "menor")
  expect_equal(t$extra$pares, 20L)
  expect_equal(t$extra$M, 20L)
  expect_equal(t$estatistica, 4.472136, tolerance = 1e-6)
  expect_equal(t$p_valor, 7.744216e-06, tolerance = 1e-6)
  expect_equal(t$decisao_5, "rejeita H0")
  expect_equal(t$conclusao, "há tendência de aumento")
  expect_match(t$fonte, "Cox")
  expect_match(t$nota, "terços", fixed = TRUE)
  expect_match(t$nota, "20 pares", fixed = TRUE)
  expect_match(t$nota, "aproximação normal", fixed = TRUE)
})

test_that("o pareamento em metades é outro teste, e a nota o assume", {
  # O param não é cosmético: mesma série, mesma H0, dois números. Se um dia os
  # dois ramos convergirem, é porque o pareamento parou de fazer efeito.
  t <- tr_series_cox_stuart(serie_crescente(), pareamento = "metades")
  expect_equal(t$extra$pares, 30L)
  expect_equal(t$extra$M, 30L)
  expect_equal(t$estatistica, 5.477226, tolerance = 1e-6)
  expect_equal(t$p_valor, 4.320463e-08, tolerance = 1e-6)
  expect_equal(t$conclusao, "há tendência de aumento")
  # O `fonte` continua nomeando Cox & Stuart, e o método aqui é o da
  # dissertação: sem a ressalva, o relatório citaria um artigo pela conta de
  # outro trabalho.
  expect_match(t$nota, "dissertação", fixed = TRUE)
  expect_match(t$nota, "metades", fixed = TRUE)
  # A distância entre os dois medida como RAZÃO, e nunca por `all.equal`: abaixo
  # da própria tolerância o `all.equal` troca a diferença relativa pela
  # ABSOLUTA e chama dois p-valores minúsculos de iguais — foi assim que um
  # teste da tarefa anterior passou sem medir nada. São duas ordens de
  # grandeza, e é isso que se afirma.
  expect_gt(tr_series_cox_stuart(serie_crescente())$p_valor / t$p_valor, 100)
})

test_that("Cox-Stuart em terços bate com o trend::cs.test na mesma série", {
  # A conferência contra o pacote de referência: o pareamento em terços é o
  # teste original, e é exatamente o que o `trend` calcula. Sem ela, um erro de
  # um índice na ponta da série (pegar do miolo em vez da cauda) sairia com um
  # p-valor plausível e errado.
  # A igualdade só vale sob TODAS estas condições, e cada uma foi medida:
  #
  #   1. tamanho múltiplo de três — senão o `trend` padroniza pelos momentos de
  #      n/3 pares enquanto forma ceiling(n/3) deles;
  #   2. nenhum par empatado — empate descartado muda o k daqui e não muda o n
  #      de lá;
  #   3. pelo menos VINTE pares sobrando, ou seja n >= 60 — abaixo disso daqui
  #      sai o M da binomial exata, e não há Z para comparar com nada;
  #   4. série subindo — o sinal é nosso, e o `trend` toma max(table(sign(u))),
  #      que é sempre positivo.
  #
  # Medido: n = 33, 36, 45, 54 e 57 divergem (todos múltiplos de três, todos sem
  # empate — é a condição 3 que falha); n = 60, 66 e 90 batem. E `rev()` de uma
  # série crescente de 60 pontos diverge só pelo sinal.
  #
  # Duas versões anteriores deste comentário afirmavam um conjunto menor de
  # condições e estavam erradas. Com uma série só, a conferência seria um ponto
  # escolhido a dedo passando por regra geral.
  skip_if_not_installed("trend")
  series <- list(serie_crescente())
  for (n in c(66L, 90L)) {
    set.seed(1)
    series[[length(series) + 1L]] <- as.numeric(seq_len(n) + stats::rnorm(n, sd = 5))
  }
  for (x in series) {
    t <- tr_series_cox_stuart(x)
    ref <- trend::cs.test(x)
    expect_equal(t$estatistica, unname(ref$statistic), tolerance = 1e-9,
                 info = paste("n =", length(x)))
    expect_equal(t$p_valor, unname(ref$p.value), tolerance = 1e-9,
                 info = paste("n =", length(x)))
  }
})

test_that("fora do múltiplo de três o Cox-Stuart daqui NÃO bate com o trend, e é de propósito", {
  # O outro lado da conferência acima: o limite escrito, para que um ajuste
  # futuro na padronização não o atravesse sem ninguém ver. O `Nile` tem n = 100,
  # que não é múltiplo de três. Nós padronizamos pelos pares que SOBRARAM,
  # `(M - k/2) / sqrt(k/4)`, com sinal; o `trend` padroniza pelo tamanho da
  # série, `abs(S - n/6) / sqrt(n/12)`, e toma `max(table(sign(u)))`, que é
  # sempre positivo. Medido: o `trend` devolve 4.272391992 e p = 1.933872e-05
  # aqui, contra os nossos números abaixo.
  #
  # Os valores vão LITERAIS, e não como "difere do trend": desigualdade via
  # `all.equal` troca diferença relativa por absoluta abaixo da própria
  # tolerância e passa calada justamente quando os dois números são minúsculos.
  t <- tr_series_cox_stuart(datasets::Nile)
  expect_equal(t$estatistica, -4.115966043, tolerance = 1e-9)
  expect_equal(t$p_valor, 3.855610716e-05, tolerance = 1e-9)
  # O sinal negativo é o ganho que se perde ao copiar a convenção do `trend`: é
  # ele que deixa a conclusão nomear a direção.
  expect_equal(t$conclusao, "há tendência de queda")
})

test_that("com menos de vinte pares o p-valor vem da binomial exata, e a estatística é M", {
  # Dezenove pontos dão sete pares em terços — o ramo exato. A estatística
  # reportada é o M, porque Z nenhum foi calculado aqui.
  t <- tr_series_cox_stuart(as.numeric(1:19))
  expect_equal(t$extra$pares, 7L)
  expect_equal(t$rotulo_estat, "M")
  expect_equal(t$estatistica, 7)
  expect_equal(t$p_valor, 2 * 0.5^7)
  expect_match(t$nota, "binomial exata", fixed = TRUE)
  expect_equal(t$decisao_5, "rejeita H0")
  expect_equal(t$conclusao, "há tendência de aumento")
})

test_that("a série que cai conclui queda, e não aumento", {
  queda <- rev(serie_crescente())
  t <- tr_series_cox_stuart(queda)
  expect_equal(t$extra$M, 0L)
  expect_true(t$estatistica < 0)
  expect_equal(t$decisao_5, "rejeita H0")
  expect_equal(t$conclusao, "há tendência de queda")
})

test_that("Cox-Stuart não acha tendência em ruído branco", {
  # Semente própria, e não a do Mann-Kendall: os dois testes olham pares
  # diferentes, e a série que um vê como ruído o outro pode ver como queda. Sob
  # a semente 7 este aqui rejeitava — o que é o erro tipo I acontecendo, não um
  # defeito, mas um teste que depende de um acaso desses não vale nada.
  set.seed(1)
  t <- tr_series_cox_stuart(stats::rnorm(100))
  expect_equal(t$decisao_5, "não rejeita H0")
  expect_equal(t$conclusao, "não há evidência de tendência")
  expect_true(t$p_valor > 0.05)
})

test_that("Cox-Stuart recusa faltante, série curta e pareamento inventado", {
  expect_error(tr_series_cox_stuart(datasets::presidents),
               class = "tr_series_error_missing_values")
  err <- tryCatch(tr_series_cox_stuart(stats::ts(1:15)), condition = identity)
  expect_s3_class(err, "tr_series_error_too_short")
  expect_match(conditionMessage(err), "pelo menos 16", fixed = TRUE)
  # Dezesseis é o piso, e não o primeiro recusado: seis pares, que é o menor k
  # em que a binomial bilateral ainda consegue rejeitar.
  expect_s3_class(tr_series_cox_stuart(stats::ts(1:16)), "tr_series_test")
  expect_error(tr_series_cox_stuart(serie_crescente(), pareamento = "quartos"),
               class = "tr_series_error_bad_option")
})

test_that("empates demais deixam o teste sem pares, e isso é cartão vermelho", {
  # Série longa e constante nas pontas: passa pelo piso de dezesseis e chega ao
  # pareamento sem par nenhum que aponte direção. Um veredito aqui estaria
  # decidido antes de olhar o dado.
  err <- tryCatch(tr_series_cox_stuart(rep(1, 20)), condition = identity)
  expect_s3_class(err, "tr_series_error_too_short")
  expect_match(conditionMessage(err), "empates", fixed = TRUE)
})

test_that("o Cox-Stuart atravessa o adaptador, e leva o M e os pares como colunas", {
  t <- tr_series_cox_stuart(serie_mensal())
  tb <- tabela_teste(t)
  expect_equal(nrow(tb), 1L)
  expect_equal(tb$teste, "Cox-Stuart")
  expect_false(is.na(tb$p_valor))
  expect_true(is.na(tb$valor_critico_5))
  expect_equal(tb$extra_M, t$extra$M)
  expect_equal(tb$extra_pares, t$extra$pares)
})

test_that("Run mede as sequências, o Z e o p da reta com ruído", {
  t <- tr_series_runs(serie_crescente())
  expect_s3_class(t, "tr_series_test")
  expect_equal(t$teste, "Run")
  # A H0 deste bloco é ALEATORIEDADE, e não ausência de tendência: é o campo que
  # distingue o Run dos dois irmãos de categoria, e é dele que sai a conclusão.
  expect_equal(t$h0, "a série é aleatória")
  expect_equal(t$rotulo_estat, "Z")
  expect_equal(t$sentido, "menor")
  expect_equal(t$extra$sequencias, 10)
  expect_equal(t$estatistica, -5.468720, tolerance = 1e-6)
  expect_equal(t$p_valor, 4.532976e-08, tolerance = 1e-6)
  expect_equal(t$decisao_5, "rejeita H0")
  expect_match(t$fonte, "Wald")
})

test_that("Run bate com o randtests::runs.test na mesma série", {
  # A conferência contra o pacote de referência, que é o que separa "a fórmula
  # roda" de "a fórmula é a certa". Aqui ela pega especificamente o descarte dos
  # empates e a contagem das trocas de sinal: errar qualquer um dos dois sai com
  # um Z plausível, e só a comparação denuncia.
  skip_if_not_installed("randtests")
  x <- serie_crescente()
  t <- tr_series_runs(x)
  ref <- randtests::runs.test(x)
  expect_equal(t$estatistica, unname(ref$statistic), tolerance = 1e-9)
  expect_equal(t$p_valor, unname(ref$p.value), tolerance = 1e-9)
})

test_that("a conclusão do Run fala de aleatoriedade, e nunca de tendência", {
  # O ponto da tarefa posto como teste. Rejeitar aqui não autoriza dizer "há
  # tendência": sequências de menos saem de tendência, mas também de degrau, de
  # ciclo e de agrupamento, e o bloco não tem como separar os três. Uma versão
  # que copiasse a conclusão do Mann-Kendall passaria em tudo o que está acima e
  # mentiria no card — é exatamente esta linha que a impede.
  t <- tr_series_runs(serie_crescente())
  expect_false(grepl("há tendência", t$conclusao, fixed = TRUE))
  expect_match(t$conclusao, "não é aleatória", fixed = TRUE)
  # E o sinal de Z diz de que JEITO a aleatoriedade falhou: a reta com ruído tem
  # sequências de MENOS.
  expect_true(t$estatistica < 0)
  expect_match(t$conclusao, "sequências de menos", fixed = TRUE)
})

test_that("Run não acha nada em ruído branco", {
  # Semente conferida, e não herdada: sob a semente 7 este teste REJEITA o ruído
  # branco (p = 6e-04), que é o erro tipo I acontecendo — não um defeito, mas um
  # teste que depende de um acaso desses não vale nada. A semente 1 dá p = 0,69,
  # longe da borda dos dois lados.
  set.seed(1)
  t <- tr_series_runs(stats::rnorm(100))
  expect_equal(t$decisao_5, "não rejeita H0")
  expect_equal(t$conclusao, "não há evidência contra a aleatoriedade")
  expect_true(t$p_valor > 0.05)
})

test_that("empate com a mediana sai da conta, e a nota diz quantos", {
  # Doze valores exatamente na mediana no meio da série. O descarte é invisível
  # no card — muda N e não muda nada que se veja —, então é a nota que responde
  # por ele. Sem o descarte, N seria 52 e as duas sequências vizinhas ao platô
  # virariam uma só.
  y <- c(rep(1:10, each = 2), rep(11, 12), rep(12:21, each = 2))
  expect_equal(length(y), 52L)
  expect_equal(stats::median(y), 11)
  t <- tr_series_runs(y)
  # Cinquenta e duas entraram, quarenta contaram: os doze empates saíram.
  expect_match(t$nota, "em 40 observações", fixed = TRUE)
  expect_match(t$nota, "20 acima e 20 abaixo da mediana", fixed = TRUE)
  expect_match(t$nota, "12 empatadas com a mediana, fora da conta", fixed = TRUE)
  expect_equal(t$extra$sequencias, 2)
  expect_equal(t$estatistica, -6.086871, tolerance = 1e-6)
})

test_that("um platô na mediana deixa o teste sem símbolo, e isso é cartão vermelho", {
  # Sessenta observações passam o piso de tamanho e chegam ao teste com só
  # quinze de cada lado: o piso de cima olha a SÉRIE, este olha o que sobrou
  # dela. No extremo um dos lados zera, o desvio sai NaN e o card mostraria um Z
  # vazio em vez de dizer o que houve.
  z <- c(1:15, rep(16, 30), 17:31)
  expect_equal(length(z), 60L)
  err <- tryCatch(tr_series_runs(z), condition = identity)
  expect_s3_class(err, "tr_series_error_too_short")
  expect_match(conditionMessage(err), "empates", fixed = TRUE)
  expect_match(conditionMessage(err), "20 de cada lado", fixed = TRUE)
})

test_that("Run recusa faltante e série curta", {
  expect_error(tr_series_runs(datasets::presidents),
               class = "tr_series_error_missing_values")
  err <- tryCatch(tr_series_runs(stats::ts(1:39)), condition = identity)
  expect_s3_class(err, "tr_series_error_too_short")
  expect_match(conditionMessage(err), "pelo menos 40", fixed = TRUE)
  # Quarenta é o piso, e não o primeiro recusado: é onde o corte pela mediana
  # entrega os vinte símbolos de cada lado que a aproximação normal pede.
  expect_s3_class(tr_series_runs(stats::ts(1:40)), "tr_series_test")
})

test_that("o Run atravessa o adaptador, e leva as sequências como coluna", {
  t <- tr_series_runs(serie_mensal())
  tb <- tabela_teste(t)
  expect_equal(nrow(tb), 1L)
  expect_equal(tb$teste, "Run")
  expect_false(is.na(tb$p_valor))
  expect_true(is.na(tb$valor_critico_5))
  expect_true("extra_sequencias" %in% names(tb))
  expect_equal(tb$extra_sequencias, t$extra$sequencias)
})

test_that("Pettitt mede o K, o ponto e o p da reta com ruído", {
  t <- tr_series_pettitt(serie_crescente())
  expect_s3_class(t, "tr_series_test")
  expect_equal(t$teste, "Pettitt")
  # A H0 deste bloco é HOMOGENEIDADE, e não ausência de tendência: é o campo que
  # o distingue dos três irmãos de categoria.
  expect_equal(t$h0, "a série é homogênea, sem ponto de mudança")
  expect_equal(t$rotulo_estat, "K")
  expect_equal(t$sentido, "menor")
  expect_equal(t$estatistica, 878)
  expect_equal(t$extra$ponto_de_mudanca, 30L)
  expect_equal(t$p_valor, 1.424768e-09, tolerance = 1e-6)
  expect_equal(t$decisao_5, "rejeita H0")
  expect_match(t$fonte, "Pettitt")
  # Série sem calendário: o rótulo seria o próprio índice, e o bloco não o
  # repete entre parênteses.
  expect_equal(t$extra$quando, "30")
  expect_equal(t$conclusao, "há um ponto de mudança na observação 30")
})

test_that("Pettitt bate com o trend::pettitt.test na mesma série", {
  # A conferência contra o pacote de referência, que é o que separa "a fórmula
  # roda" de "a fórmula é a certa". Lá a estatística se chama `U*` e o `estimate`
  # é o ponto de mudança; os três números têm de bater com os três daqui.
  skip_if_not_installed("trend")
  x <- serie_crescente()
  t <- tr_series_pettitt(x)
  ref <- trend::pettitt.test(x)
  expect_equal(t$estatistica, unname(ref$statistic), tolerance = 1e-9)
  expect_equal(t$p_valor, unname(ref$p.value), tolerance = 1e-9)
  expect_equal(t$extra$ponto_de_mudanca, unname(ref$estimate))
  # E numa segunda série, de outra forma: a reta com ruído tem a quebra no meio
  # por construção, e uma implementação que acertasse só ela acertaria por sorte.
  n <- tr_series_pettitt(serie_anual())
  rn <- trend::pettitt.test(serie_anual())
  expect_equal(n$estatistica, unname(rn$statistic), tolerance = 1e-9)
  expect_equal(n$p_valor, unname(rn$p.value), tolerance = 1e-9)
  expect_equal(n$extra$ponto_de_mudanca, unname(rn$estimate))
})

test_that("a conclusão do Pettitt nomeia a quebra, e nunca fala de tendência", {
  # O ponto da tarefa posto como teste, e a linha que impede o copiar-colar dos
  # três irmãos de categoria: eles todos testam TENDÊNCIA e este não. Rejeitar
  # aqui localiza uma RUPTURA — a dissertação é enfática, citando Niel et al.
  # (1998) —, e não autoriza afirmar que a série sobe ou desce. Uma versão que
  # copiasse a conclusão do Mann-Kendall passaria em tudo o que está acima e
  # mentiria no card.
  for (s in list(serie_crescente(), rev(serie_crescente()), serie_mensal(), serie_anual())) {
    t <- tr_series_pettitt(s)
    expect_false(grepl("tendência", t$conclusao))
    expect_false(grepl("aumento|queda", t$conclusao))
    expect_match(t$conclusao, "ponto de mudança", fixed = TRUE)
  }
  # E a série que cai dá a MESMA quebra da que sobe, espelhada: o teste não
  # enxerga direção nenhuma, que é justamente o que a conclusão não pode afirmar.
  sobe <- tr_series_pettitt(serie_crescente())
  cai <- tr_series_pettitt(rev(serie_crescente()))
  expect_equal(cai$estatistica, sobe$estatistica)
  expect_equal(cai$p_valor, sobe$p_valor)
})

test_that("Pettitt acha o degrau onde ele está", {
  # Uma quebra de verdade: quarenta pontos de ruído e quarenta do mesmo ruído
  # cinco unidades acima. Não há tendência nenhuma dentro de cada metade — o que
  # existe é a MUDANÇA DE NÍVEL, que é o que este teste procura e o único dos
  # quatro da categoria que sabe localizar.
  set.seed(1)
  x <- stats::ts(c(stats::rnorm(40), stats::rnorm(40) + 5), start = c(1950, 1),
                 frequency = 12)
  t <- tr_series_pettitt(x)
  expect_equal(t$decisao_5, "rejeita H0")
  expect_equal(t$extra$ponto_de_mudanca, 40L)
  expect_lt(t$p_valor, 1e-10)
  # O índice é a última observação do trecho de baixo, e o rótulo é o período
  # dela no calendário da série: quarenta meses a partir de janeiro de 1950.
  expect_equal(t$extra$quando, "1953 abr")
  expect_equal(t$conclusao, "há um ponto de mudança na observação 40 (1953 abr)")
  expect_match(t$nota, "40 observações antes do ponto de mudança e 40 a partir dele",
               fixed = TRUE)
})

test_that("Pettitt não acha quebra em série homogênea", {
  # Semente CONFERIDA, e não herdada: sob a semente 7 dois outros blocos desta
  # fase rejeitaram ruído branco, que é o erro tipo I acontecendo — não um
  # defeito, mas um teste que depende de um acaso desses não vale nada. Sob a 42
  # este sai com p = 0,67, longe da borda.
  set.seed(42)
  t <- tr_series_pettitt(stats::rnorm(100))
  expect_equal(t$decisao_5, "não rejeita H0")
  expect_equal(t$conclusao, "não há evidência de ponto de mudança")
  expect_gt(t$p_valor, 0.05)
  expect_equal(t$p_valor, 0.666798, tolerance = 1e-5)
  # Mesmo sem rejeitar, a posição é publicada: é onde o corte foi MAIS
  # favorável, e nem assim deu.
  expect_equal(t$extra$ponto_de_mudanca, 12L)
})

test_that("o p-valor aproximado preso no teto sai com a ressalva", {
  # A aproximação da eq. 3.11 PASSA de 1 em série curta e homogênea, e o
  # `min(1, )` a prende. Sem a ressalva, o card mostraria "p = 1", que se lê como
  # certeza de homogeneidade quando é só a fórmula estourando.
  set.seed(3)
  t <- tr_series_pettitt(stats::rnorm(12))
  expect_equal(t$p_valor, 1)
  expect_match(t$nota, "preso em 1", fixed = TRUE)
  expect_equal(t$decisao_5, "não rejeita H0")
  # E a ressalva NÃO aparece onde não há truncamento: ressalva que sai sempre é
  # ressalva que se aprende a ignorar.
  expect_false(grepl("preso em 1", tr_series_pettitt(serie_crescente())$nota, fixed = TRUE))
})

test_that("o rótulo do ponto sai do calendário da série mensal", {
  # O índice é o que se confere contra outro pacote; o rótulo é o que uma pessoa
  # lê. Numa mensal que começa em janeiro de 1949, a observação 74 é fevereiro de
  # 1955 — e é isso que tem de chegar ao relatório, não o número 74.
  t <- tr_series_pettitt(serie_mensal())
  expect_equal(t$extra$ponto_de_mudanca, 74L)
  expect_equal(t$extra$quando, "1955 fev")
  expect_equal(t$extra$quando, .tr_series_rotulo_em(serie_mensal(), 74))
  expect_match(t$conclusao, "observação 74 (1955 fev)", fixed = TRUE)
  # Numa anual o rótulo é o ano, e aí ele NÃO é o índice: a série do Nilo começa
  # em 1871, e a observação 28 é 1898.
  a <- tr_series_pettitt(serie_anual())
  expect_equal(a$extra$ponto_de_mudanca, 28L)
  expect_equal(a$extra$quando, "1898")
  expect_match(a$conclusao, "observação 28 (1898)", fixed = TRUE)
})

test_that("Pettitt recusa faltante e série curta", {
  expect_error(tr_series_pettitt(datasets::presidents),
               class = "tr_series_error_missing_values")
  err <- tryCatch(tr_series_pettitt(stats::ts(1:10)), condition = identity)
  expect_s3_class(err, "tr_series_error_too_short")
  expect_match(conditionMessage(err), "pelo menos 11", fixed = TRUE)
  # Onze é o piso, e não o primeiro recusado — e o número foi MEDIDO, não
  # escolhido: com dez observações o maior K possível é 25 (a série monotônica,
  # conferida por força bruta sobre as permutações de 1:10) e a eq. 3.11 devolve
  # 0,0661 para ele, acima do corte. Abaixo de onze nem o dado mais extremo que
  # existe alcançaria o corte de 5%, e o bloco devolveria um "não rejeita H0"
  # decidido antes de olhar a série — a mesma armadilha que fixou o piso do
  # `series/cox_stuart`. Com onze, o mesmo caso extremo passa.
  t <- tr_series_pettitt(stats::ts(1:11))
  expect_s3_class(t, "tr_series_test")
  expect_equal(t$estatistica, 30)
  expect_equal(t$decisao_5, "rejeita H0")
  expect_lt(t$p_valor, 0.05)
  # E o piso é justo: um a menos não teria como rejeitar. A conta é feita aqui
  # porque o bloco, corretamente, se recusa a fazê-la.
  expect_gt(min(1, 2 * exp(-6 * 25^2 / (10^3 + 10^2))), 0.05)
})

test_that("o Pettitt atravessa o adaptador, e leva o ponto e o rótulo como colunas", {
  # Este é o primeiro bloco cujo `extra` carrega um valor de TEXTO até uma coluna
  # de relatório: o `cbind` do adaptador tinha visto só número até aqui.
  t <- tr_series_pettitt(serie_mensal())
  tb <- tabela_teste(t)
  expect_s3_class(tb, "tbl_df")
  expect_equal(nrow(tb), 1L)
  expect_equal(tb$teste, "Pettitt")
  expect_false(is.na(tb$p_valor))
  expect_true(is.na(tb$valor_critico_5))
  expect_equal(tb$extra_ponto_de_mudanca, t$extra$ponto_de_mudanca)
  # Texto, e texto de verdade: nem fator, nem NA.
  expect_type(tb$extra_quando, "character")
  expect_equal(tb$extra_quando, "1955 fev")
})

# Sazonalidade ADITIVA, com o padrão escrito à mão: quatro estações, uma bem
# alta, uma média e duas baixas, e ruído pequeno por cima. É o caso em que a
# resposta é conhecida de antemão.
serie_sazonal_aditiva <- function() {
  set.seed(42)
  stats::ts(rep(c(10, 2, -6, -6), 10) + stats::rnorm(40, sd = 1), frequency = 4)
}

# Ruído sem estação nenhuma, e a seed foi CONFERIDA, não escolhida por parecer
# inocente: com `set.seed(2)` esta mesma construção dá H = 7.93 contra um corte
# de 7.81 e REJEITA a 5% — um erro tipo I que se leria como defeito do bloco. As
# doze primeiras seeds foram medidas e esta é uma das que não rejeitam.
serie_sem_estacao <- function() {
  set.seed(1)
  stats::ts(stats::rnorm(40), frequency = 4)
}

test_that("Kruskal-Wallis mede o H, os graus e o p da sazonalidade aditiva", {
  t <- tr_series_kruskal_wallis(serie_sazonal_aditiva())
  expect_s3_class(t, "tr_series_test")
  expect_equal(t$teste, "Kruskal-Wallis")
  expect_equal(t$h0, "as estações têm a mesma distribuição")
  expect_equal(t$rotulo_estat, "H")
  expect_equal(t$estatistica, 33.25610, tolerance = 1e-5)
  expect_equal(t$p_valor, 2.836e-07, tolerance = 1e-3)
  # Os graus são o número de ESTAÇÕES menos um, e não nada derivado do tamanho
  # da série: trimestral dá três, qualquer que seja o número de anos.
  expect_equal(t$extra$graus, 3L)
  expect_equal(t$decisao_5, "rejeita H0")
  expect_equal(t$conclusao, "há sazonalidade")
  expect_match(t$fonte, "Morettin")
  expect_equal(t$nota, "4 estações, com 10 observações cada")
})

test_that("sem estação nenhuma, o Kruskal-Wallis não rejeita", {
  t <- tr_series_kruskal_wallis(serie_sem_estacao())
  expect_equal(t$estatistica, 2.21120, tolerance = 1e-5)
  expect_equal(t$p_valor, 0.52975, tolerance = 1e-4)
  expect_equal(t$decisao_5, "não rejeita H0")
  expect_equal(t$conclusao, "não há evidência de sazonalidade")
})

test_that("a cauda é a SUPERIOR: H grande derruba H0, H pequeno não", {
  # O campo que um copiar-colar do `series/mann_kendall` erraria em silêncio: lá
  # o `sentido` é "menor". Com p-valor na mão quem decide é o `p < alfa`, então
  # um `sentido` errado não inverteria a decisão — ele desenharia a região
  # crítica no lado errado do qui-quadrado no card. Por isso o campo é conferido
  # E o H é posto dos dois lados do corte, que é a checagem que de fato prova
  # que a cauda de cima é a que rejeita.
  com <- tr_series_kruskal_wallis(serie_sazonal_aditiva())
  sem <- tr_series_kruskal_wallis(serie_sem_estacao())
  expect_equal(com$sentido, "maior")
  expect_equal(sem$sentido, "maior")
  corte <- stats::qchisq(0.95, 3)
  # Quem está ACIMA do corte é quem rejeita. Se a cauda fosse a de baixo, estes
  # dois vereditos estariam trocados.
  expect_gt(com$estatistica, corte)
  expect_equal(com$decisao_5, "rejeita H0")
  expect_lt(sem$estatistica, corte)
  expect_equal(sem$decisao_5, "não rejeita H0")
})

test_that("o AirPassengers não rejeita, o log dá idêntico, e tirar a tendência resolve", {
  # O caso que a página do nó explica, preso aqui para que a explicação não possa
  # envelhecer sem o teste reclamar.
  t <- tr_series_kruskal_wallis(serie_mensal())
  expect_equal(t$estatistica, 11.14840, tolerance = 1e-5)
  expect_equal(t$extra$graus, 11L)
  expect_equal(t$p_valor, 0.4309159, tolerance = 1e-6)
  expect_equal(t$decisao_5, "não rejeita H0")
  expect_lt(t$estatistica, stats::qchisq(0.95, 11))
  # O log NÃO ajuda, e não é "ajuda pouco": o teste é de POSTOS e o log é
  # monotônico, então os postos são os MESMOS e os números saem idênticos bit a
  # bit. `expect_identical` de propósito — uma tolerância aqui deixaria passar a
  # afirmação mais fraca, que é justamente a que a página nega.
  l <- tr_series_kruskal_wallis(log(serie_mensal()))
  expect_identical(l$estatistica, t$estatistica)
  expect_identical(l$p_valor, t$p_valor)
  # O que resolve é tirar a TENDÊNCIA, e a diferença simples basta.
  d <- tr_series_kruskal_wallis(diff(serie_mensal()))
  expect_equal(d$estatistica, 119.2025, tolerance = 1e-4)
  expect_equal(d$decisao_5, "rejeita H0")
  expect_gt(d$estatistica, stats::qchisq(0.95, 11))
  expect_equal(d$extra$graus, 11L)
})

test_that("Kruskal-Wallis recusa série sem frequência, série curta e faltante", {
  # Frequência 1: `cycle()` devolveria uma coluna de 1s e o teste compararia um
  # grupo só. A mensagem nomeia a frequência que chegou, que é o que diz à
  # pessoa onde consertar.
  err <- tryCatch(tr_series_kruskal_wallis(serie_anual()), condition = identity)
  expect_s3_class(err, "tr_series_error_no_season")
  expect_match(conditionMessage(err), "frequência 1", fixed = TRUE)
  # Três ciclos, e não dois: com duas observações por estação o maior H possível
  # na trimestral é 6.667, abaixo do corte de 7.815, e o teste não teria como
  # rejeitar. A série 1:8 abaixo é a separação PERFEITA e mesmo assim não passaria.
  expect_lt(unname(stats::kruskal.test(1:8, gl(4, 2))$statistic), stats::qchisq(0.95, 3))
  curta <- tryCatch(tr_series_kruskal_wallis(stats::ts(1:11, frequency = 4)),
                    condition = identity)
  expect_s3_class(curta, "tr_series_error_too_short")
  expect_match(conditionMessage(curta), "pelo menos 3 ciclos completos (12 observações)",
               fixed = TRUE)
  # Três ciclos é o piso, e não o primeiro recusado.
  expect_s3_class(tr_series_kruskal_wallis(stats::ts(1:12, frequency = 4)), "tr_series_test")
  # `presidents` é trimestral e tem buracos: passa no guard de sazonalidade e
  # morre no de faltante, que é a ordem em que os dois estão no bloco.
  expect_error(tr_series_kruskal_wallis(datasets::presidents),
               class = "tr_series_error_missing_values")
})

test_that("o Kruskal-Wallis atravessa o adaptador, e leva os graus como coluna", {
  t <- tr_series_kruskal_wallis(serie_mensal())
  tb <- tabela_teste(t)
  expect_s3_class(tb, "tbl_df")
  expect_equal(nrow(tb), 1L)
  expect_equal(tb$teste, "Kruskal-Wallis")
  expect_false(is.na(tb$p_valor))
  # Sem tabela de críticos: a decisão veio do p-valor.
  expect_true(is.na(tb$valor_critico_5))
  expect_true("extra_graus" %in% names(tb))
  expect_equal(tb$extra_graus, t$extra$graus)
})

# As cinco linhas medidas que a página do `series/periodicity_fisher` publica, e elas SÃO o
# contrato do bloco. Cinco, e não uma: um teste de contrato apoiado numa entrada
# escolhida a dedo prova muito menos do que parece, e foi o que custou duas
# rodadas de revisão no `series/cox_stuart`.
#
# O p-valor entra como log10, e isso NÃO é preciosismo. O `nottem` dá
# p = 7.6e-125, e abaixo da própria tolerância o `all.equal` troca a diferença
# relativa pela ABSOLUTA: medido, `all.equal(7.636e-125, 1e-300)` devolve TRUE.
# Um `expect_equal` direto sobre esse p passaria com qualquer p minúsculo — a
# asserção existiria e não afirmaria nada. No log a comparação volta a ser sobre
# a ORDEM DE GRANDEZA, que é o que de fato se quer travar.
fisher_medido <- function() {
  tibble::tribble(
    ~serie,          ~g,        ~log10p,   ~periodo, ~ciclos, ~decisao,
    "AirPassengers", 0.5018697,  -19.33473,      12,     12L, "rejeita H0",
    "UKgas",         0.5530885,  -16.46421,       4,     27L, "rejeita H0",
    "nottem",        0.9139844, -123.64436,      12,     20L, "rejeita H0",
    "lh",            0.2357169,   -1.20811,       8,      6L, "não rejeita H0",
    "Nile",          0.1833099,   -2.53111,     100,      1L, "rejeita H0")
}

test_that("o Fisher reproduz as cinco linhas medidas da página", {
  m <- fisher_medido()
  for (i in seq_len(nrow(m))) {
    lin <- m[i, ]
    t <- tr_series_fisher(get(lin$serie, envir = asNamespace("datasets")))
    expect_s3_class(t, "tr_series_test")
    expect_equal(t$teste, "Fisher", info = lin$serie)
    expect_equal(t$h0, "a série não tem periodicidade", info = lin$serie)
    expect_equal(t$rotulo_estat, "g", info = lin$serie)
    expect_equal(t$fonte, "Morais (2012)", info = lin$serie)
    # A cauda é a SUPERIOR: é o g grande — o pico que concentra a potência — que
    # derruba H0. Um "menor" copiado dos vizinhos de tendência não mudaria a
    # decisão (ela vem do p-valor), mas sombrearia o lado errado no card e faria
    # o `g > zα` publicado ao lado parecer invertido.
    expect_equal(t$sentido, "maior", info = lin$serie)
    expect_equal(t$estatistica, lin$g, tolerance = 1e-6, info = lin$serie)
    expect_equal(log10(t$p_valor), lin$log10p, tolerance = 1e-5, info = lin$serie)
    expect_equal(t$extra$periodo, lin$periodo, info = lin$serie)
    expect_equal(t$extra$ciclos, lin$ciclos, info = lin$serie)
    expect_equal(t$decisao_5, lin$decisao, info = lin$serie)
  }
})

test_that("o g contra zα e o p contra 5% decidem a mesma coisa nas cinco", {
  # A equivalência que a página AFIRMA, e que um leitor vindo da dissertação vai
  # querer conferir: ela decide por `g > zα` (eq. 3.42), o bloco decide por
  # p-valor, e as duas regras são a mesma escrita de dois jeitos. O zα é
  # recalculado AQUI, da fórmula, para que o teste não confira o bloco contra ele
  # mesmo.
  for (nm in fisher_medido()$serie) {
    s <- get(nm, envir = asNamespace("datasets"))
    t <- tr_series_fisher(s)
    # m ordenadas: sem a frequência zero e sem a de Nyquist (Fisher 1929).
    expect_equal(t$extra$ordenadas, (length(s) - 1L) %/% 2L, info = nm)
    za <- unname(t$criticos[["5%"]])
    expect_identical(t$estatistica > za, t$p_valor < 0.05, info = nm)
    expect_identical(t$decisao_5 == "rejeita H0", t$p_valor < 0.05, info = nm)
  }
  # E o caso apertado, nomeado: no `lh` o g fica logo ABAIXO de zα e o p logo
  # ACIMA de 5%. É a linha em que as duas regras poderiam discordar por
  # arredondamento, e é por isso que ela está na página e aqui.
  t <- tr_series_fisher(datasets::lh)
  za <- unname(t$criticos[["5%"]])
  expect_lt(t$estatistica, za)
  expect_gt(t$p_valor, 0.05)
  expect_equal(t$decisao_5, "não rejeita H0")
  # E por pouco: a folga entre os dois é de menos de um centésimo.
  expect_lt(za - t$estatistica, 0.01)
})

test_that("o Nile rejeita, e o período publicado denuncia o artefato", {
  # A armadilha principal do bloco. O `Nile` é anual e não tem sazonalidade
  # nenhuma, e mesmo assim o teste rejeita: o maior pico está na frequência mais
  # baixa da grade, com período igual à série INTEIRA. Fisher acha o maior pico
  # onde quer que ele esteja, e um pico ali é tendência, não estação.
  t <- tr_series_fisher(datasets::Nile)
  expect_equal(t$decisao_5, "rejeita H0")
  expect_equal(t$conclusao, "há periodicidade, com pico no período de 100 observações")
  # O número que desmente a conclusão, publicado ao lado dela: o período é o
  # comprimento da série, e o ciclo não chega a se repetir uma segunda vez.
  expect_equal(t$extra$periodo, length(datasets::Nile))
  expect_equal(t$extra$ciclos, 1L)
  # E dito em palavras, porque o número sozinho não se lê.
  expect_match(t$nota, "ATENÇÃO", fixed = TRUE)
  expect_match(t$nota, "não de estação", fixed = TRUE)
  # O aviso é do caso patológico, e não de todo card: numa série com estação de
  # verdade ele não aparece.
  expect_false(grepl("ATENÇÃO", tr_series_fisher(serie_mensal())$nota, fixed = TRUE))
})

test_that("um ciclo curto de verdade é achado no período certo", {
  # O contraponto do `Nile`: aqui a estação existe e é conhecida de antemão —
  # quatro trimestres —, e é nesse período que o pico tem de cair.
  t <- tr_series_fisher(serie_sazonal_aditiva())
  expect_equal(t$estatistica, 0.9648497, tolerance = 1e-6)
  expect_equal(log10(t$p_valor), -24.89451, tolerance = 1e-5)
  expect_equal(t$decisao_5, "rejeita H0")
  expect_equal(t$extra$periodo, 4)
  # Dez ciclos completos dentro das quarenta observações: o oposto do `Nile`.
  expect_equal(t$extra$ciclos, 10L)
  expect_match(t$nota, "ciclo declarado", fixed = TRUE)
  expect_false(grepl("ATENÇÃO", t$nota, fixed = TRUE))
  # O período sai em OBSERVAÇÕES, e a diferença importa: `1 / freq` daria 1 para
  # o `AirPassengers` mensal — a estação de doze meses chamada de "1", que se lê
  # como "um mês". Em observações ela é 12.
  expect_equal(tr_series_fisher(serie_mensal())$extra$periodo, 12)
})

test_that("o Fisher recusa faltante e série curta, e ACEITA frequência 1", {
  expect_error(tr_series_fisher(datasets::presidents),
               class = "tr_series_error_missing_values")
  curta <- tryCatch(tr_series_fisher(stats::ts(1:7)), condition = identity)
  expect_s3_class(curta, "tr_series_error_too_short")
  expect_match(conditionMessage(curta), "pelo menos 8", fixed = TRUE)
  # Oito é o piso, e não o primeiro recusado.
  expect_s3_class(tr_series_fisher(stats::ts(c(1, 4, 2, 8, 5, 7, 3, 6))), "tr_series_test")
  # A decisão que separa este bloco do irmão, e ela é deliberada: o
  # Kruskal-Wallis AGRUPA por estação e sem frequência não tem grupo; o Fisher lê
  # o periodograma, que existe para qualquer série, e recusar frequência 1
  # bloquearia justamente o uso de caçar um período que ninguém declarou. É assim
  # que a linha do `Nile` da página é medida.
  expect_s3_class(tr_series_fisher(serie_anual()), "tr_series_test")
  expect_error(tr_series_kruskal_wallis(serie_anual()),
               class = "tr_series_error_no_season")
})

test_that("o Fisher atravessa o adaptador, e leva o período como coluna", {
  t <- tr_series_fisher(serie_mensal())
  tb <- tabela_teste(t)
  expect_s3_class(tb, "tbl_df")
  expect_equal(nrow(tb), 1L)
  expect_equal(tb$teste, "Fisher")
  expect_false(is.na(tb$p_valor))
  expect_true(all(c("extra_periodo", "extra_ciclos") %in% names(tb)))
  expect_equal(tb$extra_periodo, 12)
  expect_equal(tb$extra_ciclos, 12L)
  # Diferente dos irmãos de sazonalidade, este bloco TEM tabela de críticos: o zα
  # da dissertação sai na coluna do valor crítico a 5%, que é o que deixa a regra
  # `g > zα` ser conferida no relatório sem refazer a conta.
  expect_equal(tb$valor_critico_5, 0.0983575, tolerance = 1e-5)
})

# ---- Oráculo do Fisher (revisão metodológica, fase 1) -------------------------
# Fisher (1929) define g sobre as m = floor((N - 1) / 2) ordenadas de Fourier
# j = 1..m — sem a frequência zero e sem a de Nyquist, cuja ordenada é um
# qui-quadrado com UM grau (as outras têm dois) — e dá o p-valor exato
# P(g > x) = sum_{j=1}^{floor(1/x)} (-1)^(j-1) choose(m, j) (1 - j x)^(m-1).
# A implementação de referência é `GeneCycle::fisher.g.test` (Wichert,
# Fokianos & Strimmer 2004), que tira só a média, descarta a ordenada de Nyquist
# e soma a série inteira.
test_that("Fisher com remover = 'media' reproduz GeneCycle::fisher.g.test", {
  skip_if_not_installed("GeneCycle")
  for (nm in c("AirPassengers", "UKgas", "nottem", "lh", "Nile", "sunspot.year")) {
    s <- get(nm, envir = asNamespace("datasets"))
    t <- tr_series_fisher(s, remover = "media")
    expect_equal(t$p_valor, GeneCycle::fisher.g.test(as.numeric(s)),
                 tolerance = 1e-10, info = nm)
  }
  # Série ímpar: não há ordenada de Nyquist a descartar.
  s <- stats::ts(datasets::lh[-1])
  expect_equal(tr_series_fisher(s, remover = "media")$p_valor,
               GeneCycle::fisher.g.test(as.numeric(s)), tolerance = 1e-10)
})

test_that("Fisher com remover = 'reta' (padrão) é o GeneCycle sobre os resíduos da reta", {
  skip_if_not_installed("GeneCycle")
  for (nm in c("AirPassengers", "UKgas", "nottem", "lh", "Nile")) {
    s <- get(nm, envir = asNamespace("datasets"))
    tt <- seq_along(s)
    e <- stats::residuals(stats::lm(as.numeric(s) ~ tt))
    expect_equal(tr_series_fisher(s)$p_valor, GeneCycle::fisher.g.test(e),
                 tolerance = 1e-10, info = nm)
  }
})

test_that("o crítico a 5% do Fisher é o quantil EXATO: p(zα) = 0,05", {
  for (nm in c("AirPassengers", "lh", "Nile")) {
    s <- get(nm, envir = asNamespace("datasets"))
    t <- tr_series_fisher(s)
    za <- unname(t$criticos[["5%"]])
    m <- t$extra$ordenadas
    j <- seq_len(floor(1 / za))
    p_za <- sum((-1)^(j - 1) * choose(m, j) * (1 - j * za)^(m - 1))
    expect_equal(p_za, 0.05, tolerance = 1e-8, info = nm)
    expect_identical(t$estatistica > za, t$p_valor < 0.05, info = nm)
  }
  # Conferência analítica: quando o crítico passa de 1/2 a série exata tem um
  # termo só, e ele é a fórmula fechada 1 - (alfa/m)^(1/(m-1)) (eq. 3.42 da
  # dissertação). Com m = 5, 0,6838.
  expect_equal(.tr_series_fisher_critico(5, 0.05), 1 - (0.05 / 5)^(1 / 4), tolerance = 1e-10)
  expect_equal(round(.tr_series_fisher_critico(5, 0.05), 4), 0.6838)
  # Abaixo de 1/2 o primeiro termo SUPERESTIMA o crítico (é conservador).
  expect_lt(.tr_series_fisher_critico(50, 0.05), 1 - (0.05 / 50)^(1 / 49))
})

test_that("Fisher recusa opção de remoção desconhecida", {
  expect_error(tr_series_fisher(serie_mensal(), remover = "nada"),
               class = "tr_series_error_bad_option")
})

# ---- Phillips-Perron só com constante (revisão metodológica, fase 1) ----------
# Z(t) de Phillips & Perron (1988), na forma geral (Hamilton 1994, eq. 17.6.8):
# regressão de y_t em 1 e y_{t-1}, variância de longo prazo de Newey-West com
# janela trunc(4 (n/100)^(1/4)) e pesos de Bartlett.
test_that("PP com 'tendência' (padrão) segue idêntico ao stats::PP.test e ao tseries", {
  set.seed(1)
  x <- stats::ts(cumsum(stats::rnorm(150)))
  p <- tr_series_phillips_perron(x)
  pp <- stats::PP.test(as.numeric(x))
  expect_equal(p$estatistica, unname(pp$statistic), tolerance = 1e-12)
  expect_equal(p$p_valor, pp$p.value, tolerance = 1e-12)
  skip_if_not_installed("tseries")
  expect_equal(p$estatistica,
               unname(suppressWarnings(tseries::pp.test(as.numeric(x), type = "Z(t_alpha)"))$statistic),
               tolerance = 1e-12)
})

test_that("PP com 'constante' reproduz aTSA::pp.test (tipo 2) e o urca::ur.pp", {
  skip_if_not_installed("aTSA")
  set.seed(1)
  for (n in c(60, 150, 400)) {
    x <- cumsum(stats::rnorm(n))
    p <- tr_series_phillips_perron(stats::ts(x), deterministico = "constante")
    ref <- aTSA::pp.test(x, type = "Z_tau", lag.short = TRUE, output = FALSE)
    expect_equal(p$estatistica, ref[2, "Z_tau"], tolerance = 1e-10, info = as.character(n))
    # O p-valor é o de MacKinnon (1996); o aTSA interpola a tabela de Fuller.
    # Longe da borda os dois concordam na decisão.
    expect_identical(p$p_valor < 0.05, ref[2, "p.value"] < 0.05)
    # O urca normaliza a variância do erro de outro jeito (MacKinnon): concorda
    # a menos de 0,5%.
    u <- urca::ur.pp(x, type = "Z-tau", model = "constant", lags = "short")
    expect_equal(p$estatistica, u@teststat[[1]], tolerance = 5e-3, info = as.character(n))
  }
  # A nota diz qual determinístico entrou.
  expect_match(tr_series_phillips_perron(serie_mensal(), "constante")$nota, "só constante")
})

test_that("PP 'constante': p-valor pela superfície de resposta de MacKinnon (1996)", {
  # Oráculo 1: os críticos assintóticos de tau_c de MacKinnon (2010, tab. 1)
  # voltam como 1%, 5% e 10%.
  expect_equal(.tr_series_pp_p_constante(c(-3.43035, -2.86154, -2.56677), Inf),
               c(0.01, 0.05, 0.10), tolerance = 1e-3)
  # Oráculo 2: as colunas de 1% e 5% da tabela tau_mu de Fuller (1976, tab.
  # 8.5.2; n = 25 e 100) caem a menos de 0,002 do nível.
  expect_lt(max(abs(.tr_series_pp_p_constante(c(-3.75, -3.00), 25) - c(0.01, 0.05))), 0.002)
  expect_lt(max(abs(.tr_series_pp_p_constante(c(-3.51, -2.89), 100) - c(0.01, 0.05))), 0.002)
  # Oráculo 3: é a função do urca, com N = observações da regressão.
  expect_equal(.tr_series_pp_p_constante(-2.5, 59),
               urca::punitroot(-2.5, N = 59, trend = "c", statistic = "t"), tolerance = 1e-12)
  # Sem borda presa: um Z muito negativo sai abaixo de 0,01, sem nota de truncagem.
  set.seed(6)
  x <- stats::ts(stats::arima.sim(list(ar = 0.2), 200, rand.gen = function(n, ...) stats::rnorm(n)))
  t <- tr_series_phillips_perron(x, "constante")
  expect_lt(t$p_valor, 0.01)
  expect_false(grepl("truncado", t$nota))
})

test_that("PP em série curta (< 25) avisa na nota, nos dois determinísticos", {
  # Medido sob passeio aleatório, 4000 réplicas: com n = 12 rejeita a 5% em 7%
  # (constante) e 10% (tendência); com n = 25, 5,9% e 4,9%.
  x <- stats::ts(cumsum(c(0.3, -1.2, 0.8, 1.1, -0.4, 0.5, -0.9, 1.4, 0.2, -0.6, 0.7, 1.0, -0.2, 0.4)))
  for (d in c("constante", "tendência")) {
    expect_match(tr_series_phillips_perron(x, d)$nota, "menos de 25 observações", fixed = TRUE)
  }
  set.seed(8)
  expect_false(grepl("menos de 25", tr_series_phillips_perron(stats::ts(cumsum(stats::rnorm(25))),
                                                             "constante")$nota))
})

test_that("PP 'constante' tem mais poder que 'tendência' em série estacionária sem tendência", {
  # É o motivo da opção (Phillips & Perron 1988): tendência supérflua gasta
  # poder. AR(1) com phi = 0,85, n = 80, 300 réplicas.
  set.seed(11)
  rej <- replicate(300, {
    x <- stats::ts(stats::arima.sim(list(ar = 0.85), 80))
    c(tr_series_phillips_perron(x, "constante")$p_valor < 0.05,
      tr_series_phillips_perron(x, "tendência")$p_valor < 0.05)
  })
  expect_gt(mean(rej[1, ]), mean(rej[2, ]))
})

test_that("PP recusa determinístico desconhecido", {
  expect_error(tr_series_phillips_perron(serie_mensal(), "nenhum"),
               class = "tr_series_error_bad_option")
})

# ---- Zivot-Andrews: defasagens do geral para o específico (fase 1) ------------
# Zivot & Andrews (1992) escolhem k como Perron (1989): partem de um teto e
# tiram a última diferença defasada enquanto o t dela não for significativo a
# 10%. É o padrão desde a versão 2 do nó (`selecao = "t_sig"`).
test_that("a regressão do corte refaz o t do urca::ur.za", {
  set.seed(4)
  x <- cumsum(stats::rnorm(80))
  for (m in c("intercept", "trend", "both")) {
    z <- urca::ur.za(x, model = m, lag = 3L)
    for (q in c(15L, 40L, 60L)) {
      cf <- stats::coef(summary(.tr_series_za_lm(x, m, 3L, q)))
      expect_equal((cf["y.l1", 1] - 1) / cf["y.l1", 2], z@tstats[[q]],
                   tolerance = 1e-10, info = paste(m, q))
    }
  }
})

test_that("a seleção geral→específico para no primeiro k com última defasagem significativa", {
  set.seed(8)
  # AR(2) nas diferenças: a segunda defasagem importa, as de cima não.
  e <- stats::arima.sim(list(ar = c(0.5, -0.4)), 150)
  x <- stats::ts(cumsum(e))
  t <- tr_series_zivot_andrews(x, mudanca = "nível", defasagens = 8L)
  k <- t$extra$defasagens
  expect_true(k >= 1L && k <= 8L)
  crit <- stats::qnorm(0.95)
  x <- as.numeric(x)
  # A última escolhida é significativa, no corte escolhido com ela...
  cf <- stats::coef(summary(.tr_series_za_lm(x, "intercept", k, t$extra$quebra)))
  expect_gte(abs(cf[paste0("y.dl", k), "t value"]), crit)
  # ...e toda k maior até o teto foi descartada por não ser.
  for (kk in seq.int(k + 1L, length.out = 8L - k)) {
    q <- .tr_series_za_janela(x, "intercept", kk)$quebra
    cf <- stats::coef(summary(.tr_series_za_lm(x, "intercept", kk, q)))
    expect_lt(abs(cf[paste0("y.dl", kk), "t value"]), crit)
  }
  # O resultado é o ur.za com o k escolhido, mínimo na janela de 15% a 85%.
  z <- urca::ur.za(x, model = "intercept", lag = k)
  n <- length(x)
  expect_equal(t$estatistica, min(z@tstats[ceiling(0.15 * n):floor(0.85 * n)]),
               tolerance = 1e-12)
  expect_match(t$nota, "do geral para o específico (teto 8", fixed = TRUE)
})

test_that("ruído branco nas diferenças leva a seleção até k = 0", {
  set.seed(2)
  x <- stats::ts(cumsum(stats::rnorm(100)))
  t <- tr_series_zivot_andrews(x, mudanca = "nível", defasagens = 4L)
  # Nem sempre 0 (10% de chance por degrau), mas nesta semente sim, e a
  # estatística é a do ur.za com lag = 0.
  expect_equal(t$extra$defasagens, 0L)
  z <- urca::ur.za(as.numeric(x), model = "intercept", lag = 0L)
  expect_equal(t$estatistica, min(z@tstats[15:85]), tolerance = 1e-12)
})

test_that("Zivot & Andrews (1992): PNB real de Nelson-Plosser, modelo A, k = 8", {
  # Série anual 1909-1970 em log (urca::nporg), modelo de mudança de nível com
  # k = 8, o valor que o artigo usa (herdado de Perron 1989). O artigo publica
  # t = -5.58 com quebra em 1929; conferido aqui a duas casas.
  data("nporg", package = "urca", envir = environment())
  d <- stats::na.omit(nporg[nporg$year <= 1970, c("year", "gnp.r")])
  x <- stats::ts(log(d$gnp.r), start = d$year[[1]])
  t <- tr_series_zivot_andrews(x, mudanca = "nível", defasagens = 8L, selecao = "fixa")
  expect_equal(round(t$estatistica, 2), -5.58)
  expect_equal(t$extra$quando, "1929")
  expect_equal(t$decisao_5, "rejeita H0")
  # PNB nominal: -5.82, 1929.
  d <- stats::na.omit(nporg[nporg$year <= 1970, c("year", "gnp.n")])
  x <- stats::ts(log(d$gnp.n), start = d$year[[1]])
  t <- tr_series_zivot_andrews(x, mudanca = "nível", defasagens = 8L, selecao = "fixa")
  expect_equal(round(t$estatistica, 2), -5.82)
  expect_equal(t$extra$quando, "1929")
})

test_that("teto de defasagens que não cabe é recusado também na seleção", {
  e <- tryCatch(tr_series_zivot_andrews(stats::ts(stats::rnorm(20)), defasagens = 7L),
                condition = identity)
  expect_s3_class(e, "tr_series_error_bad_option")
  # O teto AUTOMÁTICO (Schwert) nunca é erro: é limitado ao que cabe.
  expect_s3_class(tr_series_zivot_andrews(stats::ts(stats::rnorm(20))), "tr_series_test")
  expect_error(tr_series_zivot_andrews(serie_mensal(), selecao = "aic"),
               class = "tr_series_error_bad_option")
})

# ---- F com erro ARMA (GLS): Wald F, refeito à mão ------------------------------
wald_f <- function(fit, idx) {
  b <- stats::coef(fit)[idx]; V <- stats::vcov(fit)[idx, idx, drop = FALSE]
  q <- length(idx)
  Fv <- as.numeric(t(b) %*% solve(V, b)) / q
  c(F = Fv, p = stats::pf(Fv, q, length(stats::fitted(fit)) - length(stats::coef(fit)),
                           lower.tail = FALSE))
}

test_that("os três F com erro AR(1) são o F de Wald do GLS", {
  x <- log(datasets::AirPassengers)
  r <- tr_series_regression(x, grau = 2L, erro = "arma", ar = 1L)
  nm <- names(stats::coef(r$ajuste))
  g <- tr_series_f_global(r); s <- tr_series_f_sazonal(r); d <- tr_series_f_tendencia(r)
  for (par in list(list(g, which(nm != "(Intercept)")),
                   list(s, grep("^estacao", nm)),
                   list(d, match(c("t1", "t2"), nm)))) {
    w <- wald_f(r$ajuste, par[[2]])
    expect_equal(par[[1]]$estatistica, w[["F"]], tolerance = 1e-8)
    expect_equal(par[[1]]$p_valor, w[["p"]], tolerance = 1e-8)
    expect_match(par[[1]]$nota, "erro ARMA(1, 0)", fixed = TRUE)
  }
  expect_match(s$nota, "11 graus no numerador")
})

test_that("com erro AR(1) o F de MQO é otimista e o do GLS chega perto do nominal", {
  # O motivo da opção. Sem tendência nenhuma, erro AR(1) com phi = 0,6, n = 120:
  # medido em 300 réplicas, o F de tendência do MQO rejeita em 37% e o do GLS
  # em 8%. Com phi = 0,9 são 69% e 17%: o GLS melhora muito, mas o F de Wald
  # ainda passa do nominal perto da raiz unitária (a página avisa).
  set.seed(21)
  rej <- replicate(150, {
    e <- stats::arima.sim(list(ar = 0.6), 120)
    x <- stats::ts(as.numeric(e), frequency = 12)
    c(tr_series_f_tendencia(tr_series_regression(x, sazonalidade = FALSE))$p_valor < 0.05,
      tr_series_f_tendencia(tr_series_regression(x, sazonalidade = FALSE, erro = "arma"))$p_valor < 0.05)
  })
  expect_gt(mean(rej[1, ]), 0.25)
  expect_lt(mean(rej[2, ]), 0.12)
})
