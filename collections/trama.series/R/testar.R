# Testes: série entra, UM TESTE sai — um bloco, uma hipótese nula, um veredito.
#
# Tipo próprio, e não tabela, porque o card de um teste tem de mostrar a
# conclusão de relance, e oito colunas num card de 240px não mostram. A tabela
# continua a um fio de distância pelo adaptador — é ela que exporta, e o card
# que se lê.

#' Venceu o corte de `alfa`?
#'
#' Duas fontes de decisão, e a ordem importa: p-valor quando existe, tabela de
#' valor crítico quando não. Nunca interpolar um p-valor a partir da tabela —
#' seria inventar precisão que o `urca` não dá.
#'
#' `sentido` diz que cauda rejeita: ADF e Phillips-Perron rejeitam ABAIXO do
#' crítico, KPSS ACIMA. Sem esse campo a regra viveria num `if` por nome de
#' teste, e teste novo obrigaria a mexer aqui e no JavaScript.
#' @noRd
.tr_series_rejeita <- function(estatistica, p_valor, criticos, sentido, alfa = 0.05) {
  if (!is.na(p_valor)) return(p_valor < alfa)
  cv <- criticos[[.tr_series_rotulo_nivel(alfa)]]
  if (is.null(cv) || is.na(cv)) return(NA)
  if (sentido == "menor") estatistica < cv else estatistica > cv
}

#' O rótulo do nível, montado sem aritmética de ponto flutuante.
#'
#' `alfa * 100` para 0,1 dá 10.000000000000002 — a chave nunca bateria na
#' tabela de críticos. O mesmo cuidado está do lado do JavaScript.
#' @noRd
.tr_series_rotulo_nivel <- function(alfa) {
  switch(as.character(alfa), "0.1" = "10%", "0.05" = "5%", "0.01" = "1%",
         .tr_series_option("alfa", alfa, "0.1, 0.05, 0.01"))
}

#' O registro de um teste.
#' @noRd
.tr_series_teste <- function(teste, h0, estatistica, rotulo_estat, p_valor = NA_real_,
                             criticos = NULL, sentido = "menor",
                             conclusao_sim, conclusao_nao, nota = "", fonte, extra = NULL) {
  sentido <- .tr_series_enum(sentido, c("menor", "maior"), "sentido")
  # Um teste precisa de ALGUMA fonte de decisão. Sem p-valor e sem tabela, o
  # `.tr_series_rejeita` devolve NA e o `isTRUE()` lá embaixo o transforma num
  # "não rejeita H0" confiante — a mesma mentira silenciosa que a conferência
  # da tabela fecha, entrando pela outra porta.
  if (is.na(p_valor) && is.null(criticos)) {
    .tr_series_abort("tr_series_error_bad_criticos",
                     paste0("'%s': o bloco não trouxe nem p-valor nem tabela de valores ",
                            "críticos, e sem um dos dois não há como decidir."), teste)
  }
  # A tabela é conferida AQUI, e não lá dentro do `.tr_series_rejeita`: é aqui
  # que o bloco a monta, e é aqui que o nome do teste existe pra pôr no erro.
  if (!is.null(criticos)) .tr_series_criticos(criticos, teste)
  rejeita <- .tr_series_rejeita(estatistica, p_valor, criticos, sentido)
  structure(list(
    teste = teste, h0 = h0,
    estatistica = as.numeric(estatistica), rotulo_estat = rotulo_estat,
    p_valor = as.numeric(p_valor), criticos = criticos, sentido = sentido,
    decisao_5 = if (isTRUE(rejeita)) "rejeita H0" else "não rejeita H0",
    conclusao = if (isTRUE(rejeita)) conclusao_sim else conclusao_nao,
    nota = nota, fonte = fonte, extra = extra
  ), class = "tr_series_test")
}

#' ADF: a série tem raiz unitária?
#'
#' H0 é a RAIZ UNITÁRIA, e isso inverte a leitura de quem está acostumado a
#' p-valor pequeno ser "achei alguma coisa": aqui rejeitar é concluir
#' ESTACIONÁRIA. O KPSS é o outro lado, com H0 oposta, e é a concordância dos
#' dois que dá chão à conclusão — a ajuda dos dois aponta um para o outro.
#' @export
tr_series_adf <- function(serie, deterministico = "constante", defasagens = 0L) {
  det <- .tr_series_enum(deterministico, c("constante", "tendência"), "deterministico")
  .tr_series_sem_na(serie, "series/adf")
  .tr_series_minimo(serie, 12L, "series/adf", "um teste de raiz unitária")
  x <- as.numeric(serie)
  k <- .tr_series_int(defasagens, "defasagens", min = 0L)
  if (k == 0L) k <- as.integer(trunc((length(x) - 1)^(1 / 3)))
  a <- urca::ur.df(x, type = if (det == "constante") "drift" else "trend",
                   lags = k, selectlags = "AIC")
  # Linha 1 da tabela do `urca` é a do tau — `tau2` no drift, `tau3` no trend
  # —, e é a que corresponde a `teststat[[1]]`. As linhas de baixo são os F
  # conjuntos (`phi*`), de hipótese nula OUTRA: ler um `phi` como se fosse o
  # tau daria um veredito plausível e errado.
  cv <- a@cval[1, ]
  .tr_series_teste(
    "ADF", "a série tem raiz unitária", a@teststat[[1]], "t",
    criticos = c(`10%` = cv[["10pct"]], `5%` = cv[["5pct"]], `1%` = cv[["1pct"]]),
    sentido = "menor",
    conclusao_sim = "estacionária",
    conclusao_nao = "não há evidência contra a raiz unitária",
    nota = sprintf("defasagens por AIC, até %d; %s", k, det),
    fonte = "Dickey & Fuller (1979, 1981)")
}

#' KPSS: a série é estacionária?
#'
#' O outro lado do ADF: aqui H0 é a ESTACIONARIEDADE, e a rejeição vem da
#' cauda de CIMA (`sentido = "maior"`). É o único teste da coleção assim, e é
#' por causa dele que o campo existe — sem ele a regra de decisão viveria num
#' `if` por nome de teste.
#' @export
tr_series_kpss <- function(serie, deterministico = "constante") {
  det <- .tr_series_enum(deterministico, c("constante", "tendência"), "deterministico")
  .tr_series_sem_na(serie, "series/kpss")
  .tr_series_minimo(serie, 12L, "series/kpss", "um teste de estacionariedade")
  kp <- urca::ur.kpss(as.numeric(serie),
                      type = if (det == "constante") "mu" else "tau", lags = "short")
  # A tabela do `ur.kpss` tem QUATRO colunas — 10pct, 5pct, 2.5pct, 1pct —, e
  # não três como a do `ur.df`. Montada por posição, o que entra como "1%" é
  # na verdade o 2,5%: um corte mais frouxo, plausível o bastante para
  # ninguém desconfiar. Por nome, então.
  cv <- kp@cval[1, ]
  .tr_series_teste(
    # H0 diz em torno DE QUÊ a série é estacionária: com `tendência` o KPSS
    # aceita uma reta como parte da hipótese nula, e "estacionária" sozinho
    # esconderia isso de quem lê o relatório.
    "KPSS", sprintf("a série é estacionária em torno de %s",
                    if (det == "constante") "um nível" else "uma tendência"),
    kp@teststat[[1]], "eta",
    criticos = c(`10%` = cv[["10pct"]], `5%` = cv[["5pct"]], `1%` = cv[["1pct"]]),
    sentido = "maior",
    conclusao_sim = "não estacionária",
    # Não rejeitar não é mostrar estacionariedade: com série curta o KPSS deixa
    # de rejeitar por falta de poder, e "estacionária" aqui afirmaria H0 como fato.
    conclusao_nao = "não há evidência contra a estacionariedade",
    nota = sprintf("%s; janela de defasagens curta", det),
    fonte = "Kwiatkowski et al. (1992)")
}

#' Phillips-Perron: a série tem raiz unitária?
#'
#' Mesma H0 do ADF — raiz unitária —, e mesma leitura invertida: rejeitar é
#' concluir estacionária. Difere no meio do caminho: em vez de acrescentar
#' defasagens à regressão, corrige a estatística para a autocorrelação dos
#' resíduos.
#' @export
tr_series_phillips_perron <- function(serie, deterministico = "tendência") {
  det <- .tr_series_enum(deterministico, c("constante", "tendência"), "deterministico")
  .tr_series_sem_na(serie, "series/phillips_perron")
  .tr_series_minimo(serie, 12L, "series/phillips_perron", "um teste de raiz unitária")
  # O p-valor é interpolado numa tabela e, fora dela, PRESO na borda
  # (`approx(rule = 2)`) sem aviso nenhum. Um "0,01" que é na verdade "menor que
  # 0,01" não pode sair sem a ressalva, então ela é deduzida do próprio valor e
  # vai para a nota. As bordas são 0,01 e 0,99 (as das duas tabelas), e não
  # 0,1: o passeio aleatório sai com p = 0,62, interpolado, e um corte em 0,1 o
  # marcaria como truncado sem estar.
  if (det == "tendência") {
    # Com tendência, o `stats::PP.test` — inalterado desde a versão 1 do nó.
    pp <- stats::PP.test(as.numeric(serie))
    estat <- unname(pp$statistic)
    p <- pp$p.value
    nota <- "com constante e tendência"
  } else {
    # Só constante: o `stats` não tem, e a conta é a forma geral do Z(t)
    # (Phillips & Perron 1988; Hamilton 1994, eq. 17.6.8) com as MESMAS
    # convenções do `PP.test` — janela curta, pesos de Bartlett, gamma_0 com
    # divisor n. Conferida contra `aTSA::pp.test` (tipo 2) a 1e-10.
    estat <- .tr_series_pp_z_constante(as.numeric(serie))
    p <- .tr_series_pp_p_constante(estat, length(serie) - 1L)
    nota <- "só constante (sem tendência)"
  }
  if (p <= 0.01) {
    nota <- paste0(nota, "; p-valor truncado na borda da tabela: o verdadeiro é <= 0,01")
  }
  if (p >= 0.99) {
    nota <- paste0(nota, "; p-valor truncado na borda da tabela: o verdadeiro é >= 0,99")
  }
  .tr_series_teste(
    "Phillips-Perron", "a série tem raiz unitária", estat, "Z(t)",
    p_valor = p,
    sentido = "menor",
    conclusao_sim = "estacionária",
    conclusao_nao = "não há evidência contra a raiz unitária",
    nota = nota,
    fonte = "Phillips & Perron (1988)")
}

#' Z(t) de Phillips-Perron com só constante.
#'
#' Regressão y_t = a + rho y_{t-1} + u_t; t de rho = 1 corrigido pela variância
#' de longo prazo lambda² (Newey-West, janela trunc(4 (n/100)^(1/4))):
#' Z(t) = sqrt(g0/lambda²) t - (lambda² - g0) n se(rho) / (2 lambda s).
#' @noRd
.tr_series_pp_z_constante <- function(x) {
  z <- stats::embed(x, 2)
  yt <- z[, 1]; yt1 <- z[, 2]
  n <- length(yt)
  fit <- stats::lm(yt ~ yt1)
  cf <- stats::coef(summary(fit))["yt1", ]
  tstat <- (cf[[1]] - 1) / cf[[2]]
  u <- stats::residuals(fit)
  g0 <- sum(u^2) / n
  l <- trunc(4 * (n / 100)^0.25)
  gam <- vapply(seq_len(l), function(i) sum(u[-seq_len(i)] * u[seq_len(n - i)]) / n, 0)
  lam <- g0 + 2 * sum((1 - seq_len(l) / (l + 1)) * gam)
  s <- summary(fit)$sigma
  sqrt(g0 / lam) * tstat - (lam - g0) * n * cf[[2]] / (2 * sqrt(lam) * s)
}

#' P-valor do Z(t) com constante, pela tabela tau_mu de Fuller (1976, tab.
#' 8.5.2), interpolada em n e depois no quantil — o mesmo esquema do
#' `stats::PP.test` com a tabela tau_tau.
#' @noRd
.tr_series_pp_p_constante <- function(estat, n) {
  tabela <- rbind(
    c(-3.75, -3.33, -3.00, -2.63, -0.37,  0.00, 0.34, 0.72),
    c(-3.58, -3.22, -2.93, -2.60, -0.40, -0.03, 0.29, 0.66),
    c(-3.51, -3.17, -2.89, -2.58, -0.42, -0.05, 0.26, 0.63),
    c(-3.46, -3.14, -2.88, -2.57, -0.42, -0.06, 0.24, 0.62),
    c(-3.44, -3.13, -2.87, -2.57, -0.43, -0.07, 0.24, 0.61),
    c(-3.43, -3.12, -2.86, -2.57, -0.44, -0.07, 0.23, 0.60))
  tam <- c(25, 50, 100, 250, 500, 1e5)
  prob <- c(0.01, 0.025, 0.05, 0.1, 0.9, 0.95, 0.975, 0.99)
  q <- vapply(seq_len(ncol(tabela)),
              function(j) stats::approx(tam, tabela[, j], n, rule = 2)$y, 0)
  stats::approx(q, prob, estat, rule = 2)$y
}

#' Zivot-Andrews: raiz unitária, com a quebra estimada pelo próprio teste?
#'
#' Mesma H0 do `series/adf` — raiz unitária — e mesma leitura invertida:
#' rejeitar é concluir estacionária. O que o justifica é o VIÉS dos outros dois:
#' ADF e Phillips-Perron tendem a NÃO rejeitar quando a série tem quebra
#' estrutural (MARGARIDO, 2001, citado na dissertação), porque uma série
#' estacionária em torno de uma média deslocada se parece com passeio aleatório
#' para os dois. Aqui a data da quebra é ESTIMADA, não informada: o teste varre
#' os cortes entre 15% e 85% da série e fica com o mais favorável à
#' estacionariedade. A janela é a do teste publicado, e não a do `urca`, que varre
#' todos os cortes.
#'
#' É o segundo bloco da coleção a publicar um ponto localizado na série, depois
#' do `series/pettitt`, e usa o mesmo `.tr_series_rotulo_em()` para o rótulo.
#' @export
tr_series_zivot_andrews <- function(serie, mudanca = "ambas", defasagens = 0L) {
  mud <- .tr_series_enum(mudanca, c("nível", "inclinação", "ambas"), "mudanca")
  # Eq. 3.35 (nível), 3.36 (inclinação) e 3.37 (ambas) da dissertação, nesta ordem.
  modelo <- switch(mud, "nível" = "intercept", "inclinação" = "trend", "ambas" = "both")
  .tr_series_sem_na(serie, "series/zivot_andrews")
  # Vinte, e o número foi MEDIDO, não herdado dos irmãos de categoria: o piso
  # deles é doze e aqui doze não serve, porque este teste gasta parâmetros que o
  # ADF não gasta (a dummy da quebra, e duas no modelo "ambas") e ainda escolhe o
  # corte que MAIS favorece a rejeição entre n - 1 candidatos.
  #
  # Sob passeio aleatório — H0 verdadeira, portanto toda rejeição é erro tipo I —
  # o teste rejeita a 5%, no modelo "ambas", em 68% das amostras com n = 11, 24%
  # com n = 14, 18% com n = 18, contra 14% com n = 20 (300 repetições por
  # tamanho, conferido num segundo lote independente). Abaixo de
  # vinte o bloco não está testando: está devolvendo "estacionária com quebra"
  # para série com raiz unitária de verdade na maioria das vezes, que é o
  # veredito decidido antes de olhar o dado — a mesma armadilha que fixou o piso
  # do `series/pettitt`, chegando pela cauda oposta.
  #
  # O excesso de tamanho NÃO acaba em vinte, ele só deixa de ser catastrófico; o
  # resto está na página do nó e na `nota`, porque é ressalva a ler, não motivo
  # para recusar a série.
  .tr_series_minimo(serie, 20L, "series/zivot_andrews", "a varredura da quebra")
  x <- as.numeric(serie)
  n <- length(x)
  k <- .tr_series_int(defasagens, "defasagens", min = 0L)
  if (k == 0L) k <- as.integer(trunc((n - 1)^(1 / 3)))
  # O `ur.za` ajusta, em CADA corte candidato, uma regressão com intercepto,
  # y_{t-1}, tendência, as k diferenças defasadas e a dummy da quebra — duas
  # dummies no modelo "ambas" —, sobre as n - 1 - k linhas que sobram depois das
  # defasagens. Quando os graus de liberdade residuais chegam a zero o `lm` fica
  # singular e o `ur.za` morre com um erro CRU do R ("valor ausente onde
  # TRUE/FALSE necessário") ou devolve lixo, conforme o dado — medido em n = 9 e
  # n = 10 com k = 2, onde nem sempre é o erro que aparece. O guard do
  # próprio pacote (`n < lag + 5`) não pega esse caso, e pior: em outras
  # combinações ele deixa passar e devolve um número calculado sobre variância
  # estimada em nada, sem erro nenhum. A conta é refeita aqui para que o card
  # fique vermelho dizendo qual defasagem cabe.
  # `fixos` são os parâmetros que não dependem de k: intercepto, y_{t-1},
  # tendência e a(s) dummy(s). Com eles, gl = n - 1 - 2k - fixos, e o maior k que
  # ainda deixa um grau de liberdade sai de gl >= 1.
  fixos <- if (modelo == "both") 5L else 4L
  gl <- n - 1L - 2L * k - fixos
  if (gl < 1L) {
    .tr_series_abort("tr_series_error_bad_option",
                     paste0("Param 'defasagens': %d defasagens não cabem numa série de %d ",
                            "observações neste modelo — a regressão da quebra ficaria sem ",
                            "graus de liberdade. O máximo aqui é %d."),
                     k, n, max(0L, (n - 2L - fixos) %/% 2L))
  }
  z <- urca::ur.za(x, model = modelo, lag = k)
  # `z@cval` chega SEM NOMES e na ordem 1%, 5%, 10% — invertida em relação à
  # ordem que esta coleção usa em todo lugar. Indexar por "5%" devolveria NULL, e
  # indexar por posição supondo a ordem da casa trocaria o corte de 1% pelo de
  # 10% e INVERTERIA o veredito, sem erro nenhum no caminho. Nomeado à mão, então.
  #
  # E a tabela é OUTRA em cada modelo — o `urca` a escolhe junto com o `model` —,
  # então ela não pode ser escrita como constante aqui: é lida do objeto, e por
  # isso está certa por construção.
  cv <- as.numeric(z@cval)
  criticos <- c(`10%` = cv[[3]], `5%` = cv[[2]], `1%` = cv[[1]])
  # A varredura fica entre 15% e 85% da série, como em Zivot & Andrews (1992), de
  # onde vem a tabela de críticos que o `urca` devolve — e não em TODOS os cortes,
  # que é o que o `urca` varre. Medido sob passeio aleatório, aparar quase não
  # mexe na taxa de rejeição (um ponto percentual ou menos) e NÃO é correção do
  # excesso de tamanho; o que muda é a DATA: sem aparar, a quebra publicada cai
  # fora da janela em 4% a 14% das amostras sob H0 (n de 20 a 100, 500
  # repetições por caso), e na borda extrema, com um trecho de uma observação só,
  # em até 8% delas com n = 20. "Quebra na observação 1" não se interpreta.
  #
  # `z@tstats[i]` é o t do corte i, e o `z@teststat` do pacote é o mínimo deles
  # (conferido: `z@tstats[z@bpoint] == z@teststat`), então refazer o mínimo na
  # janela é o mesmo teste com outra varredura. O `min(, length)` protege o fim:
  # o vetor tem n - 1 cortes, não n. Os críticos continuam os de `z@cval`, que já
  # são os da tabela aparada.
  lo <- as.integer(ceiling(0.15 * n))
  hi <- as.integer(min(floor(0.85 * n), length(z@tstats)))
  janela <- z@tstats[lo:hi]
  estat <- min(janela, na.rm = TRUE)
  quebra <- as.integer(lo - 1L + which.min(janela))
  quando <- .tr_series_rotulo_em(serie, quebra)
  # Numa série sem calendário o rótulo é o próprio índice, e "observação 60 (60)"
  # é ruído que ensina a ignorar o parêntese justamente onde ele carrega a data.
  # Mesma regra do `series/pettitt`.
  onde <- sprintf("observação %d", quebra)
  if (!identical(quando, as.character(quebra))) onde <- sprintf("%s (%s)", onde, quando)
  # "quebra de ambas" não é frase: o enum é rótulo de parâmetro, e a nota é
  # prosa. Os três viram o que se lê em voz alta.
  oque <- switch(mud, "nível" = "nível", "inclinação" = "inclinação",
                 "ambas" = "nível e inclinação")
  nota <- sprintf("quebra de %s, estimada pelo teste; %d defasagens; %d observações antes da quebra e %d a partir dela",
                  oque, k, quebra, n - quebra)
  # A ressalva do excesso de tamanho, e só onde ela vale: medido, a taxa de
  # rejeição sob passeio aleatório fica em torno de 10% a 14% até n = 30 e segue
  # acima do nominal depois — perto de 9% em n = 40 e 7% a 8% em n = 100, na
  # medida de 500 repetições feita ao aparar a janela. O aviso fica abaixo de 40,
  # onde o excesso é maior; acima dele o resto está na página do nó. Pôr o aviso em toda série o transformaria em ruído que se
  # aprende a pular — que é como a ressalva do `series/phillips_perron` teria
  # morrido se o corte dela fosse 0,1 em vez da borda de verdade.
  if (n < 40L) {
    nota <- paste0(nota, "; série curta para este teste: com menos de 40 observações ",
                   "ele rejeita mais do que o nível nominal, então um \"rejeita H0\" ",
                   "apertado aqui pede confirmação")
  }
  .tr_series_teste(
    # A quebra só entra na ALTERNATIVA (Zivot & Andrews, 1992; eq. 3.35-3.37 da
    # dissertação): H0 é a raiz unitária sem quebra, e dizer "mesmo admitindo
    # uma quebra" aqui poria a quebra na hipótese errada.
    "Zivot-Andrews", "a série tem raiz unitária, sem quebra", estat, "t",
    # Sem p-valor de propósito: o `urca` publica só a tabela, e interpolar um
    # p-valor a partir dela seria inventar precisão que o pacote não dá.
    criticos = criticos,
    # MEDIDO, e não copiado do ADF por parentesco: numa série de degrau o t sai
    # em -11,0 contra um corte de -4,80 e REJEITA; num passeio aleatório sai em
    # -3,9 contra o mesmo corte e NÃO rejeita. A cauda que derruba H0 é a de
    # baixo.
    sentido = "menor",
    conclusao_sim = sprintf("estacionária, com uma quebra na %s", onde),
    # "mesmo admitindo uma quebra" não é enfeite: não rejeitar AQUI pesa mais
    # que não rejeitar no `series/adf`, porque a explicação alternativa mais
    # comum já foi dada de graça à série. Mas continua sendo falta de evidência
    # contra H0, e não prova dela.
    conclusao_nao = "não há evidência contra a raiz unitária, mesmo admitindo uma quebra",
    nota = nota,
    fonte = "Zivot & Andrews (1992)",
    # O índice porque é o que se confere contra o `ur.za`; o rótulo porque é o
    # que uma pessoa lê no relatório. Mesmo par do `series/pettitt`.
    extra = list(quebra = quebra, quando = quando))
}

#' O miolo dos dois testes de ruído branco.
#'
#' Ljung-Box e Box-Pierce são a MESMA estatística com correções de amostra
#' pequena diferentes: tudo o que muda entre os dois é o `type` do
#' `stats::Box.test`, o rótulo e a fonte. Separados em duas cópias, a regra de
#' Hyndman e o desconto de graus divergiriam na primeira correção feita só de
#' um lado — e o usuário veria dois blocos que discordam sem motivo.
#' @noRd
.tr_series_box <- function(serie, defasagens, graus, tipo, no, fonte) {
  # Sem `.tr_series_sem_na`, ao contrário dos irmãos de raiz unitária, e de
  # propósito: o `Box.test` é NA-safe por construção (`acf(na.action =
  # na.pass)`, e o n dele já é `sum(!is.na(x))`), e o uso típico destes dois é
  # diagnosticar RESÍDUO — que vem com buraco sempre que o modelo perdeu
  # observação. Exigir série cheia aqui recusaria o caso mais comum do bloco.
  # A ajuda dos dois nós avisa que faltantes são ignorados na conta.
  n <- sum(!is.na(serie))
  f <- stats::frequency(serie)
  lag <- .tr_series_int(defasagens, "defasagens", min = 0)
  # A regra de Hyndman: 10 na série sem ciclo, dois ciclos na sazonal, e nunca
  # mais que um quinto da série — defasagem demais dilui o sinal no ruído das
  # autocorrelações de cauda.
  if (lag == 0L) lag <- as.integer(max(1, min(if (f > 1) 2 * f else 10, n %/% 5)))
  g <- .tr_series_int(graus, "graus", min = 0)
  # Sem defasagem sobrando depois do desconto, o qui-quadrado fica sem graus
  # de liberdade e o `Box.test` devolveria um p-valor de fantasia (ou NaN).
  if (lag <= g) {
    .tr_series_option("defasagens", lag, sprintf("um número maior que os graus descontados (%d)", g))
  }
  # O piso é ABSOLUTO, e não `lag + 2`: a regra de Hyndman encolhe a defasagem
  # junto com a série (`n %/% 5`), então um mínimo derivado dela encolhe junto e
  # nunca dispara. Medido: quarenta pontos com seis válidos saíam com um "não
  # rejeita H0" confiante calculado sobre UMA defasagem — resultado errado com
  # cara de certo, que é o inimigo declarado da coleção. Doze é o mesmo piso dos
  # blocos de raiz unitária.
  #
  # E a conta é sobre as VÁLIDAS, como a página promete: com um valor só, o
  # `Box.test` devolvia p = NaN e quem chegava ao usuário era o erro da tabela
  # de críticos, apontando para o lugar errado.
  .tr_series_minimo(serie, max(12L, lag + 2L), no,
                    sprintf("um teste de autocorrelação com %d %s",
                            lag, if (lag == 1L) "defasagem" else "defasagens"),
                    validas = n)
  b <- stats::Box.test(serie, lag = lag, type = tipo, fitdf = g)
  .tr_series_teste(
    # A hipótese nula é sobre as autocorrelações ATÉ a defasagem usada, e não
    # sobre ruído branco em geral: dependência além dela o teste não olha.
    tipo, sprintf("as autocorrelações até a defasagem %d são nulas", lag), b$statistic, "X²",
    p_valor = b$p.value,
    sentido = "menor",
    conclusao_sim = "há autocorrelação: não é ruído branco",
    conclusao_nao = "não há evidência de autocorrelação",
    # As duas escolhas podem ter sido automáticas (as defasagens) ou passar
    # despercebidas (os graus): quem for reportar o número precisa das duas.
    nota = sprintf("%d defasagens, %d graus descontados", lag, g),
    fonte = fonte)
}

#' Ljung-Box: a série é ruído branco?
#'
#' O corrigido para amostra pequena, e por isso o que se reporta — o
#' `series/box_pierce` testa a mesma hipótese nula pela fórmula original.
#'
#' O uso típico é nos RESÍDUOS de um modelo, e aí `graus` importa: os
#' parâmetros ajustados (p + q + P + Q de um ARIMA) consomem graus de
#' liberdade, e sem descontá-los o teste fica generoso demais — aceita ruído
#' branco onde há estrutura.
#' @export
tr_series_ljung_box <- function(serie, defasagens = 0L, graus = 0L) {
  .tr_series_box(serie, defasagens, graus, "Ljung-Box", "series/ljung_box",
                 "Ljung & Box (1978)")
}

#' Box-Pierce: a série é ruído branco?
#'
#' A mesma hipótese nula do `series/ljung_box`, na fórmula original de 1970 —
#' sem a correção de amostra pequena, que é justamente o que o Ljung-Box
#' acrescentou. Fica na coleção para comparar com trabalho antigo que o
#' reportou; para decidir, o Ljung-Box.
#' @export
tr_series_box_pierce <- function(serie, defasagens = 0L, graus = 0L) {
  .tr_series_box(serie, defasagens, graus, "Box-Pierce", "series/box_pierce",
                 "Box & Pierce (1970)")
}

#' Quantas diferenças a série pede para ficar estacionária.
#'
#' É a pergunta que se faz ANTES de montar o `series/diff` ou de fixar o `d`
#' e o `D` de um ARIMA à mão. As diferenças sazonais vêm do teste de força
#' sazonal do `forecast`, e só existem para série com ciclo — na anual a linha
#' sai NA com o motivo, e não zero, que afirmaria "testei e não precisa".
#' @export
tr_series_ndiffs <- function(serie, teste = "kpss") {
  teste <- .tr_series_enum(teste, c("kpss", "adf", "pp"), "teste")
  .tr_series_sem_na(serie, "series/ndiffs")
  .tr_series_minimo(serie, 12L, "series/ndiffs", "o teste")
  f <- stats::frequency(serie)
  tem_ciclo <- f > 1 && length(serie) >= 2 * f
  tibble::tibble(
    tipo = c("simples", "sazonal"),
    diferencas = c(forecast::ndiffs(serie, test = teste),
                   if (tem_ciclo) forecast::nsdiffs(serie) else NA_integer_),
    teste = c(teste, "força sazonal (seas)"),
    nota = c("", if (tem_ciclo) "" else "a série não tem ciclo (frequência 1) ou tem menos de dois")
  )
}

#' O ajuste que chegou é mesmo uma regressão?
#'
#' Os três blocos de F recebem um `tr_series_reg`, e não uma série. Sem o
#' guard, o `ajuste$ajuste` de um objeto qualquer é NULL e o `summary` devolve
#' um erro cru, sem classe e sem dizer qual nó reclamou.
#' @noRd
.tr_series_exige_reg <- function(ajuste, no) {
  if (!inherits(ajuste, "tr_series_reg")) {
    .tr_series_abort("tr_series_error_not_a_regression",
                     "'%s' recebeu um objeto '%s', não uma regressão.", no, class(ajuste)[[1]])
  }
  invisible(ajuste)
}

#' O miolo dos dois F de bloco.
#'
#' Sazonal e tendência são o MESMO teste com outro conjunto de termos: o que
#' muda entre eles é a lista de rótulos que sai da fórmula, e mais nada.
#' Copiado duas vezes, uma correção feita de um lado só faria os dois blocos
#' discordarem sobre o que é um F parcial.
#' @noRd
.tr_series_f_parcial <- function(ajuste, rotulo, h0, termos, sim, nao) {
  fit <- ajuste$ajuste
  # Teste PARCIAL: reajusta sem o bloco e compara. O reajuste sai de
  # `fit$model`, que carrega o fator com o contraste já posto — por isso o
  # contraste é atribuído ao fator, e não ao `lm`, em `tr_series_regression`.
  resto <- setdiff(attr(stats::terms(fit), "term.labels"), termos)
  forma <- stats::reformulate(if (length(resto)) resto else "1", response = "y")
  a <- stats::anova(stats::lm(forma, data = fit$model), fit)
  pv <- a$`Pr(>F)`[[2]]
  gl <- as.integer(a$Df[[2]])
  .tr_series_teste(
    rotulo, h0, a$F[[2]], "F",
    p_valor = pv,
    sentido = "menor",
    conclusao_sim = sim,
    conclusao_nao = nao,
    # Quantos termos o bloco tinha — é por este número que se confere que o
    # reajuste derrubou o bloco inteiro, e só ele.
    nota = sprintf("F parcial, %d %s no numerador", gl, if (gl == 1L) "grau" else "graus"),
    fonte = "Morettin & Toloi (2006)")
}

#' O bloco pedido não está no ajuste.
#' @noRd
.tr_series_sem_bloco <- function(no, bloco, saida) {
  .tr_series_abort("tr_series_error_no_block",
                   paste0("'%s' testa o bloco %s, e a regressão ligada não o tem. %s no ",
                          "'series/regression', ou tire este bloco do fluxo."),
                   no, bloco, saida)
}

#' F global: o modelo explica alguma coisa?
#'
#' H0 é que NENHUM termo explica a série: rejeitar é concluir que o ajuste
#' inteiro — tendência e sazonalidade juntas — captura parte do movimento. Diz
#' se há sinal, não de onde ele vem; para separar, os dois F de bloco.
#' @export
tr_series_f_global <- function(ajuste) {
  .tr_series_exige_reg(ajuste, "series/f_global")
  s <- summary(ajuste$ajuste)
  fs <- s$fstatistic
  p <- stats::pf(fs[[1]], fs[[2]], fs[[3]], lower.tail = FALSE)
  .tr_series_teste(
    "F global", "todos os coeficientes, fora o intercepto, são nulos", fs[[1]], "F",
    p_valor = p,
    sentido = "menor",
    conclusao_sim = "o modelo explica parte da série",
    conclusao_nao = "não há evidência de que o modelo explique a série",
    nota = sprintf("%d e %d graus de liberdade", as.integer(fs[[2]]), as.integer(fs[[3]])),
    fonte = "Morettin & Toloi (2006)")
}

#' F do bloco sazonal: há sazonalidade?
#'
#' Testa as onze dummies de uma mensal EM BLOCO, e não uma a uma: com onze
#' p-valores individuais são onze chances de achar um "significativo" por
#' acaso.
#' @export
tr_series_f_sazonal <- function(ajuste) {
  .tr_series_exige_reg(ajuste, "series/f_sazonal")
  if (!isTRUE(ajuste$sazonalidade)) {
    .tr_series_sem_bloco("series/f_sazonal", "sazonal", "Ligue a sazonalidade")
  }
  .tr_series_f_parcial(ajuste, "F do bloco sazonal",
                       "os coeficientes sazonais são todos nulos", "estacao",
                       "há sazonalidade", "não há evidência de sazonalidade")
}

#' F do bloco de tendência: há tendência?
#'
#' Testa os termos do polinômio EM BLOCO. Com grau 2 ou 3, o linear e o
#' quadrático dividem o mesmo sinal e nenhum dos dois aparece sozinho —
#' olhá-los um a um faria concluir que não há tendência nenhuma.
#' @export
tr_series_f_tendencia <- function(ajuste) {
  .tr_series_exige_reg(ajuste, "series/f_tendencia")
  if (ajuste$grau == 0L) {
    .tr_series_sem_bloco("series/f_tendencia", "de tendência", "Suba o grau do polinômio")
  }
  .tr_series_f_parcial(ajuste, "F do bloco de tendência",
                       "os coeficientes do polinômio são todos nulos",
                       paste0("t", seq_len(ajuste$grau)),
                       "há tendência", "não há evidência de tendência")
}

#' Mann-Kendall: a série tem tendência?
#'
#' O teste de tendência mais usado em climatologia, e não paramétrico: conta,
#' par a par, quantas vezes o futuro supera o passado. Não supõe distribuição
#' nenhuma, que é por que ele é o padrão justamente onde a série não é normal —
#' o `series/f_tendencia` mede a mesma coisa pedindo erro normal em troca.
#'
#' A tendência que ele enxerga é MONOTÔNICA: numa série que sobe e depois desce,
#' os pares de um lado cancelam os do outro e o teste pode sair sem tendência
#' nenhuma. A direção, quando rejeita, está no SINAL de Z, e é ela que vai para
#' a conclusão — um bilateral que só dissesse "há tendência" jogaria fora o que
#' o usuário foi perguntar.
#' @export
tr_series_mann_kendall <- function(serie) {
  .tr_series_sem_na(serie, "series/mann_kendall")
  # A Z é uma aproximação NORMAL da distribuição exata de S, e com meia dúzia de
  # pontos ela é ruim: o p-valor sairia com uma precisão que a amostra não
  # sustenta, e sem nada no card avisando. Daí o piso — abaixo dele o caminho é
  # a tabela exata de S, que este bloco não traz.
  .tr_series_minimo(serie, 10L, "series/mann_kendall", "a aproximação normal")
  x <- as.numeric(serie)
  n <- length(x)
  S <- sum(vapply(seq_len(n - 1L), function(i) sum(sign(x[(i + 1L):n] - x[i])), 0))
  # A variância leva a correção de empates (eq. 3.20 da dissertação), e ela não é
  # refinamento: par empatado não aponta direção nenhuma, e sem descontá-lo a
  # variância fica grande demais para o S que de fato pode sair. O efeito é um
  # p-valor OTIMISTA numa série cheia de valores repetidos — medição arredondada,
  # chuva com corrida de zeros —, que é a série climatológica típica.
  empates <- table(x)
  v <- (n * (n - 1) * (2 * n + 5) - sum(empates * (empates - 1) * (2 * empates + 5))) / 18
  # A correção de continuidade (o -1 e o +1): S é discreto e a normal não é.
  Z <- if (S > 0) (S - 1) / sqrt(v) else if (S < 0) (S + 1) / sqrt(v) else 0
  p <- 2 * stats::pnorm(-abs(Z))
  grupos <- sum(empates > 1L)
  .tr_series_teste(
    "Mann-Kendall", "a série não tem tendência monótona", Z, "Z",
    p_valor = p,
    sentido = "menor",
    conclusao_sim = if (Z > 0) "há tendência de aumento" else "há tendência de queda",
    conclusao_nao = "não há evidência de tendência",
    # O S bruto e o tamanho da correção: é por eles que se confere o teste contra
    # o que outro pacote reportou, e é o empate que explica um Z menor que o
    # esperado para um S grande.
    nota = sprintf("S = %d; %d %s de empates corrigidos", as.integer(S), grupos,
                   if (grupos == 1L) "grupo" else "grupos"),
    fonte = "Mann (1945)",
    extra = list(S = S))
}

#' Cox-Stuart: a série tem tendência?
#'
#' Um teste de SINAL: pareia observações distantes no tempo, conta quantas vezes
#' a segunda supera a primeira e pergunta se essa contagem é compatível com
#' cara-ou-coroa. Responde à mesma pergunta do `series/mann_kendall` por um
#' caminho mais barato — uma contagem de pares em vez de todos os pares da série
#' —, e a dissertação põe os dois lado a lado.
#'
#' O `pareamento` existe porque o artigo original e a dissertação DISCORDAM, e as
#' duas leituras se defendem. Em TERÇOS (o original, e o que o `trend` faz) o
#' miolo da série é descartado: são os extremos que acumularam a tendência, e
#' parear vizinho dilui o contraste. Em METADES (a dissertação, seção 3.3.4) cada
#' observação é pareada com a que está meia série adiante — entram mais pares,
#' cada um com contraste menor. O param não é cosmético: os dois caminhos dão
#' estatísticas diferentes na mesma série, e é por isso que a escolha vai na nota.
#' @export
tr_series_cox_stuart <- function(serie, pareamento = "terços") {
  par <- .tr_series_enum(pareamento, c("terços", "metades"), "pareamento")
  .tr_series_sem_na(serie, "series/cox_stuart")
  # Dezesseis, e não os dez do Mann-Kendall: aqui quem manda não é o tamanho da
  # série, é quantos PARES sobram depois do descarte. Em terços, dezesseis
  # observações deixam seis pares, e seis é o menor k em que a binomial
  # bilateral consegue passar abaixo do corte de 5% (2 * 0.5^6 = 0.03125); com
  # cinco, nem o caso mais extremo possível rejeita, e o bloco devolveria um
  # "não rejeita H0" que nunca teve como ser outra coisa — resultado errado com
  # cara de certo. O piso é o MESMO nos dois pareamentos de propósito: trocar o
  # param não pode acender em vermelho um fluxo que estava verde.
  .tr_series_minimo(serie, 16L, "series/cox_stuart", "o pareamento")
  x <- as.numeric(serie)
  n <- length(x)
  c0 <- if (par == "terços") ceiling(n / 3) else floor(n / 2)
  a <- x[seq_len(c0)]
  # Em terços o segundo membro do par vem da PONTA da série e o miolo fica de
  # fora; em metades ele vem logo depois do corte.
  b <- if (par == "terços") x[(n - c0 + 1L):n] else x[(c0 + 1L):(2L * c0)]
  d <- b - a
  descartados <- sum(d == 0)
  # Par empatado não aponta direção nenhuma, e o método manda eliminá-lo. Contá-lo
  # como "não subiu" seria pôr no prato da queda um par que não disse nada.
  d <- d[d != 0]
  M <- sum(d > 0)
  k <- length(d)
  if (k < 6L) {
    # A mesma conta do piso de cima, agora sobre o que os empates deixaram: uma
    # série longa e cheia de valores repetidos chega aqui com k pequeno, e o
    # veredito sairia decidido de antemão.
    .tr_series_abort("tr_series_error_too_short",
                     paste0("'series/cox_stuart': o pareamento em %s deixou %d pares depois ",
                            "de descartar os empates, e o teste precisa de pelo menos 6 para ",
                            "poder rejeitar."), par, k)
  }
  # O corte entre exata e aproximada é no número de PARES, e não no tamanho da
  # série, porque é k que entra na binomial. Vinte pares é onde a dissertação
  # libera a aproximação normal (seção 3.3.4, eq. 3.22). O `trend::cs.test` NÃO
  # serve de guia para a borda: ele não tem ramo exato nenhum e usa a normal
  # sempre (em k = 6 dá p = 0.041 contra a exata 0.031). A borda é nossa, e fica
  # em "a partir de vinte" — não "acima de vinte" — porque a reta com ruído de
  # sessenta pontos sai com exatamente vinte pares em terços, e é nela que a
  # conferência contra o `trend` acontece: com a exata em k = 20 a conferência
  # compararia binomial com normal. Ver `docs/fontes.md`.
  exata <- k < 20L
  if (exata) {
    p <- stats::binom.test(M, k, 0.5)$p.value
    # Sem Z calculado, quem vai para `estatistica` é o próprio M — é ele que
    # entrou na binomial, e o `rotulo_estat` diz isso no card e na coluna do
    # relatório. Publicar um Z aqui seria reportar um número que ninguém computou.
    est <- M
    rotulo <- "M"
  } else {
    est <- (M - k / 2) / sqrt(k / 4)
    p <- 2 * stats::pnorm(-abs(est))
    rotulo <- "Z"
  }
  # As duas escolhas foram automáticas — o pareamento tem default e o método de
  # p-valor ninguém pediu —, e quem for reportar o número precisa das duas, mais
  # o k, que é o que explica um M pequeno numa série grande.
  nota <- sprintf("pareamento em %s: %d pares, %s", par, k,
                  if (exata) "binomial exata" else "aproximação normal")
  if (descartados > 0L) {
    nota <- sprintf("%s; %d par%s empatado%s descartado%s", nota, descartados,
                    if (descartados == 1L) "" else "es", if (descartados == 1L) "" else "s",
                    if (descartados == 1L) "" else "s")
  }
  if (par == "metades") {
    # O campo `fonte` nomeia um artigo cujo método NÃO foi usado neste caminho —
    # sem esta linha, o relatório citaria Cox & Stuart por uma conta que é da
    # dissertação.
    nota <- paste0(nota, "; formulação da dissertação (Paiva, 2020), não a original")
  }
  .tr_series_teste(
    "Cox-Stuart", "a série não tem tendência monótona", est, rotulo,
    p_valor = p,
    sentido = "menor",
    conclusao_sim = if (M > k / 2) "há tendência de aumento" else "há tendência de queda",
    conclusao_nao = "não há evidência de tendência",
    nota = nota,
    fonte = "Cox & Stuart (1955)",
    extra = list(M = M, pares = k))
}

#' Run (Wald-Wolfowitz): a série é aleatória?
#'
#' Conta SEQUÊNCIAS (runs): trechos seguidos de valores do mesmo lado da
#' mediana. Poucas sequências é sinal de que os valores se agrupam; muitas, de
#' que alternam. H0 aqui é ALEATORIEDADE, e não ausência de tendência — o que
#' separa este bloco dos dois irmãos de categoria, que perguntam por tendência
#' direto. Rejeitar diz que a série não foi gerada ao acaso; tendência é UMA das
#' causas possíveis, ao lado de mudança de nível, ciclo e agrupamento, e é por
#' isso que a conclusão não fala em tendência.
#' @export
tr_series_runs <- function(serie) {
  .tr_series_sem_na(serie, "series/runs")
  # Quarenta, e não os dez do Mann-Kendall: a dissertação libera a normal para
  # `n1` ou `n2` maior que 20, e abaixo disso o caminho é a distribuição exata
  # do número de sequências, que este bloco não traz. Como o corte é na MEDIANA,
  # n1 e n2 saem praticamente iguais por construção — metade de cada lado —, e
  # então exigir 40 observações é exatamente o que entrega os 20 símbolos de
  # cada lado que a aproximação pede. Medido: 40 pontos sem empate dão n1 = n2 =
  # 20, e 39 dão 19. Implementar o ramo exato foi a alternativa rejeitada: ele
  # dobraria o contrato do bloco para servir a série curta, e série curta é
  # justamente onde este teste — o que menos detectou entre os TRÊS que Back
  # (2001) comparou, conforme a literatura que a ajuda cita — tem menos a dizer.
  .tr_series_minimo(serie, 40L, "series/runs", "a aproximação normal do número de sequências")
  x <- as.numeric(serie)
  med <- stats::median(x)
  # Valor IGUAL à mediana não está nem acima nem abaixo, e o método manda tirá-lo
  # da conta. Empurrá-lo para um dos lados inventaria um símbolo que o dado não
  # deu, e ainda emendaria duas sequências vizinhas numa só. O descarte muda N, e
  # é invisível no card — daí ele ir para a nota.
  s <- x[x != med]
  descartados <- length(x) - length(s)
  s <- sign(s - med)
  n1 <- sum(s > 0)
  n2 <- sum(s < 0)
  N <- n1 + n2
  if (min(n1, n2) < 20L) {
    # O piso de cima olhou o tamanho da SÉRIE; este olha o que sobrou dela. Uma
    # série longa com um platô na mediana — medição arredondada, uma corrida de
    # zeros — passa lá e chega aqui sem símbolo suficiente de um dos lados. No
    # extremo, todos os restantes caem do mesmo lado, `n1` ou `n2` zera e o
    # desvio sai NaN: o bloco devolveria um card com Z vazio em vez de dizer o
    # que houve.
    .tr_series_abort("tr_series_error_too_short",
                     paste0("'series/runs': depois de descartar %d empate%s com a mediana sobraram ",
                            "%d observaç%s acima e %d abaixo, e a aproximação normal pede pelo ",
                            "menos 20 de cada lado."),
                     descartados, if (descartados == 1L) "" else "s",
                     n1, if (n1 == 1L) "ão" else "ões", n2)
  }
  # Uma sequência nova começa a cada troca de símbolo: o número de sequências é o
  # número de trocas mais um.
  r <- 1 + sum(s[-1] != s[-length(s)])
  mu <- 2 * n1 * n2 / N + 1
  sg <- sqrt(2 * n1 * n2 * (2 * n1 * n2 - N) / (N^2 * (N - 1)))
  Z <- (r - mu) / sg
  p <- 2 * stats::pnorm(-abs(Z))
  nota <- sprintf("%d sequências em %d observações: %d acima e %d abaixo da mediana",
                  r, N, n1, n2)
  if (descartados > 0L) {
    nota <- sprintf("%s; %d empatada%s com a mediana, fora da conta", nota, descartados,
                    if (descartados == 1L) "" else "s")
  }
  .tr_series_teste(
    "Run", "a série é aleatória", Z, "Z",
    p_valor = p,
    sentido = "menor",
    # O que se conclui é sobre ALEATORIEDADE, e o sinal de Z diz de que jeito ela
    # falhou. Escrever "há tendência" aqui seria dar por testada uma hipótese que
    # este bloco não formulou: sequências de menos saem de tendência, mas também
    # de degrau e de agrupamento, e o card não tem como distinguir os três.
    conclusao_sim = if (Z < 0) {
      "a série não é aleatória: sequências de menos (tendência, mudança de nível ou agrupamento)"
    } else {
      "a série não é aleatória: sequências demais (os valores alternam em torno da mediana)"
    },
    conclusao_nao = "não há evidência contra a aleatoriedade",
    nota = nota,
    fonte = "Wald & Wolfowitz (1940)",
    extra = list(sequencias = r))
}

#' Pettitt: a série tem um ponto de mudança?
#'
#' O único dos quatro não paramétricos desta categoria que NÃO pergunta por
#' tendência. A hipótese nula é HOMOGENEIDADE: que `Z_1..Z_t` e `Z_{t+1}..Z_N`
#' venham da mesma população. Rejeitar localiza uma RUPTURA — a dissertação é
#' enfática nisso, citando Niel et al. (1998) —, e não autoriza dizer que a série
#' sobe ou desce. Os três irmãos de categoria testam tendência, então a conclusão
#' daqui é o lugar mais fácil da coleção para um copiar-colar mentir: ela nomeia
#' QUANDO a série mudou e cala sobre direção, de propósito.
#' @export
tr_series_pettitt <- function(serie) {
  .tr_series_sem_na(serie, "series/pettitt")
  # Onze, e não os dez do Mann-Kendall, e o número foi MEDIDO, não escolhido: com
  # dez observações o maior K possível é 25 — a série monotônica, conferida por
  # força bruta sobre as permutações de 1:10 —, e a eq. 3.11 devolve p = 0,066
  # para ele. Ou seja, abaixo de onze nem o dado mais extremo que existe alcança
  # o corte de 5%, e o bloco devolveria um "não rejeita H0" decidido antes de
  # olhar a série. Com onze o mesmo caso extremo dá 0,0485 e passa. É a mesma
  # conta que fixou o piso do `series/cox_stuart`, aplicada a esta fórmula.
  .tr_series_minimo(serie, 11L, "series/pettitt", "a aproximação do p-valor")
  x <- as.numeric(serie)
  n <- length(x)
  # U_t é a estatística de Mann-Whitney acumulada até t: quantas vezes o que veio
  # até aqui supera o que vem depois. Onde |U| é máximo é onde as duas metades
  # mais se separam, e esse t é o candidato a ponto de mudança.
  U <- cumsum(vapply(seq_len(n), function(t) sum(sign(x[t] - x)), 0))
  K <- max(abs(U))
  ponto <- which.max(abs(U))
  # Eq. 3.11. O `min(1, )` não é cosmético: a aproximação PASSA de 1 em série
  # curta e homogênea, e um p-valor de 1,4 sairia do card como se fosse número.
  p <- min(1, 2 * exp(-6 * K^2 / (n^3 + n^2)))
  quando <- .tr_series_rotulo_em(serie, ponto)
  # Numa série sem calendário — vetor cru, ou `ts` que começa em 1 — o rótulo é o
  # próprio índice, e "observação 30 (30)" é ruído que ensina a ignorar o
  # parêntese justamente onde ele carrega a data.
  onde <- sprintf("observação %d", ponto)
  if (!identical(quando, as.character(ponto))) onde <- sprintf("%s (%s)", onde, quando)
  # O tamanho das duas metades é o que explica um K grande ou pequeno, e não
  # aparece em lugar nenhum do card.
  nota <- sprintf("%d observações antes do ponto de mudança e %d a partir dele", ponto, n - ponto)
  # `which.max` fica com o PRIMEIRO dos empatados, e o empate é comum em série
  # com platô — no `austres` as posições 44 e 45 têm o mesmo K, e o
  # `trend::pettitt.test` reporta as duas. Publicar só uma sem dizer que havia
  # outras faz o card afirmar um ponto único que o teste não escolheu; o
  # reportado não muda, o silêncio é que acaba.
  empatados <- sum(abs(U) == K)
  if (empatados > 1L) {
    nota <- sprintf("%s; %d cortes empatam no mesmo K máximo, e o reportado é o primeiro deles",
                    nota, empatados)
  }
  if (p >= 1) {
    # Sem esta ressalva, um p-valor preso no teto sai do card como "p = 1", que
    # se lê como certeza de homogeneidade — e é só a aproximação estourando.
    nota <- paste0(nota, "; p-valor aproximado preso em 1 (a fórmula passa de 1 ",
                   "em série curta e homogênea)")
  }
  .tr_series_teste(
    "Pettitt", "a série é homogênea, sem ponto de mudança", K, "K",
    p_valor = p,
    sentido = "menor",
    # O que se conclui é RUPTURA, com a posição dela, e nada sobre direção:
    # escrever "há tendência" aqui daria por testada uma hipótese que este bloco
    # não formulou. Quem quer a direção liga um `series/mann_kendall` ao lado.
    conclusao_sim = sprintf("há um ponto de mudança na %s", onde),
    conclusao_nao = "não há evidência de ponto de mudança",
    nota = nota,
    fonte = "Pettitt (1979)",
    # O índice porque é o que o `trend::pettitt.test` devolve e o que um teste
    # confere; o rótulo porque é o que uma pessoa lê no relatório.
    extra = list(ponto_de_mudanca = ponto, quando = quando))
}

#' Kruskal-Wallis: a série tem sazonalidade?
#'
#' O não paramétrico da sazonalidade, e o primeiro bloco da categoria: põe TODAS
#' as observações em postos e pergunta se a soma dos postos muda de uma estação
#' para outra. Janeiro sempre alto e julho sempre baixo afastam as somas, e o H
#' cresce. O `series/f_sazonal` responde à mesma pergunta pedindo erro normal em
#' troca.
#'
#' A sazonalidade que ele enxerga é DETERMINÍSTICA: o padrão que se repete igual
#' todo ciclo. A ESTOCÁSTICA, a que vai mudando de ano para ano, não tem um
#' "nível de janeiro" fixo para o posto encontrar e passa por aqui sem ser vista
#' — a dissertação separa as duas na seção 3.4.
#'
#' E o que mais engana: TENDÊNCIA ATRAPALHA. Numa série que cresce, os postos
#' altos são todos dos anos finais e se espalham por todas as estações, que é
#' por que o `AirPassengers` — a série sazonal de livro — não rejeita aqui.
#' Tirar a tendência antes (`series/diff`) resolve; passar um log NÃO, porque o
#' log é monotônico e não troca a ordem de valor nenhum, então não mexe em posto
#' nenhum. O detalhe está na página do nó, com os números medidos.
#' @export
tr_series_kruskal_wallis <- function(serie) {
  .tr_series_sem_na(serie, "series/kruskal_wallis")
  # Frequência 1 não tem estação nenhuma para comparar: `cycle()` devolveria uma
  # coluna de 1s e o `kruskal.test` seria mandado comparar UM grupo só.
  #
  # E TRÊS ciclos, não os dois dos irmãos, porque com duas observações por
  # estação o H MÁXIMO possível — estações perfeitamente separadas, sem empate —
  # não alcança o corte do qui-quadrado. Medido: na trimestral o maior H é 6.667
  # contra um corte de 7.815 a 5%, e na semestral 2.4 contra 3.841. O teste NÃO
  # PODE rejeitar ali, e a taxa de rejeição sob H0 sai 0.000; um "não rejeita"
  # seria o veredito decidido antes de ler o dado. Com três ciclos a trimestral
  # chega a 10.38 e a semestral a 3.857, os dois acima do corte. A mensal já
  # rejeitaria com dois (22.88 contra 19.68), mas um piso por frequência seria
  # uma regra a mais para explicar em troca de um ano de dado; fica um só.
  .tr_series_sazonal(serie, "series/kruskal_wallis", ciclos = 3L)
  estacoes <- factor(stats::cycle(serie))
  s <- stats::kruskal.test(as.numeric(serie), estacoes)
  k <- nlevels(estacoes)
  n_por <- tabulate(estacoes, nbins = k)
  nota <- if (min(n_por) == max(n_por)) {
    sprintf("%d estações, com %d observações cada", k, min(n_por))
  } else {
    # Ciclo incompleto na ponta deixa umas estações com uma observação a mais
    # que as outras. O teste aceita grupo desbalanceado, mas é o desbalanço que
    # explica um H diferente do que dá a mesma série fechada no último ciclo.
    sprintf("%d estações, de %d a %d observações por estação", k, min(n_por), max(n_por))
  }
  .tr_series_teste(
    # H0 do Kruskal-Wallis é a igualdade das DISTRIBUIÇÕES por estação, que é o
    # que a conta de postos testa; "não existe sazonalidade" é a leitura dela.
    "Kruskal-Wallis", "as estações têm a mesma distribuição", s$statistic, "H",
    p_valor = s$p.value,
    # Cauda SUPERIOR, e é o segundo caso da coleção depois do KPSS: H é um
    # qui-quadrado que mede o quanto as somas de postos se AFASTAM da igualdade,
    # então é o H GRANDE que derruba H0. Com p-valor na mão quem decide é o
    # `p < alfa`, e não este campo — o `sentido` é o que diz ao card qual cauda
    # sombrear e onde fica a região crítica. Um "menor" copiado dos vizinhos de
    # tendência desenharia a região crítica no lado errado do qui-quadrado.
    sentido = "maior",
    conclusao_sim = "há sazonalidade",
    conclusao_nao = "não há evidência de sazonalidade",
    nota = nota,
    fonte = "Morettin & Toloi (2006)",
    # Os graus de liberdade porque é com eles que se lê o H contra a tabela do
    # qui-quadrado, e porque é o número de estações menos um — a única coisa que
    # a estatística sozinha não conta.
    extra = list(graus = k - 1L))
}

#' Fisher: existe uma periodicidade escondida?
#'
#' O irmão do `series/kruskal_wallis` que faz a pergunta ao contrário. O
#' Kruskal-Wallis compara estações que VOCÊ já declarou — ele precisa da
#' frequência da série para saber o que é janeiro. Este aqui não declara nada: ele
#' varre o periodograma inteiro e pergunta se o MAIOR pico é maior do que o acaso
#' produziria. Quem sabe qual é o ciclo usa o Kruskal-Wallis; quem está caçando um
#' ciclo desconhecido usa este.
#'
#' E é dessa varredura que vem a única armadilha da página: o teste acha o maior
#' pico ONDE QUER QUE ELE ESTEJA, e nem todo pico é estação. No `Nile` — uma série
#' anual sem sazonalidade nenhuma — ele rejeita, e o pico está no período 100, que
#' é a série inteira. Isso não é um ciclo: é a tendência que sobrou. Por isso o
#' bloco publica o PERÍODO junto com o veredito, e avisa na `nota` quando o pico
#' não chega a se repetir dentro da série.
#' @export
tr_series_fisher <- function(serie, remover = "reta") {
  remover <- .tr_series_enum(remover, c("reta", "media"), "remover")
  .tr_series_sem_na(serie, "series/fisher")
  # O piso NÃO saiu da conta que fixou os do `series/pettitt` e do
  # `series/cox_stuart`, e vale dizer por quê em vez de fingir que saiu: lá a
  # pergunta era "qual o menor n em que o dado mais extremo possível ainda
  # rejeita", e aqui ela não morde. Medido por força bruta sobre senoides de
  # Fourier em toda fase, de N=4 a N=16: com a fase certa o pico concentra TODA a
  # potência, g dá 1 exatamente e p dá 0 — ou seja, o dado mais extremo rejeita em
  # qualquer tamanho, e essa derivação devolveria um piso de quatro observações.
  #
  # O que de fato limita é a GRADE de períodos. Com N observações o periodograma
  # só enxerga os períodos N/1, N/2, ... e o menor ciclo que esta coleção sabe
  # declarar é o trimestral. Para que um pico de período 4 exista nessa grade E
  # ainda se repita ao menos duas vezes dentro da série são precisas oito
  # observações: em N=8 a grade é 8, 4, 2.67 e 2, e em N=7 ela é 7, 3.5 e 2.33 —
  # não tem o 4. Abaixo de oito, portanto, o "período do pico" não é uma escolha
  # entre alternativas: é o único lugar onde ele podia cair.
  .tr_series_minimo(serie, 8L, "series/fisher", "a grade de períodos do periodograma")
  # Sem `.tr_series_sazonal`, e a decisão é deliberada. O Kruskal-Wallis precisa do
  # guard porque AGRUPA por estação, e sem frequência não há grupo. Este não
  # agrupa: ele lê o periodograma, que existe para qualquer série. Recusar
  # frequência 1 bloquearia justamente o uso legítimo do bloco — caçar um período
  # que ninguém declarou —, e foi assim que a linha do `Nile` da página foi
  # medida. O preço é deixar entrar a leitura errada do `Nile`, e é por isso que o
  # período e o aviso de "não se repete" saem no resultado em vez de ficarem só na
  # documentação.
  # Convenções de Fisher (1929), conferidas contra `GeneCycle::fisher.g.test`
  # (Wichert, Fokianos & Strimmer 2004), que é o oráculo dos testes:
  #   - g é tomado sobre as m = floor((N - 1) / 2) ordenadas de Fourier j = 1..m.
  #     A de Nyquist (N par) sai: ela é um qui-quadrado com UM grau, as outras
  #     têm dois, e a distribuição de g supõe m ordenadas iguais em lei. Até a
  #     versão 1 do nó ela entrava na soma.
  #   - o p-valor é a série EXATA, com os floor(1/g) termos, e não só o primeiro
  #     (versão 1): o primeiro termo é conservador e, em m pequeno e g baixo,
  #     passa de 1.
  #   - `remover = "media"` é a formulação original (ruído branco em torno de
  #     uma média); `"reta"` (padrão, a da dissertação) tira antes uma reta de
  #     mínimos quadrados — o mesmo teste aplicado aos resíduos da reta, que é
  #     como o oráculo o confere.
  pg <- stats::spec.pgram(serie, taper = 0, detrend = remover == "reta",
                          demean = TRUE, fast = FALSE, plot = FALSE)
  I <- pg$spec
  if (length(serie) %% 2L == 0L) I <- I[-length(I)]
  n <- length(I)
  pico <- which.max(I)
  # Eq. 3.41: a fração da potência total que o maior pico sozinho carrega.
  g <- max(I) / sum(I)
  p <- .tr_series_fisher_p(g, n)
  # O corte zα agora é o quantil EXATO a 5% da mesma distribuição, e não a
  # fórmula de primeiro termo da eq. 3.42: assim `g > zα` e `p < 0,05` são a
  # mesma regra por construção, e não por aproximação.
  z_alfa <- .tr_series_fisher_critico(n, 0.05)
  f <- stats::frequency(serie)
  # O período sai em OBSERVAÇÕES, e não na unidade de tempo da série, que é o que
  # `1 / pg$freq` daria. Num `AirPassengers` mensal aquilo vale 1 — a estação de
  # doze meses chamada de "1", que se lê como "um mês" e inverte o sentido da
  # frase. Em observações o mesmo pico é 12, que é o que a pessoa conta no
  # gráfico, e para série de frequência 1 os dois números coincidem.
  periodo <- f / pg$freq[[pico]]
  # Quantas vezes o ciclo cabe na série. É exatamente o índice de Fourier do pico
  # (período = N/j), então é inteiro, e é ele que separa uma estação de um
  # artefato: j = 1 é a frequência mais baixa que o periodograma tem, cujo período
  # é o comprimento da série.
  ciclos <- as.integer(round(length(serie) / periodo))
  per_txt <- sprintf("%g", round(periodo, 2))
  onde <- sprintf("pico no período de %s observações", per_txt)
  # Quando o pico cai em cima do ciclo declarado, dizê-lo poupa a conta mental de
  # conferir se 12 observações são o ano da série mensal.
  if (f > 1 && isTRUE(all.equal(periodo, f))) {
    onde <- sprintf("%s, que é exatamente o ciclo declarado da série", onde)
  }
  nota <- sprintf("%s; ele cabe %d %s na série, e o periodograma tem %d ordenadas",
                  onde, ciclos, if (ciclos == 1L) "vez" else "vezes", n)
  if (ciclos < 2L) {
    # O caso `Nile`. Sem isto o card diria "há periodicidade" sobre uma série que
    # não tem nenhuma, e o número que desmente a frase estaria publicado ao lado
    # sem ninguém para lê-lo.
    nota <- paste0(nota, ". ATENÇÃO: o pico está na frequência mais baixa que o ",
                   "periodograma enxerga, e um ciclo que não chega a se repetir ",
                   "dentro da série é assinatura de tendência ou de estrutura de ",
                   "baixa frequência, não de estação")
  }
  .tr_series_teste(
    # "periodicidade", e não "sazonalidade": o g acha o maior pico ONDE QUER QUE
    # ele esteja, inclusive fora das estações (o `Nile`, período 100).
    "Fisher", "a série não tem periodicidade", g, "g",
    p_valor = p,
    criticos = c(`5%` = z_alfa),
    # Cauda SUPERIOR, como no Kruskal-Wallis e pelo mesmo motivo: é o g GRANDE —
    # o pico que concentra a potência — que derruba H0. Quem decide é o
    # `p < alfa`, porque há p-valor; o `sentido` diz ao card qual lado sombrear, e
    # um "menor" copiado dos vizinhos de tendência desenharia a região crítica no
    # lado errado e faria o `g > zα` publicado logo ao lado parecer invertido.
    sentido = "maior",
    conclusao_sim = sprintf("há periodicidade, com %s", onde),
    conclusao_nao = "não há evidência de periodicidade",
    nota = nota,
    fonte = "Morais (2012)",
    # O período porque é o que distingue uma estação de um artefato, e é o que a
    # página inteira gira em torno de; os ciclos porque são a leitura já feita
    # desse número, e um relatório com vários testes não tem onde fazer a conta.
    extra = list(periodo = periodo, ciclos = ciclos, ordenadas = n))
}

#' P-valor exato do g de Fisher (1929) com m ordenadas.
#'
#' P(g > x) = sum_{j=1}^{floor(1/x)} (-1)^(j-1) choose(m, j) (1 - j x)^(m-1),
#' somado em log para os binomiais grandes, como em `GeneCycle`. Preso em
#' [0, 1]: a soma alternada pode sair um ulp fora.
#' @noRd
.tr_series_fisher_p <- function(g, m) {
  if (g <= 0) return(1)
  j <- seq_len(floor(1 / g))
  termos <- (-1)^(j - 1) * exp(lchoose(m, j) + (m - 1) * log(pmax(1 - j * g, 0)))
  min(1, max(0, sum(termos)))
}

#' Valor crítico exato de g ao nível alfa: resolve P(g > x) = alfa.
#' @noRd
.tr_series_fisher_critico <- function(m, alfa) {
  # O limite de cima é a fórmula de primeiro termo, que superestima o crítico
  # (a série é alternada). O de baixo parte da metade dele e desce enquanto o p
  # não passar de alfa — perto de 1/m a soma alternada tem termos enormes e se
  # cancela mal, então o intervalo não começa ali.
  hi <- 1 - (alfa / m)^(1 / (m - 1))
  # Acima de 1/2 a série exata tem um termo só: a fórmula fechada É o crítico.
  if (hi >= 0.5) return(hi)
  lo <- hi / 2
  while (.tr_series_fisher_p(lo, m) < alfa) lo <- lo / 2
  stats::uniroot(function(x) .tr_series_fisher_p(x, m) - alfa,
                 c(lo, hi), tol = 1e-12)$root
}
