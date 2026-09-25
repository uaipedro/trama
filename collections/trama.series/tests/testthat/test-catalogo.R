# As varreduras das coleções irmãs, aplicadas aqui: como elas percorrem o
# registro inteiro em vez de listar nós um a um, nó novo nasce REPROVADO até
# ganhar a sua página — que é o ponto.

nos_series <- function(reg) Filter(function(n) startsWith(n$id, "series/"), reg$nodes)

test_that("a coleção carrega sobre data e view, e não sozinha", {
  expect_no_error(series_registry())
  reg <- trama::tr_registry()
  err <- tryCatch(trama::tr_use(trama_collection(), registry = reg), condition = identity)
  expect_s3_class(err, "tr_error_unknown_type")
  reg <- trama::tr_registry()
  trama::tr_use("trama.data", registry = reg)
  err <- tryCatch(trama::tr_use(trama_collection(), registry = reg), condition = identity)
  expect_match(conditionMessage(err), "view/plot", fixed = TRUE)
})

test_that("as categorias não pisam nas das outras coleções", {
  # O registro de categorias é GLOBAL: um id repetido sobrescreveria a
  # categoria alheia sem erro nenhum.
  reg <- trama::tr_registry()
  trama::tr_use("trama.data", registry = reg)
  trama::tr_use("trama.view", registry = reg)
  antes <- names(reg$categories)
  ids <- vapply(trama_collection()$categories, function(k) k$id, "")
  expect_length(intersect(ids, antes), 0L)
})

test_that("todo nó tem help no formato, e todo campo digitável tem exemplo", {
  reg <- series_registry()
  digitaveis <- c("expr", "cols", "path", "text")
  nos <- nos_series(reg)
  expect_length(nos, 46L)
  for (n in nos) {
    for (secao in c("## Descrição", "## Parâmetros", "## Valor", "## Exemplos", "## Veja também")) {
      expect_match(n$help, secao, fixed = TRUE, info = n$id)
    }
    for (nm in names(n$params)) {
      p <- n$params[[nm]]
      if (!p$kind %in% digitaveis) next
      expect_true(!is.null(p$example) && nzchar(p$example), info = paste(n$id, nm))
    }
  }
})

test_that("todo gráfico da coleção é view/plot com os seis cosméticos e a ajuda deles", {
  reg <- series_registry()
  graficos <- Filter(function(n) identical(n$outputs$out$type, "view/plot"), nos_series(reg))
  expect_length(graficos, 8L)
  comuns <- c("aspecto", "tema", "titulo", "rotulo_x", "rotulo_y", "legenda")
  for (n in graficos) {
    expect_equal(utils::tail(names(n$params), 6L), comuns, info = n$id)
    expect_true(grepl(trama.view::tr_view_help_appearance(), n$help, fixed = TRUE), info = n$id)
    # O default do spec e o do `fn` têm de bater: o card desenha pelo spec, o
    # console pelo `fn`, e os dois têm de dar o mesmo gráfico.
    expect_equal(n$params$aspecto$default, formals(n$fn)$aspecto, info = n$id)
    expect_equal(n$params$tema$default, formals(n$fn)$tema, info = n$id)
  }
})

test_that("toda referência cruzada da ajuda aponta pra nó ou tipo que existe", {
  reg <- series_registry()
  conhecidos <- c(names(reg$nodes), names(reg$types))
  for (n in nos_series(reg)) {
    citados <- regmatches(n$help, gregexpr("`[a-z][a-z0-9_.]*/[a-z0-9_]+`", n$help))[[1]]
    citados <- unique(gsub("`", "", citados, fixed = TRUE))
    expect_true(length(citados) > 0L, info = n$id)
    for (id in citados) expect_true(id %in% conhecidos, info = paste(n$id, "cita", id))
  }
})

test_that("os exemplos da ajuda rodam como DSL de verdade", {
  # Exemplo que não roda é pior que exemplo nenhum. Cada bloco é avaliado com
  # o registro real; só a leitura de arquivo (que não existe aqui) é poupada.
  reg <- series_registry()
  env <- new.env(parent = asNamespace("trama"))
  env$reg <- reg
  for (n in nos_series(reg)) {
    blocos <- regmatches(n$help, gregexpr("```r\n.*?\n```", n$help))[[1]]
    for (b in blocos) {
      codigo <- sub("^```r\n", "", sub("\n```$", "", b))
      if (grepl("data/read_csv", codigo, fixed = TRUE)) next
      expect_no_error(eval(parse(text = codigo), envir = env), message = n$id)
    }
  }
})

test_that("param de texto com default não vazio recusa o branco, culpando o campo", {
  # A doutrina do param vazio da `data`: default preenchido quer dizer que o
  # nó precisa do campo. Aqui só o código do ETS é assim.
  reg <- series_registry()
  for (n in nos_series(reg)) for (nm in names(n$params)) {
    p <- n$params[[nm]]
    if (!p$kind %in% c("text", "expr") || !nzchar(p$default)) next
    args <- list(serie = datasets::AirPassengers)
    args[[nm]] <- ""
    err <- tryCatch(do.call(n$fn, args), error = identity)
    expect_equal(class(err)[[1]], "tr_series_error_blank_param", info = paste(n$id, nm))
    expect_match(conditionMessage(err), nm, fixed = TRUE)
  }
})

test_that("o catálogo sai com a ajuda e os adaptadores", {
  cat_json <- as.character(trama::tr_catalog_json(series_registry()))
  expect_match(cat_json, "Série de exemplo", fixed = TRUE)
  expect_match(cat_json, "\"from\":\"series/ts\"", fixed = TRUE)
})

test_that("todo número decimal da prosa existe como literal no código do nó", {
  # A Phase 2 perdeu uma borda de p-valor por um conserto de código, um conserto
  # de plano e uma revisão de fase inteira: a ajuda do `series/phillips_perron`
  # ensinava que a tabela ia "de 0,01 a 0,1" enquanto o código emitia a ressalva
  # em 0,99. As varreduras de cima não pegavam — elas conferem se a REFERÊNCIA
  # resolve e se o exemplo RODA, não se o número é verdade.
  #
  # Esta varre a metade MECÂNICA desse defeito, e só ela: que o número escrito na
  # página exista no código do nó. Se ele está na frase certa continua sendo
  # leitura humana — o que se ganha é não gastar atenção de revisor nessa metade.
  #
  # Só a forma brasileira (vírgula decimal), e é a vírgula que torna a varredura
  # precisa em vez de barulhenta: ano (1970, 1988), defasagem (10, 24) e
  # porcentagem (5%, 1%) são inteiros e nunca casam. Sobra o número com cara de
  # LIMIAR, que é a classe a que o bug do Phillips-Perron pertencia.
  #
  # `removeSource()` porque o `deparse()` de uma função carregada com srcref traz
  # os COMENTÁRIOS junto, e comentário é prosa, não código. O comentário do
  # Phillips-Perron cita o `tablep` do `stats` (`0.01, 0.025, 0.05, 0.1, ...`):
  # deixá-lo contar como literal teria deixado esta varredura VERDE justamente no
  # bug que a motivou — medido.
  #
  # Recortada aos blocos que emitem `data/test`, que é onde está o sinal — e
  # SEM lista de exceções, de propósito. A primeira versão varria a coleção
  # inteira e precisou de ONZE exceções em seis nós: valor de exemplo para o
  # usuário digitar, corte de leitura citado de livro, medições trazidas de fora,
  # e até uma vírgula de tupla (`ARIMA(0,1,1)`) que o regex leu como decimal.
  # Nenhuma delas era defeito. Ou seja, fora dos blocos de teste a varredura não
  # achava nada e cobrava onze buracos permanentes por isso — e buraco de
  # whitelist é como varredura morre: cada um é um lugar onde um erro de verdade
  # pode se esconder para sempre. Nos blocos de teste ela precisa de ZERO, e é
  # esta forma que vai ser copiada para os sete que ainda faltam.
  #
  # O limite conhecido, dito em voz alta: limiar que o código CALCULA em vez de
  # escrever não tem literal para casar (a banda do `series/acf` é
  # `qnorm(.975)`, nunca o texto 1,96). Se algum bloco de teste passar a derivar
  # um corte assim, a saída certa não é abrir exceção aqui — é o teste do bloco
  # conferir o valor calculado.
  #
  # Corolário para quem escreve as páginas: o nível da decisão se escreve "5%",
  # que é inteiro e não casa, e não "0,05", que casaria e viveria em
  # `trama::tr_test`, não no corpo do bloco.
  reg <- series_registry()
  testes <- Filter(function(n) identical(n$outputs$out$type, "data/test"), nos_series(reg))
  # Sem este piso a varredura viraria silenciosamente um laço sobre lista vazia
  # no dia em que o tipo de saída mudasse de nome — verde, e sem testar nada. É
  # um PISO, e não a contagem exata: contagem exata pediria uma edição aqui a
  # cada bloco novo, sem sinal nenhum em troca.
  #
  # E sim, a varredura hoje faz DUAS asserções sobre quinze blocos — esse é o
  # estado esperado, não um defeito. O corolário logo acima é o motivo: as
  # páginas escrevem "5%", que não casa com o regex de decimal, e a rede fica
  # vazia POR CONSTRUÇÃO. Quem vier depois não deve "consertar" isso alargando o
  # regex: ele voltaria a pescar tupla de ARIMA e valor de exemplo, que foi de
  # onde vieram as onze exceções que esta forma existe para não ter.
  expect_gte(length(testes), 15L)
  for (n in testes) {
    corpo <- paste(deparse(body(removeSource(n$fn))), collapse = "\n")
    numeros <- unique(regmatches(n$help, gregexpr("\\b\\d+,\\d+\\b", n$help))[[1]])
    for (num in numeros) {
      literal <- gsub(".", "\\.", gsub(",", ".", num, fixed = TRUE), fixed = TRUE)
      expect_true(grepl(paste0("\\b", literal, "\\b"), corpo),
                  info = paste(n$id, "escreve", num, "na ajuda e não o tem no código"))
    }
  }
})

test_that("a decisão do código sobre faltante está escrita na página, nos dois sentidos", {
  # Não é conferência de prosa contra prosa: é conferir que uma decisão tomada no
  # CÓDIGO foi escrita em algum lugar para o usuário. É a armadilha da divergência
  # entre irmãos posta como regra — e é o que faz os sete blocos que ainda vêm
  # nascerem seguros, porque um bloco que tolera NA sem dizer nasce VERMELHO.
  #
  # O marcador é uma frase fixa porque as páginas são escritas à mão e cada autor
  # diria a mesma coisa de um jeito. A comparação normaliza o espaço em branco: a
  # frase é longa o bastante para cair numa quebra de linha na hora de reencher o
  # parágrafo, e um marcador que some quando alguém rearruma o texto é um teste
  # que se aprende a desligar. (Medido: a primeira versão deste teste reprovou o
  # `series/decompose` por exatamente isso.)
  marcador <- "não aceita faltantes"
  achata <- function(s) gsub("\\s+", " ", s)
  reg <- series_registry()
  for (n in nos_series(reg)) {
    corpo <- paste(deparse(body(removeSource(n$fn))), collapse = "\n")
    # Direção 1, sobre a coleção INTEIRA: quem recusa faltante tem de dizer que
    # recusa. É a direção barata — uma frase por página — e não custa nada
    # aplicá-la também a quem decompõe, ajusta e modela.
    if (grepl(".tr_series_sem_na", corpo, fixed = TRUE)) {
      expect_true(grepl(marcador, achata(n$help), fixed = TRUE),
                  info = paste(n$id, "chama .tr_series_sem_na e não diz isso na ajuda"))
    }
  }
  # Direção 2, sobre os BLOCOS DE TESTE que recebem uma série: quem NÃO recusa
  # tomou a decisão mais silenciosa das duas — o nó engole o buraco e devolve um
  # veredito — e por isso deve uma seção inteira, não uma frase.
  #
  # O recorte é a saída `data/test`, que é a forma que vai ser copiada sete
  # vezes. Exigir a seção de todo nó que recebe série puxaria dezoito páginas de
  # transformação e de gráfico para uma reestruturação que esta rodada não quer.
  #
  # Os três blocos de F ficam de FORA: eles recebem um `series/regression`, nunca
  # uma série, e o `series/regression` já recusou o faltante lá atrás. A pergunta
  # sobre NA não é deles, e cobrá-la aqui seria pedir que a página respondesse uma
  # pergunta que o bloco não faz.
  testes <- Filter(function(n) identical(n$outputs$out$type, "data/test") &&
                     identical(n$inputs[[1]]$type, "series/ts"), nos_series(reg))
  expect_length(testes, 12L)
  for (n in testes) {
    corpo <- paste(deparse(body(removeSource(n$fn))), collapse = "\n")
    if (grepl(".tr_series_sem_na", corpo, fixed = TRUE)) next
    expect_true(grepl("### Faltantes", n$help, fixed = TRUE),
                info = paste(n$id, "aceita faltante em silêncio: falta a seção '### Faltantes'"))
  }
})

test_that("o fn de todo nó está exportado no NAMESPACE", {
  # O NAMESPACE desta coleção é escrito à MÃO: não tem cabeçalho de roxygen e
  # está em ordem de fonte, então o `roxygenise()` o ignora em silêncio e uma
  # `@export` esquecida não aparece em lugar nenhum. O app nem repara — o
  # `tr_node(fn = )` guarda o OBJETO da função —, só quebra o acesso pelo
  # console (`trama.series::tr_series_adf`), que esta coleção trata como
  # contrato de verdade.
  #
  # A fonte da verdade é o ARQUIVO, e não `getNamespaceExports()`: o
  # `pkgload::load_all` da suíte exporta tudo, então a função mentiria aqui e a
  # varredura passaria sempre. É a mesma lição do sweep de erros, um passo
  # adiante — lá bastava o namespace, aqui o namespace é justamente o que está
  # adulterado.
  # `expect_true`, e não `skip_if_not`: sob `R CMD check` o cwd é
  # `<pkg>.Rcheck/tests/testthat` e o caminho não resolve. Com um skip, a
  # varredura sumiria em silêncio justamente onde mais importa — e uma
  # varredura que pula é uma varredura que não faz nada, que é o defeito que
  # ela existe para pegar.
  expect_true(file.exists("../../NAMESPACE"))
  linhas <- grep("^export\\(", readLines("../../NAMESPACE"), value = TRUE)
  exportados <- sub("^export\\((.*)\\)$", "\\1", linhas)

  ns <- asNamespace("trama.series")
  fns <- Filter(is.function, mget(ls(ns, all.names = TRUE), envir = ns, inherits = FALSE))
  for (n in nos_series(series_registry())) {
    bate <- vapply(fns, identical, TRUE, n$fn)
    expect_true(any(bate), info = paste(n$id, "tem fn que não está no namespace"))
    nome <- names(fns)[bate][[1]]
    expect_true(nome %in% exportados,
                info = paste(n$id, "usa", nome, "— falta export() no NAMESPACE"))
  }
})

test_that("todo bloco de teste explica os pontinhos, com o texto comum", {
  # Os quinze desenham o MESMO widget. A seção vem de um helper só, e esta
  # varredura é o que obriga o décimo sexto bloco a ligá-la: sem ela, um teste
  # novo nasceria com três pontinhos que a página dele não explica.
  reg <- series_registry()
  testes <- Filter(function(n) identical(n$outputs$out$type, "data/test"), nos_series(reg))
  # Piso pelo mesmo motivo da varredura de decimais: laço sobre lista vazia é verde.
  expect_gte(length(testes), 15L)
  secao <- .tr_series_ajuda_pontinhos()
  for (n in testes) {
    expect_true(grepl(secao, n$help, fixed = TRUE), info = n$id)
  }
  # E só neles: nos outros nós não há pontinho nenhum para explicar.
  for (n in Filter(function(n) !identical(n$outputs$out$type, "data/test"), nos_series(reg))) {
    expect_false(grepl(secao, n$help, fixed = TRUE), info = n$id)
  }
})

test_that("toda fonte de bloco de teste está em docs/fontes.md, e toda linha de lá é um bloco", {
  # O crédito das fontes foi pedido para publicação, e documento que ninguém
  # confere apodrece: um bloco novo entraria sem linha, ou um bloco renomeado
  # deixaria a linha antiga apontando para o nada. Mesmas duas direções da
  # varredura do catálogo de erros.
  #
  # A `fonte` sai de uma CHAMADA mínima, e não do corpo da função via
  # `deparse()`: quatro dos quinze não têm o texto no próprio corpo — o
  # Ljung-Box e o Box-Pierce a passam como argumento ao `.tr_series_box`, e o F
  # sazonal e o de tendência a herdam de dentro de `.tr_series_f_parcial`. Ler o
  # corpo daria string vazia justamente nesses, e uma varredura que só confere
  # os que têm o texto à vista não pega o caso que mais tende a escapar.
  #
  # `expect_true(file.exists())`, e não `skip`: pelo mesmo motivo da varredura do
  # NAMESPACE, um skip faria esta conferência sumir calada onde o caminho não
  # resolve.
  caminho <- "../../../../docs/fontes.md"
  expect_true(file.exists(caminho))
  doc <- paste(readLines(caminho, encoding = "UTF-8"), collapse = "\n")

  reg <- series_registry()
  testes <- Filter(function(n) identical(n$outputs$out$type, "data/test"), nos_series(reg))
  expect_gte(length(testes), 15L)
  ap <- datasets::AirPassengers
  ajuste <- tr_series_regression(ap)
  # Uma linha da tabela: "| `series/x` | fonte | ...". A fonte tem de estar NA
  # linha do bloco, e não em qualquer lugar do documento — "Morettin & Toloi
  # (2006)" aparece várias vezes, e casar no texto solto deixaria passar a
  # linha apagada de um dos quatro blocos que a usam.
  linhas <- regmatches(doc, gregexpr("(?m)^\\| `series/[a-z0-9_]+` \\|[^|]*\\|", doc, perl = TRUE))[[1]]
  tabela <- stats::setNames(
    trimws(sub("^\\| `series/[a-z0-9_]+` \\|([^|]*)\\|$", "\\1", linhas)),
    sub("^\\| `(series/[a-z0-9_]+)` \\|.*$", "\\1", linhas))

  for (n in testes) {
    entrada <- if (identical(n$inputs[[1]]$type, "series/regression")) ajuste else ap
    t <- n$fn(entrada)
    expect_true(nzchar(t$fonte), info = n$id)
    expect_true(n$id %in% names(tabela), info = paste(n$id, "não tem linha em docs/fontes.md"))
    if (n$id %in% names(tabela)) {
      expect_equal(unname(tabela[[n$id]]), t$fonte,
                   info = paste(n$id, "tem outra fonte em docs/fontes.md"))
    }
  }
  # Direção 2: linha de bloco que não existe mais é crédito dado a nada.
  ids <- vapply(testes, function(n) n$id, "")
  for (id in names(tabela)) {
    expect_true(id %in% ids, info = paste("docs/fontes.md lista", id, "que não é bloco de teste"))
  }
  expect_length(tabela, length(testes))
})

# Glossário (docs/glossario-parametros.md): os ids em português e os que
# diziam o autor em vez da pergunta mudaram. Fluxo salvo com o id antigo abre
# com o novo — e a aresta continua ligada, porque as portas não mudaram.
test_that("fluxos salvos com os ids antigos abrem com os novos", {
  reg <- series_registry()
  antigos <- c("series/f_sazonal" = "series/f_seasonal",
               "series/f_tendencia" = "series/f_trend",
               "series/kruskal_wallis" = "series/seasonality_kw",
               "series/fisher" = "series/periodicity_fisher")
  for (velho in names(antigos)) {
    doc <- list(nodes = list(n = list(type = velho, params = list())), edges = list())
    m <- trama::tr_doc_migrate(doc, reg)
    expect_equal(m$nodes$n$type, antigos[[velho]], info = velho)
    expect_false(is.null(reg$nodes[[antigos[[velho]]]]), info = velho)
  }
})

test_that("nenhum nó declara os tipos de teste antigos", {
  reg <- series_registry()
  tipos <- unlist(lapply(reg$nodes, function(n) c(vapply(n$inputs, `[[`, "", "type"),
                                                  vapply(n$outputs, `[[`, "", "type"))))
  expect_false(any(tipos %in% c("models/test", "series/test")))
  expect_null(reg$types[["series/test"]])
})
