# Os dados de exemplo: reais, e simulados com estrutura CONHECIDA.
#
# O segundo grupo é o que ensina. Um PCA em `USArrests` mostra o que a técnica
# faz; uma análise fatorial num questionário cujas cargas foram PLANTADAS mostra
# se ela acerta — e mostra o que a rotação muda, porque a resposta certa está
# escrita na ajuda.
#
# Nada é sorteado com a semente do usuário: cada conjunto simulado fixa a sua,
# com o gerador explícito, e devolve o estado do RNG como estava. Sem isso o
# `questionario` de hoje seria outro amanhã, e a ajuda que cita as cargas
# mentiria.

.TR_MULTI_EXEMPLOS <- c("iris", "USArrests", "estados", "caranguejos", "vinhos",
                        "questionario", "harman_fisicas", "harman_24_testes", "pima")

#' Roda `expr` com semente própria, sem mexer na do usuário.
#' @noRd
.tr_multi_com_semente <- function(seed, expr) {
  tem <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (tem) antigo <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
  antigo_kind <- RNGkind()
  on.exit({
    do.call(RNGkind, as.list(antigo_kind))
    if (tem) assign(".Random.seed", antigo, envir = globalenv())
    else rm(".Random.seed", envir = globalenv())
  }, add = TRUE)
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(seed)
  force(expr)
}

#' Amostra normal cuja covariância AMOSTRAL é exatamente `S`.
#'
#' Pela Cholesky, e não pelo `MASS::mvrnorm(empirical = TRUE)`: o `mvrnorm`
#' passa por `eigen()`, e o sinal de um autovetor depende da LAPACK da máquina —
#' a correlação sairia a mesma, mas as LINHAS seriam outras em cada computador.
#' A Cholesky é única. Primeiro os sorteios são branqueados (covariância
#' amostral identidade), depois recebem a de `S`.
#' @noRd
.tr_multi_normal_exata <- function(n, S) {
  p <- ncol(S)
  z <- scale(matrix(stats::rnorm(n * p), n, p), center = TRUE, scale = FALSE)
  w <- z %*% solve(chol(stats::cov(z)))
  x <- w %*% chol(S)
  colnames(x) <- colnames(S)
  x
}

.tr_multi_harman <- function(base, nomes) {
  x <- .tr_multi_com_semente(1976L, .tr_multi_normal_exata(base$n.obs, base$cov))
  colnames(x) <- nomes
  tibble::as_tibble(as.data.frame(x))
}

#' O questionário: 15 itens, três fatores correlacionados, cargas plantadas.
#'
#' A matriz de cargas e a correlação entre fatores são as de `.tr_multi_questionario_verdade()`,
#' que os testes usam para conferir que a fatoração as encontra. As respostas
#' contínuas são cortadas numa escala de 1 a 5, como um Likert de verdade — o
#' que atenua as cargas um pouco, e a ajuda diz isso.
#' @noRd
.tr_multi_questionario_verdade <- function() {
  itens <- c(paste0("ans", 1:5), paste0("soc", 1:5), paste0("org", 1:5))
  L <- matrix(0, 15, 3, dimnames = list(itens, c("ansiedade", "sociabilidade", "organizacao")))
  L[1:5, 1] <- c(.80, .75, .70, .65, .60)
  L[6:10, 2] <- c(.80, .75, .70, .60, -.60)
  L[11:15, 3] <- c(.80, .75, .65, .60, .45)
  L["org5", "ansiedade"] <- .40
  phi <- matrix(c(1, -.30, -.25,
                  -.30, 1, .20,
                  -.25, .20, 1), 3, 3, dimnames = list(colnames(L), colnames(L)))
  list(cargas = L, phi = phi)
}

.tr_multi_questionario <- function() {
  v <- .tr_multi_questionario_verdade()
  L <- v$cargas
  comum <- L %*% v$phi %*% t(L)
  S <- comum + diag(1 - diag(comum))
  n <- 400L
  x <- .tr_multi_com_semente(2026L, {
    z <- matrix(stats::rnorm(n * ncol(S)), n, ncol(S))
    z %*% chol(S)
  })
  likert <- apply(x, 2L, function(col) as.integer(cut(col, c(-Inf, -1.5, -.5, .5, 1.5, Inf))))
  colnames(likert) <- rownames(L)
  # O identificador é TEXTO: numérico, ele entraria como variável em todo nó
  # com as colunas em branco, e seria o primeiro "item" do questionário.
  tibble::as_tibble(cbind(data.frame(respondente = sprintf("r%03d", seq_len(n))),
                          as.data.frame(likert)))
}

#' Vinhos de três cultivares, com covariância COMUM aos grupos.
#'
#' Médias e desvios inspirados nos do conjunto de vinhos de Forina et al.
#' (repositório UCI), mas os dados são simulados — o conjunto original não vem
#' com o R, e trazê-lo seria copiar dado de terceiro. A covariância é a mesma
#' nos três grupos DE PROPÓSITO: é o caso em que a LDA é a regra ótima, e o M de
#' Box não deve rejeitar. Quem quiser ver o caso contrário tem os `caranguejos`.
#' @noRd
.tr_multi_vinhos <- function() {
  vars <- c("alcool", "acidez_malica", "fenois_totais", "flavonoides", "intensidade_cor", "magnesio")
  medias <- rbind(A = c(13.7, 2.0, 2.8, 3.0, 5.5, 106),
                  B = c(12.3, 1.9, 2.3, 2.1, 3.1, 94),
                  C = c(13.2, 3.3, 1.7, 0.8, 7.4, 99))
  dp <- c(.50, .90, .45, .50, 1.40, 13)
  R <- diag(6)
  R[1, 5] <- R[5, 1] <- .45
  R[3, 4] <- R[4, 3] <- .80
  R[2, 4] <- R[4, 2] <- -.25
  R[1, 6] <- R[6, 1] <- .20
  R[3, 6] <- R[6, 3] <- .15
  S <- diag(dp) %*% R %*% diag(dp)
  ns <- c(A = 59L, B = 71L, C = 48L)
  x <- .tr_multi_com_semente(1988L, lapply(names(ns), function(g) {
    z <- matrix(stats::rnorm(ns[[g]] * 6L), ns[[g]], 6L) %*% chol(S)
    sweep(z, 2L, medias[g, ], "+")
  }))
  m <- do.call(rbind, x)
  colnames(m) <- vars
  m[, 1:5] <- round(m[, 1:5], 2)
  m[, 6] <- round(m[, 6])
  tibble::as_tibble(cbind(data.frame(cultivar = factor(rep(names(ns), ns))), as.data.frame(m)))
}

#' Mulheres pima: diabetes (sim/não) por sete medidas clínicas.
#'
#' Os dois pedaços da MASS (`Pima.tr`, 200, e `Pima.te`, 332) juntos, com a
#' coluna `amostra` dizendo de qual veio: é a divisão treino/teste que o livro
#' de Venables e Ripley usa, e deixa um `data/filter` refazê-la. `amostra` é
#' TEXTO para não entrar como preditor quando as colunas ficam em branco.
#' @noRd
.tr_multi_pima <- function() {
  junta <- function(d, rotulo) {
    data.frame(amostra = rotulo, gestacoes = d$npreg, glicose = d$glu, pressao = d$bp,
               pele = d$skin, imc = d$bmi, pedigree = d$ped, idade = d$age,
               diabetes = factor(ifelse(d$type == "Yes", "sim", "não"), levels = c("não", "sim")),
               stringsAsFactors = FALSE)
  }
  tibble::as_tibble(rbind(junta(MASS::Pima.tr, "treino"), junta(MASS::Pima.te, "teste")))
}

#' Carrega um conjunto de exemplo para análise multivariada.
#' @param dataset nome do conjunto (ver a ajuda do nó `multi/example`).
#' @return tibble.
#' @export
tr_multi_example <- function(dataset = "iris") {
  dataset <- .tr_multi_enum(dataset, .TR_MULTI_EXEMPLOS, "dataset")
  switch(dataset,
    iris = tibble::as_tibble(datasets::iris),
    USArrests = tibble::as_tibble(datasets::USArrests, rownames = "nome"),
    estados = {
      x <- as.data.frame(datasets::state.x77)
      names(x) <- c("populacao", "renda", "analfabetismo", "expectativa_vida", "homicidios",
                    "ensino_medio", "geada", "area")
      regiao <- factor(datasets::state.region,
                       labels = c("Nordeste", "Sul", "Centro-Norte", "Oeste"))
      tibble::as_tibble(cbind(data.frame(nome = rownames(x), regiao = regiao), x))
    },
    caranguejos = {
      x <- MASS::crabs
      especie <- factor(x$sp, levels = c("B", "O"), labels = c("azul", "laranja"))
      sexo <- factor(x$sex, levels = c("F", "M"), labels = c("fêmea", "macho"))
      tibble::tibble(especie = especie, sexo = sexo,
                     grupo = factor(paste(especie, sexo)),
                     lobo_frontal = x$FL, largura_traseira = x$RW,
                     comprimento_carapaca = x$CL, largura_carapaca = x$CW,
                     profundidade = x$BD)
    },
    vinhos = .tr_multi_vinhos(),
    questionario = .tr_multi_questionario(),
    pima = .tr_multi_pima(),
    harman_fisicas = .tr_multi_harman(datasets::Harman23.cor, c(
      "altura", "envergadura", "antebraco", "perna", "peso",
      "diametro_bitrocanteriano", "circunferencia_torax", "largura_torax")),
    harman_24_testes = .tr_multi_harman(datasets::Harman74.cor, c(
      "percepcao_visual", "cubos", "tabuleiro_formas", "bandeiras", "informacao_geral",
      "compreensao_paragrafo", "completar_sentencas", "classificar_palavras",
      "significado_palavras", "adicao", "codigo", "contar_pontos", "maiusculas_retas_curvas",
      "reconhecer_palavras", "reconhecer_numeros", "reconhecer_figuras", "objeto_numero",
      "numero_figura", "figura_palavra", "deducao", "enigmas_numericos",
      "raciocinio_problemas", "completar_series", "problemas_aritmeticos"))
  )
}

.tr_multi_nos_fonte <- function() {
  list(
    trama::tr_node("multi/example", fn = tr_multi_example, label = "Exemplo multivariado",
      category = "multi_fonte", icon = trama::tr_icon("database"),
      description = "Carrega um conjunto de dados escolhido para ensinar análise multivariada.",
      outputs = list(out = "data/table"),
      params = list(dataset = trama::tr_param_enum("iris", .TR_MULTI_EXEMPLOS, label = "Conjunto")),
      help = .tr_multi_ajuda(r"---[
Carrega um conjunto de dados pensado para as técnicas desta coleção. Metade é
dado real que vem com o R; a outra metade é SIMULADA com estrutura conhecida —
e é essa metade que deixa conferir se a técnica acha o que foi plantado.

### Reais

- **iris** — 150 flores, 4 medidas de sépala e pétala, 3 espécies (`Species`).
  O caso de livro da discriminante (Fisher, 1936): duas funções separam as
  espécies quase sem erro. Na PCA, colorir o biplot por `Species` mostra que o
  primeiro componente já é quase a espécie.
- **USArrests** — taxas de crime nos 50 estados dos EUA (1973). O exemplo de
  por que PADRONIZAR: sem padronizar, `Assault` (na casa das centenas) vira
  sozinha o primeiro componente, só por causa da escala.
- **estados** — 8 indicadores dos estados dos EUA (`state.x77`), com `nome` e
  `regiao`: população, renda, analfabetismo, expectativa de vida, homicídios,
  ensino médio, dias de geada, área. Bom para biplot com rótulo.
- **caranguejos** — 200 caranguejos *Leptograpsus* (`MASS::crabs`), 5 medidas
  em mm, 4 grupos (`especie` × `sexo`, na coluna `grupo`). O TAMANHO domina
  todas as medidas, e é por isso que a discriminante acha o que a PCA não acha:
  o primeiro componente é "caranguejo grande", e a espécie está na forma.
- **pima** — 532 mulheres pima (`MASS::Pima.tr` e `Pima.te`), 7 medidas
  (`gestacoes`, `glicose`, `pressao`, `pele`, `imc`, `pedigree`, `idade`) e o
  diagnóstico de `diabetes` (não/sim). Dois grupos que se SOBREPÕEM: o caso da
  `multi/logistic` binária e da curva ROC, onde LDA e logística acertam cerca
  de 78% e a pergunta é qual corte usar. `amostra` diz se a linha era do
  treino ou do teste do livro.

### Reconstruídos de uma matriz publicada

- **harman_fisicas** — 305 meninas, 8 medidas físicas (Harman, 1976). A
  estrutura é de 2 fatores: comprimento (altura, envergadura, antebraço, perna)
  e volume (peso, diâmetros, tórax).
- **harman_24_testes** — 145 alunos, 24 testes de habilidade (Holzinger e
  Swineford, via Harman). A análise fatorial clássica: 4 fatores — espacial,
  verbal, velocidade e memória.

O R guarda esses dois só como MATRIZ de correlação. As tabelas daqui são
sorteadas de modo que a correlação amostral seja EXATAMENTE a publicada; por
isso qualquer fatoração delas reproduz a do livro. Os valores são escores
padronizados (média 0, desvio 1) — a escala original não foi publicada, e a
correlação é tudo o que a análise fatorial usa.

### Simulados, com a resposta conhecida

- **questionario** — 400 respondentes, 15 itens de 1 a 5, três fatores:
  ansiedade (`ans1`–`ans5`), sociabilidade (`soc1`–`soc5`) e organização
  (`org1`–`org5`). As cargas plantadas vão de 0,80 a 0,45, e três detalhes estão
  lá de propósito: `soc5` é item INVERTIDO (carga −0,60, "prefiro ficar
  sozinho"); `org5` tem carga CRUZADA (0,40 em ansiedade); e os fatores são
  CORRELACIONADOS (ansiedade × sociabilidade −0,30, ansiedade × organização
  −0,25, sociabilidade × organização 0,20). É o conjunto para comparar varimax
  com oblimin: a oblíqua devolve a correlação plantada, a ortogonal a esconde
  nas cargas. Cortar em cinco categorias atenua as cargas um pouco — as
  estimadas saem menores que as plantadas, e isso é esperado.
- **vinhos** — 178 vinhos de três cultivares (`cultivar` A, B e C), 6 medidas
  químicas. Médias e desvios inspirados no conjunto de vinhos de Forina et al.
  (UCI), dados simulados. A covariância é IGUAL nos três grupos, que é o caso
  em que a discriminante linear é a regra ótima e o M de Box não rejeita.
  Flavonoides e fenóis são muito correlacionados (0,80), para a PCA ter o que
  resumir.

Os simulados usam semente própria: saem iguais em todo computador, e rodar
este nó não mexe na semente do resto do fluxo.
]---", r"---[
- **Conjunto** — qual conjunto carregar.
]---", r"---[
Uma tabela (`data/table`). Colunas de texto ou fator (`Species`, `regiao`,
`cultivar`) ficam de fora das técnicas quando as variáveis são deixadas em
branco, e servem de grupo e de cor.
]---", r"---[
tr_flow(reg) |>
  tr_add("q", "multi/example", dataset = "questionario") |>
  tr_add("af", "multi/factor_analysis", cols = "", fatores = 3L, rotacao = "oblimin", from = "q")
]---", r"---[
`multi/pca` e `multi/factor_analysis` para resumir as medidas; `multi/discriminant`
para separar os grupos; `data/example` para os outros conjuntos do R.
]---"))
  )
}
