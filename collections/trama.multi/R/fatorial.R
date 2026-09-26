# Análise fatorial: extração (máxima verossimilhança ou eixo principal),
# rotação e escores, e as duas maneiras de olhar o resultado — a tabela das
# cargas e o mapa de calor.
#
# A extração e a rotação moram num nó só, e não em dois, porque a rotação não
# tem sentido sem as cargas que ela gira e custa milissegundos: dois cards
# `multi/factor_analysis` lado a lado, um varimax e um oblimin, são a
# comparação que se quer fazer, e dois nós encadeados a tornariam mais longa
# sem ensinar nada a mais. As rotações estão em `R/rotacao.R`.

.TR_MULTI_METODOS_AF <- c("ml", "paf")
.TR_MULTI_ESCORES <- c("regressão", "bartlett", "nenhum")
.TR_MULTI_MATRIZES <- c("padrão", "estrutura", "correlação entre fatores")

#' Graus de liberdade do modelo fatorial com p variáveis e k fatores.
#'
#' `((p − k)² − (p + k)) / 2`: os p(p − 1)/2 correlações que se tem, menos os
#' parâmetros livres do modelo. Negativo quer dizer mais incógnitas que dados —
#' infinitas soluções igualmente boas, e qualquer uma que o otimizador devolva
#' é arbitrária. Vale para ML e PAF: é o modelo que não se identifica, e não o
#' método que falha.
#' @noRd
.tr_multi_gl_fatorial <- function(p, k) ((p - k)^2 - (p + k)) / 2

.tr_multi_fatores <- function(fatores, p, no) {
  k <- .tr_multi_int(fatores, "fatores", min = 1)
  if (.tr_multi_gl_fatorial(p, k) < 0) {
    possiveis <- Filter(function(j) .tr_multi_gl_fatorial(p, j) >= 0, seq_len(p))
    .tr_multi_abort("tr_multi_error_too_many_factors",
                    paste0("'%s': %d fatores com %d variáveis dão graus de liberdade negativos (%g): ",
                           "o modelo não se identifica. %s"),
                    no, k, p, .tr_multi_gl_fatorial(p, k),
                    if (length(possiveis)) {
                      sprintf("Com %d variáveis, o máximo é %d fator(es).", p, max(possiveis))
                    } else {
                      sprintf("Com %d variáveis nenhum número de fatores se identifica; são precisas pelo menos 3.", p)
                    })
  }
  k
}

#' Fatoração pelo eixo principal, iterada nas comunalidades.
#'
#' Troca a diagonal da correlação pelas comunalidades, extrai os k maiores
#' autovetores, recalcula as comunalidades como a soma dos quadrados das cargas,
#' e repete até elas pararem de mudar. A partida é a correlação múltipla ao
#' quadrado (SMC, `1 − 1/diag(R⁻¹)`), que é um limite inferior da comunalidade.
#'
#' **Caso Heywood é erro.** Se uma comunalidade passa de 1, a unicidade dela
#' ficaria negativa — variância negativa, solução impossível. O `psych` avisa e
#' segue; aqui o card fica vermelho nomeando a variável, porque um aviso não
#' aparece em quem só olha o mapa das cargas, e cargas de um caso Heywood não
#' são interpretáveis. Quase sempre é fator demais, ou uma variável que é
#' praticamente cópia de outra.
#' @noRd
.tr_multi_paf <- function(R, k, no, maxit = 200L, tol = 1e-6) {
  h2 <- 1 - 1 / diag(solve(R))
  convergiu <- FALSE
  for (it in seq_len(maxit)) {
    Rr <- R
    diag(Rr) <- h2
    e <- eigen(Rr, symmetric = TRUE)
    L <- e$vectors[, seq_len(k), drop = FALSE] %*% diag(sqrt(pmax(e$values[seq_len(k)], 0)), k)
    novo <- rowSums(L^2)
    if (any(novo > 1)) {
      .tr_multi_abort("tr_multi_error_heywood",
                      paste0("'%s': caso Heywood — a comunalidade de %s passou de 1 na iteração %d, ",
                             "e a unicidade ficaria negativa. Tente menos fatores, tire a variável ",
                             "redundante, ou use metodo = \"ml\"."),
                      no, paste(colnames(R)[novo > 1], collapse = ", "), it)
    }
    delta <- max(abs(novo - h2))
    h2 <- novo
    if (delta < tol) {
      convergiu <- TRUE
      break
    }
  }
  dimnames(L) <- list(colnames(R), NULL)
  list(cargas = L, unicidade = 1 - h2, convergiu = convergiu, ajuste_ml = NULL)
}

#' Máxima verossimilhança pelo `factanal`, sem rotação: a rotação é a daqui.
#'
#' Na matriz de correlação, com `n.obs`, que dá o mesmo ajuste que passar a
#' tabela e deixa a checagem de singularidade com `.tr_multi_correlacao`. O
#' `factanal` segura as unicidades acima de 0,005 — um caso Heywood no ML não
#' erra, encosta nesse piso; o resumo do card aponta as variáveis que
#' encostaram.
#' @noRd
.tr_multi_ml <- function(R, k, n, no) {
  fit <- .tr_multi_ajustar(stats::factanal(covmat = R, factors = k, n.obs = n, rotation = "none"), no)
  est <- if (is.null(fit$STATISTIC)) NA_real_ else unname(fit$STATISTIC)
  pv <- if (is.null(fit$PVAL)) NA_real_ else unname(fit$PVAL)
  list(cargas = unclass(fit$loadings)[, seq_len(k), drop = FALSE],
       unicidade = fit$uniquenesses,
       convergiu = isTRUE(fit$converged),
       ajuste_ml = list(estatistica = est, gl = fit$dof, p_valor = pv))
}

#' Escores fatoriais.
#'
#' - **regressão** (Thurstone): `Z R⁻¹ S`, com S a ESTRUTURA (correlação
#'   variável-fator). É a previsão de mínimos quadrados do fator a partir das
#'   variáveis; maximiza a correlação com o fator verdadeiro, mas os escores
#'   saem encolhidos (variância menor que 1) e, na ortogonal, correlacionados.
#' - **bartlett**: `Z U⁻² L (L' U⁻² L)⁻¹`, com L o PADRÃO e U² as unicidades.
#'   Mínimos quadrados ponderados: não viesado (a média condicional ao fator é o
#'   fator), à custa de mais variância.
#'
#' Nas duas fórmulas a oblíqua já está tratada: a regressão usa a estrutura
#' (`L Φ`) e a de Bartlett, o padrão.
#' @noRd
.tr_multi_escores <- function(m, R, cargas, estrutura, unicidade, escores) {
  if (escores == "nenhum") return(NULL)
  Z <- scale(m)
  sc <- if (escores == "regressão") {
    Z %*% solve(R, estrutura)
  } else {
    tmp <- t(cargas / unicidade)
    t(solve(tmp %*% cargas, tmp %*% t(Z)))
  }
  dimnames(sc) <- list(NULL, colnames(cargas))
  sc
}

#' Análise fatorial exploratória, com rotação e escores.
#' @param dados tabela.
#' @param cols variáveis, separadas por vírgula; em branco, todas as numéricas.
#' @param fatores número de fatores.
#' @param metodo `"ml"` (máxima verossimilhança) ou `"paf"` (eixo principal).
#' @param rotacao `"nenhuma"`, `"varimax"`, `"quartimax"`, `"equamax"`,
#'   `"promax"` ou `"oblimin"`.
#' @param normalizar normalização de Kaiser antes de rotacionar.
#' @param escores `"regressão"`, `"bartlett"` ou `"nenhum"`.
#' @return objeto `tr_multi_fa` (tipo `multi/fa`).
#' @export
tr_multi_factor_analysis <- function(dados, cols = "", fatores = 2L, metodo = "ml",
                                     rotacao = "varimax", normalizar = TRUE,
                                     escores = "regressão") {
  no <- "multi/factor_analysis"
  metodo <- .tr_multi_enum(metodo, .TR_MULTI_METODOS_AF, "metodo")
  rotacao <- .tr_multi_enum(rotacao, .TR_MULTI_ROTACOES, "rotacao")
  escores <- .tr_multi_enum(escores, .TR_MULTI_ESCORES, "escores")
  variaveis <- .tr_multi_variaveis(dados, cols)
  m <- .tr_multi_matriz(dados, variaveis, no)
  R <- .tr_multi_correlacao(m, no)
  p <- ncol(R)
  k <- .tr_multi_fatores(fatores, p, no)
  # Com um fator só não há rotação possível; o objeto diz "nenhuma", e não
  # "varimax", para o card não prometer o que não aconteceu.
  if (k == 1L) rotacao <- "nenhuma"

  ext <- if (metodo == "ml") .tr_multi_ml(R, k, nrow(m), no) else .tr_multi_paf(R, k, no)
  nomes <- paste0("F", seq_len(k))
  brutas <- ext$cargas %*% .tr_multi_orientacao(ext$cargas)
  dimnames(brutas) <- list(variaveis, nomes)
  rot <- .tr_multi_rotacionar(brutas, rotacao, normalizar)
  estrutura <- rot$cargas %*% rot$phi
  # A comunalidade é a das cargas NÃO rotacionadas: rotacionar não muda o que
  # os fatores, juntos, explicam de cada variável — só como isso se reparte.
  comunalidade <- stats::setNames(rowSums(brutas^2), variaveis)
  unicidade <- stats::setNames(as.numeric(ext$unicidade), variaveis)

  .tr_multi_fa_obj(
    cargas = rot$cargas, cargas_brutas = brutas, rotacao = rotacao, rotmat = rot$rotmat,
    phi = rot$phi, estrutura = estrutura, comunalidade = comunalidade, unicidade = unicidade,
    metodo = metodo,
    escores = .tr_multi_escores(m, R, rot$cargas, estrutura, unicidade, escores),
    dados = dados, variaveis = variaveis, correlacao = R, n = nrow(m),
    ajuste_ml = ext$ajuste_ml, convergiu = isTRUE(ext$convergiu) && rot$convergiu,
    normalizar = isTRUE(normalizar)
  )
}

#' Tabela das cargas: padrão, estrutura, ou a correlação entre fatores.
#' @param fa objeto `tr_multi_fa`.
#' @param matriz `"padrão"`, `"estrutura"` ou `"correlação entre fatores"`.
#' @param ordenar ordena as variáveis pelo fator dominante e pela carga.
#' @return tibble.
#' @export
tr_multi_fa_loadings <- function(fa, matriz = "padrão", ordenar = TRUE) {
  matriz <- .tr_multi_enum(matriz, .TR_MULTI_MATRIZES, "matriz")
  if (matriz == "correlação entre fatores") {
    return(tibble::as_tibble(cbind(data.frame(fator = colnames(fa$phi)),
                                   stats::setNames(as.data.frame(unname(fa$phi)), colnames(fa$phi)))))
  }
  M <- if (matriz == "padrão") fa$cargas else fa$estrutura
  d <- tibble::as_tibble(cbind(data.frame(variavel = fa$variaveis),
                               stats::setNames(as.data.frame(unname(M)), colnames(M))))
  d$comunalidade <- unname(fa$comunalidade)
  d$unicidade <- unname(fa$unicidade)
  # Complexidade de Hoffman: 1 quando a variável carrega num fator só, k quando
  # se reparte por igual entre os k. É o número que diz "esta variável não é de
  # fator nenhum" sem precisar comparar colunas a olho.
  d$complexidade <- unname(rowSums(M^2)^2 / rowSums(M^4))
  if (isTRUE(ordenar)) d <- d[.tr_multi_ordem_cargas(M), ]
  d
}

#' A ordem clássica das "cargas ordenadas": primeiro as variáveis do F1, da
#' maior carga (em módulo) para a menor, depois as do F2, e assim por diante.
#' Com ela, os blocos de estrutura simples aparecem como uma escada.
#' @noRd
.tr_multi_ordem_cargas <- function(M) {
  dom <- max.col(abs(M), ties.method = "first")
  order(dom, -abs(M[cbind(seq_len(nrow(M)), dom)]))
}

#' Mapa de calor das cargas.
#' @param fa objeto `tr_multi_fa`.
#' @param corte cargas com módulo menor que isto ficam apagadas e sem número.
#' @param ordenar ordena as variáveis pelo fator dominante.
#' @inheritParams trama.view::tr_view_finish
#' @return ggplot.
#' @export
tr_multi_plot_loadings <- function(fa, corte = 0.3, ordenar = TRUE, aspecto = "4:3", tema = "padrão",
                                   titulo = "", rotulo_x = "", rotulo_y = "", legenda = "direita") {
  corte <- .tr_multi_num(corte, "corte", min = 0, max = 1)
  M <- fa$cargas
  ordem <- if (isTRUE(ordenar)) .tr_multi_ordem_cargas(M) else seq_len(nrow(M))
  vars <- fa$variaveis[ordem]
  d <- data.frame(variavel = rep(fa$variaveis, ncol(M)),
                  fator = rep(colnames(M), each = nrow(M)),
                  carga = as.vector(M))
  # A primeira variável da ordem fica EM CIMA, como numa tabela: o eixo y do
  # ggplot cresce para cima, daí os níveis invertidos.
  d$variavel <- factor(d$variavel, levels = rev(vars))
  d$fator <- factor(d$fator, levels = colnames(M))
  d$forte <- abs(d$carga) >= corte
  d$rotulo <- ifelse(d$forte, formatC(d$carga, format = "f", digits = 2, decimal.mark = ","), "")
  # Cargas de padrão numa oblíqua podem passar de 1 em módulo; a escala fica em
  # −1..1 (a das correlações) e o que passa sai com a cor do extremo.
  aparar <- function(x, range = c(-1, 1)) pmin(pmax(x, range[1]), range[2])
  metodo <- if (fa$metodo == "ml") "máxima verossimilhança" else "eixo principal"
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[["fator"]], y = .data[["variavel"]])) +
    ggplot2::geom_tile(ggplot2::aes(fill = .data[["carga"]], alpha = .data[["forte"]]),
                       colour = NA) +
    ggplot2::geom_text(ggplot2::aes(label = .data[["rotulo"]]), size = 3, colour = "#111111") +
    ggplot2::scale_fill_gradient2(low = .TR_MULTI_COR_2, mid = .TR_MULTI_CINZA, high = .TR_MULTI_COR,
                                  midpoint = 0, limits = c(-1, 1), oob = aparar, name = "carga") +
    ggplot2::scale_alpha_manual(values = c(`FALSE` = .3, `TRUE` = 1), guide = "none") +
    ggplot2::labs(x = "fator", y = NULL,
                  subtitle = sprintf("%s, rotação %s; número onde |carga| >= %s", metodo, fa$rotacao,
                                     formatC(corte, format = "f", digits = 2, decimal.mark = ",")))
  trama.view::tr_view_finish(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

#' O resumo do card.
#' @noRd
.tr_multi_fa_resumo <- function(x) {
  k <- ncol(x$cargas)
  r <- list(metodo = x$metodo, rotacao = x$rotacao, fatores = k, observacoes = x$n,
            variancia_explicada = round(100 * sum(x$comunalidade) / length(x$variaveis), 1))
  if (!is.null(x$ajuste_ml)) r$p_valor_chi2 <- signif(x$ajuste_ml$p_valor, 3)
  r$convergiu <- isTRUE(x$convergiu)
  # O `factanal` segura a unicidade em 0,005: a variável que encosta nesse piso
  # é um caso Heywood disfarçado, e a única pista está aqui.
  piso <- x$variaveis[x$unicidade <= 0.0051]
  if (x$metodo == "ml" && length(piso)) r$heywood <- paste(piso, collapse = ", ")
  r
}

#' Adaptador `multi/fa` → `data/table`: as cargas de padrão, na ordem das
#' variáveis (a tabela que se exporta; a ordenada é do nó `multi/fa_loadings`).
#' @noRd
.tr_multi_fa_tabela <- function(x) tr_multi_fa_loadings(x, "padrão", ordenar = FALSE)

.tr_multi_nos_fatorial <- function() {
  P <- trama::tr_param
  list(
    trama::tr_node("multi/factor_analysis",
      pressupostos = .tr_multi_doc("multi/factor_analysis")$pressupostos,
      referencias = .tr_multi_doc("multi/factor_analysis")$referencias,
      fn = tr_multi_factor_analysis, label = "Análise fatorial",
      category = "multi_fatorial", icon = trama::tr_icon("layers"),
      description = "Extrai fatores latentes (ML ou eixo principal), rotaciona e calcula escores.",
      inputs = list(dados = "data/table"), outputs = list(out = "multi/fa"),
      params = list(
        cols = P("cols", "", label = "Variáveis", example = "ans1, ans2, ans3, soc1, soc2, soc3"),
        fatores = trama::tr_param_int(2L, min = 1, label = "Fatores"),
        metodo = trama::tr_param_enum("ml", .TR_MULTI_METODOS_AF, label = "Método"),
        rotacao = trama::tr_param_enum("varimax", .TR_MULTI_ROTACOES, label = "Rotação"),
        normalizar = trama::tr_when(trama::tr_param_bool(TRUE, label = "Normalização de Kaiser"),
          rotacao = setdiff(.TR_MULTI_ROTACOES, "nenhuma")),
        escores = trama::tr_param_enum("regressão", .TR_MULTI_ESCORES, label = "Escores")),
      help = .tr_multi_ajuda(r"---[
Análise fatorial exploratória: supõe que as correlações entre muitas variáveis
vêm de poucos FATORES latentes que não se medem diretamente — "habilidade
verbal" por trás de cinco testes de vocabulário, "ansiedade" por trás de cinco
itens de questionário. Cada variável é um pedaço de fator comum mais um pedaço
só dela (a unicidade).

É diferente da PCA (`multi/pca`): a PCA resume TODA a variância das variáveis;
a fatorial explica só a variância COMPARTILHADA, e deixa o resto de fora como
erro. Se a pergunta é "que construto está por trás disto?", é fatorial.

### Antes de fatorar

Confira que a matriz tem correlação para fatorar (`multi/kmo_bartlett`) e
escolha o número de fatores com a análise paralela (`multi/parallel`) — a
regra "autovalor maior que 1" costuma sugerir fatores demais.

### Método

- **ml** — máxima verossimilhança (`factanal` do R). Dá um TESTE qui-quadrado
  de que k fatores bastam (o p-valor sai no resumo do card), e por isso é o
  preferido quando se quer comparar modelos. Supõe normalidade multivariada, e
  com n grande o teste rejeita qualquer modelo imperfeito. Casos Heywood (uma
  unicidade que iria a zero) não erram: a unicidade encosta em 0,005, e o
  resumo do card lista as variáveis que encostaram (`heywood`).
- **paf** — eixo principal, iterado nas comunalidades a partir das
  correlações múltiplas ao quadrado. Não supõe distribuição nenhuma, funciona
  com itens Likert e amostras pequenas, e converge onde o ML falha. Não dá
  teste de ajuste. Se ainda assim uma comunalidade passar de 1 (caso Heywood),
  o card fica vermelho nomeando a variável: a solução é impossível, e em geral
  quer dizer fatores demais.

### Rotação

A solução sem rotação é matematicamente certa e quase ilegível: o primeiro
fator pega um pouco de tudo. Rotacionar gira os eixos em busca de ESTRUTURA
SIMPLES — cada variável carregando alto num fator só — sem mudar o quanto o
modelo explica.

- **varimax** (ortogonal) — simplifica as colunas: cada fator com poucas
  cargas grandes. O default de quase todo software.
- **quartimax** (ortogonal) — simplifica as linhas: cada variável num fator;
  tende a produzir um fator geral que pega tudo.
- **equamax** (ortogonal) — o meio-termo entre os dois.
- **oblimin** (oblíqua, quartimin) — deixa os fatores se CORRELACIONAREM.
- **promax** (oblíqua) — parte do varimax e o "exagera" (potência 4); rápida,
  resultado próximo do oblimin.
- **nenhuma** — as cargas como a extração as deu.

**Quando preferir a oblíqua**: quase sempre em ciências humanas. Ansiedade e
sociabilidade não têm por que ser independentes, e a rotação ORTOGONAL força
que sejam: a correlação que existe entre os fatores não some, vai parar nas
cargas cruzadas, e a estrutura fica menos simples do que é. Rode a oblíqua
primeiro; se as correlações entre fatores saírem todas perto de zero, a
ortogonal dá o mesmo e é mais simples de contar. O conjunto `questionario` de
`multi/example` tem a resposta plantada (fatores correlacionados −0,30, −0,25
e 0,20, e a ajuda de `multi/example` diz as cargas): compare dois cards, um
varimax e um oblimin, e veja qual devolve o que foi plantado.

**Normalização de Kaiser**: antes de girar, cada variável é reescalada para
comunalidade 1, e depois volta. Sem ela, as variáveis bem explicadas mandam na
rotação. Deixe ligada, a menos que queira reproduzir um software que não a usa.

### Sinal e ordem dos fatores

O sinal de um fator é arbitrário: F1 com todas as cargas trocadas de sinal é o
mesmo fator. Aqui cada fator é virado para que a soma das suas cargas seja
positiva, e os fatores saem em ordem decrescente de variância explicada. O
nome ("ansiedade") é você quem dá, lendo as cargas.

### Escores

- **regressão** (Thurstone) — prevê o fator a partir das variáveis;
  correlaciona o máximo com o fator verdadeiro, mas os escores saem encolhidos.
- **bartlett** — não viesado: a média do escore, dado o fator, é o fator.
  Preferível quando o escore vai ser usado como variável numa análise seguinte.
- **nenhum** — não calcula.

Com um fator só não há rotação: o objeto registra `nenhuma`.
]---", r"---[
- **Variáveis** — as colunas a fatorar, separadas por vírgula. Em branco, todas
  as numéricas. Faltante é erro (use `data/drop_na` antes).
- **Fatores** — quantos fatores extrair. Com p variáveis, o máximo é o maior k
  com `(p − k)² ≥ p + k`; acima disso o modelo não se identifica e o card diz o
  máximo.
- **Método** — `ml` ou `paf` (acima).
- **Rotação** — `nenhuma`, `varimax`, `quartimax`, `equamax`, `promax`,
  `oblimin`.
- **Normalização de Kaiser** — reescalar as variáveis antes de rotacionar.
- **Escores** — `regressão`, `bartlett` ou `nenhum`.
]---", r"---[
Uma análise fatorial (`multi/fa`). O card mostra o mapa de calor das cargas
(`multi/plot_loadings`) e o resumo: método, rotação, fatores, variância
explicada (a soma das comunalidades, em % do número de variáveis), o p-valor
do qui-quadrado no ML (p pequeno: k fatores NÃO bastam) e se convergiu.

Ligada a um nó da `data`, vira a tabela das cargas de padrão, com
`variavel`, `F1..Fk`, `comunalidade`, `unicidade` e `complexidade`.
]---", r"---[
tr_flow(reg) |>
  tr_add("h", "multi/example", dataset = "harman_24_testes") |>
  tr_add("af", "multi/factor_analysis", fatores = 4L, rotacao = "oblimin", from = "h")
]---", r"---[
`multi/fa_loadings` para as tabelas de padrão, estrutura e correlação entre
fatores; `multi/plot_loadings` para o mapa de calor com outro corte;
`multi/kmo_bartlett` e `multi/parallel` antes de fatorar; `multi/pca` quando o
que se quer é resumir, e não achar construtos.
]---")),

    trama::tr_node("multi/fa_loadings", role = "leitura", fn = tr_multi_fa_loadings, label = "Cargas fatoriais",
      category = "multi_fatorial", icon = trama::tr_icon("table-2"),
      description = "Tabela das cargas de padrão ou de estrutura, ou a correlação entre fatores.",
      inputs = list(fa = "multi/fa"), outputs = list(out = "data/table"),
      params = list(
        matriz = trama::tr_param_enum("padrão", .TR_MULTI_MATRIZES, label = "Matriz"),
        ordenar = trama::tr_param_bool(TRUE, label = "Ordenar pelas cargas")),
      help = .tr_multi_ajuda(r"---[
As cargas de uma análise fatorial (`multi/factor_analysis`) como tabela.

### Padrão × estrutura

Na rotação ORTOGONAL as duas são a mesma matriz. Na OBLÍQUA elas se separam, e
lê-las trocadas é o erro mais comum da análise fatorial:

- **padrão** — o peso de cada fator na variável, descontados os outros
  fatores (como coeficientes de uma regressão). É o que se lê para dizer "este
  item é do fator de ansiedade". Pode passar de 1 em módulo.
- **estrutura** — a CORRELAÇÃO simples entre variável e fator
  (`padrão × Φ`). Inclui a parte que vem por tabela, através da correlação
  entre fatores: um item de ansiedade correlaciona com sociabilidade só porque
  os dois fatores correlacionam.
- **correlação entre fatores** — a matriz Φ. Identidade na ortogonal; na
  oblíqua, é a resposta à pergunta "os construtos são independentes?".

### As colunas de diagnóstico

- **comunalidade** — a fração da variância da variável que os fatores, juntos,
  explicam (h²). Não muda com a rotação. Abaixo de uns 0,2, a variável quase
  não pertence ao modelo.
- **unicidade** — o que sobra (1 − h² na PAF; a estimada pelo `factanal` no
  ML): variância específica mais erro.
- **complexidade** — o índice de Hoffman: 1 quando a variável carrega num fator
  só, perto de k quando se reparte entre k fatores. Um item de complexidade 2
  é ambíguo, e costuma ser o candidato a sair do questionário.

### Ordenar

Com **Ordenar** ligado, as variáveis saem agrupadas pelo fator em que carregam
mais (em módulo) e, dentro do grupo, da maior carga para a menor — as "cargas
ordenadas" dos livros, onde a estrutura simples aparece como uma escada.
]---", r"---[
- **Matriz** — `padrão`, `estrutura` ou `correlação entre fatores`.
- **Ordenar pelas cargas** — agrupar as variáveis pelo fator dominante. Não se
  aplica à correlação entre fatores.
]---", r"---[
Uma tabela (`data/table`). Para padrão e estrutura: `variavel`, `F1..Fk`,
`comunalidade`, `unicidade`, `complexidade` (esta calculada na matriz
mostrada). Para a correlação entre fatores: `fator` e `F1..Fk`.
]---", r"---[
tr_flow(reg) |>
  tr_add("q", "multi/example", dataset = "questionario") |>
  tr_add("af", "multi/factor_analysis", fatores = 3L, rotacao = "oblimin", from = "q") |>
  tr_add("phi", "multi/fa_loadings", matriz = "correlação entre fatores", from = "af")
]---", r"---[
`multi/factor_analysis` para ajustar; `multi/plot_loadings` para ver as mesmas
cargas como mapa de calor; `data/filter` para ficar só com as cargas altas.
]---")),

    trama::tr_node("multi/plot_loadings", role = "leitura", fn = tr_multi_plot_loadings, label = "Mapa das cargas",
      category = "multi_fatorial", icon = trama::tr_icon("grid-3x3"),
      description = "Mapa de calor das cargas fatoriais, com as pequenas apagadas.",
      inputs = list(fa = "multi/fa"), outputs = list(out = "view/plot"),
      params = .tr_multi_props(
        corte = trama::tr_param_num(0.3, min = 0, max = 1, step = 0.05, label = "Corte"),
        ordenar = trama::tr_param_bool(TRUE, label = "Ordenar pelas cargas"),
        .aspecto = "4:3"),
      help = .tr_multi_ajuda(r"---[
As cargas de uma análise fatorial (`multi/factor_analysis`) como mapa de calor:
uma linha por variável, uma coluna por fator, azul-violeta para carga positiva
e rosa para negativa. É o preview do card da análise fatorial.

Na oblíqua, as cargas desenhadas são as de PADRÃO (veja a diferença para a
estrutura em `multi/fa_loadings`).

Cargas com módulo abaixo do **corte** ficam apagadas e sem número. O corte é
de LEITURA, e não estatístico: 0,30 é o usual (o fator explica ~9% da variância
da variável), 0,40 dá uma leitura mais limpa com amostras grandes. Nada é
removido do modelo.

Com **Ordenar**, as variáveis são agrupadas pelo fator dominante, e a
estrutura simples aparece como uma escada: um bloco escuro por coluna. Uma
linha com dois blocos é carga cruzada; um bloco rosa no meio do azul é item
invertido (como `soc5` no `questionario` de `multi/example`).

O subtítulo diz o método e a rotação, para dois cards lado a lado não se
confundirem.
]---", r"---[
- **Corte** — módulo mínimo para a carga aparecer com cor cheia e número.
- **Ordenar pelas cargas** — agrupar as variáveis pelo fator dominante.
]---", r"---[
Um gráfico (`view/plot`).
]---", r"---[
tr_flow(reg) |>
  tr_add("q", "multi/example", dataset = "questionario") |>
  tr_add("af", "multi/factor_analysis", fatores = 3L, rotacao = "varimax", from = "q") |>
  tr_add("mapa", "multi/plot_loadings", corte = 0.4, from = "af")
]---", r"---[
`multi/fa_loadings` para os mesmos números em tabela; `multi/plot_correlation`
para o mapa da correlação antes de fatorar.
]---", grafico = TRUE))
  )
}
