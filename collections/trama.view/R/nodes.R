# Nós da coleção `view`. Cada `fn` é uma função R comum, exportada e chamável
# direto no console — o "nível 1" que o trama preserva de propósito. Aqui isso
# vale dobrado: quem chama `tr_points(df, "x", "y")` no console recebe um
# ggplot de verdade, somável (`+ labs(...)`), e não uma imagem.
#
# Todas terminam em `.tr_view_acabar()`, que é o funil de tema, rótulos e
# proporção. Nenhum `fn` aplica tema por conta própria.
#
# Todo param acrescentado depois da primeira versão nasce com o default que
# reproduz o desenho antigo: um fluxo gravado antes renderiza igual depois, e
# o teste "os defaults não mudam o desenho" trava isso.

#' Mapeamento estético a partir de NOMES DE COLUNA vindos de params.
#'
#' `.data[[...]]` e não `aes_string()`/`!!sym()`: é a forma que não depende do
#' ambiente de avaliação na hora de DESENHAR, e é isso que faz o objeto
#' restaurado do store num processo diferente ainda imprimir. Coluna em branco
#' vira NULL aqui, e quem tira a estética NULL do objeto é `.tr_view_mapa()`
#' logo abaixo — passar o NULL direto pro `aes()` NÃO a removeria, que é o
#' engano medido e documentado lá.
#' @noRd
.tr_view_aes <- function(data, ..., .param_obrigatorio = character()) {
  cols <- list(...)
  for (nm in names(cols)) {
    v <- as.character(cols[[nm]] %||% "")[[1]]
    if (nm %in% .param_obrigatorio) .tr_view_obrigatorio(v, nm)
    cols[[nm]] <- if (nzchar(trimws(v))) .tr_view_col(data, trimws(v), nm) else NULL
  }
  cols
}

#' Monta o `aes()` com as estéticas efetivamente ligadas, e só elas.
#'
#' Existe por uma medida, não por gosto: no ggplot2 4.0 um
#' `aes(colour = if (is.null(x)) NULL else .data[[x]])` NÃO some do mapeamento
#' — o `aes()` guarda a quosura do `if` inteiro e só descobre o NULL na hora de
#' desenhar. O desenho sai certo, mas o objeto passa a ANUNCIAR um mapeamento
#' que ele não tem, e a quosura fantasma guarda `.data[[NULL]]` — de modo que a
#' premissa que o tipo `view/plot` declara ("toda quosura aqui é um
#' `.data[["nome"]]` puro") deixa de ser verdade e vira quase-verdade, que é o
#' tipo de coisa que só cobra o preço na restauração, longe daqui. Montando a
#' lista ANTES e chamando `aes()` com `!!!`, o que não está ligado simplesmente
#' não existe no objeto.
#'
#' O que este helper NÃO resolve, e não vale fingir que resolve: o ambiente do
#' `fn` viaja no `saveRDS` de qualquer jeito, por `p$layers[[1]]` e
#' `p$plot_env`, porque `ggplot()` e `geom_*()` são chamados aqui dentro.
#' Medido: com e sem o fantasma, o artefato tem o mesmo tamanho.
#'
#' Convenção dos argumentos: texto é NOME DE COLUNA e vira `.data[[texto]]`;
#' NULL é estética desligada e some; qualquer outra coisa entra literal. Uma
#' CONSTANTE de texto (a caixa única do boxplot) precisa então vir embrulhada
#' em `rlang::quo()`, senão viraria uma busca pela coluna de nome vazio.
#' @noRd
.tr_view_mapa <- function(...) {
  partes <- Filter(Negate(is.null), list(...))
  partes <- lapply(partes, function(v) {
    if (is.character(v) && length(v) == 1L) rlang::expr(.data[[!!v]]) else v
  })
  rlang::inject(ggplot2::aes(!!!partes))
}

#' Confere um param de escolha no nível 1, e devolve o valor.
#'
#' O enum protege o card; o console não tem enum. Mesma razão do
#' `.tr_view_option()`, que é quem de fato recusa.
#' @noRd
.tr_view_escolha <- function(valor, param, aceitos) {
  if (!length(valor) || !as.character(valor)[[1]] %in% aceitos) .tr_view_option(param, valor, aceitos)
  as.character(valor)[[1]]
}

#' Painéis por uma coluna: `facet_wrap`, ou nada.
#'
#' Painel é a resposta honesta a dois vícios que a ajuda desta coleção
#' denuncia — o empilhamento, em que só a fatia de baixo começa no zero, e a
#' nuvem sobreposta, em que um grupo esconde o outro. Cada painel ganha o seu
#' zero e o seu espaço, na MESMA escala (`scales = "fixed"`), que é o que
#' permite comparar um painel com o vizinho de olho. Escala livre por painel
#' faria cada um parecer cheio e mentiria na comparação.
#'
#' A quosura é `.data[["nome"]]` pura, pela mesma premissa de restauração do
#' `.tr_view_mapa()`.
#' @noRd
.tr_view_painel <- function(p, data, painel) {
  v <- trimws(as.character(painel %||% "")[[1]])
  if (!nzchar(v)) return(p)
  col <- .tr_view_col(data, v, "painel")
  p + rlang::inject(ggplot2::facet_wrap(ggplot2::vars(.data[[!!col]])))
}

#' Recusa medida que não é número, onde a conta a exige.
#'
#' Nos gráficos que o ggplot desenha direto, a medida de texto falha no
#' DESENHO com a mensagem do ggplot ("discrete value supplied to continuous
#' scale"), e é aceitável. Nos que agregam aqui dentro (médias, mapa de calor),
#' `mean()` de texto dá `NA` com um aviso e o gráfico sai VAZIO — plausível e
#' errado. Aqui a recusa tem de ser nossa.
#' @noRd
.tr_view_numerica <- function(data, col, param) {
  if (!is.numeric(data[[col]])) {
    rlang::abort(
      sprintf("Param '%s': a coluna '%s' é %s, e o gráfico precisa de número. Converta-a antes num Converter tipo.",
              param, col, class(data[[col]])[[1]]),
      class = "tr_view_error_not_numeric")
  }
  col
}

.TR_VIEW_LOG <- c("nenhum", "X", "Y", "ambos")

#' Eixo em log: a escala, e a recusa do que o log não tem.
#'
#' Log de zero ou de negativo não existe, e o ggplot não falha: descarta a
#' linha com um aviso ("introduced infinite values") que o card nunca mostra,
#' e o gráfico sai com pontos a menos, plausível e errado. Por isso a coluna é
#' conferida AQUI, antes do desenho, e o erro diz quantos valores impedem.
#'
#' Os rótulos do eixo são números por extenso, e não `1e+03`: quem liga o log
#' quer ler as ordens de grandeza, e a notação científica as esconde.
#' @noRd
.tr_view_log <- function(p, data, x = NULL, y = NULL) {
  rotulos <- scales::label_number(big.mark = ".", decimal.mark = ",", drop0trailing = TRUE)
  for (eixo in c("x", "y")) {
    col <- if (eixo == "x") x else y
    if (is.null(col)) next
    .tr_view_numerica(data, col, "log")
    ruins <- sum(data[[col]] <= 0, na.rm = TRUE)
    if (ruins > 0) {
      rlang::abort(
        sprintf("Param 'log': a coluna '%s' tem %d valor(es) menor(es) ou igual(is) a zero, que não existem em escala log. Filtre-os ou desligue o log.",
                col, ruins),
        class = "tr_view_error_not_positive")
    }
    p <- p + if (eixo == "x") ggplot2::scale_x_log10(labels = rotulos)
             else ggplot2::scale_y_log10(labels = rotulos)
  }
  p
}

#' Qual eixo o enum de log liga, para gráficos de dois eixos contínuos.
#' @noRd
.tr_view_log2 <- function(p, data, log, m) {
  log <- .tr_view_escolha(log, "log", .TR_VIEW_LOG)
  .tr_view_log(p, data, x = if (log %in% c("X", "ambos")) m$x, y = if (log %in% c("Y", "ambos")) m$y)
}

#' Números escritos no gráfico, com a precisão da ordem de grandeza.
#'
#' `label_number()` sem `accuracy` escolhe as casas pela menor DIFERENÇA entre
#' os valores, e somas como 154,141 e 170,430 saíam com três decimais que
#' ninguém lê num rótulo. Aqui a regra é a de quem escreve à mão: inteiro a
#' partir de 100 (e sempre, para contagens), uma casa a partir de 10, duas
#' abaixo disso.
#' @noRd
.tr_view_numero <- function(v) {
  topo <- suppressWarnings(max(abs(v), na.rm = TRUE))
  inteiros <- all(abs(v - round(v)) < 1e-9, na.rm = TRUE)
  precisao <- if (inteiros || !is.finite(topo) || topo >= 100) 1 else if (topo >= 10) .1 else .01
  scales::label_number(accuracy = precisao, big.mark = ".", decimal.mark = ",")(v)
}

#' Agrega por chaves, preservando o TIPO de cada chave.
#'
#' `aggregate()` devolveria as chaves como vieram do `by`, mas perde a ordem
#' dos níveis em alguns caminhos e quer fórmula ou lista. Aqui as chaves saem
#' das PRIMEIRAS linhas de cada grupo da tabela original: fator continua fator,
#' com os mesmos níveis, e data continua data — é o que mantém a ordem do eixo
#' igual à dos gráficos que não agregam. Linha com chave faltante fica fora,
#' como no `group_by` + `summarise` sem `.drop`, e a ajuda dos nós diz isso.
#'
#' `f` recebe os índices de um grupo e devolve uma lista nomeada de escalares.
#' @noRd
.tr_view_agregar <- function(data, chaves, f) {
  data <- as.data.frame(data)
  ok <- stats::complete.cases(data[chaves])
  linhas <- seq_len(nrow(data))[ok]
  if (!length(linhas)) {
    out <- data[0L, chaves, drop = FALSE]
    return(out)
  }
  grupos <- split(linhas, lapply(chaves, function(k) data[[k]][ok]), drop = TRUE)
  primeiras <- vapply(grupos, `[[`, integer(1), 1L)
  out <- data[primeiras, chaves, drop = FALSE]
  valores <- do.call(rbind, lapply(grupos, function(i) as.data.frame(f(i))))
  out <- cbind(out, valores)
  rownames(out) <- NULL
  out
}

# ---- Relação -----------------------------------------------------------------

.TR_VIEW_TENDENCIAS <- c("nenhuma", "linear", "suave")

#' Disperso: uma marca por linha. O gráfico de RELAÇÃO entre duas medidas.
#'
#' `tendencia` usa `formula = y ~ x` explícita: sem ela o ggplot escreve no
#' console qual fórmula escolheu, e com `suave` escolheria `gam` acima de mil
#' linhas — que exige o `mgcv`, dependência que a coleção não declara. `loess`
#' sempre é o mesmo método para qualquer tamanho, o que a ajuda pode descrever.
#' @export
tr_points <- function(dados, x = "", y = "", cor = "", tendencia = "nenhuma", log = "nenhum",
                      painel = "",
                      aspecto = "16:9", tema = "padrão", titulo = "", rotulo_x = "",
                      rotulo_y = "", legenda = "direita") {
  m <- .tr_view_aes(dados, x = x, y = y, cor = cor, .param_obrigatorio = c("x", "y"))
  tendencia <- .tr_view_escolha(tendencia, "tendencia", .TR_VIEW_TENDENCIAS)
  p <- ggplot2::ggplot(dados, .tr_view_mapa(x = m$x, y = m$y, colour = m$cor)) +
    ggplot2::geom_point(size = 2.4, alpha = .85) +
    ggplot2::labs(x = m$x, y = m$y, colour = m$cor)
  if (tendencia != "nenhuma") {
    p <- p + ggplot2::geom_smooth(method = if (tendencia == "linear") "lm" else "loess",
                                  formula = y ~ x, se = TRUE, linewidth = .9, alpha = .2)
  }
  # O log vem antes da tendência ser calculada no desenho: com a escala em log,
  # o `lm` ajusta nos valores transformados, e a reta sai reta no gráfico.
  p <- .tr_view_log2(p, dados, log, m)
  p <- .tr_view_painel(p, dados, painel)
  .tr_view_acabar(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

#' Linha: a mesma relação, quando o X tem ORDEM (tempo, dose, tamanho).
#' @export
tr_line <- function(dados, x = "", y = "", cor = "", marcar = FALSE, log = "nenhum", painel = "",
                    aspecto = "16:9", tema = "padrão", titulo = "", rotulo_x = "",
                    rotulo_y = "", legenda = "direita") {
  m <- .tr_view_aes(dados, x = x, y = y, cor = cor, .param_obrigatorio = c("x", "y"))
  # `group` explícito porque sem cor o ggplot agruparia por nada e desenharia
  # uma linha só — que aqui é o certo — mas com um X discreto ele agruparia por
  # X e desenharia segmentos soltos, invisíveis. O `1L` fixa a linha única.
  p <- ggplot2::ggplot(dados, .tr_view_mapa(
        x = m$x, y = m$y, colour = m$cor,
        group = if (is.null(m$cor)) 1L else m$cor)) +
    ggplot2::geom_line(linewidth = .7) +
    ggplot2::labs(x = m$x, y = m$y, colour = m$cor)
  if (isTRUE(marcar)) p <- p + ggplot2::geom_point(size = 1.9)
  p <- .tr_view_log2(p, dados, log, m)
  p <- .tr_view_painel(p, dados, painel)
  .tr_view_acabar(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

#' Área: a linha preenchida até o zero, EMPILHADA por grupo.
#'
#' No ggplot2 4.0 o `geom_area` usa `stat_align`, que interpola cada grupo nos
#' X de todos os outros antes de empilhar — sem isso, séries com meses
#' diferentes empilhariam dentes. É o motivo de não haver `position` aqui:
#' empilhar é a razão de ser do gráfico, e área SOBREPOSTA esconde a série de
#' trás; para séries lado a lado, a linha responde melhor.
#' @export
tr_area <- function(dados, x = "", y = "", cor = "", painel = "", aspecto = "16:9",
                    tema = "padrão", titulo = "", rotulo_x = "", rotulo_y = "",
                    legenda = "direita") {
  m <- .tr_view_aes(dados, x = x, y = y, cor = cor, .param_obrigatorio = c("x", "y"))
  p <- ggplot2::ggplot(dados, .tr_view_mapa(x = m$x, y = m$y, fill = m$cor)) +
    ggplot2::geom_area(alpha = .9, colour = NA) +
    ggplot2::labs(x = m$x, y = m$y, fill = m$cor)
  p <- .tr_view_painel(p, dados, painel)
  .tr_view_acabar(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

#' Grade de densidade: o disperso que CONTA, para quando as marcas se empilham.
#' @export
tr_bin2d <- function(dados, x = "", y = "", classes = 40L, log = "nenhum", painel = "",
                     aspecto = "16:9",
                     tema = "padrão", titulo = "", rotulo_x = "", rotulo_y = "",
                     legenda = "direita") {
  m <- .tr_view_aes(dados, x = x, y = y, .param_obrigatorio = c("x", "y"))
  p <- ggplot2::ggplot(dados, .tr_view_mapa(x = m$x, y = m$y)) +
    ggplot2::geom_bin_2d(bins = max(2L, as.integer(classes))) +
    ggplot2::labs(x = m$x, y = m$y, fill = "contagem")
  p <- .tr_view_log2(p, dados, log, m)
  p <- .tr_view_painel(p, dados, painel)
  .tr_view_acabar(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

#' Mapa de calor: uma célula por par de categorias, pintada por um número.
#'
#' Agrega AQUI, e passa a tabela agregada ao ggplot, em vez de deixar o
#' `geom_tile` desenhar uma célula por linha: com duas linhas no mesmo par, o
#' tile desenharia as duas no mesmo lugar e só a última apareceria — o número
#' da célula seria o da última linha, em silêncio. Agregado, o número está no
#' `p$data` do objeto e é o mesmo que o rótulo escreve. A regra é a das
#' barras, para que as duas digam a mesma coisa da mesma tabela: `valor` vazio
#' CONTA linhas, preenchido SOMA.
#'
#' O rótulo tem fundo na cor do fundo do tema, meio transparente: texto de cor
#' fixa some na metade clara ou na metade escura de qualquer escala contínua.
#' @export
tr_heatmap <- function(dados, x = "", y = "", valor = "", rotulos = FALSE, aspecto = "16:9",
                       tema = "padrão", titulo = "", rotulo_x = "", rotulo_y = "",
                       legenda = "direita") {
  m <- .tr_view_aes(dados, x = x, y = y, valor = valor, .param_obrigatorio = c("x", "y"))
  if (!is.null(m$valor)) .tr_view_numerica(dados, m$valor, "valor")
  nome <- m$valor %||% "contagem"
  agregado <- .tr_view_agregar(dados, unique(c(m$x, m$y)), function(i) {
    stats::setNames(list(if (is.null(m$valor)) length(i) else sum(dados[[m$valor]][i], na.rm = TRUE)),
                    nome)
  })
  # O texto do rótulo vira COLUNA da tabela, e não função no `aes()`: uma
  # função local no mapeamento não seria encontrada na hora de desenhar, e
  # viajaria no `rds` como closure.
  if (isTRUE(rotulos)) {
    agregado$rotulo <- .tr_view_numero(agregado[[nome]])
  }
  p <- ggplot2::ggplot(agregado, .tr_view_mapa(x = m$x, y = m$y, fill = nome)) +
    ggplot2::geom_tile(colour = NA) +
    ggplot2::labs(x = m$x, y = m$y, fill = nome)
  if (isTRUE(rotulos)) {
    t <- trama::tr_theme(tema)
    p <- p + ggplot2::geom_label(
      .tr_view_mapa(label = "rotulo"),
      fill = scales::alpha(t$fundo, .7), colour = t$texto, border.colour = NA, size = 3.4)
  }
  .tr_view_acabar(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

# ---- Distribuição ------------------------------------------------------------

.TR_VIEW_POSICOES_HIST <- c("empilhar", "sobrepor")

#' Histograma: a forma da distribuição de UMA medida.
#' @export
tr_histogram <- function(dados, x = "", cor = "", classes = 30L, posicao = "empilhar",
                         log = FALSE, painel = "", aspecto = "16:9", tema = "padrão", titulo = "",
                         rotulo_x = "", rotulo_y = "", legenda = "direita") {
  m <- .tr_view_aes(dados, x = x, cor = cor, .param_obrigatorio = "x")
  posicao <- .tr_view_escolha(posicao, "posicao", .TR_VIEW_POSICOES_HIST)
  # Sobrepor só muda algo com grupos; sem cor, a barra única é a mesma nos dois.
  geom <- if (posicao == "sobrepor" && !is.null(m$cor)) {
    ggplot2::geom_histogram(bins = max(2L, as.integer(classes)), colour = NA,
                            position = "identity", alpha = .5)
  } else {
    ggplot2::geom_histogram(bins = max(2L, as.integer(classes)), colour = NA)
  }
  p <- ggplot2::ggplot(dados, .tr_view_mapa(x = m$x, fill = m$cor)) + geom +
    ggplot2::labs(x = m$x, y = "contagem", fill = m$cor)
  # Em log as classes têm largura igual na escala TRANSFORMADA: cada uma cobre
  # a mesma razão (de 10 a 20, de 100 a 200), que é o que se quer numa cauda.
  if (isTRUE(log)) p <- .tr_view_log(p, dados, x = m$x)
  p <- .tr_view_painel(p, dados, painel)
  .tr_view_acabar(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

#' Densidade: o histograma sem classes, alisado por um núcleo.
#'
#' `suavidade` é o `adjust` do `density()`: multiplica a largura de banda que a
#' regra de Silverman escolheu. É o `classes` desta forma — o param que muda a
#' conclusão —, e por isso é um multiplicador em torno de 1, e não a largura em
#' unidades da coluna: `1` é "a escolha automática", e o número faz sentido
#' para qualquer medida.
#' @export
tr_density <- function(dados, x = "", cor = "", suavidade = 1, log = FALSE, painel = "",
                       aspecto = "16:9",
                       tema = "padrão", titulo = "", rotulo_x = "", rotulo_y = "",
                       legenda = "direita") {
  m <- .tr_view_aes(dados, x = x, cor = cor, .param_obrigatorio = "x")
  ajuste <- suppressWarnings(as.numeric(suavidade)[[1]])
  if (is.na(ajuste) || ajuste <= 0) .tr_view_option("suavidade", suavidade, "número maior que zero")
  p <- ggplot2::ggplot(dados, .tr_view_mapa(x = m$x, colour = m$cor, fill = m$cor)) +
    ggplot2::geom_density(adjust = ajuste, alpha = if (is.null(m$cor)) 1 else .3, linewidth = .8) +
    ggplot2::labs(x = m$x, y = "densidade", colour = m$cor, fill = m$cor)
  if (isTRUE(log)) p <- .tr_view_log(p, dados, x = m$x)
  p <- .tr_view_painel(p, dados, painel)
  .tr_view_acabar(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

#' Boxplot: a distribuição de uma medida, COMPARADA entre grupos.
#'
#' `x` em branco é desligado e vira uma caixa só, da amostra inteira — que é a
#' pergunta legítima de quem ainda não tem grupo.
#'
#' Com `pontos`, os pontos soltos da própria caixa somem (`outlier.shape =
#' NA`): senão cada outlier sairia DUAS vezes, uma da caixa e outra das
#' observações, e o grupo pareceria ter mais extremos do que tem. O `seed` fixo
#' do jitter não é enfeite: o PNG é cacheado pelo que o nó recebe, e um jitter
#' sorteado a cada execução daria imagens diferentes para a mesma chave.
#' @export
tr_boxplot <- function(dados, x = "", y = "", cor = "", pontos = FALSE, log = FALSE, painel = "",
                       aspecto = "16:9", tema = "padrão", titulo = "", rotulo_x = "",
                       rotulo_y = "", legenda = "direita") {
  m <- .tr_view_aes(dados, x = x, y = y, cor = cor, .param_obrigatorio = "y")
  # Sem X, o mapeamento vai para a constante `""`: uma categoria só, uma caixa
  # só, e o eixo sem rótulo pendurado. Deixar o X fora do `aes()` daria o mesmo
  # desenho, mas com o eixo em 0 — um número que não quer dizer nada ali.
  p <- ggplot2::ggplot(dados, .tr_view_mapa(
        x = if (is.null(m$x)) rlang::quo("") else m$x, y = m$y, fill = m$cor)) +
    (if (isTRUE(pontos)) ggplot2::geom_boxplot(width = .55, outlier.shape = NA)
     else ggplot2::geom_boxplot(width = .55, outlier.alpha = .8)) +
    ggplot2::labs(x = m$x %||% "", y = m$y, fill = m$cor)
  if (isTRUE(pontos)) p <- p + .tr_view_observacoes(!is.null(m$cor), largura = .55)
  if (isTRUE(log)) p <- .tr_view_log(p, dados, y = m$y)
  p <- .tr_view_painel(p, dados, painel)
  .tr_view_acabar(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

#' As observações por cima de caixa ou violino, espalhadas na horizontal.
#'
#' Com subgrupos (`fill` ligado), o jitter precisa ESQUIVAR junto com as
#' caixas, ou os pontos de todos os subgrupos cairiam no meio da categoria.
#' Altura zero sempre: o valor vertical é o dado, e sacudi-lo seria mentir.
#' @noRd
.tr_view_observacoes <- function(subgrupos, largura) {
  pos <- if (subgrupos) {
    ggplot2::position_jitterdodge(jitter.width = .12, jitter.height = 0,
                                  dodge.width = largura, seed = 1L)
  } else {
    ggplot2::position_jitter(width = .12, height = 0, seed = 1L)
  }
  ggplot2::geom_point(position = pos, size = 1.3, alpha = .4, show.legend = FALSE)
}

#' Violino: a densidade de cada grupo, espelhada — a forma que a caixa esconde.
#'
#' A caixa por dentro é estreita e sem outliers, e herda o `fill` dos grupos
#' só para ESQUIVAR junto; a cor dela é a do fundo do tema, para que a mediana
#' leia sobre qualquer violino. `position_dodge(.9)` é o do próprio violino.
#' @export
tr_violin <- function(dados, x = "", y = "", cor = "", caixa = TRUE, pontos = FALSE,
                      log = FALSE, painel = "", aspecto = "16:9", tema = "padrão", titulo = "",
                      rotulo_x = "", rotulo_y = "", legenda = "direita") {
  m <- .tr_view_aes(dados, x = x, y = y, cor = cor, .param_obrigatorio = "y")
  gx <- if (is.null(m$x)) rlang::quo("") else m$x
  t <- trama::tr_theme(tema)
  # Sem grupo de cor, o violino herdaria o preenchimento padrão do ggplot 4,
  # que é a cor do PAPEL: no tema claro ele some no fundo. A primeira cor da
  # paleta do tema é a cor "de um grupo só" que os outros gráficos já usam.
  violino <- if (is.null(m$cor)) {
    ggplot2::geom_violin(trim = TRUE, colour = NA, alpha = .85, fill = t$paleta[[1]])
  } else {
    ggplot2::geom_violin(trim = TRUE, colour = NA, alpha = .85)
  }
  p <- ggplot2::ggplot(dados, .tr_view_mapa(x = gx, y = m$y, fill = m$cor)) + violino +
    ggplot2::labs(x = m$x %||% "", y = m$y, fill = m$cor)
  if (isTRUE(caixa)) {
    p <- p + ggplot2::geom_boxplot(
      width = .1, outlier.shape = NA, fill = t$fundo, colour = t$texto, alpha = .8,
      position = ggplot2::position_dodge(width = .9), show.legend = FALSE)
  }
  if (isTRUE(pontos)) p <- p + .tr_view_observacoes(!is.null(m$cor), largura = .9)
  if (isTRUE(log)) p <- .tr_view_log(p, dados, y = m$y)
  p <- .tr_view_painel(p, dados, painel)
  .tr_view_acabar(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

#' Acumulada: a proporção das linhas até cada valor. Nada a escolher.
#'
#' É a forma da distribuição sem os dois params que mudam a conclusão dos
#' irmãos — nem classes, nem suavidade. Cada degrau é uma observação, e as
#' curvas de grupos diferentes se comparam na horizontal (a mediana de cada um
#' é onde a curva cruza 0,5).
#' @export
tr_ecdf <- function(dados, x = "", cor = "", log = FALSE, painel = "", aspecto = "16:9",
                    tema = "padrão",
                    titulo = "", rotulo_x = "", rotulo_y = "", legenda = "direita") {
  m <- .tr_view_aes(dados, x = x, cor = cor, .param_obrigatorio = "x")
  p <- ggplot2::ggplot(dados, .tr_view_mapa(x = m$x, colour = m$cor)) +
    ggplot2::stat_ecdf(geom = "step", linewidth = .8) +
    ggplot2::scale_y_continuous(labels = scales::label_percent(decimal.mark = ",")) +
    ggplot2::labs(x = m$x, y = "proporção acumulada", colour = m$cor)
  if (isTRUE(log)) p <- .tr_view_log(p, dados, x = m$x)
  p <- .tr_view_painel(p, dados, painel)
  .tr_view_acabar(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

#' Quantil-quantil contra a normal: a amostra ordenada contra o que seria.
#'
#' A reta é a do `qqline()` — pelos quartis, e não pela média e pelo desvio —,
#' que é a que não se deixa puxar pelas caudas que o gráfico existe para
#' mostrar.
#' @export
tr_qq <- function(dados, y = "", cor = "", painel = "", aspecto = "16:9", tema = "padrão",
                  titulo = "", rotulo_x = "", rotulo_y = "", legenda = "direita") {
  m <- .tr_view_aes(dados, y = y, cor = cor, .param_obrigatorio = "y")
  p <- ggplot2::ggplot(dados, .tr_view_mapa(sample = m$y, colour = m$cor)) +
    ggplot2::stat_qq_line(linewidth = .7, alpha = .7) +
    ggplot2::stat_qq(size = 1.9, alpha = .85) +
    ggplot2::labs(x = "quantil teórico (normal)", y = m$y, colour = m$cor)
  p <- .tr_view_painel(p, dados, painel)
  .tr_view_acabar(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

# ---- Comparação --------------------------------------------------------------

.TR_VIEW_POSICOES_BARRAS <- c("empilhar", "lado a lado", "proporção")

#' Barras: comparação entre categorias.
#'
#' `y` em branco é DESLIGADO com sentido próprio: sem Y, a barra conta linhas
#' (`stat = "count"`), que é a pergunta natural de quem só tem a categoria.
#' Falhar aqui seria exigir que a pessoa agregue antes pra ver quantas são.
#'
#' `ordenar` reordena os NÍVEIS da categoria pela altura total (a mesma soma
#' ou contagem que a barra desenha), numa cópia da tabela. Deitada, a maior
#' vai para CIMA, que é onde o olho começa a ler um ranking — por isso os
#' níveis se invertem com `deitar`.
#'
#' `rotulos` agrega a tabela ANTES, como o `lado a lado` já fazia: o número
#' escrito tem de ser o da barra, e com várias linhas por barra só a tabela
#' agregada tem esse número numa linha só. O rótulo mora numa coluna da tabela,
#' e não numa função no `aes()`, pela mesma razão do mapa de calor.
#' @export
tr_bars <- function(dados, x = "", y = "", cor = "", posicao = "empilhar", ordenar = FALSE,
                    deitar = FALSE, rotulos = FALSE, painel = "", aspecto = "16:9",
                    tema = "padrão", titulo = "", rotulo_x = "", rotulo_y = "",
                    legenda = "direita") {
  m <- .tr_view_aes(dados, x = x, y = y, cor = cor, painel = painel, .param_obrigatorio = "x")
  posicao <- .tr_view_escolha(posicao, "posicao", .TR_VIEW_POSICOES_BARRAS)
  if (isTRUE(ordenar)) {
    altura <- if (is.null(m$y)) rep(1, nrow(dados)) else dados[[m$y]]
    total <- tapply(altura, as.character(dados[[m$x]]), sum, na.rm = TRUE)
    niveis <- names(sort(total, decreasing = !isTRUE(deitar)))
    dados[[m$x]] <- factor(as.character(dados[[m$x]]), levels = niveis)
  }
  eixo_y <- if (posicao == "proporção") "proporção" else m$y %||% "contagem"
  # Lado a lado, várias linhas no mesmo par categoria × subgrupo seriam barras
  # SOBREPOSTAS na mesma vaga — só a mais alta apareceria, e a barra mostraria
  # um valor, não a soma. Empilhar e proporção somam por construção; aqui a
  # soma é feita antes, para as três posições dizerem o mesmo número.
  agregar <- isTRUE(rotulos) || (posicao == "lado a lado" && !is.null(m$y))
  if (agregar) {
    chaves <- unique(c(m$x, m$cor, m$painel))
    nome <- m$y %||% "contagem"
    vals <- if (is.null(m$y)) NULL else dados[[m$y]]
    dados <- .tr_view_agregar(dados, chaves, function(i) stats::setNames(
      list(if (is.null(vals)) length(i) else sum(vals[i], na.rm = TRUE)), nome))
    m$y <- nome
  }
  pos <- switch(posicao, empilhar = "stack",
                `lado a lado` = ggplot2::position_dodge(preserve = "single"),
                `proporção` = "fill")
  p <- ggplot2::ggplot(dados, .tr_view_mapa(x = m$x, y = m$y, fill = m$cor)) +
    (if (is.null(m$y)) ggplot2::geom_bar(position = pos) else ggplot2::geom_col(position = pos)) +
    ggplot2::labs(x = m$x, fill = m$cor, y = eixo_y)
  if (isTRUE(rotulos)) p <- .tr_view_rotulos_barras(p, dados, m, posicao, deitar, tema)
  if (posicao == "proporção") {
    p <- p + ggplot2::scale_y_continuous(labels = scales::label_percent(decimal.mark = ","))
  } else if (isTRUE(rotulos)) {
    # Rótulo acima da barra precisa de espaço acima da maior: sem a folga,
    # o número da barra mais alta sai cortado pela borda do painel.
    p <- p + ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .1)))
  }
  if (isTRUE(deitar)) p <- p + ggplot2::coord_flip()
  p <- .tr_view_painel(p, dados, m$painel %||% "")
  .tr_view_acabar(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

#' Os números sobre as barras, onde cada posição os lê melhor.
#'
#' Fatia empilhada tem o número NO MEIO dela, com fundo — fora da fatia não se
#' saberia de qual é. Barra inteira (sem subgrupo, ou lado a lado) tem o número
#' logo além da ponta, sem fundo. Na proporção, o número é a PARCELA da
#' categoria, que é o que o eixo mostra; a soma absoluta ali contradiria o eixo.
#' @noRd
.tr_view_rotulos_barras <- function(p, dados, m, posicao, deitar, tema) {
  t <- trama::tr_theme(tema)
  if (posicao == "proporção") {
    chave <- interaction(dados[c(m$x, m$painel)], drop = TRUE)
    parcela <- dados[[m$y]] / stats::ave(dados[[m$y]], chave, FUN = function(v) sum(v, na.rm = TRUE))
    p$data$rotulo <- scales::label_percent(accuracy = 1, decimal.mark = ",")(parcela)
  } else {
    p$data$rotulo <- .tr_view_numero(dados[[m$y]])
  }
  dentro <- posicao == "proporção" || (posicao == "empilhar" && !is.null(m$cor))
  if (dentro) {
    return(p + ggplot2::geom_label(
      .tr_view_mapa(label = "rotulo", group = m$cor),
      position = if (posicao == "proporção") ggplot2::position_fill(vjust = .5)
                 else ggplot2::position_stack(vjust = .5),
      fill = scales::alpha(t$fundo, .7), colour = t$texto, border.colour = NA, size = 3.2))
  }
  p + ggplot2::geom_text(
    .tr_view_mapa(label = "rotulo", group = m$cor),
    position = if (posicao == "lado a lado") ggplot2::position_dodge(width = .9, preserve = "single")
               else "identity",
    vjust = if (isTRUE(deitar)) .5 else -.45, hjust = if (isTRUE(deitar)) -.15 else .5,
    colour = t$texto, size = 3.3)
}

# O nível do IC mora em `confianca`, não no nome da opção: com ele embutido
# ("IC 95%") cada nível pedia uma opção nova, e as outras barras ignoram nível.
.TR_VIEW_BARRAS_ERRO <- c("IC", "erro padrão", "desvio padrão")

#' Médias com barras: um ponto por grupo, e a incerteza em volta dele.
#'
#' Calcula a tabela ANTES e passa a tabela ao ggplot, em vez de um
#' `stat_summary` com função própria: o número desenhado fica em `p$data`,
#' conferível no console, e nenhuma closure da coleção viaja no `rds`. O IC é
#' o da t de Student com n − 1 graus de liberdade, o mesmo do `t.test()`; grupo
#' com uma linha só não tem desvio, e sai o ponto sem barra.
#'
#' As três barras respondem perguntas diferentes, e a ajuda insiste nisso:
#' desvio padrão é o espalhamento dos DADOS; erro padrão e IC, a incerteza da
#' MÉDIA, que encolhe com n. `confianca` só vale para `barra = "IC"`.
#' @export
tr_means <- function(dados, x = "", y = "", cor = "", barra = "IC", confianca = 0.95, painel = "",
                     aspecto = "16:9", tema = "padrão", titulo = "", rotulo_x = "",
                     rotulo_y = "", legenda = "direita") {
  m <- .tr_view_aes(dados, x = x, y = y, cor = cor, painel = painel,
                    .param_obrigatorio = c("x", "y"))
  barra <- .tr_view_escolha(barra, "barra", .TR_VIEW_BARRAS_ERRO)
  if (barra == "IC" && !(is.numeric(confianca) && length(confianca) == 1L && !is.na(confianca) &&
                         confianca > 0 && confianca < 1)) {
    rlang::abort("`confianca` precisa ser um número entre 0 e 1 (0,95 = 95%).",
                 class = "tr_view_error_bad_option")
  }
  # O rótulo diz o nível de verdade ("IC 90%"), não só "IC".
  nome_barra <- if (barra == "IC") {
    paste0("IC ", format(round(100 * confianca, 1), decimal.mark = ",", trim = TRUE), "%")
  } else barra
  .tr_view_numerica(dados, m$y, "y")
  vals <- dados[[m$y]]
  resumo <- .tr_view_agregar(dados, unique(c(m$x, m$cor, m$painel)), function(i) {
    v <- vals[i][!is.na(vals[i])]
    n <- length(v)
    s <- if (n > 1L) stats::sd(v) else NA_real_
    meia <- switch(barra,
      IC = if (n > 1L) stats::qt(1 - (1 - confianca) / 2, n - 1L) * s / sqrt(n) else NA_real_,
      `erro padrão` = s / sqrt(n),
      `desvio padrão` = s)
    media <- if (n) mean(v) else NA_real_
    list(n = n, media = media, inferior = media - meia, superior = media + meia)
  })
  esquiva <- ggplot2::position_dodge(width = if (is.null(m$cor)) 0 else .45)
  p <- ggplot2::ggplot(resumo, .tr_view_mapa(x = m$x, y = "media", colour = m$cor)) +
    ggplot2::geom_errorbar(.tr_view_mapa(ymin = "inferior", ymax = "superior"),
                           width = .18, linewidth = .7, position = esquiva, na.rm = TRUE) +
    ggplot2::geom_point(size = 3.2, position = esquiva) +
    ggplot2::labs(x = m$x, y = sprintf("%s (média ± %s)", m$y, nome_barra), colour = m$cor)
  p <- .tr_view_painel(p, resumo, m$painel %||% "")
  .tr_view_acabar(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

# ---- Segundo lote ------------------------------------------------------------

#' Os níveis de uma coluna categórica, na ordem em que o eixo os mostra.
#'
#' Fator mantém os níveis que tem (com os vazios de fora); o resto sai em
#' ordem crescente, como o ggplot ordenaria. É a regra que a ajuda descreve
#' para "antes/depois": texto sai alfabético, e quem quer outra ordem converte
#' para fator.
#' @noRd
.tr_view_niveis <- function(v) {
  if (is.factor(v)) levels(droplevels(v)) else as.character(sort(unique(v[!is.na(v)])))
}

#' Recusa coluna de condição com menos valores do que o gráfico liga.
#' @noRd
.tr_view_min_niveis <- function(data, col, param, minimo) {
  n <- length(.tr_view_niveis(data[[col]]))
  if (n < minimo) {
    rlang::abort(
      sprintf("Param '%s': a coluna '%s' tem %d valor(es) distinto(s), e o gráfico liga ao menos %d.",
              param, col, n, minimo),
      class = "tr_view_error_levels")
  }
  col
}

#' Disperso com rótulos: o disperso que diz QUEM é cada ponto.
#'
#' `evitar` é o `check_overlap` do `geom_text`: um rótulo que colidiria com
#' outro já escrito é OMITIDO, e não empurrado. Empurrar é o que o `ggrepel`
#' faz, e ele não é dependência da coleção. Omitir é honesto se a ajuda disser
#' — o ponto continua lá, só sem nome —, e a ordem de escrita é a da tabela:
#' ordenar antes decide quem ganha o nome.
#' @export
tr_labels <- function(dados, x = "", y = "", rotulo = "", cor = "", evitar = TRUE,
                      log = "nenhum", painel = "", aspecto = "16:9", tema = "padrão",
                      titulo = "", rotulo_x = "", rotulo_y = "", legenda = "direita") {
  m <- .tr_view_aes(dados, x = x, y = y, rotulo = rotulo, cor = cor,
                    .param_obrigatorio = c("x", "y", "rotulo"))
  p <- ggplot2::ggplot(dados, .tr_view_mapa(x = m$x, y = m$y, colour = m$cor)) +
    ggplot2::geom_point(size = 2.4, alpha = .85) +
    ggplot2::geom_text(.tr_view_mapa(label = m$rotulo), vjust = -.9, size = 3.3,
                       check_overlap = isTRUE(evitar), show.legend = FALSE) +
    ggplot2::labs(x = m$x, y = m$y, colour = m$cor)
  p <- .tr_view_log2(p, dados, log, m)
  p <- .tr_view_painel(p, dados, painel)
  .tr_view_acabar(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

.TR_VIEW_ESTILOS_FAIXA <- c("colmeia", "espalhado")
.TR_VIEW_RESUMOS <- c("nenhum", "média", "mediana")

#' Deslocamento horizontal de cada ponto, sem sorteio.
#'
#' `colmeia` corta o eixo vertical em 60 fatias; dentro de cada fatia de cada
#' grupo, os pontos se alternam para a direita e para a esquerda do centro, e
#' a largura da faixa passa a desenhar a densidade — é o beeswarm sem pacote.
#' `espalhado` usa a sequência da razão áurea sobre a ordem das linhas: parece
#' sorteio, mas é a MESMA a cada execução, que é o que o cache da imagem pede
#' — e não mexe na semente global de quem chamou, como um `set.seed` mexeria.
#' O passo é o mesmo em todos os grupos, para que largura compare com largura.
#' @noRd
.tr_view_deslocar <- function(y, grupo, estilo) {
  off <- numeric(length(y))
  ok <- !is.na(y) & !is.na(grupo)
  if (!any(ok)) return(off)
  if (estilo == "espalhado") {
    off[ok] <- (((seq_len(sum(ok)) * 0.618034) %% 1) - .5) * .5
    return(off)
  }
  faixa <- diff(range(y[ok])); if (faixa == 0) faixa <- 1
  fatia <- floor((y - min(y[ok])) / (faixa / 60))
  chave <- interaction(grupo, fatia, drop = TRUE)
  idx <- split(which(ok), chave[ok], drop = TRUE)
  passo <- min(.06, .42 / max(1, ceiling(max(lengths(idx)) / 2)))
  for (ii in idx) {
    ii <- ii[order(y[ii])]
    k <- seq_along(ii)
    off[ii] <- ceiling((k - 1) / 2) * ifelse(k %% 2 == 0, 1, -1) * passo
  }
  off
}

#' Faixa de pontos: as observações cruas, uma por linha, por grupo.
#'
#' O eixo horizontal é NUMÉRICO por baixo (índice do grupo + deslocamento), com
#' os nomes dos grupos nos marcadores: é o que permite deslocar cada ponto sem
#' sorteio e desenhar o traço do resumo exatamente sobre o grupo. A coluna
#' `posicao_x` fica na tabela do objeto por isso.
#' @export
tr_strip <- function(dados, x = "", y = "", cor = "", estilo = "colmeia", resumo = "mediana",
                     log = FALSE, painel = "", aspecto = "16:9", tema = "padrão", titulo = "",
                     rotulo_x = "", rotulo_y = "", legenda = "direita") {
  m <- .tr_view_aes(dados, x = x, y = y, cor = cor, painel = painel, .param_obrigatorio = "y")
  estilo <- .tr_view_escolha(estilo, "estilo", .TR_VIEW_ESTILOS_FAIXA)
  resumo <- .tr_view_escolha(resumo, "resumo", .TR_VIEW_RESUMOS)
  .tr_view_numerica(dados, m$y, "y")
  dados <- as.data.frame(dados)
  grupo <- if (is.null(m$x)) rep("", nrow(dados)) else dados[[m$x]]
  niveis <- if (is.null(m$x)) "" else .tr_view_niveis(grupo)
  indice <- match(as.character(grupo), niveis)
  # Em log, a colmeia se forma na escala que o olho vê: fatias iguais em log.
  alvo <- if (isTRUE(log)) suppressWarnings(log10(dados[[m$y]])) else dados[[m$y]]
  dados$posicao_x <- indice + .tr_view_deslocar(alvo, indice, estilo)
  p <- ggplot2::ggplot(dados, .tr_view_mapa(x = "posicao_x", y = m$y, colour = m$cor)) +
    ggplot2::geom_point(size = 1.9, alpha = .8) +
    ggplot2::scale_x_continuous(breaks = seq_along(niveis), labels = niveis,
                                limits = c(.5, length(niveis) + .5)) +
    ggplot2::labs(x = m$x %||% "", y = m$y, colour = m$cor)
  if (resumo != "nenhum") {
    t <- trama::tr_theme(tema)
    f <- if (resumo == "média") mean else stats::median
    vals <- dados[[m$y]]
    dados$grupo_indice <- indice
    tab <- .tr_view_agregar(dados, c("grupo_indice", m$painel), function(i)
      list(resumo = f(vals[i], na.rm = TRUE)))
    tab$de <- tab$grupo_indice - .32
    tab$ate <- tab$grupo_indice + .32
    p <- p + ggplot2::geom_segment(
      data = tab, .tr_view_mapa(x = "de", xend = "ate", y = "resumo", yend = "resumo"),
      inherit.aes = FALSE, colour = t$texto, linewidth = 1.2)
  }
  if (isTRUE(log)) p <- .tr_view_log(p, dados, y = m$y)
  p <- .tr_view_painel(p, dados, m$painel %||% "")
  .tr_view_acabar(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

.TR_VIEW_ESTILOS_PONTOS <- c("pontos", "pirulito")

#' Pontos ordenados: um ponto por categoria, deitado, em ranking.
#'
#' Categoria no eixo VERTICAL sempre — o gráfico existe para dezenas de
#' categorias com nome, e é deitado que os nomes cabem. Por isso **Rótulo do
#' X** aqui é o do eixo horizontal, que é o do VALOR. A regra do valor é a das
#' barras: vazio conta, preenchido soma. O ponto, ao contrário da barra, não
#' precisa começar no zero — é a vantagem quando os valores são todos altos e
#' próximos; o `pirulito` devolve a haste até o zero para quem a quer.
#' @export
tr_dotplot <- function(dados, x = "", y = "", cor = "", estilo = "pontos", ordenar = TRUE,
                       painel = "", aspecto = "16:9", tema = "padrão", titulo = "",
                       rotulo_x = "", rotulo_y = "", legenda = "direita") {
  m <- .tr_view_aes(dados, x = x, y = y, cor = cor, painel = painel, .param_obrigatorio = "x")
  estilo <- .tr_view_escolha(estilo, "estilo", .TR_VIEW_ESTILOS_PONTOS)
  if (!is.null(m$y)) .tr_view_numerica(dados, m$y, "y")
  nome <- m$y %||% "contagem"
  vals <- if (is.null(m$y)) NULL else dados[[m$y]]
  tab <- .tr_view_agregar(dados, unique(c(m$x, m$cor, m$painel)), function(i) stats::setNames(
    list(if (is.null(vals)) length(i) else sum(vals[i], na.rm = TRUE)), nome))
  # Crescente nos níveis = maior EM CIMA, porque o eixo vertical desenha o
  # primeiro nível embaixo.
  niveis <- if (isTRUE(ordenar)) {
    total <- tapply(tab[[nome]], as.character(tab[[m$x]]), sum)
    names(sort(total))
  } else rev(.tr_view_niveis(tab[[m$x]]))
  tab[[m$x]] <- factor(as.character(tab[[m$x]]), levels = niveis)
  p <- ggplot2::ggplot(tab, .tr_view_mapa(x = nome, y = m$x, colour = m$cor))
  if (estilo == "pirulito") {
    p <- p + ggplot2::geom_segment(.tr_view_mapa(x = 0, xend = nome, yend = m$x),
                                   linewidth = .8, alpha = .8)
  }
  p <- p + ggplot2::geom_point(size = 3.4) +
    ggplot2::labs(x = nome, y = m$x, colour = m$cor)
  p <- .tr_view_painel(p, tab, m$painel %||% "")
  .tr_view_acabar(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

#' Halteres: duas condições por categoria, e a distância entre elas.
#'
#' A tabela vem LONGA (categoria, condição, valor), que é a forma em que ela
#' sai de um `pivot_longer` ou de um agrupar por duas chaves. Com três
#' condições ou mais, o segmento vai do menor ao maior valor e todos os pontos
#' aparecem; a diferença que ordena é sempre ÚLTIMA condição menos PRIMEIRA,
#' na ordem dos níveis — a do "depois − antes".
#' @export
tr_dumbbell <- function(dados, x = "", cor = "", y = "", ordenar = TRUE, aspecto = "16:9",
                        tema = "padrão", titulo = "", rotulo_x = "", rotulo_y = "",
                        legenda = "direita") {
  m <- .tr_view_aes(dados, x = x, cor = cor, y = y, .param_obrigatorio = c("x", "cor", "y"))
  .tr_view_numerica(dados, m$y, "y")
  .tr_view_min_niveis(dados, m$cor, "cor", 2L)
  vals <- dados[[m$y]]
  tab <- .tr_view_agregar(dados, c(m$x, m$cor), function(i)
    stats::setNames(list(sum(vals[i], na.rm = TRUE)), m$y))
  condicoes <- .tr_view_niveis(dados[[m$cor]])
  tab[[m$cor]] <- factor(as.character(tab[[m$cor]]), levels = condicoes)
  cats <- split(tab, as.character(tab[[m$x]]))
  seg <- data.frame(
    categoria = names(cats),
    de = vapply(cats, function(d) min(d[[m$y]]), 0),
    ate = vapply(cats, function(d) max(d[[m$y]]), 0),
    diferenca = vapply(cats, function(d) {
      a <- d[[m$y]][d[[m$cor]] == condicoes[[1]]]
      b <- d[[m$y]][d[[m$cor]] == condicoes[[length(condicoes)]]]
      if (length(a) && length(b)) b - a else NA_real_
    }, 0),
    stringsAsFactors = FALSE)
  niveis <- if (isTRUE(ordenar)) seg$categoria[order(seg$diferenca, na.last = FALSE)]
            else rev(.tr_view_niveis(dados[[m$x]]))
  tab[[m$x]] <- factor(as.character(tab[[m$x]]), levels = niveis)
  seg$categoria <- factor(seg$categoria, levels = niveis)
  t <- trama::tr_theme(tema)
  p <- ggplot2::ggplot(tab, .tr_view_mapa(x = m$y, y = m$x, colour = m$cor)) +
    ggplot2::geom_segment(data = seg, .tr_view_mapa(x = "de", xend = "ate", y = "categoria",
                                                    yend = "categoria"),
                          inherit.aes = FALSE, colour = t$eixos, linewidth = 1.6, alpha = .6) +
    ggplot2::geom_point(size = 3.6) +
    ggplot2::labs(x = m$y, y = m$x, colour = m$cor)
  .tr_view_acabar(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

#' Pareamento: a mesma unidade medida em cada condição, ligada por uma linha.
#'
#' Par unidade × condição REPETIDO é erro, e não média: pareado quer dizer uma
#' medida por unidade por condição, e repetição quase sempre é a coluna de
#' unidade errada (o lote no lugar da planta). Tirar a média calado desenharia
#' um pareamento que os dados não têm.
#' @export
tr_paired <- function(dados, x = "", y = "", unidade = "", cor = "", media = TRUE, painel = "",
                      aspecto = "16:9", tema = "padrão", titulo = "", rotulo_x = "",
                      rotulo_y = "", legenda = "direita") {
  m <- .tr_view_aes(dados, x = x, y = y, unidade = unidade, cor = cor, painel = painel,
                    .param_obrigatorio = c("x", "y", "unidade"))
  .tr_view_numerica(dados, m$y, "y")
  .tr_view_min_niveis(dados, m$x, "x", 2L)
  dados <- as.data.frame(dados)
  chave <- dados[unique(c(m$unidade, m$x, m$painel))]
  repetidos <- sum(duplicated(chave))
  if (repetidos > 0) {
    rlang::abort(
      sprintf("Param 'unidade': %d par(es) '%s' × '%s' aparecem mais de uma vez. Pareamento pede uma medida por unidade por condição; confira se '%s' identifica a unidade, ou resuma antes.",
              repetidos, m$unidade, m$x, m$unidade),
      class = "tr_view_error_not_unique")
  }
  dados[[m$x]] <- factor(as.character(dados[[m$x]]), levels = .tr_view_niveis(dados[[m$x]]))
  t <- trama::tr_theme(tema)
  individuais <- if (is.null(m$cor)) {
    ggplot2::geom_line(.tr_view_mapa(group = m$unidade), colour = t$eixos, alpha = .45, linewidth = .5)
  } else {
    ggplot2::geom_line(.tr_view_mapa(group = m$unidade), alpha = .35, linewidth = .5)
  }
  p <- ggplot2::ggplot(dados, .tr_view_mapa(x = m$x, y = m$y, colour = m$cor)) +
    individuais +
    (if (is.null(m$cor)) ggplot2::geom_point(colour = t$eixos, alpha = .6, size = 1.7)
     else ggplot2::geom_point(alpha = .5, size = 1.7)) +
    ggplot2::labs(x = m$x, y = m$y, colour = m$cor)
  if (isTRUE(media)) {
    vals <- dados[[m$y]]
    tab <- .tr_view_agregar(dados, unique(c(m$x, m$cor, m$painel)), function(i)
      stats::setNames(list(mean(vals[i], na.rm = TRUE)), m$y))
    grupo <- if (is.null(m$cor)) 1L else m$cor
    p <- p + if (is.null(m$cor)) {
      list(ggplot2::geom_line(data = tab, .tr_view_mapa(group = grupo), colour = t$paleta[[1]],
                              linewidth = 1.6),
           ggplot2::geom_point(data = tab, colour = t$paleta[[1]], size = 3.4))
    } else {
      list(ggplot2::geom_line(data = tab, .tr_view_mapa(group = grupo), linewidth = 1.6),
           ggplot2::geom_point(data = tab, size = 3.4))
    }
  }
  p <- .tr_view_painel(p, dados, m$painel %||% "")
  .tr_view_acabar(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}

#' Pareto: as categorias da maior para a menor, e quanto do total elas somam.
#'
#' A linha do acumulado é desenhada na MESMA unidade das barras (a soma
#' corrida), e o eixo da direita só a relê em porcentagem do total. Duas
#' escalas independentes, a do ggplot antigo, deixariam o 100% em qualquer
#' altura; aqui o 100% é exatamente a soma de todas as barras, e a referência
#' de 80% cai onde deve.
#' @export
tr_pareto <- function(dados, x = "", y = "", referencia = 80, aspecto = "16:9",
                      tema = "padrão", titulo = "", rotulo_x = "", rotulo_y = "",
                      legenda = "direita") {
  m <- .tr_view_aes(dados, x = x, y = y, .param_obrigatorio = "x")
  if (!is.null(m$y)) {
    .tr_view_numerica(dados, m$y, "y")
    if (any(dados[[m$y]] < 0, na.rm = TRUE)) {
      rlang::abort(sprintf("Param 'y': a coluna '%s' tem valores negativos, e o Pareto acumula parcelas de um total.",
                           m$y), class = "tr_view_error_not_positive")
    }
  }
  ref <- suppressWarnings(as.numeric(referencia)[[1]])
  if (is.na(ref) || ref < 0 || ref > 100) .tr_view_option("referencia", referencia, "número de 0 a 100")
  nome <- m$y %||% "contagem"
  vals <- if (is.null(m$y)) NULL else dados[[m$y]]
  tab <- .tr_view_agregar(dados, m$x, function(i) stats::setNames(
    list(if (is.null(vals)) length(i) else sum(vals[i], na.rm = TRUE)), nome))
  tab <- tab[order(-tab[[nome]]), , drop = FALSE]
  tab[[m$x]] <- factor(as.character(tab[[m$x]]), levels = as.character(tab[[m$x]]))
  total <- sum(tab[[nome]])
  tab$acumulado <- cumsum(tab[[nome]])
  tab$acumulado_pct <- tab$acumulado / total
  t <- trama::tr_theme(tema)
  p <- ggplot2::ggplot(tab, .tr_view_mapa(x = m$x, y = nome)) +
    ggplot2::geom_col(fill = t$paleta[[1]]) +
    ggplot2::geom_line(.tr_view_mapa(y = "acumulado", group = 1L), colour = t$texto, linewidth = .8) +
    ggplot2::geom_point(.tr_view_mapa(y = "acumulado"), colour = t$texto, size = 2.2) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, .04)),
      sec.axis = ggplot2::sec_axis(transform = ~ . / total, name = "acumulado",
                                   labels = scales::label_percent(decimal.mark = ","))) +
    ggplot2::labs(x = m$x, y = nome)
  if (ref > 0) {
    p <- p + ggplot2::geom_hline(yintercept = total * ref / 100, linetype = "dashed",
                                 colour = t$eixos, linewidth = .5)
  }
  .tr_view_acabar(p, aspecto, tema, titulo, rotulo_x, rotulo_y, legenda)
}
