#' Temas de gráfico do projeto.
#'
#' Moram no `trama.json`, e não na máquina de quem usa: o mesmo documento tem
#' que renderizar igual em qualquer lugar, e o relatório tem que reproduzir. O
#' núcleo não sabe desenhar gráfico — ele só guarda, valida e resolve campos
#' declarativos; quem os transforma em ggplot é a coleção `view`.
#'
#' Os campos são fechados (lista abaixo) e cada um tem vocabulário pequeno:
#' tema entra no hash do plano, então tudo que ele carrega tem que ser dado
#' puro e comparável, nunca código.
#' @noRd
.TR_TEMA_CAMPOS <- list(base = "minimal", tamanho = 13, fonte = "sans",
  fundo = "#11151c", texto = "#c9d1d9", eixos = "#8b949e", grade = "#212936",
  paleta = c("#5b8def", "#f59e0b", "#10b981", "#ef4444", "#a855f7", "#06b6d4", "#f472b6", "#84cc16"),
  continua = "viridis")
.TR_TEMA_BASES     <- c("minimal", "bw", "classic")
.TR_TEMA_FONTES    <- c("sans", "serif", "mono")
.TR_TEMA_CONTINUAS <- c("viridis", "magma", "cividis", "azuis", "divergente")

#' Os três de sempre, no formato novo: projeto sem `temas` não muda de cara.
#' @noRd
.tr_temas_embutidos <- function() {
  claro_paleta <- c("#2563eb", "#d97706", "#059669", "#dc2626", "#7c3aed", "#0891b2", "#db2777", "#65a30d")
  sobre <- function(...) { t <- .TR_TEMA_CAMPOS; v <- list(...); t[names(v)] <- v; t }
  list(
    escuro = .TR_TEMA_CAMPOS,
    claro = sobre(fundo = "#ffffff", texto = "#1f2937", eixos = "#4b5563",
                  grade = "#e5e7eb", paleta = claro_paleta),
    `clássico` = sobre(base = "bw", fundo = "#ffffff", texto = "#000000",
                       eixos = "#333333", grade = "#d9d9d9", paleta = claro_paleta))
}

#' Erro nomeia tema, campo e o arquivo: quem edita o `trama.json` à mão
#' precisa saber ONDE mexer sem ler código — "Tema 'rel'" solto não diz que o
#' problema está no manifesto e não no documento.
#' @noRd
.tr_bad_theme <- function(nome, campo, esperado) {
  rlang::abort(.tr_msg("theme.bad_field", nome, campo, esperado),
               class = "tr_error_bad_theme")
}

#' Completa com os padrões e valida.
#'
#' Mescla campo a campo, e não com `utils::modifyList`: esta recursa em
#' listas, e `paleta = list()` sumiria na mescla deixando a paleta padrão no
#' lugar — o erro viraria silêncio. Campo desconhecido é rejeitado pelo mesmo
#' motivo: `fudno` ignorado calado é o pior jeito de falhar. JSON lido com
#' `simplifyVector = FALSE` traz `paleta` como lista e `tamanho` às vezes
#' inteiro: normaliza pra que o mesmo tema sempre gere o mesmo hash.
#' @noRd
.tr_theme_validate <- function(tema, nome) {
  if (!is.list(tema) || (length(tema) && is.null(names(tema)))) .tr_bad_theme(nome, "(tema)", "um objeto")
  extra <- setdiff(names(tema), names(.TR_TEMA_CAMPOS))
  if (length(extra)) .tr_bad_theme(nome, extra[[1]], .tr_msg("theme.field_values",
                                   paste(names(.TR_TEMA_CAMPOS), collapse = ", ")))
  t <- .TR_TEMA_CAMPOS
  for (cp in names(tema)) t[cp] <- list(tema[[cp]])
  if (is.list(t$paleta)) {
    if (!all(vapply(t$paleta, function(x) is.character(x) && length(x) == 1L, TRUE)))
      .tr_bad_theme(nome, "paleta", "uma ou mais cores #rrggbb")
    t$paleta <- as.character(unlist(t$paleta))
  }
  hex <- function(x) is.character(x) && length(x) >= 1L && !anyNA(x) && all(grepl("^#[0-9a-fA-F]{6}$", x))
  for (cp in c("fundo", "texto", "eixos", "grade"))
    if (!hex(t[[cp]]) || length(t[[cp]]) != 1L) .tr_bad_theme(nome, cp, "uma cor #rrggbb")
  if (!hex(t$paleta)) .tr_bad_theme(nome, "paleta", "uma ou mais cores #rrggbb")
  um_de <- function(x, ok) is.character(x) && length(x) == 1L && x %in% ok
  if (!um_de(t$base, .TR_TEMA_BASES)) .tr_bad_theme(nome, "base", paste(.TR_TEMA_BASES, collapse = ", "))
  if (!um_de(t$fonte, .TR_TEMA_FONTES)) .tr_bad_theme(nome, "fonte", paste(.TR_TEMA_FONTES, collapse = ", "))
  if (!um_de(t$continua, .TR_TEMA_CONTINUAS)) .tr_bad_theme(nome, "continua", paste(.TR_TEMA_CONTINUAS, collapse = ", "))
  if (!(is.numeric(t$tamanho) && length(t$tamanho) == 1L && !is.na(t$tamanho) && t$tamanho > 0))
    .tr_bad_theme(nome, "tamanho", .tr_msg("theme.expect_number"))
  t$tamanho <- as.numeric(t$tamanho)
  t[names(.TR_TEMA_CAMPOS)]
}

#' `marca` é booleano e mais nada.
#'
#' A recusa mora aqui, e não solta em cada chamador, porque são três: a leitura
#' do manifesto, o verbo que grava e o transporte — este precisa recusar o
#' pedido do painel ANTES de gravar os temas, já que os dois campos chegam no
#' mesmo gesto. `NA` cai junto com o resto: "talvez carimbe" não é resposta que
#' a exportação saiba usar.
#' @noRd
.tr_check_marca <- function(marca, onde = "") {
  if (!(is.logical(marca) && length(marca) == 1L && !is.na(marca)))
    rlang::abort(paste0("marca precisa ser true ou false", onde, "."),
                 class = "tr_error_bad_theme")
  marca
}

#' Settings do projeto a partir do manifesto lido.
#'
#' `temas` presente substitui os embutidos (não soma): o projeto que declara
#' seus temas quer controlar a lista que aparece no card. `"padrão"` é
#' reservado porque é o valor que o param guarda pra "siga o projeto".
#'
#' Nem tudo aqui é tema: `marca` decide se o PNG exportado sai carimbado. Mora
#' nos settings, e não numa preferência da máquina, pelo mesmo motivo dos
#' temas — a mesma pasta tem que exportar igual em qualquer lugar.
#' @noRd
.tr_settings <- function(cfg) {
  temas <- if (length(cfg$temas)) cfg$temas else .tr_temas_embutidos()
  if (!is.list(temas) || is.null(names(temas)) || any(!nzchar(names(temas))))
    .tr_bad_theme("?", "(nome)", "um nome")
  if ("padrão" %in% names(temas)) .tr_bad_theme("padrão", "(nome)", "um nome que não seja o reservado 'padrão'")
  temas <- stats::setNames(Map(.tr_theme_validate, temas, names(temas)), names(temas))
  padrao <- cfg$tema_padrao %||% if ("escuro" %in% names(temas)) "escuro" else names(temas)[[1]]
  # Mensagem própria: `tema_padrao` não é campo de um tema, e encaixá-lo no
  # molde "Tema 'x': campo 'y'" apontaria pra um tema que nem existe.
  if (!(is.character(padrao) && length(padrao) == 1L && padrao %in% names(temas)))
    rlang::abort(.tr_msg("theme.default_not_found", paste(padrao, collapse = ",")), class = "tr_error_bad_theme")
  # Ausente vale TRUE: projeto feito antes deste campo continua exportando com
  # a marca, que é o padrão anunciado.
  list(temas = temas, tema_padrao = padrao,
       marca = .tr_check_marca(cfg$marca %||% TRUE, " em trama.json"))
}

#' `"padrão"` ou nome -> definição. Nome que sumiu (tema apagado, documento
#' de outro projeto) cai no padrão marcado `ausente`: documento não quebra por
#' causa de cosmético, mas o card tem como avisar. Settings `NULL` (sessão sem
#' projeto) usa os embutidos.
#' @noRd
.tr_theme_resolve <- function(valor, settings) {
  s <- settings %||% .tr_settings(list())
  ok <- is.character(valor) && length(valor) == 1L && !is.na(valor)
  nome <- if (!ok || identical(valor, "padrão")) s$tema_padrao else valor
  ausente <- !nome %in% names(s$temas)
  if (ausente) nome <- s$tema_padrao
  # `nome` e `ausente` entram de propósito na lista (e portanto no hash): um
  # card em "padrão" e outro fixado no mesmo tema ganham chaves diferentes, e
  # nome sumido difere do padrão explícito. O custo é só um cache miss; em
  # troca, `fn` e o card recebem o nome e a flag pra avisar. Não "otimizar"
  # tirando esses campos.
  c(list(nome = nome), s$temas[[nome]], if (ausente) list(ausente = TRUE))
}

#' Reescreve o manifesto mexendo só no que `muda` mexeu.
#'
#' Lê o arquivo inteiro e devolve o arquivo inteiro: `tr_project()` não escreve
#' o manifesto de propósito, e nenhum verbo daqui pode virar a porta dos fundos
#' disso apagando chave que não é dele.
#'
#' Grava num temporário da MESMA pasta e renomeia, como `.tr_atomic` no store:
#' disco cheio no meio do `writeLines` deixaria um `trama.json` truncado, que é
#' pior que não gravar. `file.rename` devolve `FALSE` em vez de errar, por isso a
#' conferência. Os bytes saem por `writeBin` em UTF-8: o nome embutido
#' "clássico" não pode depender da locale de quem grava.
#'
#' `collections` vai com `I()` porque o JSON sai com `auto_unbox` (é o que
#' deixa `tema_padrao` e as cores escalares legíveis) e um vetor de um item
#' viraria string — a leitura aceita as duas formas, mas quem edita o arquivo à
#' mão veria dois formatos pra mesma coisa.
#'
#' `oque` entra na mensagem de falha ("os temas", "a marca"): quem não
#' conseguiu gravar precisa saber o que se perdeu, não só o caminho.
#' @noRd
.tr_cfg_rewrite <- function(cfg_path, oque, muda) {
  cfg <- muda(jsonlite::fromJSON(cfg_path, simplifyVector = FALSE))
  if (!is.null(cfg$collections)) cfg$collections <- I(as.character(unlist(cfg$collections)))
  json <- enc2utf8(as.character(jsonlite::toJSON(cfg, auto_unbox = TRUE, pretty = TRUE, digits = NA)))
  tmp <- tempfile(".trama-", tmpdir = dirname(cfg_path), fileext = ".part")
  on.exit(unlink(tmp), add = TRUE)
  ok <- tryCatch({
    writeBin(charToRaw(paste0(json, "\n")), tmp)
    file.rename(tmp, cfg_path)
  }, error = function(e) e)
  if (!isTRUE(ok)) {
    rlang::abort(.tr_msg("theme.write_failed", oque, cfg_path), class = "tr_error_project_write", parent = if (inherits(ok, "error")) ok)
  }
  invisible(NULL)
}

#' Grava os temas no manifesto — o único verbo que escreve `temas`.
#'
#' Valida ANTES de abrir o arquivo: manifesto com tema quebrado não abre o
#' projeto (`tr_project_at()` recusa), então gravar e validar depois deixaria o
#' usuário trancado fora do próprio projeto.
#'
#' `paleta` vai com `I()` pelo mesmo motivo de `collections` em
#' `.tr_cfg_rewrite()`: uma cor só não pode sair como string onde oito saem
#' como array.
#' `padrao` é obrigatório aqui, ao contrário da leitura: escolher o padrão
#' calado só serve pra manifesto antigo ou editado à mão. Quem GRAVA (o painel)
#' sabe qual escolheu, e mensagem sem o campo é bug do front — gravar "escuro"
#' ou o primeiro tema no lugar esconderia o bug.
#' @param root Pasta do projeto (precisa ter `trama.json`).
#' @param temas Lista nomeada de temas (campos como em `trama.json`). Lista
#'   vazia grava explicitamente os três temas embutidos (escuro, claro,
#'   clássico), e não um `temas` vazio.
#' @param padrao Nome do tema padrão (obrigatório): uma string não vazia que
#'   esteja em `temas`.
#' @return Os settings normalizados, invisível.
#' @export
tr_project_set_themes <- function(root, temas, padrao) {
  .tr_check_project(root)
  cfg_path <- file.path(normalizePath(root, mustWork = TRUE), "trama.json")
  if (!(is.character(padrao) && length(padrao) == 1L && !is.na(padrao) && nzchar(padrao)))
    rlang::abort(.tr_msg("theme.choose_default"),
                 class = "tr_error_bad_theme")
  settings <- .tr_settings(list(temas = temas, tema_padrao = padrao))
  .tr_cfg_rewrite(cfg_path, "os temas", function(cfg) {
    cfg$temas <- lapply(settings$temas, function(t) { t$paleta <- I(t$paleta); t })
    cfg$tema_padrao <- settings$tema_padrao
    cfg
  })
  # Relido do disco, e não o `settings` acima: aquele foi montado só com os
  # temas recebidos, então a `marca` dele vale sempre o padrão TRUE, e este
  # verbo devolveria "marca ligada" para um projeto que a tem desligada no
  # arquivo. O arquivo é que manda.
  invisible(.tr_settings_at(dirname(cfg_path)))
}

#' Liga ou desliga a marca d'água dos frames exportados.
#'
#' Verbo separado de `tr_project_set_themes()` porque a escolha é separada: o
#' painel salva as duas coisas no mesmo gesto, mas quem chama do console quer
#' desligar a marca sem ter que reescrever a lista de temas inteira para isso.
#'
#' Valida ANTES de abrir o arquivo, pelo mesmo motivo dos temas: valor que
#' `.tr_settings()` recusa tranca o projeto fora do editor. Mexe só na chave
#' `marca`; o resto do manifesto volta como estava.
#' @param root Pasta do projeto (precisa ter `trama.json`).
#' @param mostrar `TRUE` para carimbar o PNG exportado, `FALSE` para não
#'   carimbar. Chama-se `mostrar`, e não `marca`, porque é assim que se lê no
#'   chamador: `tr_project_set_marca(root, mostrar = FALSE)`.
#' @return Os settings do projeto já com a marca nova, invisível.
#' @export
tr_project_set_marca <- function(root, mostrar) {
  .tr_check_project(root)
  # UM root normalizado, usado pelas duas coisas que precisam dele — o caminho
  # do manifesto e a releitura do fim. Resolver o caminho por duas regras na
  # mesma função é como o arquivo lido deixa de ser o arquivo escrito quando o
  # `root` é relativo ou passa por um link.
  raiz <- normalizePath(root, mustWork = TRUE)
  cfg_path <- file.path(raiz, "trama.json")
  # `onde` nomeia o argumento porque do console a recusa chegaria solta:
  # "marca precisa ser true ou false." não diz o que foi recusado nem onde
  # mexer, enquanto as mensagens de tema sempre apontam o campo ou o arquivo.
  .tr_check_marca(mostrar, " no argumento 'mostrar'")
  .tr_cfg_rewrite(cfg_path, "a marca", function(cfg) { cfg$marca <- mostrar; cfg })
  # Relê o manifesto em vez de devolver só a marca: os temas gravados antes
  # fazem parte dos settings que quem chamou vai guardar, e devolver uma lista
  # montada aqui seria uma segunda verdade sobre o mesmo arquivo.
  invisible(.tr_settings_at(raiz))
}

#' Settings como estão NO ARQUIVO, e não como a sessão acha que estão.
#'
#' Existe para quem acabou de gravar (ou tentou gravar) e precisa devolver a
#' verdade do disco: depois de uma recusa no meio de duas escritas, o que vale
#' é o manifesto, não o que a sessão tinha em memória.
#' @noRd
.tr_settings_at <- function(root) {
  cfg_path <- file.path(root, "trama.json")
  .tr_settings(jsonlite::fromJSON(cfg_path, simplifyVector = FALSE))
}

#' Settings no formato que o front recebe.
#'
#' `sendCustomMessage` serializa com `auto_unbox`, e paleta de uma cor chegaria
#' como string — o painel teria que tratar dois formatos. `temas` é lista
#' nomeada, então sai sempre como objeto.
#' @noRd
.tr_settings_json <- function(settings) {
  list(temas = lapply(settings$temas, function(t) { t$paleta <- I(t$paleta); t }),
       tema_padrao = settings$tema_padrao,
       marca = settings$marca)
}

#' Resolve um tema pelo nome, fora de um projeto.
#'
#' Para o console e para coleções: `fn` de nó recebe a definição já resolvida
#' pelo plano, mas quem chama a função direto passa um nome ("claro"). Sem
#' projeto, valem os temas embutidos.
#'
#' Aceitar também a lista já resolvida é o que deixa a coleção ter UM caminho
#' só: ela chama `tr_theme()` sem saber de onde veio o valor. A lista é
#' revalidada porque no console qualquer um monta uma à mão, e `fudno` calado
#' seria o mesmo silêncio que o manifesto recusa. Nome desconhecido não é erro
#' aqui, pelo mesmo motivo do plano (documento não quebra por cosmético): volta
#' o padrão marcado `ausente`, e quem chama decide se isso é erro.
#' @param tema Nome de um tema, `"padrão"`, ou a definição já resolvida (lista).
#' @param settings Settings do projeto; `NULL` usa os temas embutidos.
#' @return Lista com `nome`, os campos do tema e, se o nome não existe,
#'   `ausente = TRUE`.
#' @export
tr_theme <- function(tema = "padrão", settings = NULL) {
  if (is.list(tema)) {
    campos <- tema[setdiff(names(tema), c("nome", "ausente"))]
    t <- .tr_theme_validate(campos, tema$nome %||% "?")
    return(c(list(nome = tema$nome %||% "?"), t, if (isTRUE(tema$ausente)) list(ausente = TRUE)))
  }
  .tr_theme_resolve(tema, settings)
}

#' Nomes dos temas disponíveis.
#'
#' Existe para a mensagem de erro de quem chama no console: "tema 'escurro'
#' não existe" só ajuda se disser quais existem, e a coleção não pode copiar a
#' lista dos embutidos — o projeto troca essa lista.
#' @param settings Settings do projeto; `NULL` usa os temas embutidos.
#' @return Vetor de nomes.
#' @export
tr_theme_names <- function(settings = NULL) names((settings %||% .tr_settings(list()))$temas)
