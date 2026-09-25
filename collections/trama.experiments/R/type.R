# O tipo `experiments/plan` e o adaptador que o liga à `data`.
#
# ============================================================================
# CONTRATO DO PLANO (leia antes de construir effect / error / power /
# randomization_test sobre ele)
# ============================================================================
#
# Um plano é uma lista de classe `tr_experiments_plan` com os campos de
# `.TR_EXP_CAMPOS_PLANO`:
#
# - `unidades`: tibble, UMA LINHA POR UNIDADE DE OBSERVAÇÃO (a unidade mais
#   fina: a subparcela no split-plot, a medida no tempo nas medidas repetidas,
#   o período no crossover), já NA ORDEM DE EXECUÇÃO. Colunas:
#     * `unidade` (int 1..N) e `ordem` (int, ordem de execução; hoje igual a
#       `unidade`, porque as linhas já saem ordenadas);
#     * as colunas ESTRUTURAIS que a estrutura usa, sempre com estes nomes
#       (fator do R, níveis "1", "2", ...): `bloco`, `repeticao`, `parcela`
#       (id GLOBAL da parcela, único no experimento), `subparcela`, `linha`,
#       `coluna` (as do quadrado latino), `faixa_linha`, `faixa_coluna`,
#       `individuo` (id global), `sequencia`, `periodo`, `tempo` (níveis na
#       ordem declarada), `local`, `padrao` (ordem padrão/Yates no fracionado
#       e no composto central), `tipo_ponto` (fatorial/axial/central);
#     * uma coluna por FATOR declarado, com o nome dado pelo usuário: fator do
#       R com os níveis na ordem declarada, EXCETO no fracionado e no composto
#       central, em que é numérica na escala codificada (−1/+1, 0, ±α);
#     * uma coluna numérica `NA` por covariável observada declarada.
#   Esta tabela é o que o adaptador `experiments/plan -> data/table` entrega:
#   qualquer nó de `trama.models` a consome sem contrato novo.
# - `estrutura`: id da estrutura (`dic`, `dbc`, `dql`, `fatorial`,
#   `confundimento`, `fracionado`, `composto_central`, `parcela_subdividida`,
#   `faixas`, `bib`, `medidas_repetidas`, `crossover`, `grupos`).
# - `rotulo`: frase curta ("DBC · 4 tratamentos × 5 blocos").
# - `fatores`: data.frame, uma linha por fator (tratamento, bloco, tempo,
#   covariável...), colunas `nome`, `papel` (tratamento, bloco, agrupamento,
#   tempo, covariavel), `niveis` (coluna-lista), `unidade` (nível da hierarquia
#   em que é aplicado), `escopo` (entre quais unidades é sorteado),
#   `mecanismo` (livre, restrito, em estágios, sem sorteio) e `justificativa`
#   (por que não há sorteio, quando não há).
# - `hierarquia`: data.frame do nível mais alto ao mais fino, colunas `nivel`,
#   `coluna` (a coluna de `unidades` que o identifica, ou NA), `dentro_de` e
#   `n` (quantas unidades desse nível há no experimento).
# - `geometria`: lista com `posicoes` (data.frame `unidade`, `linha`,
#   `coluna`: a posição de cada unidade na grade 2D), `eixo_linha`,
#   `eixo_coluna` (o que as linhas e colunas da grade representam) e `tipo`
#   ("campo", "tempo" ou "sequencia").
# - `analise`: lista `no` (id do nó de análise sugerido) e `params` (os params
#   dele, SEM `resposta`); quando o nó é `models/lm` ou `models/lmer`,
#   `params$formula` é o lado DIREITO ("~ bloco + trat"), para quem gerar a
#   resposta prefixar o nome dela.
# - `extras`: o que só uma estrutura tem (resolução e relação de definição do
#   fracionado, α do composto central, λ do BIB, sequências do crossover,
#   efeitos confundidos...).
# - `receita`: os argumentos com que `tr_experiments_design()` foi chamado —
#   `tr_experiments_randomize(plano, .seed)` re-sorteia a partir dela.
# - `semente`, `metodo` (o gerador), `versao` (da coleção): reprodutibilidade.
# - `avisos`: character, o que não bloqueia mas merece leitura; `nota`: texto.
#
# Decisão de forma (comparada com `trama.sampling`): a amostra de lá guarda
# `dados` + `desenho` (estrato/psu/fpc) + `receita` para re-sortear. A ideia
# de `receita` foi reaproveitada — o teste de aleatorização re-sorteia pelo
# mesmo caminho. O `desenho` de lá NÃO serve: é um vetor por linha pensado
# para o estimador do conglomerado último, e aqui o que importa é a hierarquia
# de unidades e, por fator, o escopo do sorteio. Por isso `fatores` e
# `hierarquia` são tabelas próprias. Como lá, o que é do desenho (a posição na
# grade) fica FORA da tabela, para o adaptador não despejar colunas internas.
# ============================================================================

.TR_EXP_CAMPOS_PLANO <- c("unidades", "estrutura", "rotulo", "fatores", "hierarquia", "geometria",
                          "analise", "extras", "receita", "semente", "metodo", "versao", "avisos", "nota")

#' Confere que `x` é um plano; é o `store` do tipo que o chama, como funil.
#' @noRd
.tr_exp_plano_conferir <- function(x) {
  if (!inherits(x, "tr_experiments_plan") || !all(.TR_EXP_CAMPOS_PLANO %in% names(x))) {
    .tr_experiments_abort("tr_experiments_error_not_a_plan",
                          "O objeto não é um plano de experimento (faltam campos: %s).",
                          paste(setdiff(.TR_EXP_CAMPOS_PLANO, names(x)), collapse = ", "))
  }
  invisible(x)
}

experiments_plan_type <- function() {
  trama::tr_type(
    "experiments/plan", version = 1L, label = "Plano de experimento", color = .TR_EXP_COR, ext = "rds",
    store = function(x, path) {
      .tr_exp_plano_conferir(x)
      saveRDS(x, path, compress = FALSE)
    },
    restore = function(path) readRDS(path),
    summary = function(x) list(estrutura = x$estrutura, rotulo = x$rotulo, unidades = nrow(x$unidades),
                               semente = x$semente, avisos = length(x$avisos)),
    # O card é o MAPA: "onde caiu cada tratamento" se lê sem abrir nada.
    preview = function(x, ctx) trama.view::tr_view_render(tr_experiments_view(x, "mapa"), ctx)
  )
}

.tr_experiments_adapters <- function() {
  list(trama::tr_adapter("experiments/plan", "data/table", function(x) x$unidades))
}
