#' Classes de erro do trama.
#'
#' Todo erro do núcleo é classificado (`rlang::abort(class = "tr_error_*")`) e
#' viaja com mensagem acionável — é o que permite ao front escolher o que
#' mostrar e a um modelo corrigir o próprio documento sem ler código. Esta
#' tabela é a referência; um teste garante que toda classe usada no código
#' está aqui, então ela não envelhece em silêncio.
#' @return data.frame com `class` e `when`.
#' @export
tr_errors <- function() {
  e <- c(
    tr_error_bad_adapter = "tr_adapter() sem função",
    tr_error_bad_asset = "'js' ou 'css' de tr_collection() não é caminho relativo único",
    tr_error_bad_collection = "objeto não é tr_collection",
    tr_error_bad_format = "documento em formato não suportado",
    tr_error_bad_help = "'help' de tr_node() não é uma string única",
    tr_error_bad_icon = "tr_icon() sem 'name' nem 'svg', com os dois, ou com valor que não é string única",
    tr_error_bad_id = "id de tipo, nó ou coleção fora do formato 'colecao/nome'",
    tr_error_bad_init = "formal de 'init' de tr_node() que não é param do nó",
    tr_error_bad_name = "nome de projeto vazio, com separador de caminho, ou que não é uma string única",
    tr_error_bad_op = "op malformada, ou valor de op inválido (seed, posição)",
    tr_error_bad_output = "fn não devolveu todas as portas de saída declaradas",
    tr_error_bad_param = "param de tr_node() não veio de tr_param()",
    tr_error_bad_param_value = "valor de param incompatível com o kind declarado",
    tr_error_bad_root = "abertura de projeto com argumento inválido: caminho que não existe ou não é diretório, ou registry que não é tr_registry",
    tr_error_bad_sprite = "sprite de ícones existe mas nenhum <symbol id> foi encontrado nele",
    tr_error_bad_step = paste0(
      "'step' mal declarado em tr_node() (sem 'state' como primeiro formal, ou com formal ",
      "sem input/param), ou 'step' que num passo da região de fluxo devolveu outra coisa ",
      "que não list(state = , out = )"),
    tr_error_bad_theme = "configuração do trama.json inválida: tema com campo desconhecido, valor fora do vocabulário ou nome reservado, tema_padrao inexistente, marca que não é booleana",
    tr_error_bad_type = "tr_type() mal declarado (store sem restore, etc.)",
    tr_error_cancelled = "unidade cancelada pelo coordenador (handoff, ou stop_mirai no pool)",
    tr_error_collection_not_dispatchable = "coleção sem pacote não pode ir para daemon",
    tr_error_cycle = "aresta fecharia um ciclo",
    tr_error_duplicate_collection = "coleção já carregada neste registro",
    tr_error_duplicate_edge = "aresta já existe",
    tr_error_duplicate_id = "id já existe (registro ou documento)",
    tr_error_failed_key = "chave guarda um erro, não um valor",
    tr_error_fn_not_function = "fn de tr_node() não é função",
    tr_error_foreign_id = "coleção declarou id fora do próprio namespace",
    tr_error_incomplete_online = "tr_node() com 'init' sem 'step' (ou vice-versa), ou com um dos dois não sendo função",
    tr_error_missing_collection =
      "projeto pede coleção que não está carregada nesta página do editor",
    tr_error_missing_description = "tr_node() sem 'description'",
    tr_error_missing_fingerprint = "nó impuro sem fingerprint",
    tr_error_missing_key = "chave ausente no store",
    tr_error_missing_mirai = "tr_executor_pool() sem o pacote mirai",
    tr_error_missing_object = "handle sem objeto no store",
    tr_error_name_collision = "mesmo nome usado como input e param",
    tr_error_no_output = "nó sem porta de saída consultado por tr_value()",
    tr_error_not_liftable = "nó impuro, volátil ou que pede '.ctx' dentro de uma região de fluxo",
    tr_error_not_project = "pedido de ABRIR apontado para pasta sem trama.json",
    tr_error_online_without_stream = "nó declara 'step' mas nenhuma entrada de fluxo",
    tr_error_online_without_stream_output =
      "nó com 'init'/'step' cuja saída não é fluxo: o motor o leria como o colapso da região",
    tr_error_param_no_default = "tr_param() sem default",
    tr_error_param_shadow =
      "param de nó com nome de argumento de tr_add() REALMENTE passado (from, label, seed, position)",
    tr_error_project_exists = "tr_project_new() apontado para pasta que já tem trama.json",
    tr_error_project_write = "falha ao criar as pastas ou o manifesto de um projeto (disco, permissão)",
    tr_error_retry_good_artifact = "tr_retry() pedido sobre porta de saída que guarda artefato bom, não erro",
    tr_error_store_write = "falha de escrita no store (disco, permissão)",
    tr_error_stream_bad_command = "comando de região de fluxo que não é play, pause, step, tempo nem stop",
    tr_error_stream_bad_source = "fonte de região de fluxo que não devolveu a lista de pontos",
    tr_error_stream_collapse = "colapso de região de fluxo falhou ao montar o histórico",
    tr_error_stream_escapes = "saída comum de nó interior de região de fluxo consumida fora dela",
    tr_error_stream_multi_source = "região de fluxo com mais de uma fonte",
    tr_error_stream_not_collected = "região de fluxo sem nó de colapso",
    tr_error_stream_step = "nó de região de fluxo falhou num passo do laço",
    tr_error_stream_stopped = paste0(
      "região de fluxo PARADA por comando entre passos — não é falha: o checkpoint fica, ",
      "a chave de saída continua vazia e o scheduler trata como cancelamento"),
    tr_error_type_mismatch = "tipos de porta incompatíveis e sem adaptador",
    tr_error_unknown_adapter = "adaptador da aresta não registrado no worker",
    tr_error_unknown_edge = "disconnect de aresta que não existe",
    tr_error_unknown_fn_arg = "argumento de fn sem input/param correspondente",
    tr_error_unknown_frame = "op de frame com id de frame inexistente",
    tr_error_unknown_icon = "nome de ícone que não existe no sprite do Lucide",
    tr_error_unknown_node = "tipo de nó não registrado, ou id de instância inexistente",
    tr_error_unknown_op = "op não reconhecida",
    tr_error_unknown_param = "param não declarado no tipo de nó",
    tr_error_unknown_port = "porta inexistente no tipo de nó",
    tr_error_unknown_type = "tipo de dado não registrado",
    tr_error_unnamed_ports = "outputs de tr_node() sem nome",
    tr_error_worker_died = "daemon morreu (ou foi morto) durante a unidade"
  )
  data.frame(class = names(e), when = unname(e), stringsAsFactors = FALSE)
}
