#' Classes de erro do trama.
#'
#' Todo erro do núcleo é classificado (`rlang::abort(class = "tr_error_*")`) e
#' viaja com mensagem acionável — é o que permite ao front escolher o que
#' mostrar e a um modelo corrigir o próprio documento sem ler código. Esta
#' tabela é a referência; um teste garante que toda classe usada no código
#' está aqui, então ela não envelhece em silêncio.
#' @return data.frame com `class` e `when`.
#' @examples
#' erros <- tr_errors()
#' nrow(erros)
#' erros[erros$class == "tr_error_unknown_node", ]
#'
#' # A classe viaja no condition, e e por ela que se captura.
#' reg <- tr_registry()
#' tr_use("trama", registry = reg)
#' tryCatch(tr_get_node("demo/inexistente", reg),
#'          tr_error_unknown_node = function(e) conditionMessage(e))
#' @export
tr_errors <- function() {
  e <- c(
    tr_error_bad_adapter = .tr_msg("errors.tr_error_bad_adapter"),
    tr_error_bad_asset = .tr_msg("errors.tr_error_bad_asset"),
    tr_error_bad_collection = .tr_msg("errors.tr_error_bad_collection"),
    tr_error_bad_format = .tr_msg("errors.tr_error_bad_format"),
    tr_error_bad_help = .tr_msg("errors.tr_error_bad_help"),
    tr_error_bad_icon = .tr_msg("errors.tr_error_bad_icon"),
    tr_error_bad_id = .tr_msg("errors.tr_error_bad_id"),
    tr_error_bad_name = .tr_msg("errors.tr_error_bad_name"),
    tr_error_bad_op = .tr_msg("errors.tr_error_bad_op"),
    tr_error_bad_output = .tr_msg("errors.tr_error_bad_output"),
    tr_error_bad_param = .tr_msg("errors.tr_error_bad_param"),
    tr_error_bad_param_value = .tr_msg("errors.tr_error_bad_param_value"),
    tr_error_bad_root = .tr_msg("errors.tr_error_bad_root"),
    tr_error_bad_sprite = .tr_msg("errors.tr_error_bad_sprite"),
    tr_error_bad_theme = .tr_msg("errors.tr_error_bad_theme"),
    tr_error_bad_type = .tr_msg("errors.tr_error_bad_type"),
    tr_error_cancelled = .tr_msg("errors.tr_error_cancelled"),
    tr_error_collection_not_dispatchable = .tr_msg("errors.tr_error_collection_not_dispatchable"),
    tr_error_cycle = .tr_msg("errors.tr_error_cycle"),
    tr_error_duplicate_collection = .tr_msg("errors.tr_error_duplicate_collection"),
    tr_error_duplicate_edge = .tr_msg("errors.tr_error_duplicate_edge"),
    tr_error_duplicate_id = .tr_msg("errors.tr_error_duplicate_id"),
    tr_error_failed_key = .tr_msg("errors.tr_error_failed_key"),
    tr_error_fn_not_function = .tr_msg("errors.tr_error_fn_not_function"),
    tr_error_foreign_id = .tr_msg("errors.tr_error_foreign_id"),
    tr_error_missing_collection = .tr_msg("errors.tr_error_missing_collection"),
    tr_error_missing_description = .tr_msg("errors.tr_error_missing_description"),
    tr_error_missing_fingerprint = .tr_msg("errors.tr_error_missing_fingerprint"),
    tr_error_missing_key = .tr_msg("errors.tr_error_missing_key"),
    tr_error_missing_message = .tr_msg("errors.tr_error_missing_message"),
    tr_error_missing_mirai = .tr_msg("errors.tr_error_missing_mirai"),
    tr_error_missing_object = .tr_msg("errors.tr_error_missing_object"),
    tr_error_name_collision = .tr_msg("errors.tr_error_name_collision"),
    tr_error_no_output = .tr_msg("errors.tr_error_no_output"),
    tr_error_not_project = .tr_msg("errors.tr_error_not_project"),
    tr_error_param_no_default = .tr_msg("errors.tr_error_param_no_default"),
    tr_error_param_shadow = .tr_msg("errors.tr_error_param_shadow"),
    tr_error_project_exists = .tr_msg("errors.tr_error_project_exists"),
    tr_error_project_write = .tr_msg("errors.tr_error_project_write"),
    tr_error_store_write = .tr_msg("errors.tr_error_store_write"),
    tr_error_type_mismatch = .tr_msg("errors.tr_error_type_mismatch"),
    tr_error_unknown_adapter = .tr_msg("errors.tr_error_unknown_adapter"),
    tr_error_unknown_edge = .tr_msg("errors.tr_error_unknown_edge"),
    tr_error_unknown_fn_arg = .tr_msg("errors.tr_error_unknown_fn_arg"),
    tr_error_unknown_frame = .tr_msg("errors.tr_error_unknown_frame"),
    tr_error_unknown_icon = .tr_msg("errors.tr_error_unknown_icon"),
    tr_error_unknown_node = .tr_msg("errors.tr_error_unknown_node"),
    tr_error_unknown_op = .tr_msg("errors.tr_error_unknown_op"),
    tr_error_unknown_param = .tr_msg("errors.tr_error_unknown_param"),
    tr_error_unknown_port = .tr_msg("errors.tr_error_unknown_port"),
    tr_error_unknown_type = .tr_msg("errors.tr_error_unknown_type"),
    tr_error_unnamed_ports = .tr_msg("errors.tr_error_unnamed_ports"),
    tr_error_worker_died = .tr_msg("errors.tr_error_worker_died")
  )
  data.frame(class = names(e), when = unname(e), stringsAsFactors = FALSE)
}
