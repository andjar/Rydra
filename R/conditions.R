## Internal helper for condition evaluation via full config
## Kept for backward compatibility under a new name to avoid clashing with
## the unified `apply_conditions(model_config, data)` API.
#' Apply conditions from full config using a provided model name
#'
#' @param config Parsed YAML configuration list
#' @param model_name Character model key within config
#' @param data List or data.frame used to evaluate conditions
#' @return Numeric sum of applicable conditional coefficients
#' @keywords internal
apply_conditions_with_config <- function(config, model_name, data) {
  if (is.null(config[[model_name]])) {
    stop(paste0("Model configuration '", model_name, "' not found in the config."))
  }
  apply_conditions(model_config = config[[model_name]], data = data)
}
