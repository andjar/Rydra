#' Load a Rydra configuration file
#'
#' This function reads a YAML file and returns it as an R list.
#'
#' @param file_path Path to the YAML configuration file.
#' @return A list representing the parsed YAML configuration.
#' @export
#' @importFrom yaml read_yaml
#' @examples
#' \dontrun{
#'   config <- load_config("path/to/your/config.yaml")
#' }
load_config <- function(file_path) {
  if (!file.exists(file_path)) {
    stop("Configuration file not found: ", file_path)
  }
  config <- yaml::read_yaml(file_path)
  return(config)
}
