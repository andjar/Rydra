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
#' # Load the example configuration file included with the package
#' config_path <- system.file("extdata", "example_config.yaml", package = "Rydra")
#'
#' # Fallback for development when package is not installed
#' if (config_path == "" && file.exists("inst/extdata/example_config.yaml")) {
#'   config_path <- "inst/extdata/example_config.yaml"
#' }
#'
#' if (file.exists(config_path)) {
#'   my_config <- load_config(config_path)
#'   # You can now inspect parts of the configuration
#'   # print(my_config$plgf_model$reference_paper)
#'   # print(names(my_config))
#' } else {
#'   print("Could not find example_config.yaml for load_config example.")
#' }
#' }
load_config <- function(file_path) {
  if (!file.exists(file_path)) {
    stop("Configuration file not found: ", file_path)
  }
  config <- yaml::read_yaml(file_path)
  return(config)
}

#' Get logging configuration
#'
#' Retrieves logging settings from the main configuration list.
#' Provides sensible defaults if settings are not specified.
#'
#' @param config The main configuration list, typically loaded from a YAML file via `load_config()`.
#' @return A list with logging configuration:
#'         \itemize{
#'           \item `enabled`: Logical, TRUE if logging is enabled, FALSE otherwise.
#'           \item `path`: Character, the directory path for storing logs.
#'                         Defaults to "rydra_logs" if enabled but no path is specified.
#'                         Is NULL if logging is disabled.
#'         }
#' @export
#' @examples
#' \dontrun{
#' # Example using a manually created config list
#' dummy_config_no_logging <- list(model_name = "test")
#' settings1 <- get_logging_config(dummy_config_no_logging)
#' print(paste("Logging enabled:", settings1$enabled)) # Should be FALSE
#'
#' dummy_config_with_logging <- list(
#'   model_name = "test",
#'   logging = list(enabled = TRUE, path = "my_app_logs")
#' )
#' settings2 <- get_logging_config(dummy_config_with_logging)
#' if (settings2$enabled) {
#'   print(paste("Logging enabled, path:", settings2$path))
#' }
#'
#' # Example with package's example_config.yaml
#' config_path <- system.file("extdata", "example_config.yaml", package = "Rydra")
#' if (config_path == "" && file.exists("inst/extdata/example_config.yaml")) {
#'   config_path <- "inst/extdata/example_config.yaml" # Fallback for dev
#' }
#'
#' if (file.exists(config_path)) {
#'   actual_config <- load_config(config_path)
#'   log_settings_from_file <- get_logging_config(actual_config)
#'   print(paste("Logging from file enabled:", log_settings_from_file$enabled))
#'   if (log_settings_from_file$enabled) {
#'     print(paste("Logging path from file:", log_settings_from_file$path))
#'   }
#' } else {
#'   print("Could not find example_config.yaml for get_logging_config example.")
#' }
#' }
get_logging_config <- function(config) {
  default_settings <- list(
    enabled = FALSE,
    path = NULL
  )

  if (!is.null(config$logging) && is.list(config$logging)) {
    logging_options <- config$logging

    # Check for 'enabled'
    if (!is.null(logging_options$enabled) && is.logical(logging_options$enabled)) {
      default_settings$enabled <- logging_options$enabled
    } else if (!is.null(logging_options$enabled)) {
      warning("Logging 'enabled' flag is not a logical value. Using default (FALSE).")
    }

    # If enabled, check for 'path'
    if (default_settings$enabled) {
      if (!is.null(logging_options$path) && is.character(logging_options$path) && nzchar(logging_options$path)) {
        default_settings$path <- logging_options$path
      } else {
        # Enabled but no path specified, or path is invalid
        if (!is.null(logging_options$path) && (!is.character(logging_options$path) || !nzchar(logging_options$path))) {
            warning("Logging 'path' is not a valid string or is empty. Using default ('rydra_logs/').")
        }
        default_settings$path <- "rydra_logs" # Default path if enabled and not specified
      }
    } else {
      # Logging is disabled, path should be NULL
      default_settings$path <- NULL
    }
  }
  # If config$logging is NULL or not a list, defaults are used (logging disabled)

  return(default_settings)
}
