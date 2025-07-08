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

#' Get logging configuration
#'
#' Retrieves logging settings from the main configuration list.
#' Provides sensible defaults if settings are not specified.
#'
#' @param config The main configuration list (loaded from YAML).
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
#'   main_config <- load_config("path/to/config.yaml")
#'   log_settings <- get_logging_config(main_config)
#'   if (log_settings$enabled) {
#'     print(paste("Logging to:", log_settings$path))
#'   }
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
