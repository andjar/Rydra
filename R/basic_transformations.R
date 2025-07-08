#' Center a variable
#'
#' Subtracts a centering value from a variable.
#'
#' @param x The variable to center.
#' @param center The centering value.
#' @return The centered variable.
#' @export
center_variable <- function(x, center) {
  x - center
}

#' Square a variable
#'
#' Squares a variable.
#'
#' @param x The variable to square.
#' @return The squared variable.
#' @export
square_variable <- function(x) {
  x^2
}

#' Log-transform a variable
#'
#' Log-transforms a variable.
#'
#' @param x The variable to log-transform.
#' @param base The base of the logarithm.
#' @return The log-transformed variable.
#' @export
log_transform <- function(x, base = exp(1)) {
  log(x, base = base)
}

#' Exponential-transform a variable
#'
#' Exponentiates a variable.
#'
#' @param x The variable to exponentiate.
#' @param base The base of the exponentiation (default: exp(1)).
#' @return The exponentiated variable.
#' @export
exp_transform <- function(x, base = exp(1)) {
  base ^ x
}

#' Truncate a variable
#'
#' Truncates a variable to a specified minimum and maximum range.
#' Values less than min_val are set to min_val.
#' Values greater than max_val are set to max_val.
#'
#' @param x The variable to truncate.
#' @param min_val The minimum value.
#' @param max_val The maximum value.
#' @return The truncated variable.
#' @export
truncate_variable <- function(x, min_val, max_val) {
  # Ensure min_val and max_val are single values if provided
  if (length(min_val) > 1 || length(max_val) > 1) {
    stop("min_val and max_val must be single numeric values.")
  }

  # Handle cases where min_val or max_val might be NA or Inf
  # If min_val is NA or -Inf, no lower truncation
  # If max_val is NA or Inf, no upper truncation

  x_truncated <- x

  if (!is.na(min_val) && is.finite(min_val)) {
    x_truncated[!is.na(x_truncated) & x_truncated < min_val] <- min_val
  }

  if (!is.na(max_val) && is.finite(max_val)) {
    x_truncated[!is.na(x_truncated) & x_truncated > max_val] <- max_val
  }

  return(x_truncated)
}
