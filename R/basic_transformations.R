#' Center a variable
#'
#' Subtracts a centering value from a variable.
#'
#' @param x The variable to center.
#' @param center The centering value.
#' @return The centered variable.
#' @export
#' @examples
#' center_variable(10, 5) # Returns 5
#' center_variable(c(1, 2, 3, 4, 5), 3) # Returns c(-2, -1, 0, 1, 2)
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
#' @examples
#' square_variable(5) # Returns 25
#' square_variable(c(1, 2, 3)) # Returns c(1, 4, 9)
square_variable <- function(x) {
  x^2
}

#' Log-transform a variable
#'
#' Log-transforms a variable.
#'
#' @param x The variable to log-transform.
#' @param base The base of the logarithm. Defaults to `exp(1)` (natural logarithm).
#' @return The log-transformed variable.
#' @export
#' @examples
#' log_transform(100, base = 10) # Returns 2
#' log_transform(exp(2)) # Returns 2 (natural log)
log_transform <- function(x, base = exp(1)) {
  log(x, base = base)
}

#' Exponential-transform a variable
#'
#' Exponentiates a variable.
#'
#' @param x The variable to exponentiate.
#' @param base The base of the exponentiation. Defaults to `exp(1)` (natural exponentiation).
#' @return The exponentiated variable.
#' @export
#' @examples
#' exp_transform(2, base = 10) # Returns 100
#' exp_transform(2) # Returns exp(2)
exp_transform <- function(x, base = exp(1)) {
  base ^ x
}

#' Multiply a variable by a multiplier
#'
#' Multiplies a variable by a specified multiplier.
#'
#' @param value The variable to multiply.
#' @param multiplier The multiplier value.
#' @return The variable multiplied by the multiplier.
#' @export
#' @examples
#' multiply_by(10, 5) # Returns 50
multiply_by <- function(value, multiplier) {
  value * multiplier
}

#' Add a term to a variable
#'
#' Adds a specified term to a variable.
#'
#' @param value The variable to which the term is added.
#' @param term The term to add.
#' @return The variable with the term added.
#' @export
#' @examples
#' add_value(10, 5) # Returns 15
add_value <- function(value, term) {
  value + term
}

#' Truncate a variable
#'
#' Truncates a variable to a specified minimum and maximum range.
#' Values less than `min_val` are set to `min_val`.
#' Values greater than `max_val` are set to `max_val`.
#'
#' @param x The variable to truncate.
#' @param min_val The minimum value for truncation.
#' @param max_val The maximum value for truncation.
#' @return The truncated variable.
#' @export
#' @examples
#' truncate_variable(c(1, 5, 10, 15, 20), 5, 15) # Returns c(5, 5, 10, 15, 15)
#' truncate_variable(0, 1, 10) # Returns 1
#' truncate_variable(100, 1, 10) # Returns 10
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
