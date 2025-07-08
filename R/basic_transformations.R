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

#' Multiply a variable by a multiplier
#'
#' Multiplies a variable by a specified multiplier.
#'
#' @param value The variable to multiply.
#' @param multiplier The multiplier.
#' @return The variable multiplied by the multiplier.
#' @export
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
add_value <- function(value, term) {
  value + term
}
