% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/factors.R
\name{apply_conditions}
\alias{apply_conditions}
\title{Apply conditional coefficients}
\usage{
apply_conditions(model_config, data)
}
\arguments{
\item{model_config}{The specific model configuration list (e.g., \code{config[[model_name]]})
containing condition definitions.}

\item{data}{A list containing the input data, typically after transformations.
Condition formulas are evaluated against this data.}
}
\value{
A numeric value representing the sum of coefficients from all met conditions.
}
\description{
This function processes the 'conditions' block from the configuration.
For each defined condition, it evaluates the boolean expression in the context
of the input data. If true, it resolves the specified coefficient path within
the model's configuration and adds it to a running total.
}
\keyword{internal}
