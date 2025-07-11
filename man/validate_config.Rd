% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validator.R
\name{validate_config}
\alias{validate_config}
\title{Validate a Rydra configuration}
\usage{
validate_config(config, model_name, data)
}
\arguments{
\item{config}{The full configuration list to validate, typically loaded from a YAML file.}

\item{model_name}{The name of the specific model block within the \code{config} to validate.}

\item{data}{A list or data frame representing the input data. Used to validate factor
levels against the data present in this parameter.}
}
\value{
Invisible \code{TRUE} if validation succeeds, otherwise stops with an error.
}
\description{
This function checks if the parsed YAML configuration list is valid.
It collects all errors and stops if any are found, reporting them all at once.
}
\keyword{internal}
