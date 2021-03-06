% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cesModel.R
\name{cesParseFormula}
\alias{cesParseFormula}
\title{Extract information from a CES formula}
\usage{
cesParseFormula(f, nest)
}
\arguments{
\item{f}{the CES fitting formula.}

\item{nest}{the nesting for the factors of production.}
}
\value{
a list of information extracted from the \code{f} and \code{nest}, including
\code{numFactors} (the number of factors of production),
\code{xNames} (a list containing the variable names of the factors of production,
\code{a}, \code{b}, \code{c}, and \code{d}),
\code{tName} (the variable name for time),
\code{yName} (the variable name for response),
\code{nestStr} (a string representing the nest, in the form of \code{k+l+e}),
\code{nestStrParen} (a string representing the nest, in the form of \code{(k + l) + (e)}), and
\code{cesNames} (a list of variable names in the order they appear in \code{f},
\code{response}, \code{xNames}, \code{tName}).
}
\description{
This function sets up names of parameters for a CES model fit in various formats.
In particular, it handles nesting and returns nest strings.
}
\note{
\code{f} is assumed to be in the form \code{response ~ a + b + c + d + time}
where \code{a}, \code{b}, \code{c}, and \code{d} are factors of production,
\code{response} is the response variable (typically economic output), and
\code{time} is the time variable.

\code{nest} is assumed to be integers in the form of \code{c(1,2,3,4)}.
Nest indicates the left-to-right order of parameters in the CES function.  If the length of \code{nest} is
less than the number of factors in the \code{formula}, some factors will omited.  If the length of \code{nest} is
greater than the nubmer of factors in \code{formula}, it is truncated to match.
}
