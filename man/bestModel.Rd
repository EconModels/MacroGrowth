% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Fitting.R
\name{bestModel}
\alias{bestModel}
\title{Extract the best model (least sse) from a list of models}
\usage{
bestModel(models, digits = 6, orderOnly = FALSE, constrained = FALSE)
}
\arguments{
\item{models}{the list of models}

\item{digits}{the number of digits of \code{sse} that is considered significant}

\item{orderOnly}{if \code{FALSE}, returns a reordered list of models.
If \code{TRUE}, returns an integer vector representing the order of the models.}
}
\value{
an integer vector representing the order of the models if \code{orderOnly} is \code{TRUE}.
A reordered list of models if \code{orderOnly} is \code{FALSE} (the default).
}
\description{
Extract the best model (least sse) from a list of models
}
\note{
The ordering process preserves the original order in the event of ties at the desired significance level.

This function relies upon the \code{sse} value being stored
in an attribute of the model called \code{naturalCoeffs}.
}
