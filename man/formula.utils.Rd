% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/formulaUtils.R
\name{build_formula}
\alias{build_formula}
\alias{keep_summands}
\alias{summands}
\alias{replace_summand}
\title{Utilities for working with formulas}
\usage{
build_formula(lhs, rhs, env = parent.frame(), op = "+")

keep_summands(formula, n, right = TRUE, env = environment(formula),
  op = "+")

summands(formula, n, right = TRUE, op = "+")

replace_summand(formula, n, term, right = TRUE, op = "+",
  env = environment(formula))
}
\arguments{
\item{lhs, rhs}{list or vector of character strings containing summands for left and right hand sides
of a formula.  \code{NULL}s are ignored.}

\item{env}{an environment to attach to the formula}

\item{op}{The operator to use for splitting into terms.}

\item{formula}{a formula}

\item{n}{a vector of integer indices.  Negatives can be used to deselect.}

\item{right}{a logical indicating whether to operate on the right side (else the left) of
the formula.}

\item{term}{a character string containing a new term}
}
\description{
Some functions to make it easier to build and minipulate formulas
}
\examples{
a <- build_formula(c("a", "b"), c("x", "y", "log(x*y)")); a
b <- build_formula("response", list("x", "y", NULL, "log(x)"), op="*"); b
f <- build_formula( "iGDP", list("iK", "iL", "iQp", "iYear")); f
g <- build_formula( "iGDP", c("iK", "iL", "iQp", "iYear")); g
identical(f,g)
summands(a)
summands(a, right=FALSE)
summands(b, op="*")
keep_summands(f, 3)
keep_summands(f, -3)
keep_summands(f, 2:4)
h <- replace_summand( f, 3, paste(summands(f, 3), lhs(f), sep="/")); h
}
