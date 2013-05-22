\docType{class}
\name{conpl}
\alias{conexp}
\alias{conexp-class}
\alias{conlnorm}
\alias{conlnorm-class}
\alias{conpl}
\alias{conpl-class}
\alias{dislnorm}
\alias{dislnorm-class}
\alias{displ}
\alias{displ-class}
\alias{dispois}
\alias{dispois-class}
\title{Classes for distribution}
\usage{
  conpl(...)

  conlnorm(...)

  conexp(...)

  displ(...)

  dislnorm(...)

  dispois(...)
}
\arguments{
  \item{...}{The object is typically created by passing
  data using the \code{dat} field. Each field has standard
  setters and getters.}
}
\value{
  a reference object
}
\description{
  The reference class objects \code{displ}, \code{dispois},
  \code{dislnorm}, \code{conpl} and \code{conlnorm} are
  used for parameter inference.
}
\details{
  Each class also inherits the \code{discrete_distribution}
  or \code{ctn_distribution} class.
}
\section{Fields}{
  Each distribution object has four fields. However, the
  object is typically created by passing data, using the
  \code{dat} field. Each field has standard setters and
  getters. \describe{ \item{dat}{The data set.}
  \item{xmin}{The lower threshold, xmin. Typically set
  after initialisation. For the continuous power-law, xmin
  >= 0 for the discrete distributions, xmin >0}
  \item{pars}{A parameter vector. Typically set after
  initialisation. For power-laws pars > 1. The lognormal
  distribution has two parameters.} \item{internal}{A list.
  This list differs between objects and shouldn't be
  altered.}}
}
\examples{
data(moby)
}

