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
\title{Heavy-tailed distributions}
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
  The poweRlaw package supports a number of distributions:
  \describe{ \item{displ}{Discrete power-law}
  \item{dislnorm}{Discrete log-normal}
  \item{dispois}{Discrete Poisson} \item{conpl}{Continuous
  power-law} \item{conlnorm}{Continuous log-normal}
  \item{conexp}{Continuous exponential}} Each object
  inherits the \code{discrete_distribution} or
  \code{ctn_distribution} class.
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
m = displ$new(moby)
m$getXmin()
m$getPars()
#Set Xmin and parameter
m$setXmin(2)
m$setPars(2)
#Plot the data
plot(m)
#Add fitted distributions
lines(m)
}

