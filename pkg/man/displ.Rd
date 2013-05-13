\docType{class}
\name{conpl}
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
\title{Distribution objects}
\arguments{
  \item{x}{the data vector}

  \item{xmin}{lower bound of the power-law distribution.
  For the continuous power-law, xmin >= 0 for the discrete
  distributions, xmin >0}

  \item{pars}{distribution parameters. For power-laws pars
  > 1. The lognormal distribution has two parameters.}
}
\value{
  a reference object
}
\description{
  The reference objects \code{displ}, \code{dispois},
  \code{dislnorm}, \code{conpl} and \code{conlnorm} are
  used for parameter inference. Each class also inherits
  the \code{discrete_distribution} or
  \code{ctn_distribution} class. These are the main classes
  of the poweRlaw package.
}
\examples{
data(moby)
}

