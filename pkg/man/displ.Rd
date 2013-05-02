\docType{class}
\name{conpl}
\alias{conpl}
\alias{conpl-class}
\alias{dislnorm}
\alias{dislnorm-class}
\alias{displ}
\alias{displ-class}
\alias{dispois}
\alias{dispois-class}
\alias{lnorm}
\alias{lnorm-class}
\title{Discrete and continuous power-law objects}
\arguments{
  \item{x}{the data vector}

  \item{xmin}{lower bound of the power-law distribution.
  For the continuous power-law, xmin >= 0 for the discrete
  distributions, xmin >0}

  \item{pars}{the scaling parameter: alpha > 1}
}
\value{
  a reference object
}
\description{
  The reference objects \code{displ} and \code{conpl} are
  used for parameter inference for discrete and continuous
  power-laws. Each class also inherits the
  \code{distribution} class. These are the main classes of
  the poweRlaw package.
}
\examples{
data(moby)
}

