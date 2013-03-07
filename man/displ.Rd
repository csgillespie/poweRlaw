\docType{class}
\name{displ}
\alias{conpl}
\alias{conpl-class}
\alias{displ}
\alias{displ-class}
\title{Maximum likelihood estimation of the discrete power law distribution.}
\arguments{
  \item{x}{data vector}

  \item{xmin}{Lower bound of the power-law distribution.
  For the continuous power-law, xmin >= 0 for the discrete
  distribution, xmin >0}

  \item{alpha}{The scaling parameter: alpha > 1}
}
\value{
  the mle estimate of the parameter alpha (for fixed xmin).
}
\description{
  Maximum likelihood estimation of the discrete power law
  distribution.
}
\examples{
data(moby)
}

