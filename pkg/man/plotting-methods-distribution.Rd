\docType{methods}
\name{lines}
\alias{lines}
\alias{lines,distribution-method}
\alias{plot}
\alias{plot,distribution,ANY-method}
\alias{points}
\alias{points,distribution-method}
\title{Plotting functions}
\arguments{
  \item{plot}{logical (default \code{TRUE}). Should the
  plot function plot or return the data (in a data frame
  object).}

  \item{length.out}{numeric, default 1000. How many points
  should the distribution be evaulated at. This argument is
  only for plotting distribution objects}

  \item{cut}{logical (default \code{FALSE}) - Where should
  the plot begin. If \code{cut=FALSE}, then the plot will
  start at the minimum data value. Otherwise, the plot will
  start from \code{xmin}}
}
\description{
  These are generic functions for distribution reference
  objects. Standard plotting functions, i.e. plot, points,
  and lines work with all distribution objects.
}
\note{
  This method does *not* alter the internal state of the
  distribubtion objects.
}

