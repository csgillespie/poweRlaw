
<!-- README.md is generated from README.Rmd. Please edit that file -->

# The poweRlaw package

[![codecov.io](https://codecov.io/github/csgillespie/poweRlaw/coverage.svg?branch=master)](https://app.codecov.io/github/csgillespie/poweRlaw?branch=master)
[![Downloads](https://cranlogs.r-pkg.org/badges/poweRlaw?color=brightgreen)](https://cran.r-project.org/package=poweRlaw)
[![CRAN](https://www.r-pkg.org/badges/version/poweRlaw)](https://cran.r-project.org/package=poweRlaw)

This package implements both the discrete and continuous maximum
likelihood estimators for fitting the power-law distribution to data
using the methods described in [Clauset et al,
2009](http://arxiv.org/abs/0706.1062). It also provides function to fit
log-normal and Poisson distributions. Additionally, a goodness-of-fit
based approach is used to estimate the lower cut-off for the scaling
region.

The code developed in this package was influenced from the python and R
code found at Aaron Clauset’s website. In particular, the R code of
Laurent Dubroca and Cosma Shalizi.

To cite this package in academic work, please use:

Gillespie, C. S. “*Fitting heavy tailed distributions: the poweRlaw
package.*” Journal of Statistical Software, 64(2) 2015.
([pdf](https://doi.org/10.18637/jss.v064.i02)).

For a different way of handling powerlaw type distributions, see

Gillespie, C.S. ” *Estimating the number of casualties in the American
Indian war: a Bayesian analysis using the power law distribution.*”
Annals of Applied Statistics, 2017.
([pdf](https://doi.org/10.1214/17-AOAS1082))

## Installation

This package is hosted on
[CRAN](https://cran.r-project.org/package=poweRlaw) and can be installed
in the usual way:

``` r
install.packages("poweRlaw")
```

Alternatively, the development version can be install from from github
using the devtools package:

``` r
install.packages("devtools")
devtools::install_github("csgillespie/poweRlaw")
```

## Getting Started

To get started, load the package

``` r
library("poweRlaw")
```

then work through the four vignettes (links to the current CRAN
version):

- [Getting
  started](https://cran.r-project.org/package=poweRlaw/vignettes/a_introduction.pdf)
- [Worked
  examples](https://cran.r-project.org/package=poweRlaw/vignettes/b_powerlaw_examples.pdf)
- [Comparing
  distributions](https://cran.r-project.org/package=poweRlaw/vignettes/c_comparing_distributions.pdf)
- [JSS
  paper](https://cran.r-project.org/package=poweRlaw/vignettes/d_jss_paper.pdf)

Alternatively, you can access the vignettes from within the package:

``` r
browseVignettes("poweRlaw")
```

The plots below show the line of best fit to the Moby Dick and blackout
data sets (from Clauset et al, 2009). ![Cumulative CDF of the Moby Dick
and blackout data sets with line of best fit.](man/figures/figure1.png)

## Other information

- If you have any suggestions or find bugs, please use the github [issue
  tracker](https://github.com/csgillespie/poweRlaw/issues)
- Feel free to submit pull requests
- Data was originally obtained from Arron Clausett’s website. But this
  site is no longer available.

------------------------------------------------------------------------

Development of this package was supported by [Jumping
Rivers](https://www.jumpingrivers.com)
