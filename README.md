The poweRlaw package
====================

This package implements both the discrete and continuous maximum likelihood estimators for fitting the power-law distribution to data. Additionally, a goodness-of-fit based approach is used to estimate the lower cut-off for the scaling region. 

The code developed in this package has been heavily influenced from the python and R code found at [Aaron Clauset's website](http://tuvalu.santafe.edu/~aaronc/powerlaws/). In particular, the R code of Laurent Dubroca.

Installation
------------

Currently, this package can only be installed from github and requires R >=2-15.0. The easiest way to install from github is to use the devtools package:
```r
install.packages("devtools")
library(devtools)
install_github("poweRlaw", "csgillespie", subdir="pkg")
```

Note Windows users have to first install [Rtools](http://cran.rstudio.com/bin/windows/Rtools/).

Getting Started
---------------

To get started, load the package
```r
library(poweRlaw)
```
then work the through [vignette](https://github.com/csgillespie/poweRlaw/blob/master/pkg/inst/doc/poweRlaw.pdf?raw=true) examples
```r
vignette("poweRlaw")
```
The plots below, show the line of best fit to the Moby Dick and blackout data sets (from Clauset et al, 2009.)


![Cumulative CDF of the Moby Dick and blackout data sets with line of best fit.](https://raw.github.com/csgillespie/poweRlaw/master/graphics/figure1.png)

