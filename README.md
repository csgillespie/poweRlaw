The poweRlaw package
====================
[![Build Status](https://travis-ci.org/csgillespie/poweRlaw.png?branch=master,dev)](https://travis-ci.org/csgillespie/poweRlaw)

This package implements both the discrete and continuous maximum likelihood estimators for fitting the power-law distribution to data. It also provides function to fit log-normal and Poisson distributions. Additionally, a goodness-of-fit based approach is used to estimate the lower cut-off for the scaling region. 

The code developed in this package was influenced from the python and R code found at [Aaron Clauset's website](http://tuvalu.santafe.edu/~aaronc/powerlaws/). In particular, the R code of Laurent Dubroca.

To cite this package in academic work, please use:

Colin S Gillespie (2013). *Fitting heavy tailed
  distributions: the poweRlaw package.* R package version
  **0.20.1**.


Installation
------------

This package is hosted on [CRAN](http://cran.r-project.org/web/packages/poweRlaw/index.html) and can be installed in the usual way:
```r
install.packages("poweRlaw")
```
Alternatively, the development version can be install from from github using the devtools package:
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
then work the through the two vignettes: [getting started](https://github.com/csgillespie/poweRlaw/blob/master/pkg/inst/doc/poweRlaw.pdf?raw=true) and [examples](https://github.com/csgillespie/poweRlaw/blob/master/pkg/inst/doc/examples.pdf?raw=true). Alternatively, you can access the vignettes from within the package:
```r
vignette("poweRlaw")
vignette("examples")
```
The plots below show the line of best fit to the Moby Dick and blackout data sets (from Clauset et al, 2009).


![Cumulative CDF of the Moby Dick and blackout data sets with line of best fit.](https://raw.github.com/csgillespie/poweRlaw/master/graphics/figure1.png)


Other information
-----------------

 * If you have any suggestions or find bugs, please use the github [issue tracker](https://github.com/csgillespie/poweRlaw/issues)
 * Feel free to submit pull requests



