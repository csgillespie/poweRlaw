The poweRlaw package
====================
[![Build Status](https://travis-ci.org/csgillespie/poweRlaw.png?branch=master,dev)](https://travis-ci.org/csgillespie/poweRlaw)

This package implements both the discrete and continuous maximum likelihood estimators for fitting the power-law distribution to data using the methods described in [Clauset et al, 2009](http://arxiv.org/abs/0706.1062). It also provides function to fit log-normal and Poisson distributions. Additionally, a goodness-of-fit based approach is used to estimate the lower cut-off for the scaling region. 

The code developed in this package was influenced from the python and R code found at [Aaron Clauset's website](http://tuvalu.santafe.edu/~aaronc/powerlaws/). In particular, the R code of Laurent Dubroca and Cosma Shalizi.

To cite this package in academic work, please use:

Gillespie, C. S. "*Fitting heavy tailed distributions: the poweRlaw package.*" Journal of Statistical Software, 64(2) 2015. ([arXiv preprint](http://arxiv.org/abs/1407.3492)).

Installation
------------

This package is hosted on [CRAN](http://cran.r-project.org/web/packages/poweRlaw/) and can be installed in the usual way:
```r
install.packages("poweRlaw")
```
Alternatively, the development version can be install from from github using the devtools package:
```r
install.packages("devtools")
devtools::install_github("poweRlaw", "csgillespie", subdir="pkg")
```

Note Windows users have to first install [Rtools](http://cran.rstudio.com/bin/windows/Rtools/).

Getting Started
---------------

To get started, load the package
```r
library("poweRlaw")
```
then work the through the four vignettes (links to the current CRAN version): 

 * [Getting started](http://cran.r-project.org/web/packages/poweRlaw/vignettes/poweRlaw.pdf)
 * [Worked examples](http://cran.r-project.org/web/packages/poweRlaw/vignettes/examples.pdf)
 * [Comparing distributions](http://cran.r-project.org/web/packages/poweRlaw/vignettes/compare_distributions.pdf)

Alternatively, you can access the vignettes from within the package:
```r
browseVignettes("poweRlaw")
```
The plots below show the line of best fit to the Moby Dick and blackout data sets (from Clauset et al, 2009).


![Cumulative CDF of the Moby Dick and blackout data sets with line of best fit.](https://raw.github.com/csgillespie/poweRlaw/master/graphics/figure1.png)


Other information
-----------------

 * If you have any suggestions or find bugs, please use the github [issue tracker](https://github.com/csgillespie/poweRlaw/issues)
 * Feel free to submit pull requests



