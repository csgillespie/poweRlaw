The poweRlaw package
====================

This package implements both the discrete (and in the near future continuous) maximum likelihood estimators for fitting the power-law distribution to data. Additionally, a goodness-of-fit based approach is used to estimate the lower cut-off for the scaling region. 

The code developed in this package has been heavily influenced from the python and R code found at: http://tuvalu.santafe.edu/~aaronc/powerlaws/ . In particular, the R code of Laurent Dubroca.

Installation
------------

Currently, this package can only be installed from github and requires R >=2-15.0. The easiest way to install from github is to use the devtools package:
```r
## install.packages("devtools")
library(devtools); install_github('poweRlaw', 'csgillespie')
```

Note Windows users have to first install [Rtools](http://cran.rstudio.com/bin/windows/Rtools/).

Example workflow
----------------

### The pl_data object 

First we load the package and an example data set:
```r
require(poweRlaw)
data(moby_sample)
```
The `moby_sample` data set is described in its associated help file - `?moby_sample`. This dataset contains a subset of the word frequency from the novel Moby Dick. The entire data set can be loaded using `data(moby)`.

We then create a `pl_data` object. This is an R reference object that contains the data set.
```r
pl_d = pl_data$new(moby_sample)
```
which can be plotted using the plot function:
```r
plot(pl_d)
#Alternatively, all plot & line methods return the data silently, so
d = plot(pl_d)
head(d)
```

### Estimating parameters of the discrete powerlaw

Currently the only distribution available is the discrete power-law 
distribution (more will be added soon). To create a discrete power-law object, 
we use the `displ` method:
```r
m = displ$new(pl_d)
```
Initially, the parameters of the `displ` object are `NULL`:
```r
m$getXmin()
m$getPars()
```
To estimate the lower cut-off `xmin`, we use 
```r
(xmin = estimate_xmin(m))
```
This function returns the Kolomogorov-statistic, xmin value and the alpha
scaling parameter. We can then set the `xmin` value in `m`:
value
```r
m$setXmin(xmin[2])
m$getXmin()
```
Next we estimate the scaling parameter using the `mle` method:
```r
pars = estimate_pars(m)
```
and set the parameters in the `m` object:
```r
m$setPars(pars)
m$getPars()
```
We can get a handle on the uncertainty when estimating `xmin` using 
bootstrapping:
```r
#This function runs in parallel.
#Set the number threads to the number of CPU cores
#This can take a while
bs = bootstrap_xmin(m, no_of_sims=1000, threads=1)
```
Example results are included in this package:
```r
##Moby_bootstrap relates to the entire moby data set
##See ?bootstrap_moby
data(bootstrap_moby)
bs = bootstrap_moby
```
We can plot the range of plausible `xmin` and `alpha` values using:
```r
hist(bs$bootstraps[,2])
hist(bs$bootstraps[,3])
```
The values in the first row of table 6.1 in Clauset et al, can be obtained via:
```r
##p-value
bs[[1]]
##sd of xmin. In the paper, it's 2
sd(bs[[3]][,2])
##sd of alpha. In the paper, it's 0.02
sd(bs[[3]][,3])
```

### Plotting distribution objects

Once a distribution object has been created, standard plotting functions
can be applied to it:
```r
m$setXmin(7)
m$setPars(1.95)
##Plot the data, remove point before the threshold
plot(m)
##Add in the fitted distribution
lines(m, col=2)
```
The data used in the plots can be extracted by assignment:
```r
d = plot(m)
pl = lines(m, col=2)
```





