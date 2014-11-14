## Moved tests from package to avoid BDR's wrath
install.packages("pkg/", repo=NULL)
library(poweRlaw)
library(testthat)

y = test_dir("tests/")
if(y$failed) stop("Errors")

