## Moved tests from package to avoid BDR's wrath
install.packages("pkg/", repo=NULL)
library(poweRlaw)
library(testthat)
devtools::check("pkg/")
y = test_dir("tests/")
if(any(y$failed)) stop("Errors")
