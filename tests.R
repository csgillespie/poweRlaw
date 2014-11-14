## Moved tests from package to avoid BDR's wrath
install.packages("pkg/", repo=NULL)
library(testthat)

test_dir("tests/")

test_file("tests/test_estimate_xmin.R")
test_file("tests/test_dist_cdf.R")
test_file("tests/test_bootstrap.R")
