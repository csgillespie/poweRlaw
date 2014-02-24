##All other tests are included in the package
## At present, testthat doesn't seem to handle cluster function calls
library(poweRlaw)
library(testthat)

test_file("tests/test_bootstrap.R")
test_file("tests/test_bootstrap_p.R")
