library(poweRlaw)
library(testthat)
#test_dir("tests/")
test_file("tests/test_dist_ll.R")
test_file("tests/test_dist_cdf.R")
test_file("tests/test_dist_data_cdf.R")




test_file("tests/test_discrete_powerlaw.R")
test_file("tests/test_discrete_clauset.R")
test_file("tests/test_lognormal_discrete.R")
test_file("tests/test_poisson.R")



test_file("tests/test_ctn_powerlaw.R")
test_file("tests/test_ctn_clauset.R")