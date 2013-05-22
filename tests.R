library(poweRlaw)
library(testthat)
test_dir("tests/")
test_file("tests/test_dist_ll.R")
test_file("tests/test_dist_cdf.R")
test_file("tests/test_dist_data_cdf.R")

test_file("tests/test_estimate_pars.R")
test_file("tests/test_estimate_xmin.R")

test_file("tests/test_bootstrap.R")
test_file("tests/test_bootstrap_p.R")

test_file("tests/test_plot.R")

test_file("tests/test_vuongs.R")