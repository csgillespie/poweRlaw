test_that("Testing bootstrap p function", {
  ## Avoid testthat hanging
  Sys.setenv("R_TESTS" = "") 
  x = 1:10
  mt = displ$new(x)
  bs = bootstrap_p(mt, no_of_sims=3)
  expect_is(bs, "bs_p_xmin")
  expect_equal(dim(bs$bootstraps), c(3, 4))
  expect_equal(names(bs), c("p", "gof", "bootstraps", "sim_time", "seed", 
                            "package_version", "distance"))
  
  expect_equal(bs$distance, "ks")

  bs = bootstrap_p(mt, no_of_sims=3, distance="reweight")
  expect_equal(bs$distance, "reweight")
  
  mt = conpl$new(x)
  bs = bootstrap_p(mt, no_of_sims=3)
  expect_is(bs, "bs_p_xmin")
  expect_equal(dim(bs$bootstraps), c(3, 4))
  expect_equal(names(bs), c("p", "gof", "bootstraps", "sim_time", "seed", 
                            "package_version", "distance"))
  
  
}
)