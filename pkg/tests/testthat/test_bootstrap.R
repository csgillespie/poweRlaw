test_that("Testing bootstrap function", {
  skip_on_cran()
  ## Avoid testthat hanging
  Sys.setenv("R_TESTS" = "") 
  x = 1:10
  mt = displ$new(x)
  bs = bootstrap(mt, no_of_sims=5)
  expect_is(bs, "bs_xmin")
  expect_equal(dim(bs$bootstraps), c(5, 4))
  expect_equal(names(bs), c("gof", "bootstraps", "sim_time", "seed", "package_version", "distance"))
  
  m = dislnorm$new(x)
  bs = bootstrap(m, no_of_sims=5)
  expect_is(bs, "bs_xmin")
  expect_equal(dim(bs$bootstraps), c(5, 5))
  expect_equal(names(bs), c("gof", "bootstraps", "sim_time", "seed", "package_version", "distance"))
  
  m = dispois$new(x)
  bs = bootstrap(m, no_of_sims=5)
  expect_is(bs, "bs_xmin")
  expect_equal(dim(bs$bootstraps), c(5, 4))
  expect_equal(names(bs), c("gof", "bootstraps", "sim_time", "seed", "package_version", "distance"))
  
  m = conpl$new(x)
  bs = bootstrap(m, no_of_sims=5)
  expect_is(bs, "bs_xmin")
  expect_equal(dim(bs$bootstraps), c(5, 4))
  expect_equal(names(bs), c("gof", "bootstraps", "sim_time", "seed", "package_version", "distance"))
  
  m = conlnorm$new(x)
  bs = bootstrap(m, no_of_sims=5)
  expect_is(bs, "bs_xmin")
  expect_equal(dim(bs$bootstraps), c(5, 5))
  expect_equal(names(bs), c("gof", "bootstraps", "sim_time", "seed", "package_version", "distance"))
  
  m = conexp$new(x)
  bs = bootstrap(m, no_of_sims=5)
  expect_is(bs, "bs_xmin")
  expect_equal(dim(bs$bootstraps), c(5, 4))
  expect_equal(names(bs), c("gof", "bootstraps", "sim_time", "seed", "package_version", "distance"))
  
}
)