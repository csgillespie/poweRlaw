test_that("Testing bootstrap function", {
  x = 1:10
  mt = displ$new(x)
  bs = bootstrap(mt, no_of_sims=5)
  expect_is(bs, "bs_xmin")
  expect_equal(dim(bs$bootstraps), c(5, 3))
  expect_equal(names(bs), c("gof", "bootstraps", "sim_time"))
  
  m = dislnorm$new(x)
  bs = bootstrap(m, no_of_sims=5)
  expect_is(bs, "bs_xmin")
  expect_equal(dim(bs$bootstraps), c(5, 4))
  expect_equal(names(bs), c("gof", "bootstraps", "sim_time"))
  
  m = dispois$new(x)
  bs = bootstrap(m, no_of_sims=5)
  expect_is(bs, "bs_xmin")
  expect_equal(dim(bs$bootstraps), c(5, 3))
  expect_equal(names(bs), c("gof", "bootstraps", "sim_time"))
  
  m = conpl$new(x)
  bs = bootstrap(m, no_of_sims=5)
  expect_is(bs, "bs_xmin")
  expect_equal(dim(bs$bootstraps), c(5, 3))
  expect_equal(names(bs), c("gof", "bootstraps", "sim_time"))
  
  m = conlnorm$new(x)
  bs = bootstrap(m, no_of_sims=5)
  expect_is(bs, "bs_xmin")
  expect_equal(dim(bs$bootstraps), c(5, 4))
  expect_equal(names(bs), c("gof", "bootstraps", "sim_time"))
  
}
)