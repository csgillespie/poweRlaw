test_that("Testing bootstrap p function", {

  x = 1:10
  mt = displ$new(x)
  bs = bootstrap_p(mt, no_of_sims=5)
  expect_is(bs, "bs_p_xmin")
  expect_equal(dim(bs$bootstraps), c(5, 3))
  expect_equal(names(bs), c("p", "gof", "bootstraps", "sim_time"))
  
  mt = conpl$new(x)
  bs = bootstrap_p(mt, no_of_sims=5)
  expect_is(bs, "bs_p_xmin")
  expect_equal(dim(bs$bootstraps), c(5, 3))
  expect_equal(names(bs), c("p", "gof", "bootstraps", "sim_time"))
  
  
}
)