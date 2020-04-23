test_that("Testing get_sims function", {
  skip_on_cran()

  x = c(rpldis(100, 1, alpha = 2), 1:5)
  m = displ$new(x)

  ## Standard bootstrap
  bs = bootstrap(m, no_of_sims = 4, threads = 2, seed = 1)
  sims = get_bootstrap_sims(m, 4, threads = 2, seed = 1)

  m1 = displ$new(sims[, 1])
  expect_equal(estimate_xmin(m1)$gof, bs$bootstraps$gof[1], tol = 1e-3)
  m1 = displ$new(sims[, 4])
  expect_equal(estimate_xmin(m1)$gof, bs$bootstraps$gof[4], tol = 1e-3)

  ## Standard bootstrap - expect difference
  sims = get_bootstrap_sims(m, 4, threads = 2, seed = 2)
  m1 = displ$new(sims[, 1])
  expect_gt(abs(estimate_xmin(m1)$gof - bs$bootstraps$gof[1]), 0)

  # bootstrap_p
  m$xmin = 2
  m$pars = 2
  bs = bootstrap_p(m, no_of_sims = 4, threads = 2, seed = 1)
  sims = get_bootstrap_p_sims(m, no_of_sims = 4, threads = 2, seed = 1)

  m1 = displ$new(sims[, 1])
  expect_equal(estimate_xmin(m1)$gof, bs$bootstraps$gof[1], tol = 1e-3)
  m1 = displ$new(sims[, 4])
  expect_equal(estimate_xmin(m1)$gof, bs$bootstraps$gof[4], tol = 1e-3)
  ## Expect difference
  sims = get_bootstrap_p_sims(m, 4, threads = 2, seed = 2)
  m1 = displ$new(sims[, 1])
  expect_gt(abs(estimate_xmin(m1)$gof - bs$bootstraps$gof[1]), 0)

})
