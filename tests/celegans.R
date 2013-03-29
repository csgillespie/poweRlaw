celegans_indegree = read.table("tests/celegans-indegree.txt")$V1
save(celegans_indegree, file="tests/celegans-indegree.RData")
test_that("Test C elegans", {
  load("celegans-indegree.RData")
  c1 = celegans_indegree[celegans_indegree > 0]
  m = displ$new(c1)
  #plot(m)
  est = estimate_xmin(m, pars=seq(2.8, 3, 0.0001))
  expect_equal(est$pars, 2.9967, tol=1e-2)
  expect_equal(est$xmin, 10)
  expect_equal(est$KS
})
  
#   result.alpha = result.xmin = result.L = 0;
#   plfit_discrete(data, n, &options, &result);
#   ASSERT_ALMOST_EQUAL(result.alpha, 2.9967, 1e-1);
#   ASSERT_EQUAL(result.xmin, 10);
#   ASSERT_ALMOST_EQUAL(result.L, -245.14869, 1e-4);
#   ASSERT_ALMOST_EQUAL(result.D, 0.04448, 1e-3);