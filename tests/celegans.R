celegans_indegree = read.table("tests/celegans-indegree.txt")$V1
save(celegans_indegree, file="tests/celegans-indegree.RData")
test_that("Test C elegans", {
  load("celegans-indegree.RData")
  c1 = celegans_indegree[celegans_indegree > 0]
  ll = sum(log((alpha - 1)/xmin*(c1/xmin)^(-alpha)))
  
  m = conpl$new(c1)
  m$setPars(2)
  xmin = m$getXmin(); alpha = m$getPars()
  expect_equal(dist_ll(m), ll)
 
  ##Different xmin
  ##Test for >= bug
  m$setXmin(2)
  xmin = m$getXmin(); alpha = m$getPars()
  x = c1[c1>= xmin]
  ll = sum(log((alpha - 1)/xmin*(x/xmin)^(-alpha)))
  expect_equal(dist_ll(m), ll)
  
  #plot(m)
  (est = estimate_xmin(m))

  
})


#   result.alpha = result.xmin = result.L = 0;
#   plfit_discrete(data, n, &options, &result);
#   ASSERT_ALMOST_EQUAL(result.alpha, 2.9967, 1e-1);
#   ASSERT_EQUAL(result.xmin, 10);
#   ASSERT_ALMOST_EQUAL(result.L, -245.14869, 1e-4);
#   ASSERT_ALMOST_EQUAL(result.D, 0.04448, 1e-3);