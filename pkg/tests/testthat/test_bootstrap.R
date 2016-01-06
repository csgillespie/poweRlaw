test_that("Testing bootstrap function", {
#   x = 1:10
#   mt = displ$new(x)
#   bs = bootstrap(mt, no_of_sims=5)
#   expect_is(bs, "bs_xmin")
#   expect_equal(dim(bs$bootstraps), c(5, 4))
#   expect_equal(names(bs), c("gof", "bootstraps", "sim_time", "seed", "package_version"))
#   
#   m = dislnorm$new(x)
#   bs = bootstrap(m, no_of_sims=5)
#   expect_is(bs, "bs_xmin")
#   expect_equal(dim(bs$bootstraps), c(5, 5))
#   expect_equal(names(bs), c("gof", "bootstraps", "sim_time", "seed", "package_version"))
#   
#   m = dispois$new(x)
#   bs = bootstrap(m, no_of_sims=5)
#   expect_is(bs, "bs_xmin")
#   expect_equal(dim(bs$bootstraps), c(5, 4))
#   expect_equal(names(bs), c("gof", "bootstraps", "sim_time", "seed", "package_version"))
#   
#   m = conpl$new(x)
#   bs = bootstrap(m, no_of_sims=5)
#   expect_is(bs, "bs_xmin")
#   expect_equal(dim(bs$bootstraps), c(5, 4))
#   expect_equal(names(bs), c("gof", "bootstraps", "sim_time", "seed", "package_version"))
#   
#   m = conlnorm$new(x)
#   bs = bootstrap(m, no_of_sims=5)
#   expect_is(bs, "bs_xmin")
#   expect_equal(dim(bs$bootstraps), c(5, 5))
#   expect_equal(names(bs), c("gof", "bootstraps", "sim_time", "seed", "package_version"))
#   
#   m = conexp$new(x)
#   bs = bootstrap(m, no_of_sims=5)
#   expect_is(bs, "bs_xmin")
#   expect_equal(dim(bs$bootstraps), c(5, 4))
#   expect_equal(names(bs), c("gof", "bootstraps", "sim_time", "seed", "package_version"))
  
}
)