library("poweRlaw")
## Load in the full Moby dick data set
data(moby)

##Create a distribution object
m = displ$new(moby)

##Set parameters explicitly
m$setXmin(1);m$setPars(2.2)

##Plot the data with fitted lines
plot(m)
lines(m, col=2)

##Now calculate the mle estimate and add to the plot
mle_pars = estimate_pars(m)
est = estimate_pars(m, pars=seq(1.5, 3.5, 0.001))
m$setPars(est)

lines(m, col=3)

##Estimate xmin
(est = estimate_xmin(m))
m$setXmin(est)
lines(m, col=4)

##Uncertainity in xmin
bs1 = bootstrap(m, no_of_sims=100, threads=2)
bs2 = bootstrap_p(m, no_of_sims=100, threads=2)

##This can take a while
bs1 = bootstrap(m, no_of_sims=5000, threads=4, pars = seq(1.6, 2.5, 0.01))
bs2 = bootstrap(m, no_of_sims=5000, threads=4)

##Plot uncertainity in xmin and alpha
par(mfrow=c(1, 2))
hist(bs1$bootstraps[,2], breaks="fd")
hist(bs1$bootstraps[,3], breaks="fd")

hist(bs2$bootstraps[,2], breaks="fd")
hist(bs2$bootstraps[,3], breaks="fd")

apply(bs1[[2]], 2, sd)
apply(bs2[[3]], 2, sd)

bootstrap(m, no_of_sims = 4, threads=4)
