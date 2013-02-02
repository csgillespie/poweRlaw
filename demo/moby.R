require(poweRlaw)
##Load in the full Moby dick data set
data(moby)

##Create a data object and plot
pl_d = pl_data$new(moby)
plot(pl_d)

##Create a distribution object
m = displ$new(pl_d)

##Set parameters explicitly
m$setXmin(7);m$setPars(2.2)

##Plot the data with fitted lines
plot(m)
lines(m, col=2)

##Now calculate the mle estimate and add to the plot
mle_pars = estimate_pars(m)
m$setPars(mle_pars)

lines(m, col=3)

##Estimate xmin
estimate_xmin(m)

##This can take a while
bs = bootstrap_xmin(m, no_of_sims=10, threads=4)


##Plot uncertainity in xmin and alpha
hist(bs$bootstraps[,2], breaks="fd")
hist(bs$bootstraps[,3], breaks="fd")
