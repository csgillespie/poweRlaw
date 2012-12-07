library(roxygen2)
roxygenise("../poweRlaw/")

##Load in the full Moby dick data set
data(blackouts)

##Create a data object and plot
pl_d = pl_data$new(blackouts)
plot(pl_d)

##Create a distribution object
m = conpl$new(pl_d)

##Set parameters explicitly
m$setXmin(230000);m$setPars(2.3)

##Plot the data with fitted lines
plot(m)
lines(m, col=2)

##Now calculate the mle estimate and add to the plot
m$mle()
lines(m, col=3)

##Estimate xmin
estimate_xmin(m)

##This can take a while
bs = bootstrap_xmin(m,no_of_sims=1000, threads=2)

##Plot uncertainity in xmin and alpha
hist(bs$bootstraps[,2], breaks="fd")
hist(bs$bootstraps[,3], breaks="fd")
