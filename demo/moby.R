library(roxygen2); roxygenise("../pkg1/")


data(moby)
pl_d = pl_data$new(moby)
plot(pl_d)
m = displ$new(pl_d)
m$xmin = 7;m$pars = 2
m$pl_data$x
plot(m)
lines(m)
estimate_xmin(m)
d = bootstrap_xmin(m, Bt=1000, no_of_cores=5)

cl = makeCluster(1)
pl = pl_data$new(x=1:10)
f= function(i, m)return(10)
parSapply(cl, 1:2, f, m)

bootstrap_xmin(m, Bt=8,no_of_cores=4)

lines(m, col=2)
estimate_xmin(m)
m$xmin
m$copy()

dist_rand(m, 10)

m$mle()
dist_pdf(m)
dist_cdf(m)
estimate_xmin(m)
m$xmin

m1 = m$copy(TRUE)
m1$pars = 5
m$pars
m$pars = 10
m1$pars
#plot(pl_d)
#pl_d$copy()



pl_d
m$mle()
plot(m)
lines(m, col=2)
points(m, col=3)
dist_ll(m)

estimate_xmin(m)
pldis_xmin(m)

m$copy(shallow=TRUE)

pl_d$copy()
# ggplot(data =dd, aes(x, P)) + 
#     geom_point(alpha=0.3, size=1) + 
#     scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), 
#                   labels = trans_format('log10', math_format(10^.x)))+ 
#     scale_x_log10(breaks = trans_breaks('log10', function(x) 10^x), 
#                   labels = trans_format('log10', math_format(10^.x))) + 
#     ylab("1-P(x)") + theme_bw() 
