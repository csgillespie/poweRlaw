lseq = function(from, to, length.out) {
  s = exp(seq(log(from), log(to), length.out=length.out))
  ##To avoid rounding problems in the plotting
  ##function
  s[1] = from
  s
}

#' Generic plotting functions
#'
#' These are generic functions for distribution  reference 
#' objects. Standard plotting functions, i.e. plot, points, and lines work 
#' with all distribution objects.
#' 
#' @param draw logical (default \code{TRUE}). Should the plot/lines/points function plot or 
#' return the data (in a data frame object).
#' @param cut logical (default \code{FALSE}) - 
#' Where should the plot begin. If \code{cut=FALSE}, then the 
#' plot will start at the minimum data value. Otherwise, the plot
#' will start from \code{xmin}
#' @param length.out numeric, default 100. How many points should the 
#' distribution be evaulated at. This argument is only
#' for plotting the fitted lines.
#' @docType methods
#' @note This method does *not* alter the internal state of
#' the distribubtion objects.
#' @export
setMethod("plot",
          signature = signature(x="distribution"),
          definition = function(x, 
                                cut=FALSE,
                                draw=TRUE, ...) {
            xmin = x$getXmin()
            cut_off = cut*xmin
            x_values = x$dat
            
            if(!cut) x$setXmin(min(x_values))
            y = dist_data_cdf(x, lower_tail = FALSE, xmax=max(x_values)+1)
            
            cut_off_seq = (x_values >= cut_off)
            x_axs = x_values[cut_off_seq]
            if(is(x, "discrete_distribution")) 
              x_axs = unique(x_axs)
            x$setXmin(xmin)
            
            x = x_axs
            if(draw) plot(x, y, log="xy", ...)
            invisible(data.frame(x=x, y=y))
          }
)

######################################
######################################
######################################
#' @importFrom stats qchisq qpois qt rlnorm
get_cum_summary = function(x,trim=0.1) {
  n = length(x)
  m = cumsum(x)/1:n
  x2 = cumsum(x^2)
  v = (x2 - m^2*(1:n))/(0:(n-1))
  
  dd = data.frame(m = m, v = v)
  dd$x = 1:nrow(dd)
  ## Remove the first row (no info on uncertainity)
  dd = dd[-1,]
  
  ## CI for mean is mu \pm qt(n-1*s/N)
  dd$m_up = dd$m + qt(0.975, (1:(n-1)))*sqrt(dd$v)/sqrt(2:n)
  dd$m_low = dd$m + qt(0.025, (1:(n-1)))*sqrt(dd$v)/sqrt(2:n)

  #CI for v is (n-1)*v/chi(n-1)
  dd$v_low = (1:(n-1))*dd$v/qchisq(0.975, (1:(n-1)))
  dd$v_up = (1:(n-1))*dd$v/qchisq(0.025, (1:(n-1)))
  
  dd = dd[floor(nrow(dd)*trim):nrow(dd), ]
  return(dd)
}

#' @importFrom graphics par grid
create_plots = function(l, no_plots) {
  ##Set margins for optimal viewing
  old_par = par(no.readonly = TRUE)
  on.exit(par(old_par))
  
  par(mfrow=c(2, no_plots), 
      mar=c(3,3,2,1), mgp=c(2,0.4,0), tck=-.01,
      cex.axis=0.9, las=1)
  
  ##Plot the cumulative means
  for(i in 1:length(l)) {
    upp_y = max(l[[i]]$m_up)
    low_y = min(l[[i]]$m_low)
    plot(l[[i]]$x, l[[i]]$m, type="l", ylim=c(low_y, upp_y), 
         ylab=names(l[i]),
         xlab="Iteration",
         panel.first=grid(), col=1, 
         main="Cumulative mean")
    lines(l[[i]]$x, l[[i]]$m_up, col=2)
    lines(l[[i]]$x, l[[i]]$m_low, col=2)
  }
  
  ##Plot the std deviations
  ##Plotting the p-value std dev doesn't really make sense
  for(i in 1:length(l)) {
    upp_y = max(sqrt(l[[i]]$v_up))
    low_y = min(sqrt(l[[i]]$v_low))
    if(names(l[i]) != "p-value") {
      plot(l[[i]]$x, sqrt(l[[i]]$v), 
           type="l", ylim=c(low_y, upp_y), 
           ylab=names(l[i]),
           xlab="Iteration",
           panel.first=grid(), col=1, 
           main="Cumulative std dev")
      lines(l[[i]]$x, sqrt(l[[i]]$v_up), col=2)
      lines(l[[i]]$x, sqrt(l[[i]]$v_low), col=2)
    }
  }
}
#CI for s is sqrt((n-1)*s/chi(n-1))

#' Plot methods for bootstrap objects
#' 
#' A simple wrapper around the plot function to aid with visualising the bootstrap results. 
#' The values plotted are returned as an invisible object.
#' 
#' @param x an object of class \code{bs_xmin} or \code{bs_p_xmin}
#' @param trim When plotting the cummulative means and standard deviation, the first trim percentage of values are not displayed.
#' default \code{trim=0.1}
#' @param ...  graphics parameters to be passed to the plotting routines.
#' @method plot bs_xmin
#' @export
plot.bs_xmin = function(x, trim=0.1, ...){
  ## Remove any problem bootstraps
  bs = x$bootstraps
  
  ## Change to anyNA in future (need R >= 3.1.0)
  x$bootstraps = bs[!apply(bs, 1, function(i) any(is.na(i))),]
  
  no_plots = ncol(x$bootstraps) - 1
  l = list()
  for(i in 1:no_plots){
    d = x$bootstraps[, i+1]
    l[[i]] = get_cum_summary(d, trim)
  }
  names(l) = c("Xmin", paste("Par", 1:(no_plots-2)), "ntail")
  create_plots(l, no_plots)
}

#' @rdname plot.bs_xmin
#' @method plot bs_p_xmin
#' @export
plot.bs_p_xmin = function(x, trim=0.1, ...){
  ## Remove any problem bootstraps
  bs = x$bootstraps
  x$bootstraps = bs[!apply(bs, 1, function(i) any(is.na(i))),]
  
  no_plots = ncol(x$bootstraps)
  l = list()
  for(i in 1:(no_plots-1)){
    d = x$bootstraps[, i+1]
    l[[i]] = get_cum_summary(d, trim)
  }
  l[[no_plots]] = get_cum_summary(x$gof < x$bootstraps$gof, trim)
  
  names(l) = c("Xmin", paste("Par", 1:(no_plots-3)), "ntail", "p-value")
  #no_plots = ncol(x$bootstraps) - 1
  create_plots(l, no_plots)  
}

#' @importFrom utils modifyList
#' @rdname plot.bs_xmin
#' @method plot compare_distributions
#' @export
plot.compare_distributions = function(x, ...) {
  dd = x$ratio[!duplicated(x$ratio$x),]
  defaults = list(xlab = "x", ylab = "Log-likelihood Ratio")
  args = modifyList(defaults, list(x = dd$x, y=dd$ratio, ...))
  do.call("plot", args)
  invisible(data.frame(x=dd$x, y=dd$ratio))
}





