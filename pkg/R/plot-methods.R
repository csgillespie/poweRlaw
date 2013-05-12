lseq = function(from, to, length.out) {
  s = exp(seq(log(from), log(to), length.out=length.out))
  ##To avoid rounding problems in the plotting
  ##function
  s[1] = from
  s
}


#' Plotting functions
#'
#' These are generic functions for distribution  reference 
#' objects. Standard plotting functions, i.e. plot, points, and lines work 
#' with all distribution objects.
#' 
#' @param plot logical (default \code{TRUE}). Should the plot function plot or 
#' return the data (in a data frame object).
#' @param length.out numeric, default 1000. How many points should the 
#' distribution be evaulated at. This argument is only
#' for plotting distribution objects
#' @param cut logical (default \code{FALSE}) - 
#' Where should the plot begin. If \code{cut=FALSE}, then the 
#' plot will start at the minimum data value. Otherwise, the plot
#' will start from \code{xmin}
#' @docType methods
#' @note This method does *not* alter the internal state of
#' the distribubtion objects.
#' @aliases plot,distribution,ANY-method
#' @rdname plotting-methods-distribution
#' @exportMethod plot
setMethod("plot",
          signature = signature(x="distribution"),
          definition = function(x, 
                                cut=FALSE,
                                draw=TRUE, ...) {
            xmin = x$getXmin()
            cut_off = cut*xmin
            x_values = x$dat
            
            if(!cut) x$setXmin(min(x_values))
            y = dist_data_cdf(x, FALSE)
            
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



get_cum_summary = function(x) {
  m = cumsum(x)/1:length(x)
  x2 = cumsum(x^2)/1:length(x)
  v = x2 - m^2
  sqrt(tail(v))
  std_err = qt(0.975, 1:length(x))*sqrt(v)/sqrt(1:length(x))
  dd = data.frame(m = m, std_err = std_err)
  dd$x = 1:nrow(dd)
  return(dd)
}


#' @method plot bs_xmin
#' @S3method plot bs_xmin
plot.bs_xmin = function(x, ...){
  old_par = par(no.readonly = TRUE)
  on.exit(par(old_par))
  
  
  no_plots = ncol(x$bootstraps)-1
  par(mfrow=c(1, no_plots), 
      mar=c(3,3,2,1), mgp=c(2,0.4,0), tck=-.01,
      cex.axis=0.9, las=1)
  cols = c(rgb(170,93,152, maxColorValue=255),
           rgb(103,143,57, maxColorValue=255))
  
  l = list()
  for(i in 1:no_plots){
    d = x$bootstraps[,i+1]
    l[[i]] = get_cum_summary(d)
  }
  
  ##Xmin
  upp_y = ceiling(max(l[[1]]$m + l[[1]]$std_err))
  low_y = floor(min(l[[1]]$m - l[[1]]$std_err))
  plot(l[[1]]$m, type="l", ylim=c(low_y, upp_y), 
       ylab="xmin", xlab="Iteration", 
       panel.first=grid(), col=cols[1])
  lines(l[[1]]$m + l[[1]]$std_err, col=cols[2], lty=2)
  lines(l[[1]]$m - l[[1]]$std_err, col=cols[2], lty=2)
  
  for(i in 2:length(l)) {
    upp_y = max(l[[i]]$m + l[[i]]$std_err)
    low_y = min(l[[i]]$m - l[[i]]$std_err)
    plot(l[[i]]$m, type="l", ylim=c(low_y, upp_y), 
         ylab=paste("Par", (i-1)),
         xlab="Iteration",
         panel.first=grid(), col=cols[1])
    lines(l[[i]]$m + l[[i]]$std_err, col=cols[2])
    lines(l[[i]]$m - l[[i]]$std_err, col=cols[2])
  }
  invisible(l)
  
}


#' @method plot bs_p_xmin
#' @S3method plot bs_p_xmin
plot.bs_p_xmin = function(x, ...){
  d = plot.bs_xmin(x, ...)
  invisible(d)
}





