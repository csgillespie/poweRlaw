#' @rdname plotting-methods-distribution
#' @aliases lines,distribution-method
setMethod("lines",
          signature = signature(x="distribution"),
          definition = function(x, cut=FALSE, length.out=10, ...) {
            scale = 1
            xmin = x$getXmin()
            x_values = x$dat
            x_axs = lseq(x$xmin, max(x_values), length.out) 
            if(is(x,"discrete_distribution"))
              x_axs = unique(round(x_axs))
            
            y = dist_cdf(x, x_axs, FALSE)
            if(!cut) {
              x$setXmin(1)
              d_cdf = dist_data_cdf(x, lower_tail=FALSE)
              ##If CTN, then in theory we can't have
              ##equal data points. But in practice...
              ##Take the min
              if(is(x,"discrete_distribution")) 
                scale = d_cdf[unique(x$internal[["dat"]])==xmin]
              else
                scale = max(d_cdf[x$internal[["dat"]]==xmin])
              x$setXmin(xmin)
            }
            y = y*scale
            lines(x_axs, y, ...)
            invisible(data.frame(x=x_axs, y=y))
          }
)
