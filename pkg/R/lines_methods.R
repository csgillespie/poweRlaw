#' @rdname plot-distribution-ANY-method
#' @param x a distribution reference object.
#' @param ... Further arguments passed to the \code{lines} functions.
#' @importFrom methods is
#' @importFrom stats runif
#' @export
setMethod("lines",
          signature = signature(x="distribution"),
          definition = function(x, 
                                cut=FALSE, 
                                draw=TRUE,
                                length.out=100, 
                                ...) {
            scale = 1
            xmin = x$getXmin()
            x_values = x$dat
            x_axs = lseq(xmin, max(x_values), length.out) 
            if(is(x,"discrete_distribution"))
              x_axs = unique(round(x_axs))
            
            y = dist_cdf(x, x_axs, FALSE)
            if(!cut) {
              if(is(x,"discrete_distribution")) x$setXmin(1)
              else x$setXmin(0)
              d_cdf = dist_data_cdf(x, lower_tail=FALSE)
 
              ##Linear interprolate between cdf points
              if(is(x,"discrete_distribution")) {
                dif = x$internal[["values"]] - xmin
                upper = which(dif > 0)[1]
                lower = max(upper - 1, 1)
                x_dif = x$internal[["values"]][lower] - 
                  x$internal[["values"]][upper]
                y_dif = d_cdf[lower] - d_cdf[upper]
                
                scale = d_cdf[lower] + 
                  y_dif*(xmin - x$internal[["values"]][lower])/x_dif
              } else {
                dif = x$internal[["dat"]] - xmin
                upper = which(dif >= 0)[1]
                lower = max(upper - 1, 1)
                x_dif = x$internal[["dat"]][lower] - x$internal[["dat"]][upper]
                y_dif = d_cdf[lower] - d_cdf[upper]
                scale = d_cdf[lower] + 
                  y_dif*(xmin - x$internal[["dat"]][lower])/x_dif
                
                
              }
              
              x$setXmin(xmin)
            }
            
            if(is.nan(scale)) scale = 1
            y = y*scale
            if(draw)
              lines(x_axs, y, ...)
            invisible(data.frame(x=x_axs, y=y))
          }
)






