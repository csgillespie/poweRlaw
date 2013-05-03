#' @rdname plotting-methods-distribution
#' @aliases lines,distribution-method
setMethod("lines",
          signature = signature(x="distribution"),
          definition = function(x, 
                                cut=FALSE, 
                                length.out=10, 
                                draw=TRUE,
                                ...) {
            scale = 1
            xmin = x$getXmin()
            x_values = x$dat
            x_axs = lseq(x$xmin, max(x_values), length.out) 
            if(is(x,"discrete_distribution"))
              x_axs = unique(round(x_axs))
            
            y = dist_cdf(x, x_axs, FALSE)
            if(!cut) {
              if(is(x,"discrete_distribution")) 
                x$setXmin(1)
              else 
                x$setXmin(0)
              d_cdf = dist_data_cdf(x, lower_tail=FALSE)
 
              ##Linear interprolate between cdf points
              if(is(x,"discrete_distribution")) {
                dif = x$internal[["values"]] - xmin
                upper = which(dif > 0)[1]
                lower = upper - 1
                x_dif = x$internal[["values"]][lower] - 
                  x$internal[["values"]][upper]
                y_dif = ifelse(lower, d_cdf[lower], 0) - d_cdf[upper]
                
                scale = ifelse(lower, d_cdf[lower], 0) + 
                  y_dif*(xmin - x$internal[["values"]][lower])/x_dif
                
                
              } else {
                dif = x$internal[["dat"]] - xmin
                upper = which(dif > 0)[1]
                lower = upper - 1
                x_dif = x$internal[["dat"]][lower] - x$internal[["dat"]][upper]
                y_dif = ifelse(lower, d_cdf[lower], 0) - d_cdf[upper]
                
                scale = ifelse(lower, d_cdf[lower], 0) + 
                  y_dif*(xmin - x$internal[["dat"]][lower])/x_dif
                
              }
              
              x$setXmin(xmin)
            }
            y = y*scale
            if(draw)
              lines(x_axs, y, ...)
            invisible(data.frame(x=x_axs, y=y))
          }
)
