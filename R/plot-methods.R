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
#' with distribution and pl_data objects.
#' 
#' @param plot logical (default TRUE). Should the plot function plot or 
#' return the data (in a data frame object).
#' @param length.out numeric, default 1000. How many points should the 
#' distribution be evaulated at. This argument is only
#' for plotting distribution objects
#' @param data logical (default TRUE) - for distribution objects only. 
#' When plotting, should the data (truncated at xmin) be plotted, or should the 
#' theoretical distribution be plotted.
#' @docType methods
#' @note This method does *not* alter the internal state of
#' the distribubtion objects.
#' @aliases plot,distribution,ANY-method
#' @rdname plotting-methods-distribution
#' @exportMethod plot
setMethod("plot",
          signature = signature(x="distribution"),
          definition = function(x, cut=FALSE, ...) {
            xmin = x$getXmin()
            cut_off = cut*xmin
            x_values = x$dat
            
            if(!cut) x$setXmin(min(x_values))
            y = dist_data_cdf(x, FALSE)
            
            cut_off_seq = (x_values >= cut_off)
            x_axs = x_values[cut_off_seq]
            if(x$datatype == "discrete") 
              x_axs = unique(x_axs)
            x$setXmin(xmin)
            
            x = x_axs
            plot(x, y, log="xy", ...)
            invisible(data.frame(x=x, y=y))
          }
)
