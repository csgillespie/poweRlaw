##Helper functions for plotting
lseq = function(from, to, length.out) {
  ceiling(exp(seq(log(from), log(to), length.out=length.out)))    
}

get_data_cdf = function(x, lower.tail=TRUE, pad=FALSE){ 
  if(pad)
    occur = tabulate(x)
  else
    occur = as.vector(table(x))
  occur = occur/sum(occur)
  p = occur/sum(occur)
  if(lower.tail)
    cumsum(p)
  else
    rev(cumsum(rev(p)))
}

#' Plotting functions
#'
#' These are generic functions for distribution and pl_data reference 
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
#' @note These methods do *not* alter the internal state of
#' the distribubtion objects.
#' @docType methods
#' @note This method does *not* alter the internal state of
#' the distribubtion objects.
#' @rdname plotting-methods-distribution
#' @aliases plot,pl_data,ANY-method
#' @rdname plotting-methods-distribution
#' @exportMethod plot
setMethod("plot",
          signature=signature(x="pl_data"),
          definition=function(x, plot=TRUE, ...) {
            y = get_data_cdf(x$x, FALSE)
            x = as.numeric(names(table(x$x)))
            if(plot)
              plot(x, y, log="xy", ...)
            else
              data.frame(x, y)
          }
)

#' @aliases plot,distribution,ANY-method
#' @rdname plotting-methods-distribution
#' @exportMethod plot
setMethod("plot",
          signature = signature(x="distribution"),
          definition = function(x, plot=TRUE, 
                                length.out=1000, data=TRUE,...) {
              x_values = x$pl_data$x
            if(data) {
              
              d = x_values[x_values >= x$xmin]
              y = get_data_cdf(d, FALSE)
              x_axs = as.numeric(names(table(d)))
            } else {
              x_axs = lseq(x$xmin, max(x_values), length.out)
              y = dist_cdf(m, x_axs, FALSE)
            }
            
            if(plot)
              plot(x_axs, y, log="xy", ...)
            else
              data.frame(x=x_axs, y=y)
          }
)
