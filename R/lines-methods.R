#' @rdname plotting-methods-distribution
#' @aliases lines,distribution-method
setMethod("lines",
          signature = signature(x="distribution"),
          definition = function(x, length.out=1000, ...) {
              x_values = x$pl_data$x
              x_axs = lseq(x$xmin, max(x_values), length.out)
              y = dist_cdf(x, x_axs, FALSE)
              lines(x_axs, y, ...)
              invisible(data.frame(x=x_axs, y=y))
          }
)
