#' @rdname plotting-methods-distribution
#' @aliases points,distribution-method
setMethod("points",
          signature = signature(x="distribution"),
          definition = function(x, length.out=1000, ...) {
            x_values = x$pl_data$x
            x_axs = lseq(x$xmin, max(x_values), length.out)
            y = dist_cdf(m, x_axs, FALSE)
            points(x_axs, y, ...)
          }
)









