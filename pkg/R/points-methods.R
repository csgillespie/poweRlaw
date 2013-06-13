#' @rdname plotting-methods-distribution
#' @aliases points,distribution-method
setMethod("points",
          signature = signature(x="distribution"),
          definition = function(x, 
                                cut=FALSE, 
                                draw=TRUE,
                                length.out=100, 
                                ...) {
            d = lines(x, cut, length.out, FALSE, ...)
            if(draw)
              points(d$x, d$y, ...)
            invisible(d)
          }
)









