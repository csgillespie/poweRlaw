#' @rdname plot-distribution-ANY-method
#' @export
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









