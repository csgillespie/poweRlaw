get_data_cdf_probs = function(occur, lower_tail) {
  ## Normalise
  p = occur/sum(occur)
  if(lower_tail)
    cumsum(p)
  else
    rev(cumsum(rev(p)))
}

#' @rdname dist_data_cdf-methods
#' @aliases dist_data_cdf,discrete_distribution-method
setMethod("dist_data_cdf",
          signature = signature(m="discrete_distribution"),
          definition = function(m, lower_tail=TRUE, xmax=1e5) {
            
            tab = table(m$dat[m$dat <= xmax])
            occur = as.vector(tab)
            occur = occur[as.numeric(names(tab)) >= m$xmin]
            get_data_cdf_probs(occur, lower_tail)
          }
)

#' @rdname dist_data_cdf-methods
#' @aliases dist_data_all_cdf,discrete_distribution-method
setMethod("dist_data_all_cdf",
          signature = signature(m="discrete_distribution"),
          definition = function(m, lower_tail=TRUE, xmax=1e5) {
            occur = tabulate(m$dat[m$dat <= xmax])
            if(m$xmin > 1)
              occur = occur[-(1:(m$xmin-1))]
            get_data_cdf_probs(occur, lower_tail)
          }
)

#' @rdname dist_data_cdf-methods
#' @aliases dist_data_cdf,ctn_distribution-method
setMethod("dist_data_cdf",
          signature = signature(m="ctn_distribution"),
          definition = function(m, lower_tail=TRUE, xmax=1e5) {
            n = m$internal[["n"]]
            cdf = (0:(n-1))/n
            #cdf = cdf[1:min(n, xmax)]
            if(lower_tail)
              cdf
            else
              1 - cdf
          }
)

