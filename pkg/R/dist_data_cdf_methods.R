##TODO: Change discrete and CTN signatures
#' @rdname dist_data_cdf-methods
#' @aliases dist_data_cdf,discrete_distribution-method
setMethod("dist_data_cdf",
          signature = signature(m="discrete_distribution"),
          definition = function(m, lower_tail=TRUE, all_values=FALSE) {
            if(all_values) {
              occur = tabulate(m$dat)
              if(m$xmin > 1)
                occur = occur[-(1:(m$xmin-1))]
            } else {
              tab = table(m$dat)
              occur = as.vector(tab)
              occur = occur[as.numeric(names(tab)) >= m$xmin]
            }          
            occur = occur/sum(occur)
            p = occur/sum(occur)
            
            if(lower_tail)
              cumsum(p)
            else
              rev(cumsum(rev(p)))
          }
)

#' @rdname dist_data_cdf-methods
#' @aliases dist_data_cdf,ctn_distribution-method
setMethod("dist_data_cdf",
          signature = signature(m="ctn_distribution"),
          definition = function(m, lower_tail=TRUE) {
            n = m$internal[["n"]]
            cdf = (0:(n-1))/n
            if(lower_tail)
              cdf
            else
              1 - cdf
          }
)

# get_data_cdf = function(x, lower.tail=TRUE, pad=FALSE){ 
#   if(pad)
#     occur = tabulate(x)
#   else
#     occur = as.vector(table(x))
#   occur = occur/sum(occur)
#   p = occur/sum(occur)
#   if(lower.tail)
#     cumsum(p)
#   else
#     rev(cumsum(rev(p)))
# }
#get_data_cdf(x_values, pad=TRUE)[m$xmin:max(x_values)]
