##Ref class initialisers

pl_data$methods( list(
  initialize = function(x=numeric()) {
    x <<- sort(x)
    tab = table(x)
    freq <<- as.vector(tab)
    values <<- as.numeric(names(tab))
  }))

#######################################################
#Power-law distributions
#######################################################
displ$methods( 
  list(
    initialize = function(pl_data) {
      datatype <<- "discrete"
      ##Use the internal attribute for copying
      if(!missing(pl_data)) {
        internal[["cum_slx"]] <<-
          rev(cumsum(log(rev(pl_data$values))*rev(pl_data$freq)))
        internal[["cum_n"]] <<- rev(cumsum(rev(pl_data$freq)))
        internal[["pl_data"]] <<- pl_data
        xmin <<- min(pl_data$values)
      }
    }
  )
)

conpl$methods( 
  list(
    initialize = function(pl_data) {
      datatype <<- "continuous"
      ##Use the internal attribute for copying
      if(!missing(pl_data)) {
        internal[["cum_slx"]] <<-
          rev(cumsum(log(rev(pl_data$values))*rev(pl_data$freq)))
        internal[["cum_n"]] <<- rev(cumsum(rev(pl_data$freq)))
        
        pl_data <<- pl_data
      }
    }
    
  )
)



# 
# 
# 
# test1$methods( list(
#   initialize = function(x=NULL, test2) {
#     if(missing(test2)) {
#       if(exists("test2")) {
#         cat(is.null(.self$test2)); 
#         .self$values = .self$test2
#       }
#     } 
#     else .self$test2 = test2
#     y = x
#     if(is.null(y)) y = numeric()
#     
#     x <<- y
#   }
# )
# )
