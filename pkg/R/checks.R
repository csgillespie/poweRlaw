## Non-export functions. 
## Used for input checking

check_discrete_data = function(x) {
  if(!all(floor(x) == x)) 
    stop("Your data should be discrete. 
          Either use `floor` or `round` on your data set. Or switch to a continuous distribution.")
  
  if(min(x) < 1)
    stop("Data should be strictly positive, i.e. no zeros.")
}

check_ctn_data = function(x) {
  if(min(x) <= 0)
    stop("Data should be strictly positive, i.e. no zeros.")
}