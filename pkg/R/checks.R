## Non-export functions. 
## Used for input checking

check_discrete_data = function(x) {
  if(!all(floor(x) == x)) 
    stop("Your data should be discrete. 
          Trying using `floor` or `round` before creating a reference object. ")
  
  if(min(x) < 1)
    stop("Data should be strictly positive (no zeros)")
}

check_ctn_data = function(x) {
  if(min(x) <= 0)
    stop("Data should be strictly positive (no zeros)")
}