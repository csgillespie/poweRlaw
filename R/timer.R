timer = function() {
  start_time = Sys.time()
  end_time = 0
  start = function() start_time <<- Sys.time()
  stop = function() end_time <<- Sys.time()
  get = function(stop=FALSE)  {
    if(stop)end_time <<- Sys.time()
    difftime(end_time, start_time, units="secs")
  }
  
  list(start =start, stop = stop, get =get)
}
