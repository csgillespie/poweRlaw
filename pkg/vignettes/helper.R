setnicepar = function(...)  {
  par(mar=c(3,3,2,1), 
      mgp=c(2,0.4,0), tck=-.01,
      cex.axis=0.9, las=1,...)
}

mypalette = function(set = 0) {
  
  if(set==0)
    palette("default")
  else {#I want hue - pimp
    palette(c(rgb(170,93,152, maxColorValue=255),
              rgb(103,143,57, maxColorValue=255),
              rgb(196,95,46, maxColorValue=255),
              rgb(79,134,165, maxColorValue=255),
              rgb(205,71,103, maxColorValue=255),
              rgb(203,77,202, maxColorValue=255),
              rgb(115,113,206, maxColorValue=255)))
  }
}
