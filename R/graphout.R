# 
# Function that plots ggplot object to a Rplots in current dir
# 
#'@export
graphout <- function(expr, device = pdf, width = 16, height = 10) { 
  device(width = width, height = height)
  on.exit(dev.off()) # close device on exit, even if error
  print( eval(expr) )
}

