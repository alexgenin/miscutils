# 
# Function that plots ggplot object to a Rplots in current dir
# 
#'@export
graphout <- function(expr, device = pdf, width = 16, height = 10) { 
  
  # Are we rendering in a rmarkdown doc ? If yes then we output to a device. 
  # Other wise we just print the content of the expr
  is_rendering <- !is.null(knitr::current_input())
  if ( ! is_rendering ) { 
    device(width = width, height = height)
    on.exit(dev.off()) # close device on exit, even if error
  }
  
  print( eval(expr) )
}

