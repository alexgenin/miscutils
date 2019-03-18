
# Vectorized switch function 
#'@export
stringswitch <- function(X, ...) { 
  sapply(X, function(x) { 
    switch(x, ...)
  })
}
