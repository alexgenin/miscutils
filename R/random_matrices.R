# 
# 
# Shortcuts to create random matrices
# 

#'@export
rmat <- function(nr, nc, rfunc = rnorm, ...) { 
  matrix(rfunc(nr*nc, ...), nrow = nr, ncol = nc)
}

#'@export
rsqmat <- function(n, rfunc, ...) { 
  rmat(n, n, rfunc, ...)
}
