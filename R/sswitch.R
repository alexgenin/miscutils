
# Vectorized switch function 
sswitch <- function(X, ...) { 
  sapply(X, function(x, ...) { 
    switch(x, ...)
  })
}
