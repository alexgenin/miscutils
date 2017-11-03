# 
# 
# Wrappers around plyr functions to use multicore
# 

#'@export
mcddply <- function(df, formula, fun, ..., .progress = NULL, .parallel = NULL) { 
  require(plyr)
  require(parallel)
  
  # Apply function to pieces
  result_list <- mcdlply(df, formula, fun, ..., .progress = .progress, .parallel = NULL)
  
  # Bind list 
  rbind.fill(result_list)
}

#'@export
mcdlply <- function(df, formula, fun, ..., .progress = NULL, .parallel = NULL) { 
  require(plyr)
  require(parallel)
  
  mc.cores <- options("mc.cores")$mc.cores
  if ( is.null(mc.cores) || mc.cores < 2 ) { 
    return( ddply(df, formula, fun, ..., .progress = .progress) )
  }
  
  if ( !is.null(.progress) || !is.null(.parallel) ) { 
    warning('.progress/.parallel args are ignored')
  }
  
  # Split df by hand 
  df_split <- dlply(df, formula, identity)
  
  # Apply function to pieces
  mclapply(df_split, fun, ...)
}
