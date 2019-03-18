# 
# Fcache is a memoizer for functions. It analyses a call and its arguments
# and if the result is alread computed, then it will retrieve it form disk.
# 

#'@export
fcache <- function(fun, ...,
                   cache.dir = '.cache',  # caching directory
                   cache.ignore = FALSE,  # ignore cache, always compute result
                   cache.clear = FALSE,
                   token.eval.args = TRUE,
                   verbose = FALSE) {  # other args passed to function
  
  if (cache.clear) {
    message('Removing cache files...\n')
    sapply(dir(cache.dir, pattern='Rcache',full.names=TRUE), 
           file.remove)
    return(invisible(NULL))
  }
  
  # Set the root directory for cache
  cache.dir <- paste0(getwd(),"/",cache.dir)
  if ( !file.exists(cache.dir) ) { 
    dir.create(cache.dir)
  }
  R.cache::setCacheRootPath(cache.dir)
  
  # Compute cache key by eval'ing all arguments. 
  # => If either one of the call arguments has changed, the result is 
  #      recomputed.
  args.all    <- as.list(match.call(expand.dots=FALSE))
  passed.args <- as.list(args.all[["..."]])
  if (token.eval.args) { 
    passed.args <- lapply(as.list(args.all[['...']]),eval, 
                          envir = parent.frame())
  } 
  cache.key   <- dput(passed.args)
  
  # Build subdirectory corresponding to function name
  passed.fun.name <- as.character(args.all$fun)
  
  if (verbose) {
    message('Looking up result for ',as.character(args.all[[2]]),
            ' [key:',digest::digest(cache.key),']',sep='')
  }
  
  fun.result <- NULL  # init
  
  # Try to load cache for given expressionk on the character representation
  if (!cache.ignore) {
    fun.result <- R.cache::loadCache(cache.key, dirs=passed.fun.name)
  }
  if (verbose && !is.null(fun.result)) {
    message(' -- OK\n')
  }
  
  # Compute result if no cached result is available
  if (is.null(fun.result)) {
    if (verbose) cat(' -- Nothing found\n')
    fun.result <- fun(...)
    R.cache::saveCache(fun.result, key=cache.key, dirs=passed.fun.name)
  }
  
  return(fun.result)
}


# This function returns the same function, but with memoizer enabled.
with_fcache <- function(fun) { 
  function(...) { 
    fcache(fun, ...)
  }
}
