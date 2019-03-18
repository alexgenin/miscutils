# 
# Advanced plyr compatible with 
# 

#'@importFrom parallel detectCores
#'@import plyr 

#'@export
advllply <- function(tasks, .fun, 
                     .parspec = paste0(.psockcores, "x", .ompcores), 
                     .ompcores   = 1, 
                     .psockcores = detectCores(), 
                     .exceed_cores = FALSE, 
                     .progress = "time", 
                     .progress_every = .psockcores, 
                     .parjob_args = list(), 
                     .parallel = FALSE, 
                     ...) { 
  
  # Parse parallel specification
  .psockcores <- as.numeric(sub("x.*$", "", .parspec))
  .ompcores   <- as.numeric(sub('^.*x', "", .parspec))
  
  # Sanity check 
  maxcores <- detectCores()
  if ( !.exceed_cores && (.ompcores * .psockcores > maxcores) ) { 
    stop('More cores than available requested ! ', 
         'Set .exceed_cores to TRUE to disable this check')
  }
  
  # Save multicore options 
  old.ompcores <- Sys.getenv("OMP_NUM_THREADS")
  Sys.setenv(OMP_NUM_THREADS = .ompcores)
  on.exit(Sys.setenv(OMP_NUM_THREADS = old.ompcores))
  
  # Set psock options 
  psock_ok <- do.call(parjob, c(list(cores = .psockcores), .parjob_args))
  if (psock_ok) { 
    clusterExport(cl = .LOCALCLUST, varlist = ".fun", 
                  envir = environment())
  }
  
  # Split up list into groups for progress reporting
  if ( .progress == "none" ) { 
    groups <- rep(0, length(tasks))
  } else { 
    groups <- floor( seq_along(tasks) / ( (1+length(tasks)) / .progress_every) )
  }
  tasksplit <- llply(as.list(unique(groups)), function(grp) { 
    tasks[groups == grp]
  })
  
  # Compute
  parop <- if ( psock_ok ) { `%dopar%` } else { `%do%` }
  all_results <- llply(tasksplit, function(task) { 
    parop(foreach(o = task), { 
      .fun(o,...)
    })
  }, .progress = .progress)
  
  # Flatten list 
  all_results <- do.call(c, all_results)
  
  return(all_results)
}

