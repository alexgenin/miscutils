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
                     .progress = "none", 
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
  
  if ( .progress == "none" ) { 
    .progress_every <- length(tasks)
  }
  
  # Save multicore options 
  old_.ompcores <- Sys.getenv("OMP_NUM_THREADS")
  Sys.setenv(OMP_NUM_THREADS = .ompcores)
  on.exit(Sys.setenv(OMP_NUM_THREADS = old_.ompcores))
  
  # Set psock options 
  psock_ok <- do.call(parjob, c(list(cores = .psockcores), .parjob_args))
  if (psock_ok) { 
    clusterExport(cl = .LOCALCLUST, varlist = "fun", envir = parent.frame())
  }
  
  # Split up list 
  groups <- floor( seq_along(tasks) / ( (1+length(tasks)) / .progress_every) )
  tasksplit <- llply(as.list(unique(groups)), function(grp) { 
    tasks[groups == grp]
  })
  
  # Compute
  all_results <- llply(tasksplit, function(task) { 
    foreach(o = task) %dopar% { 
      fun(o,...)
    }
  }, .progress = .progress)
  
  # Flatten list 
  all_results <- do.call(c, all_results)
  
  return(all_results)
}


#'@export 
run_simus <- function(simu_plan, simu_fun) { 
  
  browser()
  
}
