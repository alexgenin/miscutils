# 
# This file sets up R for multicore computing. Note that it uses variables in
# its enclosure.
# 
# Parjob: returns TRUE if conditions are met to run a parallel job, and set them
# up if required.
# 
# Example:
# 
# Use ddply with all the machine cores: 
#   ddply(df, ~ var, fun, .parallel = parjob()) 
# 
# 
#'@export
parjob <- function(cores = detectCores()-1, # Number of local cores asked for
                   reset = FALSE,           # reset the local cluster before launching
                   supdir = NULL,           # other directories to scan for devtools packages 
                   export_parent = TRUE,    # export all variables found in parent evir
                   export_envs = list(),    # export vars in those environments 
                   outfile = "/dev/null",   # log file for workers' stdout
                   cachedir = paste0(wd, ".cache"), 
                   wd    = getwd()) {       # working directory 

  # Load libraries
  if ( !require(foreach) || !require(doParallel) || !require(devtools) ) { 
    stop("parjob requires foreach, doParallel and devtools")
  }
  
  # Init variables if non-existent
  if ( ! exists('.LOCALCLUST') ) { 
    .LOCALCLUST <<- list()
  }
  
  # Destroy a cluster
  unregister <- function() { 
    if ( cl_ncores(.LOCALCLUST) >= 2) stopCluster(.LOCALCLUST)
    .LOCALCLUST <<- list()
    registerDoSEQ() # register the sequential backend to foreach
    message("Stopped cluster")
  }
  
  # Register a cluster
  register <- function() { 
    .LOCALCLUST <<- makeCluster(cores, outfile = outfile) # oh my <<- in the closure env
    registerDoParallel(.LOCALCLUST) # register parallel backend to foreach
    message("Started local cluster with ", cores, " cores")
  }
  
  # Test whether cluster is on
  cl_ncores <- function(cl) length(cl)
  
  # Test whether cluster is functional or whether it produces errors
  if ( cl_ncores(.LOCALCLUST) >= 2 ) {
    test_result <- try(clusterEvalQ(.LOCALCLUST, TRUE), silent=TRUE)
    if ( inherits(test_result,"try-error") ) { 
      message("Shutting down broken cluster")
      
      slaves <- system('pgrep -f -u "$USER" "/usr/lib/R/bin/exec/R --slave"', 
                       intern = TRUE)
      for (slave in slaves) { 
        message('Killing process ', slave)
        system(paste("kill",slave))
      }
      closeAllConnections() # this can interfere with knitr
      .LOCALCLUST <<- list()
    }
  }
  
  # Operating part 
  # -----------------------------------------
  
  if (reset && cl_ncores(.LOCALCLUST) >= 2) { 
    unregister()
  }
  
  # Bail if non-parallel computation, but keep cluster status unchanged
  if ( cores <= 1 ) { 
    return(FALSE)
  
  } else { 
    
    # We reset if we changed the number of cores (and the cluster has not been 
    # resetted already)
    if ( cl_ncores(.LOCALCLUST) != cores ) { 
      unregister()
      register()
    }
    
    # Get to current directory in workers
    clusterCall(.LOCALCLUST, setwd, wd)
    
    # Reload current package in workers if appropriate
    for (d in c(getwd(), supdir) ) { 
      if ( 'DESCRIPTION' %in% dir(d) ) { 
        clusterCall(.LOCALCLUST, document, pkg = d)
      }
    }
    
    # Export packages
    pkgs <- .packages()
    
    # Remove devtools-loaded package from the list if any was given
    if ( length(supdir) > 0 ) { 
      is_devtools_loaded <- sapply(pkgs, function(x) grepl(x, supdir))
      pkgs <- pkgs[!is_devtools_loaded]
    }
    
    for (pkg in pkgs) { 
      # This can fail if a package is loaded via document() from a different 
      # directory. 
      load_try <- try(clusterCall(.LOCALCLUST, library, pkg, character.only = TRUE), 
                      silent = TRUE)
      if ( class(load_try) == "try-error" ) { 
        message('Could not load package ', pkg)
      }
    }
    
    # Export DLLs loaded 
    dlls <- getLoadedDLLs()
    for (name in names(dlls)) { 
      # If not a library DLL, then export it to workers
      dll.path <- dlls[name][[1]][['path']]
      in_pkg_regexp <- '/usr/lib(64|)/|base|/R/.*-library/'
      if ( ! grepl(in_pkg_regexp, dll.path) ) { 
        message('Loading extra library ', dll.path)
        clusterCall(.LOCALCLUST, dyn.load, dll.path)
      }
    }
    
    # Make a list of envirs to export
    if ( !is.list(export_envs) ) { 
      export_envs <- list(export_envs)
    }
    if ( export_parent ) { 
      export_envs <- c(export_envs, parent.frame())
    }
    # Export environments 
    for ( env in export_envs ) { 
      vars <- ls(envir = env)
      clusterExport(.LOCALCLUST, varlist = vars, envir = parent.frame())
    }
    
    return(TRUE)
  }
}
