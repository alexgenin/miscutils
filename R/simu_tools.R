# 
# This file contains accessory functions to run simulations 
# 

#@import plyr

#'@export
make_simuplan <- function(parlist, varying = NULL) { 
  # Resolve varying params in the context of the list
  curenvir <- environment()
  
  # Classify vars 
  allvars <- names(parlist)
  fixed <- allvars[!( allvars %in% varying) & ! sapply(parlist, is.formula)]
  quoted <- setdiff(allvars, c(fixed, varying))
  
  message('Resolving: ', paste0(varying, collapse = ", "))
  for ( var in varying ) { 
    if ( is.formula(parlist[[var]]) ) { 
      parlist[[var]] <- eval.quoted(as.quoted(parlist[[var]]), 
                                    envir = parlist, 
                                    enclos = curenvir)[[1]] # get first element, hack ?
    }
  } 
  
  # Create combinations of varying params
  message('Creating parameter combinations using ', 
          paste0(varying, collapse = ", "))
  nelems <- lapply(parlist[varying], function(x) seq.int(length(x)))
  all_combi <- do.call(expand.grid, nelems)
  simuplan <- apply(all_combi, 1, function(simuline) { 
      pars <- lapply(as.list(varying), function(x) { 
          # This is a bit cryptic but that selects the corresponding elements 
          # in the input parlist as specified by simuline
          parlist[[x]][[simuline[x]]]
        })
      names(pars) <- varying 
      return(pars)
    })
  
  # Now we add to all simulations the fixed parameters 
  message('Adding fixed parameters ', paste0(fixed, collapse = ", "))
  simuplan <- lapply(simuplan, function(varpars) { 
      c(varpars, parlist[fixed])
    })
  
  # Now we add to all simulations the quoted parameters 
  message('Resolving ', paste0(quoted, collapse = ", "))
  simuplan <- lapply(simuplan, function(pars) { 
      for (var in quoted) { 
        pars[[var]] <- eval.quoted(as.quoted(as.character(parlist[[var]])[[2]]), 
                                   envir = pars, 
                                   enclos = curenvir)[[1]]
      }
      return(pars)
    })
  
  # Print info 
  simu_number_string <- Map(function(n, l) paste0(l, "[", n, "]"), 
                            varying, lapply(parlist[varying], length))
  simu_number_string <- paste0(simu_number_string, collapse = " x ")
  simu_number_string <- paste0(simu_number_string, " = ", length(simuplan), " runs")
  message('Created simu plan: ', simu_number_string)
  
  class(simuplan) <- c('simu_plan', 'list')
  return(simuplan)
}

#'@export 
runsim.simu_plan <- function(simlist, simfun, 
                             .check_strict = TRUE, 
                             .checkargs_fun = simfun, 
                             ...) { 
  
  # Check that simulation/function args match
  all_argnames <- as.list(args(.checkargs_fun))
  argnames <- names(Filter(is.symbol, all_argnames))
  all_argnames <- names(all_argnames)[-length(all_argnames)]
  has_dots <- "..." %in% names(as.list(args(.checkargs_fun)))
  
  # All elements in simlist have the same name if it has the class simu_plan, 
  # so we just pick the first one. 
  plan_names <- names(simlist[[1]])
  extra_names <- NULL
  if ( ! all(plan_names %in% all_argnames) ) { 
      extra_names <- plan_names[ ! plan_names %in% all_argnames]
      msg <- paste0('Extra parameters in the simulation plan: ', 
                    paste0(extra_names, collapse = ", "))
      if (has_dots) { 
        msg <- paste0(msg, "\n", 
                      'They will be passed to the simulation function call')
      } else { 
        msg <- paste0(msg, "\n", 
                      'They will be stripped from the simulation function call')
      }
      
      if ( .check_strict ) stop(msg) else warning(msg)
  }
  
  if ( ! all(argnames %in% plan_names) ) { 
      missing_names <- argnames[ ! argnames %in% plan_names ]
      msg <- paste0('Following parameters are missing in the simulation plan: ', 
                    paste0(missing_names, collapse = ", "))
      if ( .check_strict ) stop(msg) else warning(msg)
  }
  
  # Strip extra arguments from list 
  if ( ! has_dots && !is.null(extra_names) ) { 
    simlist <- lapply(simlist, function(o) o[ ! names(o) %in% extra_names ])
  }
  
  # Modify the function a bit so it works with llplyQ
  simfunlist <- function(list) { 
    do.call(simfun, list)
  }
  
  advllply(simlist, simfunlist, ...)
  
#   advllply(simlist, 
  
}
