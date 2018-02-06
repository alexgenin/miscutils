# 
# This file contains accessory functions to run simulations 
# 

#'@export
make_simuplan <- function(list, varying = NULL) { 
  
  #TODO: add quoted parameters
  
  # Create combinations of varying params
  nelems <- lapply(list[varying], function(x) seq.int(length(x)))
  all_combi <- do.call(expand.grid, nelems)
  simuplan <- apply(all_combi, 1, function(simuline) { 
      pars <- lapply(as.list(varying), function(x) { 
          # This is a bit cryptic but that selects the corresponding elements 
          # in the input list as specified by simuline
          list[[x]][[simuline[x]]]
        })
      names(pars) <- varying 
      return(pars)
    })
  
  # Now we add to all simulations the fixed the parameters 
  simuplan <- lapply(simuplan, function(varpars) { 
      c(varpars, list[!names(list) %in% varying])
    })
  
  # Print info 
  simu_number_string <- Map(function(n, l) paste0(l, "[", n, "]"), 
                            varying, lapply(list[varying], length))
  simu_number_string <- paste0(simu_number_string, collapse = " x ")
  simu_number_string <- paste0(simu_number_string, " = ", length(simuplan), " runs")
  message('Created simu plan: ', simu_number_string)
  
  class(simuplan) <- c('simu_plan', 'list')
  return(simuplan)
}

#'@export 
runsim.simu_plan <- function(simlist, simfun, 
                             .check = FALSE, 
                             .ignore_extra = FALSE, 
                             ...) { 
  
  # Check that simulation/function args match
  browser()
  all_argnames <- as.list(args(simfun))
  argnames <- names(Filter(is.symbol, all_argnames))
  all_argnames <- names(all_argnames)[-length(all_argnames)]
  
  # All elements in simlist have the same name if it has the class simu_plan, 
  # so we just pick the first one. 
  plan_names <- names(simlist[[1]])
  if ( !.ignore_extra & ! all(plan_names %in% all_argnames) ) { 
      extra_names <- plan_names[ ! plan_names %in% all_argnames]
      stop('Extra parameters in the simulation plan: ', 
           paste0(extra_names, collapse = ", "))
  }
  
  if ( ! all(argnames %in% plan_names) ) { 
      missing_names <- argnames[ ! argnames %in% plan_names ]
      stop('Following parameters are missing in the simulation plan: ', 
           paste0(missing_names, collapse = ", "))
  }
  browser()
  advllply(simlist, simfun, ...)
  
#   advllply(simlist, 
  
}
