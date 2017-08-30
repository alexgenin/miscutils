# 
# 
# This file contains many functions that are shortcuts to things. 
# 


# DATA TRANSFORMATION 
# -------------------

# Column/Row vectors
#'@export
colvec <- function(values) { 
  matrix(values, ncol = 1, nrow = length(values))
}
#'@export
rowvec <- function(values) { 
  matrix(values, ncol = length(values), nrow = 1)
}

# Transform a vector to a data.frame
#'@export
vec2df <- function(X) { 
  as.data.frame(as.list(X))
}

# Left join 
#'@export
ljoin <- function(a, b, ...) { 
  plyr::join(a, b, type = "left", match = "first")
}


# ORDINATION TOOLS 
# ----------------

# Returns empty sites
#'@export
isempty <- function(table) { 
  apply(table, 1, sum) == 0
}


# PLYR SHORTCUTS
# --------------
#'@export
sdply <- function(vec, ...) { 
  adply(colvec(vec), 1, ...)
}
#'@export
slply <- function(vec, ...) { 
  alply(colvec(vec), 1, ...)
}
#'@export
saply <- function(vec, ...) { 
  aaply(colvec(vec), 1, ...)
}


# SUBSETTING FUNCTIONS 
# --------------------
#'@export
except <- function(df, ...) { 
  to_remove <- do.call(c, list(...))
  df[ , !names(df) %in% to_remove, drop = FALSE]
}


# SUMMARIZING FUNCTIONS 
# ---------------------
#'@export
autosummarise <- function(df, formula, 
                          summ_fact = 'first', 
                          drop_var_fact = TRUE, 
                          summ_nums = 'mean', 
                          na.rm = FALSE, 
                          ...) { 
  
  if ( summ_fact == 'first' ) { 
    summ_fact <- function(X) X[1]
  }
  
  if ( summ_nums == "mean" ) { 
    summ_nums <- function(X) mean(X, na.rm = na.rm)
  }
  
  to_drop <- c()
  
  do_one_col <- function(X) { 
    if ( is.numeric(X) | is.logical(X) ) { 
      return( summ_nums(X) )
    } else { 
      return( summ_fact(X) )
    }
  }
  
  get_varfacts <- function(df) { 
    unlist( lapply(df, function(X) length(unique(X)) > 1 & !(is.logical(X) | is.numeric(X)) ) )
  }
  
  tab <- plyr::dlply(df, formula, function(df) { 
    list(values = as.data.frame(lapply(df, do_one_col)), 
         todrop = get_varfacts(df))
  }, ...)
  
  if ( drop_var_fact ) { 
    # Get column names to drop
    possible_drops <- do.call(rbind, lapply(tab, 
                                            function(o) as.vector(o[['todrop']])))
    to_keep <- names(df)[!apply(possible_drops, 2, all)] # here it should be any() really
    
    message('Dropping cols: ', paste(names(df)[! names(df) %in% to_keep], 
                                    collapse = ", "))
  } else { 
    to_keep <- names(df) # all cols
  }
  
  tab <- ldply(tab, function(o) o[['values']])[ ,to_keep]
  return(tab)
}


