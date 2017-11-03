
This packages provides some functions that I find useful in R. Use at your own 
risk. 
  
  * df_manipulation.R:
      colvec: create a column vector
      rowvec: create a row vector
      vec2df: transform a vector into a one-row data.frame
      ljoin: equivalent to plyr::join(a, b, type = "left", match = "first")
      isempty: return empty lines in a table (sum of line == 0)
      sdply, slply, saply: equivalent to a(d|l|a)ply(x, 1, ...)
      except: get columns with names not in the provided vector 
      autosummarise: autosummarise a data.frame
      summ_fact
      summ_nums
      do_one_col
    
  * fcache.R
      fcache: cache the result of a function
      with_fcache: run a function with caching enabled
      
  * mcplyr.R:
      mcddply, mcdlply: same than plyr but does parallel computation through 
        FORK-type clusters (unix only)
      
  * parjob.R: 
      parjob: prepare/update a cluster before computing something in parallel
  
  * random_matrices.R:
      rmat: create a matrix filled with random numbers
      rsqmat: create a square matrix filled with random numbers
      
  * sswitch.R:
      stringswitch: same as switch(...) but vectorized

