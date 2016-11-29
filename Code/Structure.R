# =============================================================================#
# Restructure de data.frame:
# =============================================================================#



## Function for parallel execution:
## --------------------------------

## ancillary function:
paste_df <- function(DF, ids, tolerance = 1)
{
  k   <- 1
  kdx <- c()
  
  e <- ''
  class(e) <- 'try-error'
  
  while ( class(e) == 'try-error' ) {
    e <- try( expr = { help <- transform_client( DF = DF, 
                                                 id = ids[1], 
                                                 tolerance = tolerance ) },
              silent = TRUE )
    kdx[k] <- k
    if ( k > length(ids) ) { stop( 'No id for transformation available' ) }
  }
  
  if ( length(ids) > 1 ) {
    k <- 1
    pb <- tcltk::tkProgressBar( "Parallel task", min = 1, max = length(ids[-1]),)
    
    for ( i in ids[-kdx] ) {
      try( { help <- rbind( help,
                            transform_client( DF = DF, 
                                              id = i, 
                                              tolerance = tolerance) ) },
           silent = TRUE )
      tcltk::setTkProgressBar( pb = pb, value = k )
      k <- k + 1
    }
    close( pb )
  }
  return( help )
}

debug_client <- function ( DF, id, tolerance = 1, debug = TRUE ) 
{
  e <- try( expr   = { transform_client(DF = DF, id = id, tolerance = tolerance) },
            silent = TRUE )
  
  if ( class(e) == 'try-error' ) {
    cat( e[1], '\n' )
    
    df <- DF[DF$clientid == id, ]
    df <- df[order(df$policystart_year), ]
    
    df <- cbind(df, period = paste( df$policystart, '-', df$policyend ) )
    
    print( df )
    
    if ( debug ) { debug(transform_client(DF = DF, id = id, tolerance = tolerance)) }
  }
}

# ### TEST:
# ### -----
# set.seed( pi )
# ids <- sample(unique(DF$clientid), 1000)
# 
# test <- paste_df( DF, ids = ids, tolerance = 2 )
# 
# # For which ids fails the algorithm:
# sum( ! ids %in% test$clientid )
# ids_check <- ids[! ids %in% test$clientid]
# 
# id_check <- ids_check[2]
# debug_client( DF, id_check, tolerance = 3.5, debug = FALSE )
# transform_client( DF, id_check, tolerance = 3.5 )

## Main restructure function:
Restructure <- function ( DF, tolerance = 1, cores = parallel::detectCores() ) 
{
  if ( ! 'clientid' %in% names(DF) ) { stop( 'There is no clientid in DF' ) }
  
  cl <- parallel::makeCluster( cores )
  doParallel::registerDoParallel( cl )
  
  clientid <- unique( DF$clientid )
  n        <- floor( length(clientid) / cores )
  
  rest     <- length(clientid) - cores * n
  
  list_ind <- list()
  
  list_ind[[1]] <- if ( rest > 1 ) { 1:(n + 1) } else { 1:n }
  
  if ( cores > 1 ) {
    for ( i in 2:cores ) {
      n_last <- max( list_ind[[i - 1]] ) + 1
      list_ind[[i]] <- if ( i <= rest) { n_last:(n_last + n) } else { n_last:(n_last + n - 1) }
    }
  }
  
  list_clid <- list()
  for ( i in seq(along = list_ind) ) {
    list_clid[[i]] <- clientid[list_ind[[i]]]
  }
  time <- proc.time()
  out <- foreach::foreach( I = list_clid, 
                           .combine = rbind, 
                           .packages = 'tcltk',
                           .export  = c('paste_df', 'transform_client') ) %dopar% { 
                             paste_df( DF = DF, ids = I, tolerance = tolerance ) 
                           }
  time <- proc.time() - time
  
  cat( 'Computation takes: ', time[3], ' Seconds')
  
  parallel::stopCluster( cl )
  
  return( out )
}


