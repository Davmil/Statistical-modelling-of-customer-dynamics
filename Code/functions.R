# =============================================================================#
# Auxiliary functions: 
# =============================================================================#

is_leapyear <- function ( year ) 
{
  if ( year < 2000 ) {
    year <- year + 2000
  } 
  out <- FALSE
  if ( year %% 4   == 0 ) {
    out <- TRUE
    
    if ( year %% 100 == 0 ) {
      out <- FALSE
      
      if ( year %% 400 == 0 ) {
        out <- TRUE
      }
    }
  }
  return ( out )
}

## Transform the rows for a given clientid to 'our' new data structure:
## --------------------------------------------------------------------
## - DF: the data.frame for the computation
## - id: clientid for which the transformation will be computed
## - tolerance: boundary for the extrapolation step, if the difference
##              falls under this boundary, than the gep for a periode is 
##              complete (the tolerance should be coused by rounding effects)

transform_client <- function ( DF, id, tolerance = 1 )
{
  df <- DF[DF$clientid == id, ]
  df <- df[order(df$policystart_year), ]
  
  df <- cbind(df, period = paste( df$policystart, '-', df$policyend ) )
  
  ## if Argument austauschen!!!!
  if ( 366 %in% df$days_in_period ) {
    for ( i in 1:nrow(df) ) {
      if ( is_leapyear( as.integer(as.character( df$policystart_year[i] ) )) ) {
        df$till_newyear[i]   <- df$till_newyear[i] - 1
        df$days_in_period[i] <- df$days_in_period[i] - 1
      }
      if ( is_leapyear( as.integer(as.character( df$policyend_year[i] ) )) ) {
        df$since_newyear[i]  <- df$since_newyear[i] - 1
        df$days_in_period[i] <- df$days_in_period[i] - 1
      }
    }
  }
  
  # new data.frame:
  df_str <- data.frame( clientid             = integer(0L),
                        period               = character(0L),
                        yearofbirth          = integer(0L),
                        gender               = character(0L),
                        distribution_channel = character(0L),
                        year                 = integer(0L),
                        paid_amount          = numeric(0L),
                        gep                  = numeric(0L),
                        product_id           = character(0L),
                        plan_id              = character(0L),
                        product_id_next      = character(0L),
                        planid_next          = character(0L),
                        product_transition   = character(0L),
                        plan_transition      = character(0L),
                        policystart_day      = integer(0L),
                        policystart_mon      = character(0L),
                        policystart_year     = integer(0L),
                        nb_clients_policy    = integer(0L),
                        Dummy_loss           = logical(0L),
                        days_in_period       = integer(0L),
                        stringsAsFactors = FALSE )
  
  # Unique periods, for each period one row:
  periods <- unique( df$period )
  idx_df  <- match( periods, df$period )
  
  ## Create main structure:
  # The 'as.character' functions are required for factors to be a string
  for ( i in seq( along = periods ) ) {
    df_str[i, 'clientid']             <- id
    df_str[i, 'period']               <- as.character( periods[i] )
    df_str[i, 'yearofbirth']          <- df[i, 'yearofbirth']
    df_str[i, 'gender']               <- as.character( df[i, 'gender'] )
    df_str[i, 'distribution_channel'] <- as.character( df[i, 'distribution_channel'] )
    df_str[i, 'year']                 <- df[idx_df[i], 'year']
    df_str[i, 'product_id']           <- as.character( df[idx_df[i], 'product_id'] )
    df_str[i, 'plan_id']              <- df[idx_df[i], 'plan_id']
    df_str[i, 'product_id_next']      <- as.character( df[idx_df[i], 'product_id_next'] )
    df_str[i, 'planid_next']          <- as.character( df[idx_df[i], 'planid_next'] )
    df_str[i, 'product_transition']   <- as.character( df[idx_df[i], 'product_transition'] )
    df_str[i, 'plan_transition']      <- as.character( df[idx_df[i], 'plan_transition'] )
    df_str[i, 'policystart_day']      <- df[idx_df[i], 'policystart_day']
    df_str[i, 'policystart_mon']      <- as.character( df[idx_df[i], 'policystart_mon'] )
    df_str[i, 'policystart_year']     <- as.integer( as.character( df[idx_df[i], 'policystart_year'] ) )
    df_str[i, 'nb_clients_policy']    <- df[i, 'nb_clients_policy']
    df_str[i, 'days_in_period']       <- df[idx_df[i], 'days_in_period']
    
    ## Add features, which maybe depends on more than one row:
    
    # Index for rows whith the same periods:
    idx <- which( df$period == periods[i] )
    
    # Counts if one of the Dummys is TRUE, if one is TRUE than the overall Dummy
    # would be set to TRUE else FALSE:
    df_str[i, 'Dummy_loss'] <- ( sum( df$Dummy_loss[idx] ) > 0 )
    
    # Sum every paid amount over the rows with identically periods:
    df_str[i, 'paid_amount'] <- sum( df$paid_amount[idx] )
  }
  
  ## Now compute the gep (toughest one):
  
  # Define counter for finished rows. If one row is finished, than add 1 to the
  # counter. Escape the loop if the counter is equal the length of unique 
  # periods.
  
  counter <- 0
  i       <- counter
  
  while ( counter != length( unique(periods) ) ) {
    # i is an index for the rows in df not df_str:
    i <- i + 1
    
    # Which period are we looking at the moment:
    p <- unique( periods )[counter + 1]
    
    # Which rows in df have that period:
    idx <- which( df$period == p )
    
    # Extrapolation:
    denominator <- if ( df$till_newyear[i] %in% c(0, 1) ) { 
      df$days_in_period[i] 
    } else { 
      df$till_newyear[i] 
    }
    
    gep  <- round( df[i, 'gep'] * ( df$days_in_period[i] / denominator ), 2)
    rest <- gep - df[i, 'gep']
    
    # Becouse of sortet observations (see above) we know for length(idx) == 2
    # TRUE, that the same period must have index 2:
    if ( length(idx) == 2 ) {
      if ( abs( df$gep[i + 1] - rest ) < tolerance ) {
        df_str[counter + 1, 'gep'] <- gep
        df[i + 1, 'gep']     <- 0
        
        counter <- counter + 1
        i       <- i + 1
      } else {
        stop ( 'Check clientid ', id, ': There is a problem with the extrapolated gep for period -> ', p, ' <-.',
               ' Especially check whether or not for the two rows ', paste0(idx, collapse = ' + '),
               ' with period -> ', p, ' <- the extrapolation of the overall gep ', gep, ' with rest ', rest,
               ' falls above the tolerance boundary of -> tolerance = ', tolerance, ' < ', round( abs( df$gep[i + 1] - rest ), 2),
               ' = error due to extrapolation <-. Therefore type in "DF[DF$clientid == ', id, ', ]" to identify the problem.' )
      }
    } 
    
    # How to handle the premium if one part of it is in the next period:
    if ( length(idx) == 1 ) {
      
      ## Check, if the days in period is smaller than one year and is in one year, 
      ## than the gep can be taken over:
      if ( ( as.integer(as.character(df$policystart_year[i])) == 
             as.integer(as.character(df$policyend_year[i])) ) && 
           df$days_in_period[i] <= 365 ) {
        
        df_str[counter + 1, 'gep'] <- df$gep[i]
        
        counter <- counter + 1
      }
      
      #       ## Case, if days in period is bigger than 366:
      #       if ( ( as.integer(as.character(df$policystart_year[i])) != 
      #              as.integer(as.character(df$policyend_year[i])) ) && 
      #            df$days_in_period[i] > 366 ) {
      #         df_str[counter + 1, 'gep'] <- gep 
      #         
      #         # Check if 
      #         #if ( nrow(df) > sum(df$period %in% periods[1:(counter + 1)]) ) {
      #         if ( nrow(df) > i ) {
      #           if ( df[i + 1, 'gep'] - rest < -tolerance ) {
      #             stop ( 'Check clientid ', id, ': There is a negative value for the residual gep in row ', i + 1 )
      #           }
      #           df[i + 1, 'gep'] <- df[i + 1, 'gep'] - rest
      #         }
      #         
      #         counter <- counter + 1
      #       }
      
      
      ## This is the 'normal' case:
      if ( ( as.integer(as.character(df$policystart_year[i])) != 
             as.integer(as.character(df$policyend_year[i])) ) && 
           df$days_in_period[i] %in% c(365, 366) ) {
        df_str[counter + 1, 'gep'] <- gep
        
        # Check if 
        #if ( nrow(df) > sum(df$period %in% periods[1:(counter + 1)]) ) {
        if ( nrow(df) > i ) {
          if ( df[i + 1, 'gep'] - rest < -tolerance ) {
            stop ( 'Check clientid ', id, ': There is a negative value for the residual gep in row ', i + 1 )
          }
          df[i + 1, 'gep'] <- df[i + 1, 'gep'] - rest
        }
        
        counter <- counter + 1
      }
    }
    
    if ( ! length(idx) %in% (1:2) ) {
      stop ( 'Check clientid ', id, ': There is for one period more than two rows or less than one' )
    }
  }
  return( df_str )
} # transform_client(DF_raw,clientid)

## Transform month labels to integers:
## -----------------------------------
month_trafo <- function (mon)
{
  return( match( mon, c( 'Jan', 'Feb', 'Mrz', 'Apr', 
                         'Mai', 'Jun', 'Jul', 'Aug', 
                         'Sep', 'Okt', 'Nov', 'Dez' ) ) )
}

## Get info about the days till new year and since new year for a given period:
## ----------------------------------------------------------------------------
days_info <- function (date_start, date_end) {
  
  year <- format( date_end, format = '%Y')
  date <- as.Date( paste0(year, '-01-01') )
  
  return( data.frame( till_newyear   = as.integer( date - date_start ),
                      since_newyear  = as.integer( date_end - date + 1 ),
                      days_in_period = ( as.integer( date - date_start ) + 
                                           as.integer( date_end - date + 1 ) ) ) )
}

## Get the data for a boxplot:
## ---------------------------
getBoxData <- function (x) 
{
  IQR        <- quantile(x, 0.75) - quantile(x, 0.25)
  maxwhisker <- abs(1.5 * IQR)
  
  if ( min(x) < quantile(x, 0.25) - maxwhisker ) {
    lowerwhisker <- sort(x)[min( which( sort(x) >= quantile(x, 0.25) - maxwhisker ) )]
  } else {
    lowerwhisker <- min(x)
  }
  
  if (max(x) > quantile(x, 0.25) - maxwhisker) {
    upperwhisker <- sort(x)[max( which( sort(x) <= quantile(x,0.75) + maxwhisker ) )]
  } else {
    upperwhisker <- max(x)
  }
  
  values <- c( lowerwhisker  = lowerwhisker, 
               lowerquantile = quantile(x, 0.25), 
               median        = median(x), 
               upperquantile = quantile(x, 0.75), 
               upperwhisker  = upperwhisker )
  
  return(values)
}

## Summary a variable of DF:
## --------------------------
summary_var <- function (data = NA, var) {
  if ( ! is.na(data)[1] ) { x <- data[, var] }
  
  cat( '\n' )
  
  if ( class( x ) %in% c('integer', 'double', 'numeric') ) {
    cat( 'Class of variable: ', class( x ), '\n',
         'len. of variable:  ', length( x ), '\n\n' )
    print( summary( x ) )
  }
  if ( class( x) %in% c('factor', 'character') ) {
    cat( 'Class of variable: ', class( x ), '\n\n',
         '   len. of variable:  ', length( x ), '\n',
         '   len. of severitys: ', length( unique(x) ), '\n\n',
         '   severitys:         ', paste0( '- ', unique(x)[1], ' -', '\n\t\t\t',
                                           paste0( paste0( '- ', unique(x)[-1], ' -'), collapse = '\n\t\t\t' ) ) )
  }
  
  cat( '\n\n' )
}

## Get the rows of DF of a specific policy id:
## -------------------------------------------
get_rows_of_id <- function (policyid, col_ind = NA) {
  idx <- which( DF$policyid == policyid )
  
  if ( is.na(col_ind)[1] ) {
    return( DF[idx, ] )
  } else {
    return( DF[idx, col_ind] )
  }
}

## Draw a grid in an existing plot:
## --------------------------------

draw_grid <- function ()
{
  xaxp <- seq( par()$xaxp[1], par()$xaxp[2], length.out = par()$xaxp[3] + 1 )
  yaxp <- seq( par()$yaxp[1], par()$yaxp[2], length.out = par()$yaxp[3] + 1 )
  
  abline(h   = yaxp, 
         v   = xaxp, 
         col = rgb(100, 100, 100, 50, maxColorValue = 255), 
         lty = 2)
}

## Own pretty barplot function:
## ----------------------------

bar_own <- function (height, ...) 
{
  Args <- list( ... )
  if ( 'col' %in% names(Args) )    { stop('col is not useable') }
  if ( 'ylim' %in% names(Args) )   { stop('ylim is not useable') }
  if ( 'border' %in% names(Args) ) { stop('border is not useable') }
  
  bp <- barplot( height = height, 
                 col    = NA,
                 ylim   = c( 0, max(height) + max(height) / 10 ),
                 ... )
  
  draw_grid()
  
  barplot( height = height, 
           add    = TRUE, 
           col    = rgb(135, 206, 235, 80, maxColorValue = 255), 
           border = rgb(0, 104,	139, 255, maxColorValue = 255), 
           ... )
  box()
  
  return( invisible(bp) )
}

box_own <- function (x, ...) {
  
  Args <- list( ... )
  if ( 'col' %in% names(Args) )    { stop('col is not useable') }
  if ( 'border' %in% names(Args) ) { stop('border is not useable') }
  
  boxplot( x, ... )
  
  draw_grid()
  
  if ( is.numeric(x) ) {
    boxplot( x      = x, 
             add    = TRUE, 
             col    = rgb(135, 206, 235, 80, maxColorValue = 255), 
             border = rgb(0, 104,	139, 255, maxColorValue = 255), 
             ... )
  }
  if ( class(x) == 'formula' ) {
    boxplot( formula = x, 
             add     = TRUE, 
             col     = rgb(135, 206, 235, 80, maxColorValue = 255), 
             border  = rgb(0, 104,	139, 255, maxColorValue = 255), 
             ... )
  }
  
  box()
}

barbox_own <- function (vec, nbreaks = NA, density = FALSE, xlab = '', ylab = '', 
                        las = 1, main = '', cex.main = 2, cex.axis = 1.5, 
                        alpha.density = 150, legend.pos = 'topleft',
                        add.curve = FALSE, expr = NA, mar2 = NA, ...)
{
  if ( is.na(nbreaks) ) { 
    
    nbreaks <- length( unique(vec) ) 
    if ( nbreaks > 100 ) { nbreaks <- 100 }
  }
  
  breaks <- seq( from = min(vec),
                 to   = max(vec),
                 length.out = nbreaks )
  
  step <- breaks[2] - breaks[1]
  
  DF <- data.frame( x = vec,
                    g = cut(vec, breaks = breaks) )
  
  height <- table(DF$g)
  boxdat <- getBoxData(vec)
  
  if ( density ) { 
    height <- height / ( length(vec) * step )
    dens   <- density(vec)
  }
  
  Args <- list( ... )
  
  if ( 'xlim' %in% names(Args) ) {
    xlim <- Args$xlim
  } else {
    xlim <- range(vec) + c(-1, 1) * diff(range(vec)) / 40
  }
  
  if ( 'ylim' %in% names(Args) ) {
    ylim <- Args$ylim
  } else {
    ylim <- c( 0, max(height) + max(height) / 10 )
  }
  
  
  
  layout( mat = matrix(c(1,1,1,2,2)) )
  mar_def <- c(5.1, 4.1, 4.1, 2.1)
  
  if ( is.na(mar2) ) {
    par( mar = c(0, mar_def[2:4]) ) 
  } 
  if ( is.numeric(mar2) && length(mar2) == 1 ) {
    par( mar = c(0, mar2, mar_def[3:4]) )
  } else {
    par( mar = c(0, mar_def[2:4]) ) 
  }
  
  plot( x    = NA,
        xlim = xlim,
        ylim = ylim,
        col  = 'white',
        axes = FALSE,
        xlab = '',
        ylab = ylab,
        main = main, 
        cex.main = cex.main )
  
  xaxp <- seq( par()$xaxp[1], par()$xaxp[2], length.out = par()$xaxp[3] + 1 )
  yaxp <- seq( par()$yaxp[1], par()$yaxp[2], length.out = par()$yaxp[3] + 1 )
  
  abline(h   = yaxp, 
         v   = xaxp, 
         col = rgb(100, 100, 100, 50, maxColorValue = 255), 
         lty = 2)
  
  for ( i in seq(along = height) ) {
    rect( xleft   = breaks[i],
          xright  = breaks[i + 1],
          ybottom = 0,
          ytop    = height[i],
          col     = rgb(135, 206, 235, 80, maxColorValue = 255), 
          border  = rgb(0, 104,	139, 255, maxColorValue = 255) )
  }
  
  if ( density ) {
    lines( x = dens$x,
           y = dens$y,
           lwd = 2, 
           col = rgb(178, 34, 34, alpha.density, maxColorValue = 255) )
    
    legend( legend.pos, 
            legend    = 'kernel density estimator', 
            lty       = 1, 
            lwd       = 2, 
            col       = rgb(178, 34, 34, alpha.density, maxColorValue = 255), 
            bg        = rgb(100, 100, 100, 10, maxColorValue = 255), 
            box.col   = NA,
            cex       = cex.axis )
  }
  
  axis( side     = 2, 
        at       = yaxp,
        labels   = yaxp,
        las      = las, 
        cex.axis = cex.axis )
  
  if ( add.curve ) {
    if ( is.na(expr) ) {
      warning ( 'No expression for curve is given' )
    } else {
      
      Args <- list( ... )
      if ( length(Args) > 0 ) {
        if ( 'add' %in% names(Args) ) {
          Args$add <- TRUE
        } else {
          Args <- c( Args, add = TRUE ) 
        }
      } else {
        Args <- list( add = TRUE )
      }
      
      chars <- character(length(Args))
      for ( i in seq( along = Args ) ) {
        if ( is.character(Args[[i]]) ) {
          chars[i] <- paste0( names(Args)[i], " = '", Args[i], "'" )
        } else {
          chars[i] <- paste0( names(Args)[i], ' = ', Args[i])
        }
      }
      str_eval <- paste0( 'curve(', expr, ', ',
                          paste0( chars, collapse = ', '), 
                          ')' )
      eval( parse( text = str_eval ) )
    }
  }  
  box()
  
  if ( is.na(mar2) ) {
    par( mar = c(mar_def[1:2], 0, mar_def[4]) )
  } 
  if ( is.numeric(mar2) && length(mar2) == 1 ) {
    par( mar = c(mar_def[1], mar2, 0, mar_def[4]) )
  } else {
    par( mar = c(mar_def[1:2], 0, mar_def[4]) )
  }
  
  plot( x    = NA,
        xlim = xlim,
        ylim = c(0, 2),
        col  = 'white',
        axes = FALSE,
        xlab = xlab,
        ylab = '',
        main = '',
        cex.main = cex.main )
  
  axis( side     = 1,
        at       = xaxp,
        labels   = xaxp, 
        cex.axis = cex.axis )
  
  yaxp <- seq( par()$yaxp[1], par()$yaxp[2], length.out = par()$yaxp[3] + 1 )
  
  abline(h   = yaxp, 
         v   = xaxp, 
         col = rgb(100, 100, 100, 50, maxColorValue = 255), 
         lty = 2)
  
  ## Boxplot:
  rect( xleft   = boxdat[2],
        xright  = boxdat[4],
        ybottom = 0.25,
        ytop    = 1.75,
        col     = rgb(135, 206, 235, 80, maxColorValue = 255), 
        border  = rgb(0, 104,	139, 255, maxColorValue = 255) )
  
  segments( x0  = boxdat[3],
            x1  = boxdat[3],
            y0  = 0.25,
            y1  = 1.75,
            lwd = 2,
            col = rgb(0, 104,	139, 255, maxColorValue = 255) )
  
  segments( x0 = boxdat[1],
            x1 = boxdat[2],
            y0 = 1,
            y1 = 1,
            lwd = 2,
            col = rgb(0, 104,	139, 255, maxColorValue = 255) )
  
  segments( x0 = boxdat[4],
            x1 = boxdat[5],
            y0 = 1,
            y1 = 1,
            lwd = 2,
            col = rgb(0, 104,	139, 255, maxColorValue = 255) )
  
  segments( x0  = boxdat[c(1,5)],
            x1  = boxdat[c(1,5)],
            y0  = 0.5,
            y1  = 1.5,
            lwd = 2,
            col = rgb(0, 104,	139, 255, maxColorValue = 255) )
  
  points( x   = vec[vec < boxdat[1] | vec > boxdat[5]],
          y   = rep(1, sum(vec < boxdat[1] | vec > boxdat[5]) ),
          pch = 4 )
  
  box()
  
  par( mfrow = c(1,1),
       mar   = mar_def )
}



## Set Termination:
## ----------------
# 
# 
# canceled_policy <- function(DF, id){
#   DF$canceled_after_period <- 
# }

