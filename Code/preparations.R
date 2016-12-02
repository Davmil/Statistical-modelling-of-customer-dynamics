# =============================================================================#
# Basic data preparations:
# =============================================================================#

DF$product_id      <- as.factor( DF$product_id )
DF$product_id_next <- as.factor( DF$product_id_next )

##### !!!
##### !!!
##### !!!
## New variables for transitions:
DF <- cbind( DF, 
             product_transition = paste0( DF$product_id, ' -> ', DF$product_id_next ),
             plan_transition    = paste0( DF$plan_id,    ' -> ', DF$planid_next ) )

## Set factors new due to level 'End' which was deleted but still stands 
## exists as level:
for ( i in names(DF) ) {
  if ( is.factor(DF[, i]) ) {
    DF[, i] <- as.factor( as.character( DF[, i] ) )
  }
}

rm( i )

## new variables for policystart/policyend decomposition:
help <- strsplit(as.character(DF$policystart), split = ' ')
DF <- cbind( DF,
             policystart_day  = as.integer( substring( text  = unlist( lapply( help, FUN = function(x){ x[1] } ) ),
                                                       first = 1,
                                                       last  = 2 ) ),
             policystart_mon  = unlist( lapply( help, FUN = function(x){ x[2] } ) ),
             policystart_year = unlist( lapply( help, FUN = function(x){ x[3] } ) ) )

help <- strsplit(as.character(DF$policyend), split = ' ')
DF <- cbind( DF,
             policyend_day  = as.integer( substring( text  = unlist( lapply( help, FUN = function(x){ x[1] } ) ),
                                                     first = 1,
                                                     last  = 2 ) ),
             policyend_mon  = unlist( lapply( help, FUN = function(x){ x[2] } ) ),
             policyend_year = unlist( lapply( help, FUN = function(x){ x[3] } ) ) )

rm( help )

## new variables for policystart_next/policyend_next decomposition:
help <- strsplit(as.character(DF$policystart_next), split = ' ')
DF <- cbind( DF,
             policystart_next_day  = as.integer( substring( text  = unlist( lapply( help, FUN = function(x){ x[1] } ) ),
                                                       first = 1,
                                                       last  = 2 ) ),
             policystart_next_mon  = unlist( lapply( help, FUN = function(x){ x[2] } ) ),
             policystart_next_year = unlist( lapply( help, FUN = function(x){ x[3] } ) ) )

help <- strsplit(as.character(DF$policyend_next), split = ' ')

DF$date_start_next = as.Date( paste( DF$policystart_next_day, 
                             month_trafo(DF$policystart_next_mon), 
                             DF$policystart_next_year ), 
                      format = '%d %m %y')



# drop all rows with policy end == 16
DF <- DF[-which( (DF$product_id_next == 'End' | DF$planid_next == 'End') & (DF$policyend_year == '16' | DF$policystart_year == '16')), ]



## Merge number of persons in one policy by the policyid:
DF_policys <- DF %>%  
  dplyr::group_by( policyid ) %>%
  dplyr::summarise( nb_clients_policy = length( unique(clientid) ) )

DF <- dplyr::left_join( x  = DF,
                        y  = DF_policys,
                        by = 'policyid' )

## Merge if there is a loss in one year:
DF <- cbind( DF, Dummy_loss = (DF$paid_amount > 0) )

## Merge how much months till new year since policystart:
DF <- cbind( DF, month_since_ps = 12 - month_trafo( DF$policystart_mon ) + 1 )

## Merge how much months are left since new year till policystart:
DF <- cbind( DF, month_till_ps = 12 - DF$month_since_ps)

## Merge dates as real Date variables:
DF <- cbind( DF, 
             date_start = as.Date( paste( DF$policystart_day, 
                                          month_trafo(DF$policystart_mon), 
                                          DF$policystart_year ), 
                                   format = '%d %m %y'),
             date_end = as.Date( paste( DF$policyend_day, 
                                        month_trafo(DF$policyend_mon), 
                                        DF$policyend_year ), 
                                 format = '%d %m %y') )

## Merge days for every row:
DF <- cbind( DF,
             days_info( date_start = DF$date_start,
                        date_end   = DF$date_end ) )

DF_dubs <- DF %>%  
  dplyr::group_by( clientid ) %>%
  dplyr::summarise( nb_policys_clients = length( unique(policyid) ) )


## Add variable days-between-period
DF$days_between_periods = as.integer(
  difftime( DF$date_start_next, DF$date_end,
            units = 'days' ) ) - 1

DF[ , !names(DF) %in% c("policystart_next_day", "policystart_next_mon", "policystart_next_year")]


## Delete clients with gep 0:
DF <- DF[! DF$clientid %in% unique( DF$clientid[DF$gep == 0] ), ]

## Delete clients with periods > 366 (just ~2700). They mostly made problems:
## e.g.:
# DF[DF$clientid == 1180912, ]
# transform_client(DF, 1180912)
# 
# DF[DF$clientid == 1181096, ]
# transform_client(DF, 1181096)

ids_367 <- DF$clientid[DF$days_in_period > 366]

DF <- DF[! DF$clientid %in% ids_367, ]

rm( ids_367 )
