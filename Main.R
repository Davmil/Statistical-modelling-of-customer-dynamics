# =============================================================================#
# Consulting: Munich Re:
# =============================================================================#

## Packages:
## ---------

library( magrittr )   # Faster access to data manipulation
library( doParallel ) # Access to parallelization for computational intense tasks

## Data:
## -----

if ( dir.exists( 'C:/Users/Daniel' ) ) {
  path <- 'C:/Users/Daniel/OneDrive/Dokumente/ProjektAktuell/Munich Re Consulting/'
}
if ( dir.exists( 'C:/Users/Media' ) ) {
  path <- 'C:/Users/Media/OneDrive/Dokumente/ProjektAktuell/Munich Re Consulting/'
}
if ( dir.exists( 'Z:/Consulting' ) ) {
  path <- 'Z:/Consulting/'
}
if ( dir.exists( 'C:/Users/milewskid/Documents/Consulting' ) ) {
  path <- 'C:/Users/milewskid/Documents/Consulting/'
}



## Read data:
if ( file.exists( paste0( path, 'Data/Movement_csv.csv') ) ) {
  DF <- read.csv2( file   = paste0( path, 'Data/Movement_csv.csv'),
                   header = TRUE )
}
if ( file.exists( paste0( path, 'data/Movement.csv') ) ) {
  DF <- read.csv2( file   = paste0( path, 'data/Movement.csv'),
                   header = TRUE )
}
s
source( paste0( path, 'Code/functions.R' ) )

# -----------------------------------------------------------------------------#
# Data preparation:
# -----------------------------------------------------------------------------#

source( paste0( path, 'Code/preparations.R' ) )

## ----------------------------------------------------------------------------#
## Create new data.frame with new structure:
## ----------------------------------------------------------------------------#

# Play with the tolerance: minimum of 2
#

source( paste0( path, 'Code/Structure.R' ) )

# DF_str <- Restructure( DF, tolerance = 2 )
# 
# save( file = paste0(path, 'DF.RData'),
#       list = 'DF' )
# 
# idx <- DF_str$clientid[ DF_str$gep > 5300 | DF_str$gep < 10 ]
# 
# DF_str <- DF_str[! DF_str$clientid %in% idx, ]
# 
# DF_str <- cbind( DF_str, gep_annual = DF_str$gep / DF_str$days_in_period * 365 )
# 
# DF_str <- DF_str[! DF_str$gep_annual > 5300, ]
# 
# rm( idx )
# 
# save( file = paste0(path, 'DF_str.RData'),
#       list = 'DF_str' )

load( file = paste0(path, 'DF_str.RData') )

DF_o   <- read.csv2( file   = paste0( path, 'data/Movement.csv'),
                        header = TRUE )
DF_raw <- DF
DF     <- DF_str

rm(DF_str)

## Missing Distribution Channel -> Broker
## --------------------------------------
DF[DF$distribution_channel=="","distribution_channel"] <- "Broker"

# ## Get policyends as date format:
# ## ------------------------------
help <- strsplit( unlist( strsplit( DF$period, split = ' - ' ) )[seq( from = 2, to = 2 * nrow(DF), by = 2)],
                  split = ' ' )

policyend_day  <- as.integer( substring( text  = unlist( lapply( help, FUN = function(x){ x[1] } ) ),
                                         first = 1,
                                         last  = 2 ) )
policyend_mon  <- unlist( lapply( help, FUN = function(x){ month_trafo( x[2] ) } ) )
policyend_year <- unlist( lapply( help, FUN = function(x){ as.integer( x[3] ) + 2000 } ) )

policyends <- as.Date( paste( policyend_year, 
                              policyend_mon, 
                              policyend_day, 
                              sep = '-' ) )

# ## Get next policystart as date format:
# ## ------------------------------------
# help <- strsplit( DF$policystart_next, split = ' ' )
# 
# policystart_next_day  <- as.integer( substring( text  = unlist( lapply( help, FUN = function(x){ x[1] } ) ),
#                                                 first = 1,
#                                                 last  = 2 ) )
# policystart_next_mon  <- unlist( lapply( help, FUN = function(x){ month_trafo( x[2] ) } ) )
# policystart_next_year <- unlist( lapply( help, FUN = function(x){ as.integer( x[3] ) + 2000 } ) )
# 
# policystart_nexts <- as.Date( paste( policystart_next_year, 
#                                      policystart_next_mon, 
#                                      policystart_next_day, 
#                                      sep = '-' ) )
# 
# ## Paste the day between periods on the data:
# ## ------------------------------------------
# 
# DF <- cbind( DF, 
#              days_between_periods = as.integer(
#                difftime( policystart_nexts, policyends,
#                          units = 'days' ) ) - 1 )
# 
# rm( policyend_day, policyend_mon, policyend_year, 
#     policystart_next_day, policystart_next_mon, policystart_next_year,
#     policyends, policystart_nexts, help )
# 
# save( file = paste0(path, 'DF.RData'),
#       list = 'DF' )


load( paste0(path, 'DF.RData') )



## Regocnition of clients which cancel the contract:
## -------------------------------------------------

## Easy one:
DF[DF$days_between_periods > 30, 'product_id_next'] <- 'End'
DF[DF$days_between_periods > 30, 'planid_next']    <- 'End'

## Redefinition of the transitions:
DF$product_transition <- paste( DF$product_id, DF$product_id_next, sep = ' -> ' )
DF$plan_transition <- paste( DF$plan_id, DF$planid_next, sep = ' -> ' )


# -----------------------------------------------------------------------------#
# Analysis:
# -----------------------------------------------------------------------------#

## exemplarily for product B:

df1 <- DF[DF$product_id == 'B', ]

bar_own(table(df1$product_transition) / nrow(df1))
barbox_own(df1$gep_annual)

box_own( as.formula(df1$gep_annual ~ df1$product_transition) )
box_own( as.formula(df1$gep ~ df1$product_transition) )
box_own( as.formula(df1$yearofbirth ~ df1$product_transition) )
box_own( as.formula(df1$paid_amount[df1$paid_amount > 0] ~ df1$product_transition[df1$paid_amount > 0]) )
box_own( as.formula(df1$nb_clients_policy ~ df1$product_transition) )
box_own( as.formula(month_trafo(df1$policystart_mon) ~ df1$product_transition) )

table( df1$gender, df1$product_transition )
table( df1$distribution_channel, df1$product_transition )
table( df1$Dummy_loss, df1$product_transition )


## Create new data.frame with interesting variables:
## -------------------------------------------------

for ( i in 1:ncol(df1) ) {
  if ( class(df1[,i]) == 'character' ) {
    df1[, i] <- as.factor( df1[, i] )
  }
}

idx.col <- c(3,4,5,7,13,17,19,20)

# Data for modelling:
df <- df1[df1$policystart_year == '14', idx.col]
df <- cbind( df, product_transition = df1[df1$policystart_year == '14', 'product_transition'] )

df <- merge( x = df, 
             y = data.frame( product_transition = names( table(df$product_transition) ), 
                              table(df$product_transition ) ), 
             by = 'product_transition' )

# Data for validation:
df_pred <- df1[df1$policystart_year == '15', idx.col]
df_pred <- cbind( df_pred, product_transition = df1[df1$policystart_year == '15', 'product_transition'] )

## Random Forest:
## --------------

options( rf.cores = parallel::detectCores(),
         mc.cores = parallel::detectCores() )

wt <-  (1 / (df$Freq / nrow(df)) ) / sum( 1 / (table(df$product_transition) / nrow(df)) )

RF <- randomForestSRC::rfsrc( formula   = product_transition ~ .,
                              data      = df,
                              mtry      = 4,
                              ntree     = 200,
                              nodesize  = 1000,
                              imprtance = TRUE,
                              do.trace  = TRUE,
                              case.wt   = wt )

Pred_RF <- randomForestSRC:::predict.rfsrc( RF, df_pred )

# Probabilitys in Matrix:
Pred_RF$predicted

table( Pred_RF$class, df_pred$product_transition )

## LDA (linear discriminance analysis):
## ------------------------------------

LDA <- MASS::lda( formula = product_transition ~ .,
                  data    = df,
                  prior   = table(df$product_transition) / nrow(df) ) # as default

Pred_LDA <- predict( LDA, df_pred )

table( Pred_LDA$class, df_pred$product_transition )

## Multinomial logistic regression:
## --------------------------------

MLR <- nnet::multinom( formula = product_transition ~ .,
                       data    = df,
                       weights = table(df$product_transition) / nrow(df) ) # to compare with LDA

Pred_MLR       <- predict( MLR, df_pred )
Pred_MLR_probs <- predict( MLR, df_pred, type = 'probs')

table( Pred_MLR, df_pred$product_transition )

## State space models:
## -------------------

## discrete choice models:
## -----------------------




