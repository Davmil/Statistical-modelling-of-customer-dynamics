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
if ( dir.exists( 'C:/Users/David/Documents/Consulting' ) ) {
  path <- 'C:/Users/David/Documents/Consulting/'
}



## Read data:
if ( file.exists( paste0( path, 'Data/Movement_csv.csv') ) ) {
  DF <- read.csv2( file   = paste0( path, 'Data/Movement_csv.csv'),
                   header = TRUE )
  DF_o   <- read.csv2( file   = paste0( path, 'Data/Movement.csv'),
                       header = TRUE )
}
if ( file.exists( paste0( path, 'data/Movement.csv') ) ) {
  DF <- read.csv2( file   = paste0( path, 'data/Movement.csv'),
                   header = TRUE )
  DF_o   <- read.csv2( file   = paste0( path, 'data/Movement.csv'),
                       header = TRUE )
}

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
# save( file = paste0(path, 'DF_str.RData'),
#       list = 'DF_str' )

# load restructred data
load( file = paste0(path, 'DF_str.RData') )

# drop 
idx <- DF_str$clientid[ DF_str$gep > 5300 | DF_str$gep < 10 ]

DF_str <- DF_str[! DF_str$clientid %in% idx, ]

DF_str <- cbind( DF_str, gep_annual = DF_str$gep / DF_str$days_in_period * 365 )

DF_str <- DF_str[! DF_str$gep_annual > 5300, ]

rm( idx )



DF_raw <- DF


DF     <- DF_str
rm(DF_str)

## Missing Distribution Channel -> Broker
## --------------------------------------
DF[DF$distribution_channel=="","distribution_channel"] <- "Broker"
DF$distribution_channel <- as.factor(as.character(DF$distribution_channel))

DF$change <- ""
DF[DF$product_id == DF$product_id_next, "change"] <- "no"
DF$change[DF$change==""] <- "yes"

## Regocnition of clients which cancel the contract:
## -------------------------------------------------

## All NA's is days-between-periods exhibit a cancelation of the contract (because all policystarts and policyends in 2016 were dropped):

DF[is.na(DF$days_between_periods),"days_between_periods"] <- 9000

## Easy one:
DF[DF$days_between_periods > 30, 'product_id_next'] <- 'End'
DF[DF$days_between_periods > 30, 'planid_next']    <- 'End'

## Redefinition of the transitions:
DF$product_transition <- paste( DF$product_id, DF$product_id_next, sep = ' -> ' )
DF$plan_transition <- paste( DF$plan_id, DF$planid_next, sep = ' -> ' )
DF$days_between_periods <- NULL



# -----------------------------------------------------------------------------#
# Analysis:
# -----------------------------------------------------------------------------#

## exemplarily for product B:

df1 <- DF[DF$product_id == 'F', ]

# bar_own(table(df1$product_transition) / nrow(df1))
# barbox_own(df1$gep_annual)

# box_own( as.formula(df1$gep_annual ~ df1$product_transition) )
# box_own( as.formula(df1$gep ~ df1$product_transition) )
# box_own( as.formula(df1$yearofbirth ~ df1$product_transition) )
# box_own( as.formula(df1$paid_amount[df1$paid_amount > 0] ~ df1$product_transition[df1$paid_amount > 0]) )
# box_own( as.formula(df1$nb_clients_policy ~ df1$product_transition) )
# box_own( as.formula(month_trafo(df1$policystart_mon) ~ df1$product_transition) )
# 
# table( df1$gender, df1$product_transition )
# table( df1$distribution_channel, df1$product_transition )
# table( df1$Dummy_loss, df1$product_transition )


## Create new data.frame with interesting variables:
## -------------------------------------------------

years <- 12:14

for ( i in 1:ncol(df1) ) {
  if ( class(df1[,i]) == 'character' ) {
    df1[, i] <- as.factor( df1[, i] )
  }
}

idx.col <- c(3,4,5,7,13,17,19,20)

# Data for modelling:
df <- df1[df1$policystart_year %in% years, idx.col]

df <- merge( x = df, 
             y = data.frame( product_transition = names( table(df$product_transition) ), 
                                        table(df$product_transition ) ), 
             by = 'product_transition' )

if ( 'Var1' %in% names(df) ) {
  df <- df[, -which(names(df) == 'Var1')]
}
# if ( 'policystart_year' %in% names(df) ) {
#   df <- df[, -which(names(df) == 'policystart_year')]
# }


# Data for validation:
df_pred <- df1[df1$policystart_year == '15', idx.col]
df_pred <- cbind( df_pred, product_transition = df1[df1$policystart_year == '15', 'product_transition'] )

## Random Forest:
## --------------

options( rf.cores = parallel::detectCores(),
         mc.cores = parallel::detectCores() )

wt <-  (1 / (df$Freq / nrow(df)) ) / sum( 1 / (table(df$product_transition) / nrow(df)) )

if ( 'Freq' %in% names(df) ) {
  df <- df[, -which(names(df) == 'Freq')]
}

RF <- randomForestSRC::rfsrc( formula   = product_transition ~ .,
                              data      = df,
                              mtry      = 4,
                              ntree     = 200,
                              nodesize  = 1000,
                              imprtance = TRUE,
                              do.trace  = TRUE )
                              # case.wt = wt )

Pred_RF <- randomForestSRC:::predict.rfsrc( RF, df_pred[,RF$xvar.names] )
plot(RF)

# Probabilitys in Matrix:
# Pred_RF$predicted

table( Pred_RF$class, df_pred$product_transition )

## LDA (linear discriminance analysis):
## ------------------------------------

LDA <- MASS::lda( formula = product_transition ~ .,
                  data    = df)
                  #prior   = table(df$product_transition) / nrow(df) ) # as default

Pred_LDA <- predict( LDA, df_pred )

table( Pred_LDA$class, df_pred$product_transition )

## Multinomial logistic regression:
## --------------------------------

MLR <- nnet::multinom( formula = product_transition ~ .,
                       data    = df)
                       #weights = table(df$product_transition) / nrow(df) ) # to compare with LDA

Pred_MLR       <- predict( MLR, df_pred )
Pred_MLR_probs <- predict( MLR, df_pred, type = 'probs')

table( Pred_MLR, df_pred$product_transition )

## State space models:
## -------------------

## discrete choice models:
## -----------------------




