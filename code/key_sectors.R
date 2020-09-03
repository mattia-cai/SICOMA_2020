rm( list = ls( ) )

# Repercussions a standardized price shock:
# - affected market size: 10,000 (10 billion EUR)
# - avoided price increase: 10%
# Produce charts and a tabulation of the results.

# If need be, change the parameters of the standardized case around line 120



##################
### Input data ###
##################

# Dependencies
require( "data.table" )
source( "code/funlib/leontiefPriceModel.R" )   # Leontief price model

# The IO system
load( file = "intmData/IO_EU28_2014.RData", verbose = T )
go <- colSums( Vt )   # industry output

# Short industry labels -
labs0 <- read.csv( "inputData/SUTs/nace2_short_labels.csv", header = T )
nace2 <- as.character( labs0$nace_2 )
labs1 <- as.character( labs0$short )
labs0 <- nace2
labs0[ labs0 %in% c( "L68A", "L68B" ) ] <- "L68"
labs <- paste0( labs1, " (", labs0, ")")
names( labs ) <- nace2

# Case dataset (only for reference)
load( file = paste0( "outputData/compCases.RData"), verbose = T )
str( dta.cases )

# # Drop industries L, O, P, Q, R, T, U from the results (when averaging)
# ind2drop <- c( "L68A", "L68B", "O84", "P85", "Q86", 'Q87_Q88', "T", "U" )



###################################################
### How large should the standard size case be? ###
###################################################

# Three things should be kept in mind:
# - The magnitude should be realistic given the type of competition policy enforcement we observe in the DG COMP dataset;
# - the standard-size case cannot be larger the output of the smallest industry. Otherwise, the affected portion of the industry is greater than 1;
# - We would like to work with round(-ish) numbers, to make reporting easy

# 1. Total volume of enforcement activity
dcast( dta.cases[ , .( mkt = sum( mkt ) ), by = .( year, type ) ], year ~ type )

# Median/ean size case (grand)
dta.cases[ , median( mkt ) ]
dta.cases[ , mean( mkt ) ]

# Median/median case size (by year/type)
dcast( dta.cases[ , .( mkt = mean( mkt ) ), by = .( year, type ) ], year ~ type )
dcast( dta.cases[ , .( mkt = median( mkt ) ), by = .( year, type ) ], year ~ type )

# 2. How large is the output of the smallest industry?
go.min <- min( go[ go > 0 ] )
which( go == go.min ) # It is fisheries
# One option: leave agriculture and the public sector out of the analysis
# This way we drop fisheries and we can assume a larger mkt size

# Note, however, that we do have a couple of merger cases in the agricultural sector:
dta.cases[ , table( nace2_2d_a64 ) ]
dta.cases[ nace2_2d_a64 %in% c( "A01", "A02", "A03" ) ]
# M.8829 - Bagged salad in Sweden: could be re-assigned to C10-12 (Food)
# M.6850 - Salmon farming: this is certainly part of A03



############################
### Standard price model ### 
############################

# Get rid of the zero in the (domestic) A matrix
tag <- which( rowSums( Ad ) == 0 & colSums( Ad ) == 0  )
tag <- setdiff( rownames( Ad ) , names( tag ) )
Ad <- Ad[ tag, tag ]

# Loop over all industries
ind.list <- colnames( Ad )

# Apply listwise
fun <- function( mkt, dp, i ){ 
  # weights
  w <- rep( 0, nrow( Ad ) ) 
  names( w ) <- rownames( Ad )
  w[ i ] <- mkt / go[ i ]
  # price reduction
  rho <- rep( 1, nrow( Ad ) ) 
  names( rho ) <- rownames( Ad )
  rho[ i ] <- 1 + dp
  
  # Within
  within <- ( rho - 1 ) * w * 100
  
  # Spillovers 
  sigma <- IOpriceSpillovers( A = Ad, w = w, rho = rho ) 
  spillover <- ( sigma - 1 ) * ( 1 - w ) * 100
  
  # total
  total <- within + spillover
  
  # Results
  dta <- data.frame( within, spillover, total, stringsAsFactors = F )
  return( dta )  
}

# If need be, change the parameters of the standardized case here
dp.list <- lapply( ind.list, function( z ){ fun( mkt = 10000, dp = -.10, i = z ) })
names( dp.list ) <- ind.list




######################################
### Format the results before save ###
######################################

# tidy up
dp.list <- lapply( dp.list, as.data.frame ) 
dp.list <- lapply( dp.list, function( x ) cbind( nace2_2d_a64 = rownames( x ), x ) )
dp.list <- lapply( dp.list, as.data.table ) 
for ( i in names( dp.list ) ){
  
  # Industry labels
  dp.list[[ i ]][ , nace2_label := labs[ dp.list[[ i ]][ , as.character( nace2_2d_a64 ) ] ] ]
  
  # Who's shocked?
  dp.list[[ i ]][ , shocked := i ]
  dp.list[[ i ]][ , shocked_labs := labs[ i ] ]
  
  # Industry size
  dp.list[[ i ]][ , go2_a64 := colSums( Vt )[ dp.list[[ i ]][ , as.character( nace2_2d_a64 ) ] ] ]
  dp.list[[ i ]][ , hh2_a64 := Ftot[  dp.list[[ i ]][ , as.character( nace2_2d_a64 ) ] , "hh" ] ]
  
  # Column order
  dp.list[[ i ]] <- dp.list[[ i ]][ , .( nace2_2d_a64, nace2_label, go2_a64, hh2_a64, shocked, shocked_labs, within, spillover, total ) ]
  
}



###################
### Aggregation ###
###################

summtab <- function( x ){
  
  # # Drop irrelevant industries
  # x <- x[ !( nace2_2d_a64 %in% ind2drop ) ]
  
  # In the aggregation, weight industries according to their output
  x[ , go.weight := go2_a64 / sum( go2_a64 ) ]
  
  # (Weighted) contribution to change in price level
  x[ , within := go.weight * within ]
  x[ , spillover := go.weight * spillover ]
  x[ , total := go.weight * total ]
  
  # Spillover concentration
  x[ , share := spillover / sum( spillover ) ]
  x[ , conc := share ^ 2 ]
  
  # # Keep industry output -- just in case
  # tmp <- x[ within != 0, go2_a64 ] / nrow( x )
  # x[ , go2_a64 := tmp ]
  
  # Aggregation
  # y <- x[ , lapply( .SD, sum ), .SDcols = c( "go2_a64", "within", "spillover", "total", "conc"), by = .( shocked, shocked_labs ) ]
  y <- x[ , lapply( .SD, sum ), .SDcols = c( "within", "spillover", "total", "conc"), by = .( shocked, shocked_labs ) ]
  
  # Maximum share 
  y[ , max.share := x[ , max( share ) ] ]
  y[ , max.share.ind := x[ share == max( share ), shocked_labs ] ]
  
  # Normalized concentration index
  y[ , conc.norm :=  ( conc - ( 1 / nrow( x ) ) ) / ( 1 - ( 1 / nrow( x ) ) ) ]
  
  return( y )
}
dta <- lapply( dp.list, summtab )
dta <- do.call( "rbind", dta )

# # Drop irrelevant industries
# dta <- dta[ !( shocked %in% ind2drop ) ]



##############################################################
### The two concentration indicators are highly correlated ###
##############################################################

dta[ , plot( conc.norm, max.share, xlab = "Herfindahl (normalized)", ylab = "Largest share"  ) ]
dta[ , cor( max.share, conc.norm, method = "pearson" ) ]
dta[ , cor( max.share, conc.norm, method = "spearman" ) ]



#######################
### Spillover ratio ###
#######################

# Ratio of spillover to within
dta[ , spill.ratio := spillover/within ]
# the spillover ratio: a) is not affected by delta; b) does not appear to be especially sensitive to mkt 
dta[ , .( shocked, within, spillover, spill.ratio ) ]



#####################################################
### Low/medium/high spillovers - percentile-based ###
#####################################################

# Percentiles
labs <- c( "low", "medium", "high" )
qnt <- dta[ , quantile( spill.ratio, probs = c( 0, 1/3, 2/3, 1 ) )  ]
dta[ , spill3.pct := cut( spill.ratio, breaks = qnt, include.lowest = T, labels = labs ) ]

# It seems to be working as expected
dta[ , table( spill3.pct ) ]
dta[ , any( is.na( spill3.pct ) ) ]
dta[ , mean( spill.ratio ) , by = spill3.pct ]

# Breakdown
split( dta[ , .( shocked_labs,  spill.ratio ) ], dta[ , spill3.pct ] )



#################################################################
### Low/medium/high spillovers - divide the interval in three ###
#################################################################

# Alternatively, you can just cut the range in 3
dta[ , spill3.cut := cut( spill.ratio, breaks = 3, include.lowest = T, labels = labs ) ]

# It seems to be working as expected
dta[ , table( spill3.cut ) ]
dta[ , any( is.na( spill3.cut ) ) ]
dta[ , mean( spill.ratio ) , by = spill3.cut ]

# Breakdown
split( dta[ , .( shocked_labs,  spill.ratio ) ], dta[ , spill3.cut ] )




############################################
### Plausibility check: forward linkages ###
############################################

# Direct forward linkages
all( rownames( Zd ) == names( go ) )    # Should be T
B <- sweep( Zd, 1, go, "/" )
fwd.dir <- rowSums( B )
dta[ , fwd.dir := fwd.dir[ dta[ , shocked ] ] ]

# Plot
dta[ , plot( fwd.dir, spill.ratio, ylab = "Spillover/within", xlab = "Direct forward linkage" ) ]

# Total forward linkages (based on ghosh inverse)
any( is.na( rowSums( B ) ) )
tag <- names( which( ! is.na( rowSums( B ) ) ) )
B <- B[ tag, tag ]
fwd.tot <- rowSums( solve( diag( nrow( B ) ) - B ) )
dta[ , fwd.tot := fwd.tot[ dta[ , shocked ] ] ]

# Some checks: total forward linkages
dta[ , plot( fwd.tot, spill.ratio, ylab = "Spillover/within", xlab = "Total forward linkage" ) ]

# # By the way
# dta[ , plot( fwd.tot, 1 + spill.ratio ) ]
# abline( 0, 1, col = "red" )
# 



######################################
### Concentration as ratio to mean ###
######################################

#labs <- c( "low", "medium", "high" )
dta[ , conc.rel := conc.norm / mean( conc.norm ) ]
dta[ , .( shocked, conc.rel )]

# Histogram
dta[ spill3.pct != "low", hist( conc.rel, xlab = "Relative concentration" ) ]



###################################################
### Concentration - medium/high spillover group ###
###################################################

# Percentiles
qnt <- dta[ spill3.pct != "low", quantile( conc.norm, probs = c( 0, 1/3, 2/3, 1 ) )  ]
dta[ spill3.pct != "low", conc3.pct := cut( conc.norm, breaks = qnt, include.lowest = T, labels = labs ) ]

# Classification
split( dta[ , shocked_labs ], dta[ , conc3.pct ] )



############
### Save ###
############

write.csv( dta, "results/spillover_intensity.csv", row.names = F, na = "" )
