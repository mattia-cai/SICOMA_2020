rm( list = ls( ) )

# Compute the price impacts using the leontief price model
# In this case I am only interested in two cases: trucks in 2016 and finance in 2013 
# author: mattia cai


##############################################
### Warning: XLConnect conflicts with xlsx ###
##############################################



##################
### Input data ###
##################

# Dependencies
require( "data.table" )
library( "stats")
#library( "XLConnect")
source( "code/funlib/leontiefPriceModel.R" )
source( "code/funlib/write_list.R" )

# dataset -- (single Nace 2 assignment)
load( file = paste0( "outputData/compCases.RData"), verbose = T )
str( dta.cases )

# EU28 supply and use system (2014)
load( "intmData/IO_EU28_2014.RData" )



#################################
### The two cases of interest ###
#################################

# Finance case
dta.cases[ year == 2013 & nace2_2d_a64 == "K64" ]
dta.cases[ year == 2013 & case_id == "AT.39914", tag := 1 ]

# Trucks
dta.cases[ year == 2016 & nace2_2d_a64 == "C29" ]
dta.cases[ year == 2016 & case_id == "AT.39824", tag := 1 ]

# Only those two cases
dta.cases[ tag == 1 ] 

# One case at a time
singleCaseSpillovers <- function( A, dt, w.var = "mkt_t_a64_log" ) { 
  w <- rep( 0, nrow( A ) )
  names( w ) <- rownames( A )
  dp <- w 
  w[ dt[, nace2_2d_a64 ] ] <- dt[ , get( w.var ) / go2_a64 ]
  dp[ dt[, nace2_2d_a64 ] ] <- dt[ , delta_p ]
  spill <- IOpriceSpilloverMatrix( A = A, w = w, rho = 1 - dp ) 
  spill <- rowSums( spill )
  x <- cbind( w = w, within = dp * w, spillover = spill )
  x <- x * 100
  x <- as.data.table( cbind( nace2 = rownames( x ), as.data.frame( x ) ) )
  x[ , nace2 := as.character( nace2 )]
  return( x )
}

# Apply listwise to the cases
dta.list <- split( dta.cases[ tag == 1 ], f = dta.cases[ tag == 1, case_id ] )
dta.list <- lapply( dta.list, function( dt ) singleCaseSpillovers( A = Ad, dt = dt ) )

# Get gross output for aggregation
dta.list <- lapply( dta.list, function( dt ) dt[ , go2 := colSums( Vt )[ dt[ , nace2 ] ] ] )

# Average
dta.list <- sapply( dta.list, function( dt ) dt[ , .( within = weighted.mean( x = within, w = go2 ), spillover = weighted.mean( x = spillover, w = go2 ) )] )
dta.list