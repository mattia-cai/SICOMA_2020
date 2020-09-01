rm( list = ls( ) )

# Impact on the overall price level
# Tables for book chapter
# author: mattia cai

# Dependencies
require( "data.table" )
library( "ggplot2" )
source( "code/funlib/write_list.R" )


# Results of the price analysis
load( file = paste0( "results/industryOfDestination.RData" ) )

# # Drop industries L, O, P, Q, R, T, U from the results (when averaging)
# ind2drop <- c( "L68A", "L68B", "O84", "P85", "Q86", 'Q87_Q88', "T", "U" )
# dta <-dta[ !nace2_2d_a64 %in% ind2drop ]

# Average
dta <- dta[ , .( within = weighted.mean( within, go2_a64 ),          
          spillover = weighted.mean( spillover, go2_a64 ),
          total = weighted.mean( total, go2_a64 ) ), by = .( year, deterrence, duration ) ]

# Split and save to separate excel files
dt.lst <- split( dta, dta[, .( deterrence, duration ) ] )
dt.lst <- lapply( dt.lst , function( x ) setkeyv( x, "year" ) )
for ( i in names( dt.lst ) ){
  dt.lst.i <- dt.lst[[ i ]]
  dt.lst.i[ , deterrence := NULL ]
  fpath <- paste0( "results/economyWideImpacts_", gsub( x = i, pattern = ".", replacement = "_", fixed = T ), ".xlsx" )
  write_list( list( "economyWideImpacts" = dt.lst.i ), fpath )
}