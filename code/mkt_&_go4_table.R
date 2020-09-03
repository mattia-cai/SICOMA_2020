rm( list = ls( ) )

# Dependencies
require( "data.table" )

# dataset -- (single Nace 2 assignment)
load( file = paste0( "outputData/compCases.RData"), verbose = T )
str( dta.cases )

# I will need to get the totals as well
dta1 <-  copy( dta.cases )
dta2 <- copy( dta.cases )
dta2[ , type := "Total"]
dta <- rbind( dta1, dta2 )

# Labels
dta[ type == "merger", type := "Mergers"]
dta[ type == "cartel", type := "Cartels"]


# # Uncomment if you want to compare with the 2019 FR results (GO4 does not match all that well...)
# dta <- dta[ year < 2019 ]

# Do it one variable at a time
summstats <- function( var ){
  tab <- dta[ , .( mean = mean( get( var ) ),
                   median = median( get( var ) ),
                   p25 = quantile( get( var ), .25 ),
                   p75 = quantile( get( var ), .75 ) ),
              by = .( type ) ]
  tab1 <- t( tab [ ,- 1 ] )
  colnames( tab1 ) <- tab[, type ]
  tab1 <- as.data.table( cbind( stat = rownames( tab1 ), tab1 ) )
  tab1 <- cbind( var = var, tab1 )
  return( tab1 )
}

# Market size
mkt <- summstats( var = "mkt" )

# GO4
go4 <- summstats( var = "go4_a64" )

# mkt/GO4
dta[ , mkt2go4 := mkt / go4_a64 ]
mkt2go4 <- summstats( var = "mkt2go4" )

# Table to export
tab <- rbind( mkt, go4, mkt2go4 )

# save
write.csv( tab, "results/case_descriptives/mkt_&_go4_table.csv", row.names = F )

