rm( list = ls( ) )

# Dependencies
require( "data.table" )

# dataset -- (single Nace 2 assignment)
load( file = paste0( "outputData/compCases.RData"), verbose = T )
str( dta.cases )


# Average case size and duration
dta.cases[ , count := 1 ]
vars <- c( "count", "duration" )
dta <- dta.cases[ , lapply( .SD, sum ), by = .( year, type ), .SDcols = vars ]
dta[ , dur := duration/count ]        # average duration
dta[ , duration := NULL ]

# Reshape
tab <- as.data.frame( dta )
tab <- reshape( tab, idvar='year', timevar='type', direction='wide')

# Overall
tab <- as.data.table( tab )
tab[, count.total := count.merger + count.cartel ]
tab[ , dur.total := ( count.merger * dur.merger + 
          count.cartel * dur.cartel ) / count.total ]

# save
write.csv( tab, "results/case_descriptives/average_duration.csv", row.names = F )

