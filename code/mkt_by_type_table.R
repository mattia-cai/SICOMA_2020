rm( list = ls( ) )

# Count 

# Dependencies
require( "data.table" )

# dataset -- (single Nace 2 assignment)
load( file = "outputData/compCases.RData", verbose = T )

# Case count and overall size by year
dta.cases[ , count := 1 ]
vars <- c( "count", "mkt" )
tab <- dta.cases[ , lapply( .SD, sum ), by = .( year, type ), .SDcols = vars ]
tab[ , mkt := mkt / 1000 ]
tab <- as.data.frame( tab )
tab <- reshape( tab, idvar='year', timevar='type', direction='wide')

# Totals
tab$count.total <- with( tab, count.merger + count.cartel )
tab$mkt.total <- with( tab, mkt.merger + mkt.cartel )

# Share of total
tab$share.merger <- with( tab, mkt.merger / mkt.total ) * 100
tab$share.cartel <- with( tab, mkt.cartel / mkt.total ) * 100
tab$share.total <- with( tab, share.merger + share.cartel )

# Averages over time
tab1 <- apply( tab, 2, mean ) 
tab1[ "year" ] <- 9999
tab <- rbind( tab, tab1 )

# Save
tab <- tab[ , c( 1:3, 8, 4, 5, 9, 6, 7, 10 ) ]


write.csv( tab, file = "results/case_descriptives/mkt_by_type.csv", row.names = F )

