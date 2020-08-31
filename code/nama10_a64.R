rm( list = ls( ) )

# Dependencies
library( "reshape2" )
library( "data.table" )



##################
### Input data ###
##################

# Raw data from Eurostat bulk dowload (Sep 2018)
dta <- read.table( file = "inputData/nama_10_a64/nama_10_a64.tsv", header = T, sep = "\t", stringsAsFactors = F )



######################
###  Basic cleanup ###
######################

# Basic cleanup
dta <- as.data.frame( dta )
tmp.id <- names( dta )[ 1 ] 
dta <- melt( dta, id.vars = tmp.id, variable.name = "year" )
dta[ , "year" ] <- as.numeric( gsub( "X", "", dta$year ) )

# There is something weird with the first column
lft.cols <- as.character( dta[ , 1 ] )
lft.cols <- tstrsplit( x = lft.cols, split = "," )
lft.cols <- do.call( "cbind", lft.cols )
colnames( lft.cols ) <- c( "unit", "nace_r2", "indic", "geo" ) 
dta <- cbind( lft.cols, dta[ , - 1 ] )

# Column order
vars <- c( "geo", "year", "indic", "unit", "nace_r2", "value" )
dta <- dta[ , vars ]



######################
### Eurostat flags ###
######################

# Flag not available
dta$na_val <- grepl( x = dta$value, pattern = ":", fixed = TRUE ) 

# Keep any available flags from eurostat
dta$flag <- trimws( gsub( '[[:digit:]]+', '', dta$value ) )

# make value numerical
dta$value <- gsub( "[^0-9.]", "", dta$value )
dta$value <- as.numeric( dta$value )

# It seems to have worked
dta <- as.data.table( dta )
dta[, table( na_val, is.na( value ), useNA = "ifany" ) ]



############
### Save ###
############

nama10_a64 <- dta
setkeyv( nama10_a64, c( "geo", "year", "indic", "unit", "nace_r2" ) )
save( nama10_a64, file = "intmData/nama10_a64.RData" )



