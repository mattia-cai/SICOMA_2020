# Merge the various data sources I am going to use in the analysis

rm( list = ls( ) )

##################
### Input data ###
##################

# Dependencies
require( "readxl" )
require( "data.table" )
require( "openxlsx" )

# Case dataset
load( file = "intmData/compCaseData.RData" ) 

# Matching NACE codes
nace2 <- read.csv( "inputData/SUTs/nace_2.csv", header = T, stringsAsFactors = F )

# SBS data
load( file = "intmData/SBS_data_wide.RData" ) 

# Markup and GO data from Ecfin
load( file = "intmData/EcfinMupGo.RData", verbose = T )

# Gross output data from the national accounts (A64 by member state)
load( file = "intmData/grossOutputA64.RData" )

# Count of subsectors in given nace2 industry (N4)
load( file = "intmData/N4_subsector_count.RData", verbose = T )

# I will also need this to aggregate from the SBS
source( "inputData/more_nace2_aggregations.R" )



#####################################
### Incomplete case to be dropped ###
#####################################

# Drop this case (there are no data, anyway...)
dta.cases[ is.na( mkt ) ]
dta.cases <- dta.cases[ ! is.na( mkt ) ]



#######################
### Data from ECFIN ###
#######################

index <- match( dta.cases[ , nace2_2d_a64 ], nace2$nace_2 )
index <- nace2[ index, "X1809_ecfin_data" ]
dta.cases[, nace2_2d_a24 := index ]
index <- match( index, dta.mup$nace2 )
dta.cases[ , mup_a24 := dta.mup[ index, "mup_wr_eu" ] ]



########################
### 4-digit SBS data ###
########################

# Observed PROD4
dta.sbs[ , prod4 := prod ]
dta.sbs[ !is.na( prod4 ), prod4_notes := "observed" ]

# Observed VA4
dta.sbs[ , va4 := va ]
dta.sbs[ !is.na( va4 ), va4_notes := "observed" ]

# When possible, fill the missing values with the average of the previous two years
for ( vname in c( "prod", "va" ) ){
  dta.sbs[ , l1 := shift( prod, n = 1 ), by = nace2 ]
  dta.sbs[ , l2 := shift( prod, n = 2 ), by = nace2 ]
  dta.sbs[ , avg := rowMeans( as.matrix( .SD ), na.rm = T ), .SDcols = c( "l1", "l2" ) ]
  v1 <- paste0( vname, "4" )
  v2 <- paste0( vname, "4_notes" )
  dta.sbs[ is.na( get( v1 ) ) & !is.na( avg ), ( v2 ) := "imputed average" ]
  dta.sbs[ is.na( get( v1 ) ) & !is.na( avg ), ( v1 ) := avg ]
}

# Year 2015
dta.sbs <- dta.sbs[ year == 2015 ]  
dta.sbs <- dta.sbs[ , .( nace2, prod4, prod4_notes, va4, va4_notes ) ]

# Merge
index <- match( dta.cases[ , nace2_4d ], dta.sbs[ , nace2 ] )
dta.cases[ , prod4 := dta.sbs[ index, prod4 ] ]
dta.cases[ , prod4_notes := dta.sbs[ index, prod4_notes ] ]
dta.cases[ , va4 := dta.sbs[ index, va4 ] ]
dta.cases[ , va4_notes := dta.sbs[ index, va4_notes ] ]



#################################
### A64 and A24 data from SBS ###
#################################

# Are all the 
all( unlist( aggregator ) %in% dta.sbs[, nace2 ] )    # Should be T

# The missing aggregates will have to be constructed
# Some are there already, though
intersect( names( aggregator ), dta.sbs[ , nace2 ] )

# Create the remaining aggregates and append them to the SBS data
agg <- aggregator[ setdiff( names( aggregator ), dta.sbs[ , nace2 ] ) ]
fun <- function( i ){
  y <- dta.sbs[ nace2 %in% agg[[ i ]],  ]
  y[ , nace2 := names( agg )[ i ] ]
  y <- y[  ,lapply( .SD , sum ), by = nace2, .SDcols = c( "prod4", "va4" ) ]
  y[, prod4_notes := "calculated" ]
  y[, va4_notes := "calculated" ]
  return( y )
}
bottom.rows <- lapply( seq_along( agg ), fun )
bottom.rows <- do.call( "rbind", bottom.rows )
dta.sbs <- rbind( dta.sbs, bottom.rows  )

# Merge the A64 data on prod/va
index <- match( dta.cases[ , nace2_2d_a64 ], dta.sbs[ , nace2 ] )
dta.cases[ , prod2_a64 := dta.sbs[ index, prod4 ] ]
dta.cases[ , va2_a64 := dta.sbs[ index, va4 ] ]

# Merge the A24 data on prod/va
index <- match( dta.cases[ , nace2_2d_a24 ], dta.sbs[ , nace2 ] )
dta.cases[ , prod2_a24 := dta.sbs[ index, prod4 ] ]
dta.cases[ , va2_a24 := dta.sbs[ index, va4 ] ]

# # Remember these complications
# dta.cases[ is.na( prod2_a64 ) & ! is.na( prod2_a24 ), ]
# dta.cases[ !is.na( prod2_a64 ) & is.na( prod2_a24 ), ]



##########################################################################
### Merge A64 class count  and gross output from the national accounts ###
##########################################################################

# Aggregate EU 28
eu28 <- grossOutputA64[ , list( "go_a64" = sum( value, na.rm = T ) ), by = nace_r2 ]

# Count of 4-digit classes in each A64 industry
count[ nace2 == "L68B", nace2 := "L68" ]
eu28[, n4 := count[ match( eu28[ , nace_r2 ] , count[ , nace2 ] ), n4 ] ]

# Merge
index <- match( dta.cases[ , nace2_2d_a64 ], eu28[ , nace_r2 ] )
dta.cases[ , go2_a64 := eu28[ index, go_a64 ] ]
dta.cases[ , n4_a64 := eu28[ index, n4 ] ]



#########################################################
### Merge A24 gross output from the national accounts ###
#########################################################

# Aggregate to ecfin detail
mkey <- unique( nace2[, c( "nace_2", "X1809_ecfin_data" ) ] )
mkey <- mkey[ match( eu28[ , nace_r2 ], mkey[ , "nace_2" ] ), "X1809_ecfin_data" ]
eu28[ , ecfin := mkey ]
go.agg <- eu28[ , list( "go_a24" = sum( go_a64 ), "n4_a24" = sum( n4 ) ), by = ecfin ]

# Merge
index <- match( dta.cases[ , nace2_2d_a24 ], go.agg[ , ecfin ] )
dta.cases[ , go2_a24 := go.agg[ index, go_a24 ] ]
dta.cases[ , n4_a24 := go.agg[ index, n4_a24 ] ]



#########################################
### Total gross output of the economy ###
#########################################

# Business economy (sectors relevant to quest )
dta.cases[ , go := eu28[ !is.na( ecfin ) & ecfin != "", sum( go_a64 ) ] ]

# Entire economy (just in case I need it)
dta.cases[ , go_all := eu28[ , sum( go_a64 ) ] ]



############
### Save ###
############

save( dta.cases, file = "intmData/full_dataset.RData" )
write.csv( dta.cases, file = "intmData/full_dataset.csv", row.names = F )