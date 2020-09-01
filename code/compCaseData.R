# Merge the various data sources

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
str( dta.sbs )
# Codebook: 
#   - to: Turnover or gross premiums written - million euro (V12110)
#   - prod: Production value - million euro (V12120)
#   - va: Value added at factor cost - million euro (V12150)

# Markup and GO data from Ecfin
load( file = "intmData/EcfinMupGo.RData", verbose = T )

# Gross output data from the national accounts (A64 by member state)
load( file = "intmData/grossOutputA64.RData" )
str( grossOutputA64 )

# Count of subsectors in given nace2 industry (N4)
load( file = "intmData/N4_subsector_count.RData", verbose = T )



#######################
### Incomplete case ###
#######################

# Drop this case (there are no data, anyway...)
dta.cases[ is.na( mkt ) ]
dta.cases <- dta.cases[ ! is.na( mkt ) ]



##############################
### Avoided price increase ###
##############################

# Price overcharge assumptions
dta.cases[ type == "merger", delta_p := .03 ]
dta.cases[ type == "cartel", delta_p := .15 ]



############################
### Merge the ECFIN data ###
############################

# Merge the MUP data
index <- match( dta.cases[ , nace2_2d_a64 ], nace2$nace_2 )
index <- nace2[ index, "X1809_ecfin_data" ]
dta.cases[, nace2_2d_a24 := index ]
index <- match( index, dta.mup$nace2 )
dta.cases[ , mup_a24 := dta.mup[ index, "mup_wr_eu" ] ]



############################
### Handling of SBS data ###
############################

# No need for flags here
dta.sbs[ , c( "to", "isConfTo", "isConfProd", "isConfVa" ) := NULL ]

# It is assumed that the price reduction resulting from a given competition policy intervention affects not only the market in which the decision takes place, but also contiguous markets within the relevant four-digit NACE sector (due to deterrent effects). Gross Output in this four-digit sector (GO4) is estimated  as

# -- (GO4) = GO2 * PROD4/PROD2, with PROD2 and PROD4 being SBS production value data at the two- and four-digit level, respectively 
dta.sbs[ , prod4 := prod ]

# -- If production value data is incomplete for a given sector in a given year, PROD4 is assumed to be the average of the PROD4 values over the previous two years
dta.sbs[ , prod1 := shift( prod, n = 1 ), by = nace2 ]
dta.sbs[ , prod2 := shift( prod, n = 2 ), by = nace2 ]
dta.sbs[ , prod.mean := rowMeans( as.matrix( .SD ), na.rm = T ), .SDcols = c( "prod1", "prod2" ) ]
nrow( dta.sbs[ is.na( prod4 ) ] )
dta.sbs[ is.na( prod4 ), prod4 := prod.mean ]
nrow( dta.sbs[ is.na( prod4 ) ] )

# -- If production value data is missing, value added data should be used instead and GO4 is estimated according to 
dta.sbs[ , va4 := va ]

# -- If value added data is incomplete for a given sector in a given year, VA4 is assumed to be the average of the VA4 values over the previous two years
dta.sbs[ , va1 := shift( va, n = 1 ), by = nace2 ]
dta.sbs[ , va2 := shift( va, n = 2 ), by = nace2 ]
dta.sbs[ , va.mean := rowMeans( as.matrix( .SD ), na.rm = T ), .SDcols = c( "va1", "va2" ) ]
nrow( dta.sbs[ is.na( va4 ) ] )
dta.sbs[ is.na( va4 ), va4 := va.mean ]
dta.sbs[ is.na( va ) ]

# -- If both production value and value added data are missing, (GO4) ̃ is estimated as GO2/N4 where N4 is the number of four digit sectors that are present within the two-digit sector -- See below

# Use 2015 -- I double checked this by phone on March 19th 2019
dta.sbs <- dta.sbs[ year == 2015 ]  
dta.sbs <- dta.sbs[ , .( nace2, prod4, va4 ) ]



##############################
### Merge 4-digit SBS data ###
##############################

# Figures for the numerical example in the report
dta.sbs[ nace2 %in% c( "J58", "J59", "J60", "J6020") ]

# match codes
index <- match( dta.cases[ , nace2_4d ], dta.sbs[ , nace2 ] )

# Get value and flag missing
dta.cases[ , prod4 := dta.sbs[ index, prod4 ] ]
dta.cases[ , va4 := dta.sbs[ index, va4 ] ]

# Our of SBS scope
dta.cases[ is.na( index ), table( nace2_2d_a64 ) ]



#######################################
### Merge A64 and A24 data from SBS ###
#######################################

# Aggregators (A64 and A24)
agg <- list(
  "B" = paste0( "B0", 5:9 ),
  "C10-C12" = paste0( "C", 10:12 ),
  "C31_C32" = paste0( "C", 31:32 ),
  "E37-E39" = paste0( "E", 37:39 ),
  "J59_J60" = paste0( "J", 59:60 ),
  "J62_J63" = paste0( "J", 62:63 ),
  "L68" = "L",
  "N80-N82" = paste0( "N", 80:82 ),
  # "R90-R92" = paste0( "R", 90:92 ),           # Not in SBS
  "10-12" = paste0( "C", 10:12 ),
  "16-18" = paste0( "C", 16:18 ),
  "20-21" = paste0( "C", 20:21 ),
  "22-23" = paste0( "C", 22:23 ),
  "24-25" = paste0( "C", 24:25 ),
  "26-27" = paste0( "C", 26:27 ),
  "28" = paste0( "C", 28 ),
  "29-30" = paste0( "C", 29:30 ),
  "31-33" = paste0( "C", 31:33 ),
  "D-E" = c( paste0( "D", 35 ), paste0( "E", 36:39 ) ),
  "G" = paste0( "G", 45:47 ),
  "H" = paste0( "H", 49:53 ),
  "58-60" = paste0( "J", 58:60 ),
  "61" = paste0( "J", 61 ),
  "62-63" = paste0( "J", 62:63 ),
  "M-N" = c( paste0( "M", 69:75 ), paste0( "N", 77:82 ) ) )

# Are all the 
all( unlist( agg ) %in% dta.sbs[, nace2 ] )    # Should be T

# The missing aggregates will have to be constructed
# Some are there already, though
intersect( names( agg ), dta.sbs[ , nace2 ] )

# Create the remaining aggregates and append them to the SBS data
agg <- agg[ setdiff( names( agg ), dta.sbs[ , nace2 ] ) ]
fun <- function( i ){
  y <- dta.sbs[ nace2 %in% agg[[ i ]],  ]
  y[ , nace2 := names( agg )[ i ] ]
  y <- y[  ,lapply( .SD , sum ), by = nace2, .SDcols = c( "prod4", "va4" ) ]
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

# Remember these complications
dta.cases[ is.na( prod2_a64 ) & ! is.na( prod2_a24 ), ]
dta.cases[ !is.na( prod2_a64 ) & is.na( prod2_a24 ), ]



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

# Numerical example for the report
eu28[ nace_r2 %in% c( "J58", "J59", "J60", "J6020") ]



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



######################
### GO4 estimation ###
######################

# Cases in which market size > 4-digit production value from SBS 
dta.cases[ mkt > prod4, .( year, type, id, nace2_4d, mkt, prod4 ) ]

# A handful of cases in which prod is missing but VA isn't
dta.cases[ is.na( prod4 / prod2_a64 ) & ! is.na( va4 / va2_a64 ) ]

# GO4 estimates
dta.cases[ , go4_a64_prod := ( prod4 / prod2_a64 ) * go2_a64  ]
dta.cases[ , go4_a24_prod := ( prod4 / prod2_a24 ) * go2_a24  ]

# Fill NAs with VA if possible
dta.cases[ , go4_a64_va := ( va4 / va2_a64 ) * go2_a64  ]
dta.cases[ , go4_a24_va := ( va4 / va2_a24 ) * go2_a24  ]

# When SBS data not available, set GO4_ = go2_a64 / n4
dta.cases[ , go4_a64_n4 := go2_a64 / n4_a64 ]
dta.cases[ , go4_a24_n4 := go2_a24 / n4_a24 ]

# GO4 estimate - based on A64
dta.cases[ , go4_a64 := go4_a64_prod ]
dta.cases[ , go4_a64_notes := "go4_a64_prod" ]
dta.cases[ is.na( go4_a64 ) & !is.na( go4_a64_va ), ]
dta.cases[ is.na( go4_a64 ) & !is.na( go4_a64_va ), go4_a64_notes := "go4_a64_va" ]
dta.cases[ is.na( go4_a64 ) & !is.na( go4_a64_va ), go4_a64 := go4_a64_va ]
dta.cases[ is.na( go4_a64 ), ]
dta.cases[ is.na( go4_a64 ) , go4_a64_notes := "go4_a64_n4" ]
dta.cases[ is.na( go4_a64 ), go4_a64 := go4_a64_n4 ]

# GO4 estimate - based on A24
dta.cases[ , go4_a24 := go4_a24_prod ]
dta.cases[ , go4_a24_notes := "go4_a24_prod" ]
dta.cases[ is.na( go4_a24 ) & !is.na( go4_a24_va ), ]
dta.cases[ is.na( go4_a24 ) & !is.na( go4_a24_va ), go4_a24_notes := "go4_a24_va" ]
dta.cases[ is.na( go4_a24 ) & !is.na( go4_a24_va ), go4_a24 := go4_a24_va ]
dta.cases[ is.na( go4_a24 ), ]
dta.cases[ is.na( go4_a24 ) , go4_a24_notes := "go4_a24_n4" ]
dta.cases[ is.na( go4_a24 ), go4_a24 := go4_a24_n4 ]



#########################################
### Total gross output of the economy ###
#########################################

# Business economy (sectors relevant to quest )
dta.cases[ , go := eu28[ !is.na( ecfin ) & ecfin != "", sum( go_a64 ) ] ]

# Entire economy (just in case I need it)
dta.cases[ , go_all := eu28[ , sum( go_a64 ) ] ]



#######################################
### Deterrence-adjusted market size ###
#######################################

# Check
any( na.omit( dta.cases[ ,  go4_a64_prod > go2_a64  ] ) )  # Should be F
any( na.omit( dta.cases[ ,  go4_a24_prod > go2_a24  ] ) )  # Should be F

# Logistic function (eq. 9)
y.fun <- function( x, chi, ef, ny ){
  h.fun <- function( x, chi, ef, ny ){ 1 / ( ( 1 + exp( - chi * ( x - ef ) ) ) ^ (  1 / ny ) ) }
  num <- ( h.fun( x = x, chi, ef, ny ) - h.fun( x = 0, chi, ef, ny ) )
  denom <- ( h.fun( x = 1, chi, ef, ny ) - h.fun( x = 0, chi, ef, ny ) )
  y <- num / denom
  return( y )
}

# Deterred market size
dta.cases[ type == "merger", y := y.fun( x = mkt / go4_a64 , chi = 100, ef = .0254, ny = 1 ) ]
dta.cases[ type == "cartel", y:= y.fun( x = mkt / go4_a64 , chi = 100, ef = .0153, ny = 1 ) ]
dta.cases[ , mktD := y * ( go4_a64 - mkt ) ]
dta.cases[ mktD < 0 ]                # cases with mkt > GO4
dta.cases[ mktD < 0, mktD := 0 ]

# Market size with deterrence
dta.cases[ , mktT := mkt + mktD ]
dta.cases[ , c( "y" ) := NULL ]



############################################
### Format according to DG COMP template ###
############################################

# Column order selection
dta.cases <- dta.cases[ , .( id, type, year, duration, delta_p,
                             mkt, mktD, mktT,
                             nace2_4d,  nace2_2d_a24, nace2_2d_a64,
                             mup_a24, go2_a24, go4_a24, go4_a24_notes, 
                             go2_a64, go4_a64, go4_a64_notes, go ) ]
setnames( dta.cases, "id", "case_id" )

# Save as .RData
save( dta.cases, file = "outputData/compCases.RData" )

# Export to Excel (with codebook)
cb <- read_excel( path = "inputData/DgComp/case_data_codebook.xlsx", sheet = 1 )
cb <- as.data.table( cb )

# Write list to excel
my_list <- list( cb, dta.cases )
names( my_list ) <- c( "codebook", "2012-2019_Dataset" )

# Save to excel
wb <- createWorkbook()
fun <- function( data, name ){
  addWorksheet( wb, name )
  writeData( wb, name, data )
}
Map( fun, my_list, names( my_list ) )
saveWorkbook( wb, file = "results/compCases.xlsx", overwrite = TRUE )



