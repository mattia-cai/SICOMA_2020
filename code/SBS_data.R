# Extract the relevant data from the Structural Business Statistics (SBS)
# author: mattia cai

# Extraction:  
# In July 2018, we decided that will use production value (V12120) as the basis for disaggregating gross output. Just to be on the safe side, I will keep extracting the variables that were used previously:
# - V12150 (Value added at factor cost - million euro)
# - I will also extract: V12110 (Turnover or gross premiums written - million euro)
# The relevant geographical aggregate is EU28


rm( list = ls( ) )
require( "data.table" )

##################
### Input data ###
##################

# sbs_na_ind_r2 -- Annual detailed enterprise statistics for industry (NACE Rev. 2, B-E)
dt.ind <- read.table( "inputData/SBS/sbs_na_ind_r2.tsv", header = T, sep = "\t" )

# sbs_na_con_r2 -- Annual detailed enterprise statistics for construction (NACE Rev. 2, F)
dt.con <- read.table( "inputData/SBS/sbs_na_con_r2.tsv", header = T, sep = "\t" )

# sbs_na_dt_r2 -- Annual detailed enterprise statistics for trade (NACE Rev. 2 G)
dt.dt <- read.table( "inputData/SBS/sbs_na_dt_r2.tsv", header = T, sep = "\t" )

# sbs_na_1a_se_r2 -- Annual detailed enterprise statistics for services (NACE Rev. 2 H-N and S95)
dt.se <- read.table( "inputData/SBS/sbs_na_1a_se_r2.tsv", header = T, sep = "\t" )



############################
### Extract and clean-up ###
############################

# Cleanup one dataset at a time
fun <- function( dta ){
  
  # There is something weird with the first column
  lft.cols <- as.character( dta[ , 1 ] )
  lft.cols <- tstrsplit( x = lft.cols, split = "," )
  lft.cols <- do.call( "cbind", lft.cols )
  colnames( lft.cols ) <- c( "nace2", "indic_sb", "geo" ) 
  dta <- cbind( lft.cols, dta[ , - 1 ] )
  
  # To long format
  dta <- as.data.table( dta )
  dta <- melt( data = dta, id.vars = c( "nace2", "indic_sb", "geo" ), variable.name = "year"  )
  dta[ , year := as.numeric( gsub( pattern = "X", "", year ) ) ]
  
  # Only keep potentially relevant items
  dta <- dta[ year %in% 2012 : 2016 ]
  dta <- dta[ geo == "EU28" ]
  dta <- dta[ indic_sb %in% c( "V12120", "V12150", "V12110" ) ]
  
  # Flags
  dta[ , val := as.numeric( gsub( "[^0-9\\.]", "", value ) ) ]
  dta[ is.na( val ), table( value ) ]
  dta[ , flags := gsub( "[^a-z]", "", value ) ]
  dta[ flags == "", flags := NA ]
  
  # Checks - all values in the FALSE column should be zero
  suppressWarnings( dta[ , table( flags, is.na( as.numeric( value ) ) ) ] )
  
  # output
  dta[ , value := NULL ]
  return( dta )
}

# Apply listwise, stack, drop unused factor levels and sort
dta.sbs <- lapply( list( dt.ind, dt.con, dt.dt, dt.se ), fun ) 
dta.sbs <- do.call( 'rbind', dta.sbs )
cols <- c( "nace2", "indic_sb", "geo" )
dta.sbs[ , ( cols ) := lapply( .SD, droplevels ), .SDcols = cols ]

# SBS data: long
setkeyv( dta.sbs, cols = c( "geo", "indic_sb", "nace2", "year" ) )
save( dta.sbs, file = "intmData/SBS_data_long.RData" ) 
write.csv( dta.sbs, file = "intmData/SBS_data_long.csv", row.names = F, na = "" ) 



###########################
### Reshape the to wide ###
###########################

# Note that all missing values in the dataset are due to confidentiality 
dta.sbs[ is.na( val ), table( flags ) ]

# As you would expect, you have more NAs in recent years
dta.sbs[ , table( year, is.na( val ) ) ]

# SBS data to wide
# dta.sbs[ , flags := ( flags == "c" ) ]
# dta.sbs[ flags == FALSE, flags := NA ]
dta.sbs <- data.table::dcast( data = dta.sbs, nace2 + year ~ indic_sb, value.var = "val" )
setnames( dta.sbs , old = c( "V12110", "V12120", "V12150" ), new = c( "to", "prod", "va" ) )

# What year do I want to use?
# Three options, I'd say:
# 1) 2015 like ISPRA (Discussed with Roberta on Jun 18, 2018)
# 2) 2014 like IOT 
# 3) 2012-14 average to minimize NAs, and for consistency with the previous set of results
dta.sbs[, table( year ) ]

# Given the use I want to make of these data, I would go for 2012-15 average
# dta.sbs <- dta.sbs[ , lapply( .SD, function( x ) mean( x, na.rm = T ) ), by = .( nace2 ), .SDcols = c( "to" , "prod", "va" ) ]
dta.sbs[ is.nan( to ), isConfTo := TRUE ]
dta.sbs[ is.nan( to ), to := NA ]
dta.sbs[ is.nan( prod ), isConfProd := TRUE ]
dta.sbs[ is.nan( prod ), prod := NA ]
dta.sbs[ is.nan( va ), isConfVa := TRUE ]
dta.sbs[ is.nan( va ), va := NA ]
setkeyv( dta.sbs, cols = c( "nace2", "year" ) )
save( dta.sbs, file = "intmData/SBS_data_wide.RData" ) 
write.csv( dta.sbs, file = "intmData/SBS_data_wide.csv", row.names = F, na = "" ) 







