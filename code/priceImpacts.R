rm( list = ls( ) )

# Compute the price impacts using the leontief price model
# Compared to the 1903 version, I have also included a second version
# of the analysis that accounts for case duration
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
require( "openxlsx" )
source( "code/funlib/leontiefPriceModel.R" )
source( "code/funlib/write_list.R" )    # Revised so that rJava is not needed

# dataset -- (single Nace 2 assignment)
load( file = paste0( "outputData/compCases.RData"), verbose = T )
str( dta.cases )

# EU28 supply and use system (2014)
load( "intmData/IO_EU28_2014.RData" )

# Results Codebook
# codebook <- read.csv( "inputData/resultCodebooks/priceImpacts_codebook.csv", header = T, stringsAsFactors = F )



##########################
### Familiar L68 issue ### 
##########################

# In the IO table L68 is broken into L68A and L68B
# L68A is imputed rents, which in practice is just a bunch of zeros.
# The only part I care about is L68B
# In my dataset I have a unique L68 industry.
# We don't have any L68 cases, it seems
setdiff( dta.cases[ , unique( nace2_2d_a64 ) ], colnames( Vt ) ) # Should be empty

# But just in case...
dta.cases[ nace2_2d_a64 == "L68", nace2_2d_a64 := "L68B" ]
all( dta.cases[ , unique( nace2_2d_a64 ) ] %in% colnames( Vt ) ) # Should be T



################################################
### Collapse the case data to 2-digit NACE 2 ###
################################################

# Here I am not making any attempt to account for case duration
dta.cases[ , case.count := 1 ]
dta <- dta.cases[ , .( case.count = sum( case.count ), 
                       mkt_none = sum( mkt ), 
                       mkt_logistic = sum( mktT ),
                       dp_none = weighted.mean( x = delta_p, w = mkt ),
                       dp_logistic = weighted.mean( x = delta_p, w = mktT ),
                       go2_a64 = mean( go2_a64 ) ),
                  by = .( year, nace2_2d_a64 ) ]

# At a first glance, it seems ok
dta.cases[ , .( mkt_none = sum( mkt ), mkt_logistic = sum( mktT ) ) , by = year ]
dta[ , .( mkt_none = sum( mkt_none ), mkt_logistic = sum( mkt_logistic ) ), by = year ]

# Reshape from wide to long
dta <- copy( reshape( dta, direction = "long",
                      varying = list(
                        grep( "mkt", colnames( dta ), value = T ),
                        grep( "dp", colnames( dta ), value = T) ),
                      v.names = c( "mkt", "dp" ) ,
                      times = c( "none", "logistic" ),
                      timevar = "deterrence" ) )



#################################
### Duration (2012-2019 data) ###
#################################

# Last year in which a case produces effects
dta.cases[ , through := year + duration - 1 ]
dta.cases[ , .( case_id, type, year, duration, through ) ]

# Years for which data are available
min.year <- dta.cases[ , min( year ) ]
max.year <- dta.cases[ , max( year ) ]

# Listwise: aggregate by year
fun <- function( t ){ 
  dt <- dta.cases[ t >= year & t <= through, ] # cases producing effects in year t
  dt <- dt[ , .( case.count = sum( case.count ), 
                 mkt_none = sum( mkt ), 
                 mkt_logistic = sum( mktT ),
                 dp_none = weighted.mean( x = delta_p, w = mkt ),
                 dp_logistic = weighted.mean( x = delta_p, w = mktT ),
                 go2_a64 = mean( go2_a64 ) ),
            by = .( nace2_2d_a64 ) ]
  dt <- cbind( year = t, dt )
  return( dt )
}
lst <- lapply( min.year : max.year, fun ) 

# Stack
dta1 <- do.call( "rbind", lst )

# From wide to long
dta1 <- copy( reshape( dta1, direction = "long",
                       varying = list(
                         grep( "mkt", colnames( dta1 ), value = T ),
                         grep( "dp", colnames( dta1 ), value = T) ),
                       v.names = c( "mkt", "dp" ) ,
                       times = c( "none", "logistic" ),
                       timevar = "deterrence" ) )



##############################################
### Combine analysis with/without duration ###
##############################################

# Without duration
dta <- cbind( duration = "woDur" , dta ) 

# With duration
dta1 <- cbind( duration = "wDur" , dta1 ) 

# Stack
dta <- rbind( dta, dta1 )
dta[ , id := NULL ]



##############################
### w and rho calculations ###
##############################

# Weights
dta[ , w := mkt / go2_a64 ]

# Any weights in excess of 1?
dta[ , any( w > 1 ) ]            # Yep...

# W > 1 only occurs when duraction is taken into account,
# it is relatively rare, an only affects industry J61 in years 2016 and 2018 
dta[ w > 1 ]

# Overall, it is not a big deal, I think: I'll just cap the weight at 1 in these cases
dta[ w > 1, w := 1 ]

# I won't need these
dta[ ,  mkt := NULL ]
dta[ ,  go2_a64 := NULL ]




###############################
### expand Nace 2 structure ###
###############################

# Fill in the missing indutries
setkeyv( dta, c( "year" , "duration", "deterrence", "nace2_2d_a64" ) )
dta.grid <- expand.grid( year = dta[ , unique( year ) ], 
                         duration = dta[ , unique( duration ) ],
                         deterrence = dta[ , unique( deterrence ) ], 
                         nace2_2d_a64 = colnames( Vt ), stringsAsFactors = F )
dta.grid <- as.data.table( dta.grid )
setkeyv( dta.grid, c( "year" , "duration", "deterrence", "nace2_2d_a64" ) )
dta <- dta[ dta.grid ]
fun <- function( z ){ z[ is.na( z ) ] <- 0; return( z ) }
cols <- c( "case.count", "w", "dp" )
dta[ , ( cols ) := lapply( .SD, fun ), .SDcols = cols ]

# The annoying L68A-B rows... don't forget
table( dta[ , nace2_2d_a64 ] == colnames( Vt ) ) 
dta[ which( dta[ , nace2_2d_a64 ] != colnames( Vt ) ) , ]
# Eventually, store a copy of the input data (with codebook relating that to the report)



# ###############################
# ### Average shock over time ###
# ###############################
# 
# # Can I market this as equivalent to the permanent shock in QUEST?
# dta.avg <- dta[ ,.( case.count = mean( case.count ), dp = mean( dp ), w = mean( w ) ) , by = .( nace2_2d_a64, deterrence, duration ) ]
# dta.avg[ , year := 9999 ]
# dta <- rbind( dta, dta.avg )



#########################################
### Recover GO2 (aggregation weights) ### 
#########################################

dta[, go2_a64 := colSums( Vt )[ dta[ , nace2_2d_a64 ] ] ]



###############################
### within-industry effects ###
###############################

#percentage price change
dta[ , within := - ( w * dp ) * 100  ]



##########################################
### Arrange for spillover calculations ###
##########################################

lst <- split( dta, f = dta[ , .( year, deterrence, duration ) ] )
lst <- lapply( lst, function( z ) z[ match( z[ , nace2_2d_a64 ], rownames( Ad ) ), ] )
# Check order
all( sapply( lst, function( z ) { all( z[ , nace2_2d_a64 ] == rownames( Ad ) ) } ) )  # Should be T



##############################################################
### Model one year-deterrence-duration combination at time ###
##############################################################

# List of spillover matrices
mat.lst <- lapply( lst, function( dt ) IOpriceSpilloverMatrix( A = Ad, w = dt[ , w ], rho = 1 - dt[ , dp ] ) )

# Averages over time
det.labs <- dta[ , unique( deterrence ) ]
dur.labs <- dta[ , unique( duration ) ]
tmpfun <- function( i, j ){ 
  x <- mat.lst[ grepl( names( mat.lst ), pattern = paste0( i,".", j ) ) ]
  x <- Reduce( '+', x ) / length( x )
  return( x )  
}
index <- expand.grid( det.labs, dur.labs, stringsAsFactors = F )
index <- split( index, f = 1 : nrow( index ) )
avg.lst <- lapply( index, function( x ) tmpfun( x[ 1 ], x[ 2 ] ) )

# Labels
names( avg.lst ) <- sapply( index, function( x ) paste0( "9999.", x[ 1 ], ".", x[ 2 ] ) )
mat.lst <- c( mat.lst, avg.lst )



##############################################################
### Save the annual spillover matrices (to separate files) ###
##############################################################

# Excel files for COMP
for ( i in dta[ , unique( deterrence ) ] ){
  for ( j in dta[ , unique( duration ) ] ){
    spill.mats <- mat.lst[ grepl( names( mat.lst ), pattern = paste0( i,".", j ) ) ]
    names( spill.mats ) <- substr( names( spill.mats ), 1, 4 ) 
    fun <- function( X ){
      X <- X * 100
      go <- colSums( Vt )[ rownames( X ) ]
      contr <- drop( ( go / sum( go ) ) %*% X )
      share <- ( contr / sum( contr ) ) * 100
      X <- rbind( X, "Contribution to spillover" = contr, "Percent of spillover" = share )
      X <- as.data.frame( X )
      X <- cbind( "gross output" = c( go, NA, NA ), X, "total" = rowSums( X ) )
      return( X )    
    }
    spill.mats <- lapply( spill.mats, fun )
    spill.mats <- lapply( spill.mats, as.data.frame )
    fpath <- paste0( "results/spillMat_", i, "_", j, ".xlsx" )
    write_list( my_list = spill.mats, wb_name = fpath )
  }
}

# Save in R format as well, just in case
spillover.matrices <- mat.lst
save( spillover.matrices, file = paste0( "results/spillMatrices.RData" ) )



#############################################
### Industries as receivers of spillovers ###
#############################################

# Spillover effects
vec <- unlist( lapply( mat.lst, rowSums ) )
vec <- vec[ dta[ , paste( year, deterrence, duration, nace2_2d_a64, sep ="." ) ] ]
dta[ , spillover := vec * 100 ]

# Total effect
dta[ , total := within + spillover ]

# Clean and save
dta[ , c( "case.count", "dp", "w" ) := NULL ]

# Average over entire period
dta.avg <- dta[ , .( go2_a64 = mean( go2_a64 ),
                     within = mean( within ),
                     spillover = mean( spillover ),
                     total = mean( total ) ), by = .( nace2_2d_a64, deterrence, duration )]
dta.avg[ , year := 9999 ]
dta <- rbind( dta, dta.avg )

# For work in R
save( dta, file = paste0( "results/industryOfDestination.RData" ) )
write.csv( dta, file = paste0( "results/industryOfDestination.csv" ), row.names = F, na = "" )

# Excel files for COMP
dt.lst <- split( dta, dta[, .( deterrence, duration ) ] )
dt.lst <- lapply( dt.lst , function( x ) setkeyv( x, c( "year", "nace2_2d_a64" ) ) )
for ( i in names( dt.lst ) ){
  dt.lst.i <- dt.lst[[ i ]]
  dt.lst.i[ , deterrence := NULL ]
  fpath <- paste0( "results/industryOfDestination_", gsub( x = i, pattern = ".", replacement = "_", fixed = T ) , ".xlsx" )
  write_list( split( dt.lst.i, dt.lst.i[, year ] ), fpath )
}



#############################################
### Industries as producers of spillovers ###
#############################################

# Aggregation weights 
dta[ , wgt := sum( go2_a64 ), by = .( year, deterrence, duration ) ]
dta[ , wgt := go2_a64 / wgt ]

# Economy-wide within effect (shock)
dta[ , within := ( within * wgt ) ]
dta[ , within.ew := sum( within ), by = .( year, deterrence, duration ) ]

#  Industry contribution to shock
dta[ , within.decomposition := ( within  / within.ew ) * 100 ]
#dta[ , sum( within.decomposition ), by = .( year, deterrence, duration ) ]

# Contribution economy-wide spillover effect
vec <- unlist( lapply( mat.lst, function( X ) drop( ( colSums( Vt )[ rownames( X ) ] / sum( Vt ) ) %*% X ) ) )
vec <- vec[ dta[ , paste( year, deterrence, duration, nace2_2d_a64, sep ="." ) ] ]
dta[ , spillover := vec * 100 ]

# Economy-wide spillover effect (shock)
dta[ , spillover.ew := sum( spillover ), by = .( year, deterrence, duration ) ]

#  Industry contributions
dta[ , spillover.decomposition := spillover / spillover.ew * 100 ]

#dta[ , sum( within.ew.share ), by = .( year, deterrence ) ]
dta[ , sum( spillover.decomposition ), by = .( year, deterrence, duration ) ]

# Total, economy-wide
dta[ , total := within + spillover ]
dta[ , total.ew := sum( total ), by = .( year, deterrence, duration ) ]

#  Industry contributions to total
dta[ , total.decomposition := total / total.ew * 100 ]

# In retrospect I should have picked better names
dta[ , c( "go2_a64", "wgt", "within", "spillover", "total" ) :=NULL]
old <- c( "within.ew", "within.decomposition", "spillover.ew", "spillover.decomposition",  "total.ew", "total.decomposition"  )
new <- c( "within", "pctOfWithin", "spillover", "pctOfSpillover",  "total", "pctOfTotal"  )
setnames( dta, old, new ) 

# Excel files for COMP
dt.lst <- split( dta, dta[, .( deterrence, duration ) ] )
dt.lst <- lapply( dt.lst , function( x ) setkeyv( x, c( "year", "nace2_2d_a64" ) ) )
for ( i in names( dt.lst ) ){
  dt.lst.i <- dt.lst[[ i ]]
  dt.lst.i[ , deterrence := NULL ]
  fpath <- paste0( "results/industryOfOrigin_", gsub( x = i, pattern = ".", replacement = "_", fixed = T ), ".xlsx" )
  write_list( split( dt.lst.i, dt.lst.i[, year ] ), fpath )
}

# For work in R
save( dta, file = paste0( "results/industryOfOrigin.RData" ) )
write.csv( dta, file = paste0( "results/industryOfOrigin.csv" ), row.names = F, na = "" )
