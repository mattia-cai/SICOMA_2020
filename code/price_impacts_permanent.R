rm( list = ls( ) )

# Compute an permanent price shock analogous to QUEST permanent shock. (Agreed with Roberta on Sep 8th, 2020)

# Dependencies
require( "data.table" )

# Auxiliary functions
flist <- list.files( "code/funlib", pattern = "*.R", full.names = TRUE )
for ( f in flist ){ source( f ) }

# Competition case dataset
load( file = paste0( "outputData/compCases.RData"), verbose = T )

# EU28 supply and use system (2014)
load( "intmData/IO_EU28_2014.RData" )

# keep track of case count
dta.cases[ , case.count := 1 ]



#############################################
### Expected mix of cases in current year ###
#############################################

# Collapse by industry and year
dta <- dta.cases[ , .( case.count = sum( case.count ),
                       mktT = sum( mktT ),
                       dp = weighted.mean( x = delta_p, w = mktT ) ),
                  by = .( year, nace2_2d_a64 ) ]

# When averaging, years with zero cases can't be ignored, they must count as zero
dta <- fullDimDta()

# Average over years 
dta <- dta[ , .( 
  case.count = mean( case.count ),
  mktT = mean( mktT ), 
  dp = mean( dp ) ), by = nace2_2d_a64 ]
dta <- cbind( from = 0, dta )  # Just a place-holder



#######################################
### Contribution of case carry over ###
#######################################

# Carry over of cases from last year (2 year duration or more); carry-over from two years ago (mix of cases with 3 year duration or more) + ...

# Loop and accumulate results in a list 
lst <- list( copy( dta ) )
duration.range <- 2 : dta.cases[ , max( duration ) ]
for ( t in duration.range ){
  dta <- dta.cases[ duration >= (t - 1) , .( case.count = sum( case.count ),
                                      mktT = sum( mktT ),
                                      dp = weighted.mean( x = delta_p, w = mktT ) ),
                    by = .( year, nace2_2d_a64 ) ]
  dta <- fullDimDta()
  dta <- dta[ , .( 
    case.count = mean( case.count ),
    mktT = mean( mktT ), 
    dp = mean( dp ) ), by = nace2_2d_a64 ]
  dta <- cbind( from = (t - 1), dta ) 
  lst <- c( lst, list( dta ) )
}

# Stack it all
dta <- do.call( "rbind", lst )

# Compute the permanent shock
dta <- dta[ , .( case.count = sum( case.count ), 
                 mktT = sum( mktT ),
                 dp = weighted.mean( x = dp, w = mktT ) ),
            by = .( nace2_2d_a64 ) ]
dta[ is.nan( dp ), summary( mktT ) ]
dta[ mktT == 0, dp := 0  ]     # fix the industries without any cases

# Keep track of GO2
dta[, go2_a64 := colSums( Vt )[ dta[ , nace2_2d_a64 ] ] ]



#################################
### Compute spillover effects ###
#################################

# Compute the w weights
dta[ , w := mktT / go2_a64 ]
dta[ go2_a64 == 0, w := 0 ]

# Flag and fix any weights in excess of 1
dta[ w > 1, w_flag := "When w > 1, set w = 1" ]
dta[ w > 1, w := 1 ]

# Ensure industry are ordered consistently
setkeyv( dta, "nace2_2d_a64"  )
nace2 <- dta[, sort( unique( nace2_2d_a64 ) ) ]
Ad <- Ad[ nace2, nace2 ]

# Warn if rows are not ordered properly 
if( any( dta[ , nace2_2d_a64 ] != rownames( Ad ) ) ) { 
  warning( "Misaligned industries" ) 
}

# Compute the spillover effects
M <- IOpriceSpilloverMatrix( A = Ad, w = dta[ , w ], rho = 1 - dta[ , dp ] )

# Within-industry, spillover and total effects
dta[ , within := - ( w * dp ) * 100 ]
dta[ , spill := rowSums( M ) * 100 ]
dta[ , total := within + spill ]

# Save
save( dta, file = "outputData/price_impacts_permanent.RData" )
write.csv( dta, file = "outputData/price_impacts_permanent.csv", row.names = F )

# Take a look at overall effect
dta[ ,.( within = weighted.mean( within, go2_a64 ), 
         spill = weighted.mean( spill, go2_a64 ),
         total = weighted.mean( total, go2_a64 ) ) ]

