rm( list = ls( ) )

# Compute the price impacts using the leontief price model. Year-by-year analysis without duration

# Dependencies
require( "data.table" )

# Auxiliary functions
flist <- list.files( "code/funlib", pattern = "*.R", full.names = TRUE )
for ( f in flist ){ source( f ) }

# Competition case dataset
load( file = paste0( "outputData/compCases.RData"), verbose = T )

# EU28 supply and use system (2014)
load( "intmData/IO_EU28_2014.RData" )

# Years for which data are available
yrs <- dta.cases[, min( year ) ] : dta.cases[, max( year ) ]

# Collapse cases by NACE 2 and year
dta.cases[ , case.count := 1 ]
dta <- dta.cases[ , .( case.count = sum( case.count ),
                       mktT = sum( mktT ),
                       dp = weighted.mean( x = delta_p, w = mktT ) ),
                  by = .( year, nace2_2d_a64 ) ]


# Expand dataset to full industry dimensions
dta <- fullDimDta()

# Keep track of GO2
dta[, go2_a64 := colSums( Vt )[ dta[ , nace2_2d_a64 ] ] ]

# Split by year
dta.lst <- split( dta, f = dta[ , year ] )

# Compute the spillover matrices listwise
mat.lst <- lapply( dta.lst, spilloverCalculations )
# If necessary, these can saved for further manipulation

# Within-industry, spillover and total effects
for ( t in as.character( yrs ) ) {
  dta.lst[[ t ]][ , within := - ( w * dp ) * 100 ]
  dta.lst[[ t ]][ , spill := rowSums( mat.lst[[ t ]] ) * 100 ]
  dta.lst[[ t ]][ , total := within + spill ]
}

# Stack and save
dta <- do.call( "rbind", dta.lst )
save( dta, file = "outputData/price_impacts_no_duration.RData" )
write.csv( dta, file = "outputData/price_impacts_no_duration.csv", row.names = F )


