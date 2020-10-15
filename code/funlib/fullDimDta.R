# Auxiliary function used only to preprocess the exogenous price shocks to IO model
fullDimDta <- function(){
  dt.grid <- expand.grid( year = dta.cases[, unique( year ) ],
                          nace2_2d_a64 = colnames( Vt ), stringsAsFactors = F ) 
  dt.grid <- as.data.table( dt.grid )
  dta <- merge( dt.grid, dta, by = c( "year", "nace2_2d_a64" ), all = T )
  cols <- c( "case.count", "mktT", "dp" )
  fun <- function( z ){ z[ is.na( z ) ] <- 0; return( z ) }
  dta[ , ( cols ) := lapply( .SD, fun ), .SDcols = cols ]
  setkeyv( dta, c( "year", "nace2_2d_a64" ) )
  return( dta )
}
