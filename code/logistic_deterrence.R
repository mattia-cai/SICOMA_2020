# Compute deterrence-adjusted market size

rm( list = ls( ) )

# Dependencies
require( "data.table" )

# Case dataset
load( file = "intmData/go4_calculations.RData", verbose = T )

# logistic function parameters
B <- data.frame( chi = c( 100, 100 ), 
            ef = c( 0.025906, 0.014891 ),  # Updated following Filippo's email of Sep 18
            ny = c( 1, 1 ) )
rownames( B ) <- c( "M", "C" )

# Checks
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

# Size of the deterred market
dta.cases[ type == "merger", y := y.fun( x = mkt / go4_a64 , 
                                         chi = B[ "M", "chi" ], 
                                         ef = B[ "M", "ef" ], 
                                         ny = B[ "M", "ny" ] ) ]
dta.cases[ type == "cartel", y := y.fun( x = mkt / go4_a64 , 
                                         chi = B[ "C", "chi" ], 
                                         ef = B[ "C", "ef" ], 
                                         ny = B[ "C", "ny" ] ) ]
dta.cases[ , mktD := y * ( go4_a64 - mkt ) ]

# How to handle the cases with mkt < GO4?
# cases with mkt > GO4
dta.cases[ mktD < 0 ]
dta.cases[ mkt > go4_a64 ]
dta.cases[ mktD < 0, mktD := 0 ]

# Market size with deterrence
dta.cases[ , mktT := mkt + mktD ]

# save
save( dta.cases, file = "intmData/logistic_deterrence.RData" )

