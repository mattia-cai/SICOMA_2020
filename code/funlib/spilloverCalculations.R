spilloverCalculations <- function( dt ){
  # Compute the w weights
  dt[ , w := mktT / go2_a64 ]
  dt[ go2_a64 == 0, w := 0 ]
  
  # Flag and fix any weights in excess of 1
  dt[ w > 1, w_flag := "When w > 1, set w = 1" ]
  dt[ w > 1, w := 1 ]
  
  # Ensure industry are ordered consistently
  setkeyv( dt, c( "year", "nace2_2d_a64" ) )
  nace2 <- dt[, sort( unique( nace2_2d_a64 ) ) ]
  Ad <- Ad[ nace2, nace2 ]
  
  # Warn if rows are not ordered properly 
  if( any( dt[ , nace2_2d_a64 ] != rownames( Ad ) ) ) { 
    warning( "Misaligned industries" ) 
  }
  
  # Compute the spillover effects
  M <- IOpriceSpilloverMatrix( A = Ad, w = dt[ , w ], rho = 1 - dt[ , dp ] )
  return( M )
}