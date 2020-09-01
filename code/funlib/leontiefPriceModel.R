# Functions that implement the Leontief price model


###########################
### Revised methodology ###
###########################

# With spillovers in all industries -- eq. 10 of the report

# IO price spillovers using the revised IO methodology
IOpriceSpillovers <- function( A, w, rho ){
  v <- 1 - colSums( A )          # "Value added" coefficients (This is not value addded, really, as it includes imports)
  A_S <- sweep( A, 1, w, "*" )    # Inputs to shocked industries
  A_N <- A - A_S                  # Inputs to non-shocked
  sigma <- drop( t( v + drop( t( rho ) %*% A_S ) ) %*% solve( diag( nrow( A ) ) - A_N ) )
  return( sigma )
}


# This function applies the same model, but returns additional detail
IOpriceSpilloverMatrix <-  function( A, w, rho ){
  A_S <- w * A                           # Input requirements: products of shocked industries 
  A_N <- ( 1 - w ) * A                   # Input requirements: products of non-shocked industries 
  v <-  1 - colSums( A )                 # Value added vector
  L <- solve( diag( nrow( A_N ) ) - A_N  )
  L <- t( L ) %*% t( A_S )               # L matrix for this model
  sigma.mat <- sweep( L, 2, ( rho - 1 ), "*" )
  # rowSums( sigma.breakdown )           # This is the same as sigma
  mat <- sweep( sigma.mat, 1, ( 1 - w ), "*" )
  return( mat )
}




# #########################
# ### Older methodology ###
# #########################
# 
# # With spillovers only in non-shocked industries -- eq. 9 of the report
# 
# # IO price spillovers using the older methodology 
# IOpriceSpillovers_OLD <- function( A, w, rho ){
#   
#   # Shocked vs non-shocked industries
#   shocked.industries <- names( which( w > 0 ) )
#   non.shocked.industries <- setdiff( names( w ), shocked.industries )
#   
#   # No shock value added vector
#   v2 <- ( 1 - colSums( A ) )[ non.shocked.industries ]  
#   
#   
#   # within industry impact (shocked)
#   pi1 <- ( 1 + w * ( rho - 1 ) )[ shocked.industries ]
#   
#   # from shocked to non-shocked
#   A12 <- A[ shocked.industries, non.shocked.industries ]
#   
#   # from non-shocked to non-shocked
#   A22 <- A[ non.shocked.industries, non.shocked.industries ]
#   
#   # Spillovers to non-shocked industries
#   pi2 <- drop( ( v2 + drop( t( pi1 ) %*% A12 ) ) %*% solve( diag( nrow( A22 ) ) - A22 ) )
#   
#   # Arrange results 
#   sigma <- rep( 1, length( w ) )
#   names( sigma ) <- names( w )
#   sigma[ non.shocked.industries ] <- pi2[ non.shocked.industries ]
#   return( sigma )
#   
# }
# 
# 
# 
