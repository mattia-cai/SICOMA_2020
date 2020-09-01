rm( list = ls() )

### Input data ###

# Input coefficients
A <- c( .15, .25, .05,
        .20, .05, .40,
        .30, .25, .05 )
A <- matrix( A, nrow = 3, ncol = 3, byrow = TRUE )

# Weights
w <- c( 0, 0, .25 )

# Exogenous price shocks
rho <- c( 1, 1, .9 )



### Calculations (equation 10) ###

# Value added
v <- 1 - colSums( A )

# Shocked
A.S <- diag( w ) %*% A

# Non shocked
A.N <- diag( 1 - w ) %*% A

# Leontief matrix
L.N <- solve( diag( 3 ) - A.N )

# Solve for spillovers
sigma <- drop( ( v + t( rho ) %*% A.S ) %*% L.N )



### Calculations (equation 11) ###

# Shock 
r <- ( rho - 1 )

# Spillovers 
s <- drop( t( L.N ) %*% t( A.S ) %*% r )


### Results ###

tab <- cbind( "shocked" = w, r,  "within" = w * r, "Non-shocked"= (1 - w), s, "Spillover"= (1 - w) * s, "Total" = w * r + (1 - w) * s ) * 100
write.csv( tab, file = "results/NumericalExample.csv", row.names = F )

