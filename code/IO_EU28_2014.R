# Read the IO data 
# Author: MC

rm( list = ls() )

# Dependencies
library( "readxl" )

########################################
### Read main SUT blocks for EU 2014 ###
########################################

# Read the 2014 SUTs 
sheet.names <- c( "supbp", "usebp", "usedombp", "useimpbp" )
for ( nm in sheet.names ){
  assign( nm, read_excel( "inputData/SUTs/SUT2014.xlsx", sheet = nm ) )
}

# Row and column identifiers for table formatting
nace <- read.csv( "inputData/SUTs/nace_2.csv" )
nace <- as.character( unique( nace$nace_2 ) )
cpa <- paste( "CPA_", nace, sep ="" )
primary <- read.csv( "inputData/SUTs/primary.csv" )
final <- read.csv( "inputData/SUTs/final.csv" )

# Useful codes
fin.desc <- as.character( final[ final$selected == 1, "desc" ] )
fin.lab <- as.character( final[ final$selected == 1, "id" ] )
prim.desc <- as.character( primary[ primary$selected == 1, "desc" ] )
prim.lab <- as.character( primary[ primary$selected == 1, "id" ] )

# A function that removes some annoying features of R objects read from excel
tidyArray <- function( X ){ array( as.numeric( as.character( unlist( X ) ) ), dim = dim( X ) ) }

# Construct a function that extracts a certain rows and columns from the relevant table to a proper array
getBlock <- function( x, rowIDs, colIDs, colOfRowIDs, rowOfColIDs ){
  tab <- data.frame( x )
  row.index <- which( as.character( tab[ , colOfRowIDs ] ) %in% rowIDs )
  col.index <- which( sapply( tab[ rowOfColIDs , ], as.character ) %in% colIDs )
  tab <- tab[ row.index, col.index ]
  tab <- tidyArray( tab )
  rownames( tab ) <- rowIDs
  colnames( tab ) <- colIDs
  return( tab )
}

# Use that function to extract the interesting elements of the SUT system:

# Intermediate use matrix
tmpfun <- function( tabnm ){ getBlock( x = tabnm, rowIDs = cpa, colIDs = nace , colOfRowIDs = 2, rowOfColIDs = 5 ) }
Utot <- tmpfun( usebp )
Ud <- tmpfun( usedombp )
Um <- tmpfun( useimpbp )

# Supply (make) matrix
Vt <- tmpfun( supbp )

# Final use matrices
tmpfun <- function( tabnm ){ getBlock( x = tabnm, rowIDs = cpa, colIDs = fin.desc, colOfRowIDs = 2, rowOfColIDs = 4 ) }
Gtot <- tmpfun( usebp )
Gd <- tmpfun( usedombp )
Gm <- tmpfun( useimpbp )
colnames( Gtot ) <- fin.lab
colnames( Gd ) <- fin.lab
colnames( Gm ) <- fin.lab

# taxes less subsidies
w <- getBlock( usebp, rowIDs = prim.desc, colIDs = nace , colOfRowIDs = 3, rowOfColIDs = 5 )
rownames( w ) <- prim.lab
tls <- w[ "tls", ]

# Value added
va <- w[ "va", ]




######################################
### Compute the main SIOT elements ### 
######################################

### Obtain an industry-by-industry table using the model D transformation matrix

# Make matrix (industry by product)
V <- t( Vt )

# Industry outputs
g <- rowSums( V )

# Product output
q <- colSums( V )

# Model D transformation matrix
T.D <- sweep( V, 2, q, "/" )
T.D[ is.nan( T.D ) ] <- 0

# Apply the transformation
Zd <- T.D %*% Ud 
Zm <- T.D %*% Um
Ztot <- T.D %*% Utot
Fd <- T.D %*% Gd 
Fm <- T.D %*% Gm
Ftot <- T.D %*% Gtot


# Input coefficient matrices (deal with zeros)
inputCoefMat <- function( mat, vec ){
  vec <- 1 / vec
  vec[ is.infinite( vec )] <- 0
  coeff <- sweep( mat, 2, vec, "*" )
  return( coeff )
}
Ad <- inputCoefMat( Zd, g )
Am <- inputCoefMat( Zm, g )
Atot <- inputCoefMat( Ztot, g )




##################################
### Store this stuff to a file ###
##################################

# Store it all
#rm( list = c( sheet.names, "fin.desc", "prim.desc", "nm", "tmpfun", "sheet.names" ) )
store.me <- c( "Utot", "Ud", "Um", "Vt", "Gtot", "Gd", "Gm", "Atot", "Ad", "Am", "Ftot", "Fd", "Fm", "Ztot", "Zd", "Zm", "va", "tls",  "T.D" )
rm( list = setdiff( ls( ), store.me ) )
save.image( file = "intmData/IO_EU28_2014.RData" )

# Keep a copy of the coefficients
write.csv( Ad, "outputData/input_coefficients.csv" )



