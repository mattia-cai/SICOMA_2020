# Break down a data table object dt
# by unique values of the variables in byVar
# save to filename
# including a codebook from pathToCodeBook

exportToExcel <- function( dt, byVar, pathToCodeBook, filename ){
  
  # Codebook
  require( "xlsx" )
  cb <- read.csv( file = pathToCodeBook, header = TRUE, stringsAsFactors = FALSE )
  xlsx::write.xlsx2( cb, file = filename, sheetName = "codebook", row.names = FALSE, na = "" )
  
  
  # Index
  index <- lapply( as.list( dt )[ byVar ], function( x ) sort( unique( x ) ) ) 
  index <- expand.grid( index, stringsAsFactors = F )
  if ( length( byVar ) == 1 ){ index$sheet.nm <- index[ , 1 ]  }
  if ( length( byVar ) > 1 ){ index$sheet.nm <- apply( index, 1, function( x ) paste0( x, collapse = "_" ) )  }
  
  # Loop over values of the index
  for( i in 1 : nrow( index ) ){
    tag <- sapply( byVar, function( x ) drop( dt[ , x, with = F ] == index[ i , x ] ) )
    tag <- rowSums( tag ) == length( byVar )
    sheet.i <- dt[ tag, ]
    sheet.nm <- as.character( index[ i, "sheet.nm" ] )
    xlsx::write.xlsx2( sheet.i, file = filename, sheetName = sheet.nm, row.names = FALSE, na = "", append = TRUE )
  }
}