# Pre-processing of the Ecfin markup/gross output data
# author: mattia cai

rm( list = ls( ) )

# Dependencies 
require( "readxl" )
require( "data.table" )
#require( "ggplot2" )

# Markup data from EU-KLEMs
dta.mup <- read_excel( "inputData/EcfinData/181015_mup_file_for_JRC_2.xlsx", sheet = "mups_new" )
dta.mup <- data.frame( dta.mup, stringsAsFactors = F )

# Matching NACE codes
nace2 <- read.csv( "inputData/SUTs/nace_2.csv", header = T, stringsAsFactors = F )

# Only keep the relevant variables
colnames( dta.mup ) <- tolower( colnames( dta.mup ) )
dta.mup <- dta.mup[ , c( "nace2", "go_eu28_2015", "mup_wr_eu" )]

# Are all the NACE2 codes accounted for in my dictionary?
setdiff( dta.mup[ , "nace2" ], nace2[ , "X1809_ecfin_data" ] )      # Should be empty
# Yes!

# Drop the unnecessary rows
dta.mup <- dta.mup[ dta.mup[ , "nace2" ] %in%  setdiff( dta.mup[ , "nace2" ], c( "Total", NA ) ), ] 

# Sort and save
index <- unique( nace2[ , "X1809_ecfin_data" ][ nace2[ , "X1809_ecfin_data" ] != "" ] )
dta.mup <- dta.mup[ match( index, dta.mup[ , "nace2" ] ), ]
names( dta.mup )[ names( dta.mup ) == "go_eu28_2015" ] <- "go_eu_2015"
save( dta.mup, file = "intmData/EcfinMupGo.RData" )


