# Compute deterrence-adjusted market size

rm( list = ls( ) )

# Dependencies
require( "data.table" )
require( "openxlsx" )

# Case dataset
load( file = "intmData/logistic_deterrence.RData", verbose = T )

# Price overcharge assumptions
dta.cases[ type == "merger", delta_p := .03 ]
dta.cases[ type == "cartel", delta_p := .15 ]


# In the IO table L68 is broken into L68A and L68B. L68A is imputed rents, which in practice is just a bunch of zeros. The part I care about is L68B. We don't seem to have any L68 cases, it seems, but just in case...
dta.cases[ nace2_2d_a64 == "L68"  ]
dta.cases[ nace2_2d_a64 == "L68", nace2_2d_a64 := "L68B" ]

# Column order selection
dta.cases <- dta.cases[ , .( id, type, year, nace2_4d,
                             duration, delta_p, mkt, mktD, mktT,
                             nace2_2d_a24, mup_a24, go2_a24, go4_a24, go4_a24_notes, 
                             nace2_2d_a64, go2_a64, go4_a64, go4_a64_notes, go ) ]
setnames( dta.cases, "id", "case_id" )

# Save as .RData
save( dta.cases, file = "outputData/compCases.RData" )

# Export to Excel (with codebook)
cb <- readxl::read_excel( path = "inputData/DgComp/case_data_codebook.xlsx", sheet = 1 )
cb <- as.data.table( cb )

# Write list to excel
my_list <- list( cb, dta.cases )
names( my_list ) <- c( "codebook", "2012-2019_Dataset" )

# Save to excel
wb <- createWorkbook()
fun <- function( data, name ){
  addWorksheet( wb, name )
  writeData( wb, name, data )
}
Map( fun, my_list, names( my_list ) )
saveWorkbook( wb, file = "outputData/compCases.xlsx", overwrite = TRUE )



