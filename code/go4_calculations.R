# GO4 estimation (Appendix C)

rm( list = ls( ) )

# Dependencies
require( "data.table" )

# Case dataset
load( file = "intmData/full_dataset.RData", verbose = T )

# Cases in which market size > 4-digit production value from SBS 
dta.cases[ mkt > prod4, .( year, type, id, nace2_4d, mkt, prod4 ) ]

# GO4 estimates
dta.cases[ , go4_a64_prod := ( prod4 / prod2_a64 ) * go2_a64  ]
dta.cases[ , go4_a24_prod := ( prod4 / prod2_a24 ) * go2_a24  ]

# Fill NAs with VA if possible
dta.cases[ , go4_a64_va := ( va4 / va2_a64 ) * go2_a64  ]
dta.cases[ , go4_a24_va := ( va4 / va2_a24 ) * go2_a24  ]

# When SBS data not available, set GO4_ = go2_a64 / n4
dta.cases[ , go4_a64_n4 := go2_a64 / n4_a64 ]
dta.cases[ , go4_a24_n4 := go2_a24 / n4_a24 ]

# GO4 estimate - based on A64
dta.cases[ , go4_a64 := go4_a64_prod ]
dta.cases[ , go4_a64_notes := "go4_a64_prod" ]
dta.cases[ is.na( go4_a64 ) & !is.na( go4_a64_va ), ]
dta.cases[ is.na( go4_a64 ) & !is.na( go4_a64_va ), go4_a64_notes := "go4_a64_va" ]
dta.cases[ is.na( go4_a64 ) & !is.na( go4_a64_va ), go4_a64 := go4_a64_va ]
dta.cases[ is.na( go4_a64 ), ]
dta.cases[ is.na( go4_a64 ) , go4_a64_notes := "go4_a64_n4" ]
dta.cases[ is.na( go4_a64 ), go4_a64 := go4_a64_n4 ]

# GO4 estimate - based on A24
dta.cases[ , go4_a24 := go4_a24_prod ]
dta.cases[ , go4_a24_notes := "go4_a24_prod" ]
dta.cases[ is.na( go4_a24 ) & !is.na( go4_a24_va ), ]
dta.cases[ is.na( go4_a24 ) & !is.na( go4_a24_va ), go4_a24_notes := "go4_a24_va" ]
dta.cases[ is.na( go4_a24 ) & !is.na( go4_a24_va ), go4_a24 := go4_a24_va ]
dta.cases[ is.na( go4_a24 ), ]
dta.cases[ is.na( go4_a24 ) , go4_a24_notes := "go4_a24_n4" ]
dta.cases[ is.na( go4_a24 ), go4_a24 := go4_a24_n4 ]

# In fact, we are not making any use of the value added data 
dta.cases[ , table( go4_a24_notes, go4_a64_notes ) ]

# # The A24 and A64 measures are reasonably close to each other 
# dta.cases[ , cor( go4_a64, go4_a24 ) ] 
# dta.cases[ , plot( go4_a64, go4_a24 ) ] 
# abline( 0, 1, col = "red" )

# Save
save( dta.cases, file = "intmData/go4_calculations.RData" )





##############################################
### Nuisance: large cases in small sectors ###
##############################################

### Nov 30th, 2020 update --- THIS IS TEMPORARY ###

# Comments to COMP:
# -- Merge with markup?
# -- Format of input data suitable for processing
# -- "A.T. 39639" vs. "AT 39639" 

# In some cases, affected market size is larger than estimated go4
dta.cases[ mkt > go4_a64 ]

# I will consider a fix proposed by in Adriaan's email of Nov 12th, 2020. Assign multiple code to these cases and assign sum of GO4 over sectors
multicode <- read.csv( "inputData/DgComp/201112_cases_with_multiple_nace2_code.csv", header = T, stringsAsFactors = F )
multicode <- as.data.table( multicode )
multicode <- melt( multicode, id.vars = c( "Year", "Case.number" ) )
names( multicode ) <-  tolower( names( multicode ) )
setnames( multicode, old = c( "case.number", "value" ), new = c( "id", "nace2_4d" ) )
multicode[ , variable := NULL ]

# Let's see if I already have these nace2 codes covered in the case data:
setdiff( multicode[ , nace2_4d ], dta.cases[ , nace2_4d ] ) # not too bad
tmp <- unique( dta.cases[, .( nace2_4d, go4_a64 ) ] )
( multicode <- merge( multicode, tmp, by = "nace2_4d", all.x = TRUE ) )
# For the time being, the rest of this calculation is done manually in excel

# Fix manually Est. GO4
go4revised <- c( 17816, 18652, 80098, 47680, 78119 ) 
index <- c( "A.T. 39639", "M.6541", "M.7881", "M.8480", "M. 8785" )
dta.cases[ , go4_rev := go4_a64 ]
dta.cases[ , tmp := go4revised[ match( dta.cases[ , id ], index ) ] ]
dta.cases[ !is.na( tmp ), go4_rev := tmp ]
dta.cases[, tmp := NULL ]

# COMMENT THIS OUT IF NECESSARY
#dta.cases[, go4_a64 := go4_rev ]
save( dta.cases, file = "intmData/go4_calculations.RData" )
