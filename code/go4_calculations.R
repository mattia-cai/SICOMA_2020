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
