rm( list = ls( ) )

###################################
### Input data and dependencies ###
###################################

# Dependencies
library( "reshape2" )
library( "data.table" )

# National accounts A64
load( file = "intmData/nama10_a64.RData" )

# Eu membership
countries <- read.csv( file = "inputData/NAMA_10_a64/geo_labels.csv", header = T, stringsAsFactors = F )

# NACE2 code dictionary
nace2 <- read.csv( "inputData/SUTs/nace_2.csv", header = T, stringsAsFactors = F )




##############
### Filter ###
##############

# Dataset
dta <- nama10_a64

# Gross output
dta <- dta[ indic == "P1" ] 

# Year
dta <- dta[ year == 2014 ]

# Current prices
dta <- dta[ unit == "CP_MEUR"  ]

# No ready-made EU data 
dta[ geo == "EU28" ]

# EU 28 countries
eu <- countries[ countries$eu == 1, "geo" ]
setdiff( eu, dta[ , geo ] )

#eu1 <- c( "AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", "RO", "SE", "SI", "SK", "UK" )
dta <- dta[ , geo := as.character( geo ) ]
dta <- dta[ geo %in% eu ]


# A64 industries as in the IO table 
dta[ , nace_r2 := as.character( nace_r2 ) ]
nace2 <- nace2[ , c( "nace_2", "X1809_ecfin_data" ) ]
setdiff( nace2$nace_2, dta[ , nace_r2 ] )
dta[ nace_r2 == "D", nace_r2 := "D35"]
nace2[ nace2$nace_2 == "L68B",  ]
nace2[ nace2$nace_2 == "L68B", "nace_2" ] <- "L68"
nace2 <- nace2[ nace2$nace_2 != "L68A",  ]
dta[ nace_r2 == "L68A", ]     # Imputed rent
dta[ nace_r2 == "L", nace_r2 := "L68" ]
dta[ nace_r2 == "O", nace_r2 := "O84"]
dta[ nace_r2 == "P", nace_r2 := "P85"]
setdiff( nace2$nace_2, dta[ , nace_r2 ] )   # Should be empty
ind <- unique( nace2$nace_2 )
dta <- dta[ nace_r2 %in% ind ]



######################################
### Are missing values a big deal? ###
######################################

# Here is how many observations I should have...
length( ind ) * length( eu )

# ... I have fewer
nrow( dta )

# Countries in which at least one industry is missing
colSums( dta[ , table( nace_r2, geo ) ] )

# It is ALWAYS industry U (which I do not care about)
table( rowSums( dta[ , table( nace_r2, geo ) ] ) < length( eu ) )
which( rowSums( dta[ , table( nace_r2, geo ) ] ) < length( eu ) )


# A look at the missing data
dta[ , table( na_val ) ]

# Where are they?
na.table <- dta[ na_val == T , table( nace_r2, geo ) ]
( na.table <- na.table[ rowSums( na.table ) > 0, colSums( na.table ) > 0 ] )

# Mostly the usual small countries...
colSums( na.table )

# Ignore this issue for now



############
### Save ###
############

grossOutputA64 <- dta
save( grossOutputA64, file = "intmData/grossOutputA64.RData" )
write.csv( grossOutputA64, file = "intmData/grossOutputA64.csv", na = "", row.names = F )


# Aggregate EU 28
eu28 <- grossOutputA64[ , list( "go_a64" = sum( value, na.rm = T ) ), by = nace_r2 ]
write.csv( eu28, file = "intmData/go_A64_EU28.csv", na = "", row.names = F )
