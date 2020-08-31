# Gross output data: compare different sources
# author: mattia cai

rm( list = ls( ) )

# Industry-level gross output data can be taken from from:
# - Ecfin file
# = IO table
# - National accoutns (Eurostat, nama_10_a64)
# EU28 would have to be aggregated from country data and there might be some missing data problems due to confidentiality


# Dependencies 
require( "readxl" )
require( "data.table" )
require( "ggplot2" )



##################
### Input data ###
##################

# Ecfin dataset
load( file = "intmData/EcfinMupGo.RData" )
str( dta.mup )

# EU28 supply and use system from 2014 - GO2 data for comparison
load( "intmData/IO_EU28_2014.RData" )
g <- colSums( Vt )  # GO2 industry output

# Gross output from the national accounts (Eurostat)
load( file = "intmData/grossOutputA64.RData" )

# Matching NACE codes
nace2 <- read.csv( "inputData/SUTs/nace_2.csv", header = T, stringsAsFactors = F )



#################################
### Compare with GO2 from IOT ###
#################################

# Get the parent classification in
dta.go <- unique( nace2[ , c( "nace_2", "X1809_ecfin_data" ) ] )
colnames( dta.go ) <- c( "nace2.iot", "nace2.ecfin" )
dta.go[ , c( "go.ecfin", "mup.ecfin" ) ] <- dta.mup[ match( dta.go[ , "nace2.ecfin" ], dta.mup[ , "nace2" ] ), c( "go_eu_2015", "mup_wr_eu" )]
dta.go$go.iot <- g[ dta.go$nace2.iot ]
dta.go <- as.data.table( dta.go )
dta.go <- dta.go[ nace2.iot != "L68A", ]
dta.go[ nace2.iot == "L68B", nace2.iot := "L68" ]
dta.go <- dta.go[, .( nace2.iot, go.iot, nace2.ecfin, go.ecfin ) ]
dta.go[ is.na( nace2.ecfin ), nace2.ecfin := "" ]

# Aggregate
dta.go <- dta.go[ , list( go.iot = sum( go.iot ), go.ecfin = mean( go.ecfin ) ), by = nace2.ecfin ]



##############################################
### Compare with GO from National accounts ###
##############################################

# Aggregate the EU 28 keeping track of how many addends I have
grossOutputA64[ na_val == F, obs := 1 ]
eu28 <- grossOutputA64[ , list( "go" = sum( value, na.rm = T ), "obs" = sum( obs, na.rm = T ) ), by = nace_r2 ]
eu28[ obs < grossOutputA64[ , length( unique( geo ) ) ], any.missing := T ]

# Get parent classificaiton for comparison with the ECFIN data
index <- match( eu28[ , nace_r2 ], nace2$nace_2 )
eu28[ , nace2.ecfin := nace2[ index, "X1809_ecfin_data"] ]
eu28[ is.na( nace2.ecfin ), nace2.ecfin := "" ]
eu28 <- eu28[, list( "go.na" = sum( go ), "gapsInNA" = sum( any.missing, na.rm = T ) ), by = nace2.ecfin ]
eu28[, gapsInNA := gapsInNA > 0 ]

# sort
eu28 <- eu28[ match( eu28[, nace2.ecfin ], dta.go[, nace2.ecfin ] ), ]
all( eu28[, nace2.ecfin ] == dta.go[, nace2.ecfin ] )   # Should be T
dta.go <- cbind( dta.go, eu28[ , .( go.na, gapsInNA ) ] )


# F*** excel (avoid certain codes being converted into dates)
tmp <- copy( dta.go )
tmp[ nace2.ecfin  != "", nace2.ecfin := paste0( nace2.ecfin, "\t" ) ]
write.csv( tmp, file = "results/GrossOutputComparisons.csv", na = "", row.names = F, quote = F )
rm( tmp )




############################
### Differences in GOTOT ###
############################

# Is the difference in GOTOT between the B1 and the B5 DeltaMUP calculations
# only due to the different set of industries considered?
dta.go[ , group := "Common industries" ]
dta.go[ is.na( go.ecfin ), group := "Out of Ecfin scope (L, O, P, Q, T)" ]
bplot <- dta.go[ , list( IOT = sum( go.iot ), Ecfin = sum( go.ecfin ), NatAcc = sum( go.na ) ), by = group ]

# Ecfin vs. National Accounts
diff <- bplot[ , sum( NatAcc ) - sum( Ecfin, na.rm = T ) ]
diff / bplot[ , sum( NatAcc ) ]

# Significant part of the difference accounted for broader industry set
bplot[ group == "Out of Ecfin scope (L, O, P, Q, T)", NatAcc ] / diff

# Common industries: total from the National Accounts exceeds the total from Ecfin by 7%
bplot[ group == "Common industries", NatAcc / Ecfin - 1 ]

# Draw a barplot
setnames( bplot, c( "IOT", "NatAcc", "Ecfin" ), c( "IOT (2014)", "NA (2014)", "Ecfin (2015)" ) )
bplot <- melt( bplot, id.vars = "group" )
bplot[ , value := value * 1e-6 ]
bplot[ , group := factor( group, levels = c( "Out of Ecfin scope (L, O, P, Q, T)", "Common industries" ) ) ]
bplot[ , variable := factor( variable, levels = c( "NA (2014)", "IOT (2014)", "Ecfin (2015)" ) ) ]
bplot <- ggplot( data = bplot ) + geom_bar( aes( variable, weight = value, fill = group ) )
bplot <- bplot + theme_bw()
bplot <- bplot + theme( legend.title = element_blank() )
bplot <- bplot + ylab( "Gross output of the EU economy (1,000 bln EUR)" )
bplot <- bplot + xlab( "Source of industry-level data" )
bplot



#################################
### Industry-level comparison ###
#################################

dta.go[ , group := NULL ]
dta.go <- dta.go[ !is.na( go.ecfin ) ]
dta.go <- dta.go[ , c( "go.na", "go.ecfin" ) := lapply( .SD, function( x ) x * 1e-3 ), .SDcols = c( "go.na", "go.ecfin" ) ]
dta.go[ , plot( go.na, go.ecfin, pch = 16, xlab = "National accounts (2014)", ylab = "Ecfin (2015) - Sep 2018", xlim = c( 0, 2500 ), ylim = c( 0, 2500 ) ) ]
abline( 0, 1, col = "red ")
#dta.go[ , text( x =  go.iot, y = go.ecfin, labels = nace2.ecfin, pos = 3 ) ]










