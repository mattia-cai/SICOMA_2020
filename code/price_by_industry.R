# Most recent year

rm( list = ls( ) )

# Dependencies
require( "data.table" )
require( "ggplot2" )

# Results - Without duration
load( file = "outputData/price_impacts_no_duration.RData" )
dta1 <- dta
dta1[, lab := "Without duration" ]

# Results - with duration
load( file = "outputData/price_impacts_duration.RData" )
dta2 <- dta
dta2[, lab := "With duration" ]

# Stack
dta <- rbind( dta1, dta2 )
dta[ , lab := factor( lab, levels = c( "Without duration", "With duration" ) )]

# Simplify a bit
dta[ , c( "case.count", "mktT", "dp", "go2_a64", "w", "w_flag" ) := NULL ]
setnames( dta, "nace2_2d_a64", "nace2" )
setkeyv( dta, c( "year", "lab", "nace2" ) )

# Average over period
tmp <- dta[ , lapply( .SD, mean ), .SDcols = c( "within", "spill", "total" ), by = .( nace2, lab ) ]
tmp[ , year := 9999 ]
dta <- rbind( dta, tmp )

# Reshape to long
dta <- melt( dta,  i = c( "year", "nace2", "total", "lab" ) )

# Order
dta[ variable == "spill", variable := "Spillover"]
dta[ variable == "within", variable := "Within"]
dta[ , variable := factor( variable, levels = c( "Spillover", "Within" ) ) ]

# Short industry labels
labs0 <- read.csv( "inputData/SUTs/nace2_short_labels.csv", header = T )
nace2 <- as.character( labs0$nace_2 )
tmp <- nace2
tmp[ tmp %in% c( "L68A", "L68B" ) ] <- "L68"
labs0 <- as.character( labs0$short )
labs0 <- paste0( labs0, " (", tmp, ")" )
names( labs0 ) <- nace2
dta[ , ind := labs0[ dta[, nace2] ] ]
dta[ , ind := factor( ind, levels = unique( ind ) ) ]

# Loop over years 
for ( yr in dta[ , unique( year ) ] ){
  
  # Plot the relevant year
  dta1 <- copy( dta[ year == yr ] )
  
  # Identify the industries with the highest bars 
  dta1[, rnk := rank( total ), by = .( lab, variable ) ]
  ind2plot <- dta1[ rnk <= 10, sort( unique( nace2 ) ) ]
  
  # Bar labels
  dta1[ , totlab := round( abs( total ), 1 )]
  dta1[ , totpos := abs( total ) + .03 * max( abs( total ) ) ]
  
  # Plot
  p <- ggplot( data = dta1[ nace2 %in% ind2plot ], aes( x = ind, y = abs( value ), fill = variable  ) )
  p <- p + geom_bar( stat = "identity", position = "stack" )
  p <- p + geom_text( aes( x = ind, y = totpos, label = totlab ), size = 6 )
  p <- p + facet_wrap( ~ lab )
  p <- p + theme_bw()
  p <- p + theme( text = element_text( size = 30 ) )
  p <- p + theme( axis.title.x = element_blank() )
  p <- p + ylab( "Price reduction (%)" )
  p <- p + theme( legend.title = element_blank() )
  p <- p + theme( axis.text.x = element_text( angle = 90, hjust = 1, vjust = 0.5 ) )
  w <- 1400    # width
  r <- 2/3     # height to width
  fname <- paste0( "results/industry_prices/yr_",yr, ".jpg" )
  jpeg( file = fname, width = w, height = r*w, quality = 200 )
  print( p )
  dev.off()
}



