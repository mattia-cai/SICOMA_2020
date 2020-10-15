rm( list = ls() )


##################
### Input data ###
##################

# Dependencies
library( "data.table")
library( "ggplot2" )
library( "gridExtra" )


# Results of year-by-year analyses
# Results of the price analysis
load( file = paste0( "outputData/price_impacts_no_duration.RData" ) )
setkeyv( dta, c( "year", "nace2_2d_a64" ) )

# Shorter variable name...
setnames( dta, "nace2_2d_a64", "nace2" )
setkeyv( dta, c( "year", "nace2") )

# Short industry labels
labs0 <- read.csv( "inputData/SUTs/nace2_short_labels.csv", header = T )
nace2 <- as.character( labs0$nace_2 )
tmp <- nace2
tmp[ tmp %in% c( "L68A", "L68B" ) ] <- "L68"
labs0 <- as.character( labs0$short )
labs0 <- paste0( tmp, " - ", labs0 )
names( labs0 ) <- nace2

# Time span in dataset
yr.rng <- dta[ , sort( unique( year ) ) ]



###########################
### Selected industries ###
###########################

# Identify the industries where the impacts are most significant
fromSmallest <-function( t ){
  x <- dta[ year == t, total ]
  names( x ) <- dta[ year == t, nace2 ]
  x <- sort( x )
  return( x )
}
vlist <- lapply( yr.rng, fromSmallest )
names( vlist ) <- yr.rng
industries <- lapply( vlist, function( x ) names( x[  1 : 7 ] ) )
industries <- sort( unique( unlist( industries ) ) )

# Most impacted industries
dta <- dta[ nace2 %in% industries ]



##############################
### Format data for charts ###
##############################

# Reshape
dta <- dta[ , .( year, nace2, within, spill, total ) ]
dta <- melt( data = dta, id.vars = c( "year", "nace2", "total"), variable.name = "Effect", value.name = "dp" )

# Absolute value
dta[ , dp := abs( dp ) ]
dta[ , total := abs( total ) ]

# Range of y-axis values 
dta[, summary( total ) ]
rng <- c( 0, 10 )

# Industry labels
dta[ , ind := labs0[ dta[, nace2] ] ]

# Bar labels
dta[ , clab := as.character( round( total, 1 ) ) ]
dta[ clab == 0, clab := "<.1" ]
#dta[ , clab := gsub( "0.", ".", clab ) ]

# Formatting 
dta <- dta[ Effect != "total" ]
dta[ Effect == "within" , Effect := "Within" ]
dta[ Effect == "spill" , Effect := "Spillover" ]
dta[ , Effect := factor( Effect, levels = c(  "Spillover", "Within" ) ) ]
setkeyv( dta, c( "year", "nace2", "Effect" ) )



###########################
### Year by year charts ###
###########################

for ( t in yr.rng ){
  
  # Title
  tlt <- as.character( t )
  if( t == 9999 ){ tlt <- "Average (2012-2018)"}
  
  png( file = paste0( "results/industry_prices/prices_", t, "_no_duration.png"), width = 1200, height = 1000  )
  print(
    ggplot( data = dta[ year == t ] ) +
      geom_bar( aes( x = ind, y = dp, fill = Effect ),
                stat = "identity",
                position = "stack" ) + 
      theme_bw() + 
      coord_cartesian( ylim = rng ) + 
      theme( axis.text.x = element_text( angle = 90, hjust = 1, vjust = 0.5 ) ) +
      labs( y = "Price reduction (%)", x = "" ) +
      geom_text( aes( x = ind, y = total + .3 , label = clab ),
                 size = 8 ) +
      theme( text = element_text( size = 30 ),
             axis.text.x = element_text( angle = 90, hjust = 1, vjust = 0.5 ),
             plot.title = element_text( hjust = 0.5 ),
             legend.text=element_text( size = 30 ),
             legend.key = element_rect( size = 10 ) ) + 
      ggtitle( tlt )
    
  )
  dev.off()
}