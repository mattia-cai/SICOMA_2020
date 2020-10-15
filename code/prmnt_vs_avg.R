# Most recent year

rm( list = ls( ) )

# Dependencies
require( "data.table" )
require( "ggplot2" )


##################
### Input data ###
##################

# QUEST-like permanent shock
load( file = "outputData/price_impacts_permanent.RData" )
dta1 <- dta
dta1[, lab := "Permanent (QUEST)" ]

# Results - with duration (average over period)
load( file = "outputData/price_impacts_duration.RData" )
dta2 <- dta[ , lapply( .SD, mean ), .SDcols = c( "within", "spill", "total" ), by = .( nace2_2d_a64 ) ]
dta2[ , lab := "Average (with duration)" ]





######################
### Rearrange data ###
######################

# Stack
dta <- rbind( dta1, dta2, fill = T )
dta[ , lab := factor( lab, levels = c( "Average (with duration)", "Permanent (QUEST)" ) )]

# Simplify a bit
dta[ , c( "case.count", "mktT", "dp", "go2_a64", "w", "w_flag" ) := NULL ]
setnames( dta, "nace2_2d_a64", "nace2" )
setkeyv( dta, c( "lab", "nace2" ) )

# Only plot within and total
dta[ , spill := NULL ]

# Reshape to long
dta <- melt( dta,  i = c( "nace2", "lab" ) )

# Order
dta[ variable == "total", variable := "Total"]
dta[ variable == "within", variable := "Within"]
dta[ , variable := factor( variable, levels = c( "Within", "Total" ) ) ]

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

# Identify the industries with the highest bars 
dta[, rnk := rank( value ), by = .( lab, variable ) ]
ind2plot <- dta[ rnk <= 10, sort( unique( nace2 ) ) ]

# Reconsider next year!
ind2plot <- setdiff( ind2plot, c( "C10-C12", "C31_C32" ) )


############
### Plot ###
############

# Value labels
dta[ , vlab := round( abs( value ), 1 )]

# Plot
p <- ggplot( data = dta[ nace2 %in% ind2plot ], aes( x = ind, y = abs( value ), fill = lab  ) )
p <- p + geom_bar( stat = "identity", position = "dodge" )
p <- p + geom_text( aes( label = vlab ),
                    position = position_dodge( width = 0.9 ),
                    vjust=-0.25, size = 6 )
p <- p + facet_wrap( ~ variable )
p <- p + theme_bw()
p <- p + theme( text = element_text( size = 30 ) )
p <- p + theme( axis.title.x = element_blank() )
p <- p + ylab( "Price reduction (%)" )
p <- p + theme( legend.title = element_blank() )
p <- p + theme( axis.text.x = element_text( angle = 90, hjust = 1, vjust = 0.5 ) )
w <- 1600    # width
r <- 1/2     # height to width
fname <- "results/industry_prices/prmnt_vs_avg.jpg"
jpeg( file = fname, width = w, height = r*w, quality = 200 )
print( p )
dev.off()
