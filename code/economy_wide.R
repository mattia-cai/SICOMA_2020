rm( list = ls( ) )

# Dependencies
require( "data.table" )
require( "ggplot2" )

# Without duration
load( file = "outputData/price_impacts_no_duration.RData" )
dta1 <- dta

# With duration
load( file = "outputData/price_impacts_duration.RData" )
dta2 <- dta

# Summary table
tab1 <- dta1[ , .( within = weighted.mean( within, go2_a64 ),
                   spill = weighted.mean( spill, go2_a64 ),
                   total = weighted.mean( total, go2_a64 ) ), by = year ]
max.year <- dta[ , max( year )]
tmp <- tab1[ , lapply( .SD, mean ), .SDcols = c( "within", "spill", "total" ) ]
tmp[, year := max.year + 1.5 ]
tab1 <- rbind( tab1, tmp )


tab2 <- dta2[ , .( within = weighted.mean( within, go2_a64 ),
                   spill = weighted.mean( spill, go2_a64 ),
                   total = weighted.mean( total, go2_a64 ) ), by = year ]
tmp <- tab2[ , lapply( .SD, mean ), .SDcols = c( "within", "spill", "total" ) ]
tmp[, year := max.year + 1.5 ]
tab2 <- rbind( tab2, tmp )
tab <- merge( tab1, tab2, by = "year" )
names( tab ) <- gsub( pattern = ".x", replacement = ".no_dur", names( tab ) )
names( tab ) <- gsub( pattern = ".y", replacement = ".dur", names( tab ) )
tab[, lab := as.character( round( year ) ) ]
tab[ year > max.year, lab := "Avg." ]
write.csv( tab, file = "results/price_impacts/economy_wide.csv", row.names = F )

# Plot
tab1 <- melt( tab1, id.vars = "year" )
tab1[, d := "Without duration" ]
tab2 <- melt( tab2, id.vars = "year" )
tab2[, d := "With duration" ]
dta <- rbind( tab1, tab2 )
dta[ , d := factor( d, levels = c( "Without duration", "With duration" ) ) ]
dta <- dta[ variable != "total" ]
dta[ variable == "spill", variable := "Spillover"]
dta[ variable == "within", variable := "Within"]
dta[ , lab := round( abs( value ), 2 )]
dta[ , variable := factor( variable, levels = c( "Spillover", "Within" ) ) ]
p <- ggplot( data = dta, aes( x = year, y = abs( value ), fill = variable, label = lab ) ) 
p <- p + geom_bar( stat = "identity", position = "stack" )
p <- p + facet_wrap( ~ d )
p <- p + labs( y = "Percent", x = "Year" )
p <- p + theme_bw()
p <- p + theme( text = element_text( size = 30 ) )
p <- p + theme( axis.title.x = element_blank() )
p <- p + theme( axis.text.x = element_text( angle = 90, vjust = 0.5 ) )
p <- p + theme( legend.title = element_blank() )
p <- p + geom_text( size = 6, position = position_stack( vjust = 0.5 ) )
p <- p + scale_x_continuous( breaks = tab[, unique( year ) ], labels = tab[, unique( lab ) ] )
w <- 1200    # width
r <- 1/2     # height to width
jpeg( file = "results/price_impacts/economy_wide.jpg", 
     width = w, height = r*w, quality = 200 )
print( p )
dev.off()

# Spillover ratios
tab[ , spill2within.no_dur := spill.no_dur / within.no_dur]
tab[ , spill2within.dur := spill.dur / within.dur]
tab[ , spill2total.no_dur := spill.no_dur / total.no_dur]
tab[ , spill2total.dur := spill.dur / total.dur]
tab3 <- tab[ year <= max.year, ]
tab3 <- tab3[, c( 1, 9:12 )  ]
tmp <- tab3[, lapply( .SD, mean ), .SDcols = setdiff( names( tab3 ), "year" ) ]
tmp[, year := "Avg." ]
tab3 <- rbind( tab3, tmp )
write.csv( tab3, file = "results/price_impacts/spillover_ratios.csv", row.names = F )
