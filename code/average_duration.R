rm( list = ls( ) )

# Dependencies
require( "data.table" )
require( "ggplot2" )

# dataset -- (single Nace 2 assignment)
load( file = paste0( "outputData/compCases.RData"), verbose = T )
str( dta.cases )


# Average case size and duration
dta.cases[ , count := 1 ]
vars <- c( "count", "duration" )
dta <- dta.cases[ , lapply( .SD, sum ), by = .( year, type ), .SDcols = vars ]
dta[ , dur := duration/count ]        # average duration
dta[ , duration := NULL ]

# Grand average
dta <- as.data.table( dta )
max.year <- dta[ , max( year )]
dta1 <- dta[, .( count = sum( count ), dur = weighted.mean( dur, count ) ), by = type ]
dta1[, year := max.year + 1.5 ]
dta <- rbind( dta, dta1 )
dta[, lab := as.character( year ) ]
dta[ year == max.year + 1.5, lab := "Avg." ]


# Overall
tab <- copy( dta )
tab[ , year := NULL ]
tab <- as.data.frame( tab )
tab <- reshape( tab, idvar='lab', timevar='type', direction='wide')
tab <- as.data.table( tab )
tab[, count.total := count.merger + count.cartel ]
tab[ , dur.total := ( count.merger * dur.merger + 
                        count.cartel * dur.cartel ) / count.total ]
write.csv( tab, "results/case_descriptives/average_duration.csv", row.names = F )


# Plot
dta[ type == "cartel", type := "Cartels" ]
dta[ type == "merger", type := "Mergers" ]
p <- ggplot( data = dta, aes( x = year, y = dur, fill = type ) )
p <- p + geom_bar( stat = "identity", position = "dodge" )
p <- p + geom_text( aes( label = round( dur, 1 ) ),
                    position = position_dodge( width=0.9 ),
                    vjust = -0.5, size = 8 )
p <- p + labs( y = "Average case duration (years)" )
p <- p + theme_bw()
p <- p + theme( text = element_text( size = 30 ) )
p <- p + theme( axis.title.x = element_blank() )
p <- p + theme( axis.text.x = element_text( angle = 90, hjust = 1, vjust = 0.5 ) )
p <- p + scale_x_continuous( breaks = dta[, unique( year ) ], labels = dta[, unique( lab ) ] )
p <- p + theme( legend.title = element_blank() )
p

# Save
w <- 1200        # width
r <- 1/2     # height to width
jpeg( file = "results/case_descriptives/average_duration.jpg",
      width = w, height = r*w, quality = 200 )
print( p )
dev.off()
