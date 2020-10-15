rm( list = ls( ) )

# Count 

# Dependencies
require( "ggplot2" )
require( "data.table" )

# dataset -- (single Nace 2 assignment)
load( file = "outputData/compCases.RData", verbose = T )


# Aggregate industries
dta.cases[ , l := substr( nace2_2d_a64, 1, 1 ) ]
dta.cases[ l == "C", ind := "Manufacturing (C)" ]
dta.cases[ l %in% c( "D", "E"), ind := "Utilities (D-E)" ]
dta.cases[ l == "G", ind := "Trade (G)" ]
dta.cases[ l == "H", ind := "Transport (H)" ]
dta.cases[ l == "J", ind := "Communication (J)" ]
dta.cases[ l == "K", ind := "Finance (K)" ]
dta.cases[ is.na( ind ), table( l ) ]
dta.cases[ is.na( ind ), ind := "Other" ]

# Factor order
lab <- dta.cases[ , levels( as.factor( ind ) ) ]
index <- order( substr( lab, nchar( lab ) - 1, nchar( lab ) - 1 ) )
lab <- c( setdiff( lab[ index ], "Other" ), "Other" )
dta.cases[ ,ind := factor( ind, levels = lab ) ]

# Add up 
dta <- dta.cases[ , .( "mkt" = sum( mkt ) ), by = .( year, type, ind ) ]

# Some formatting
dta[ type == "cartel", type := "Cartels" ]
dta[ type == "merger", type := "Mergers" ]
dta[ , mkt := mkt / 1000 ]

# Averages over time
max.year <- dta[ , max( year )]
yrs <- dta[, length( unique( year ) ) ]
dta1 <- dta[ , .( mkt = sum( mkt ) ), by = .( type, ind )  ]
dta1[ , mkt := mkt / yrs ]
dta1[, year := max.year + 1.5 ]
dta <- rbind( dta, dta1 )
dta[, lab := as.character( year ) ]
dta[ year == max.year + 1.5, lab := "Avg." ]



############
### Both ###
############

dta[ , tot := sum( mkt ), by = .( type, year ) ]
p <- ggplot( data = dta, aes( x = year, y = mkt, fill = ind ) )
p <- p + geom_bar( stat = "identity", position = "stack" )
p <- p + facet_wrap( ~ type ) 
p <- p + labs( y = "Overall market turnover (bln EUR)" )
p <- p + geom_text( aes( x = year, y = tot, 
                         label = round( tot, 1 ) ),
                      nudge_y = 5, size = 7 )
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
jpeg( file = "results/case_descriptives/cases_by_industry.jpg",
     width = w, height = r*w, quality = 200 )
print( p )
dev.off()

