rm( list = ls( ) )

# Dependencies
require( "data.table" )
require( "ggplot2" )

# dataset -- (single Nace 2 assignment)
load( file = paste0( "outputData/compCases.RData"), verbose = T )
str( dta.cases )

# 5 cases in which mkt is larger than GO4
( funny <- dta.cases[ mkt > go4_a64, .( case_id, year, type, nace2_4d, mkt, go4_a64, go4_a24 ) ]  )

# mkt/GO4
dta.cases[ , mkt2go4 := mkt / go4_a64 ]

# Histogram
dta <- dta.cases[ mkt2go4 <= 1 ]

# Mergers
p <- ggplot( dta[ type == "merger" ], aes( x = mkt2go4 ) )
p <- p + geom_histogram( aes( y = ..count../sum(..count..) * 100 ),
                         breaks = ( 0 : 10 / 10 ), color = "white" )
p <- p + theme_bw()
p <- p + theme( text = element_text( size = 30 ) )
p <- p + xlab( "mkt/GO4" )
p <- p + ylab( "Percent of cases" )
w <- 800    # width
r <- 5/8     # height to width
png( file = "results/case_descriptives/histogram_mergers.png", 
     width = w, height = r*w   )
print( p )
dev.off()

# Cartels
p <- ggplot( dta[ type == "cartel" ], aes( x = mkt2go4 ) )
p <- p + geom_histogram( aes( y = ..count../sum(..count..) * 100 ),
                         breaks = ( 0 : 10 / 10 ), color = "white" )
p <- p + theme_bw()
p <- p + theme( text = element_text( size = 30 ) )
p <- p + xlab( "mkt/GO4" )
p <- p + ylab( "Percent of cases" )
png( file = "results/case_descriptives/histogram_cartels.png", 
     width = w, height = r*w   )
print( p )
dev.off()
