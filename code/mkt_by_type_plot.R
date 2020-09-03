rm( list = ls( ) )

# Dependencies
require( "data.table" )
require( "ggplot2" )

# dataset -- (single Nace 2 assignment)
load( file = paste0( "outputData/compCases.RData"), verbose = T )

# Aggregate by type
dta <- dta.cases[ , .( "mkt" = sum( mkt ) ), by = .( year, type ) ]
dta[ type == "cartel", type := "Cartels" ]
dta[ type == "merger", type := "Mergers" ]

# Chart
p <- ggplot( data = dta, aes( x = as.factor( year ), y = mkt / 1000, fill = type ) ) 
p <- p + geom_bar( stat = "identity", position = "stack" )
p <- p + labs( y = "Affected market value (Bln EUR)", x = "Year" )
p <- p + theme_bw()
p <- p + theme( text = element_text( size = 30 ) )
p <- p + theme( legend.title = element_blank() )


# Save
w <- 800    # width
r <- 5/8     # height to width
png( file = "results/case_descriptives/mkt_by_type.png", 
     width = w, height = r*w   )
print( p )
dev.off()


