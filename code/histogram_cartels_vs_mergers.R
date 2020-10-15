rm( list = ls( ) )

# Dependencies
require( "data.table" )
require( "ggplot2" )

# dataset -- (single Nace 2 assignment)
load( file = paste0( "outputData/compCases.RData"), verbose = T )
str( dta.cases )

# # , after_stat( count )
# ggplot( dta.cases, aes( mkt, colour = type ) ) +
#   geom_density() + 
#   scale_x_continuous(trans='log10')
# 

# billion Euro
dta.cases[ , mkt := mkt / 1000 ]

# Labels
dta.cases[ type == "cartel", type := "Cartels" ]
dta.cases[ type == "merger", type := "Mergers" ]

# Bandwith
bw <- 1

# Plot
p <- ggplot( dta.cases, aes( x = mkt, group = type, fill = type ) ) +
  geom_histogram( aes( y = ( stat( density ) * bw ) ), 
                  binwidth = bw,
                  boundary = 0 ) + facet_wrap( ~ type )
p <- p + scale_y_continuous( labels = scales::percent_format( accuracy = 1 ) )
p <- p + theme_bw()
p <- p + ylab( "Share of decisions" )
p <- p + xlab( "Turnover of the affected market (bln EUR)" )
p <- p + scale_x_continuous( breaks = ( 0:10 ) * 10 )
p <- p + theme( legend.position = "none" )
p <- p + theme( text = element_text( size = 30 ) )
p

# Save
w <- 1200    # width
r <- 1/2     # height to width
jpeg( file = "results/case_descriptives/histogram_cartels_vs_mergers.jpg", width = w, height = r*w, quality = 100 )
print( p )
dev.off()
