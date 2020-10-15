rm( list = ls( ) )

# Dependencies
require( "data.table" )
require( "ggplot2" )
require( "scales" )

# dataset -- (single Nace 2 assignment)
load( file = paste0( "outputData/compCases.RData"), verbose = T )
str( dta.cases )

# 5 cases in which mkt is larger than GO4
( funny <- dta.cases[ mkt > go4_a64, .( case_id, year, type, nace2_4d, mkt, go4_a64, go4_a24 ) ]  )

# mkt/GO4
dta.cases[ , mkt2go4 := ( mkt / go4_a64 ) ]

# Multiplier mktT/mkt
dta.cases[ , mult := mktT / mkt ]

# Multiplier mktT/mkt
dta.cases[ , mkt := mkt / 1000 ]

# Labels
dta.cases[ type == "cartel", type := "Cartels" ]
dta.cases[ type == "merger", type := "Mergers" ]

# Relevance of the decision
p <- ggplot( data = dta.cases[ mkt2go4 <= 1 ] ) + 
  geom_point( aes( x = mkt, y = mult, color = type ), size = 4 )
p <- p + scale_x_continuous( trans = log10_trans(),
                             breaks = trans_breaks( "log10", function( x ) 10^x ),
                             labels = trans_format( "log10", math_format( 10^.x ) ) )
p <- p + theme_bw()
p <- p + labs( x = "Market turnover (bln EUR)", y = "Deterrence multiplier" )
p <- p + theme( legend.title = element_blank() )
p <- p + theme( text = element_text( size = 30 ) )
p

# Save
w <- 800    # width
r <- 2/3     # height to width
jpeg( file = "results/case_descriptives/deterrence_multipliers.jpg", width = w, height = r*w, quality = 200 )
print( p )
dev.off()




