rm( list = ls( ) )

# Count 

# Dependencies
require( "data.table" )
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



###############
### Mergers ###
###############

dta1 <- dta[ type == "Mergers" ]
p <- ggplot( data = dta1, aes( x = as.factor( year ), y = mkt, fill = ind ) )
p <- p + geom_bar( stat = "identity", position = "stack" )
p <- p + labs( y = "Affected market size (Bln EUR)", x = "Year" )
p <- p + theme_bw()
p <- p + theme( text = element_text( size = 30 ) )
p <- p + theme( axis.text.x = element_text( angle = 90, hjust = 1, vjust = 0.5 ) )
p <- p + theme( legend.title = element_blank() )

# Save
w <- 800    # width
r <- 5.15/8     # height to width
png( file = "results/case_descriptives/mergers_by_industry.png", 
     width = w, height = r*w   )
print( p )
dev.off()



###############
### Cartels ###
###############

dta2 <- dta[ type == "Cartels" ]
p <- ggplot( data = dta2, aes( x = as.factor( year ), y = mkt, fill = ind ) )
p <- p + geom_bar( stat = "identity", position = "stack" )
p <- p + labs( y = "Affected market size (Bln EUR)", x = "Year" )
p <- p + theme_bw()
p <- p + theme( text = element_text( size = 30 ) )
p <- p + theme( axis.text.x = element_text( angle = 90, hjust = 1, vjust = 0.5 ) )
p <- p + theme( legend.title = element_blank() )

# Save
png( file = "results/case_descriptives/cartels_by_industry.png", 
     width = w, height = r*w   )
print( p )
dev.off()



############
### Both ###
############

p <- ggplot( data = dta, aes( x = as.factor( year ), y = mkt, fill = ind ) )
p <- p + geom_bar( stat = "identity", position = "stack" )
p <- p + facet_wrap( ~ type ) 
p <- p + labs( y = "Affected market size (Bln EUR)", x = "Year" )
p <- p + theme_bw()
p <- p + theme( text = element_text( size = 30 ) )
p <- p + theme( axis.text.x = element_text( angle = 90, hjust = 1, vjust = 0.5 ) )
p <- p + theme( legend.title = element_blank() )

# Save
w <- 1000        # width
r <- 5/8     # height to width
png( file = "results/case_descriptives/cases_by_industry.png", 
     width = w, height = r*w   )
print( p )
dev.off()

