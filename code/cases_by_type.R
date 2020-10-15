rm( list = ls( ) )

# Dependencies
require( "data.table" )
require( "ggplot2" )

# dataset -- (single Nace 2 assignment)
load( file = paste0( "outputData/compCases.RData"), verbose = T )

# Case count and overall size by year
dta.cases[ , count := 1 ]
vars <- c( "count", "mkt" )
dta <- dta.cases[ , lapply( .SD, sum ), by = .( year, type ), .SDcols = vars ]
dta[ type == "cartel", type := "Cartels" ]
dta[ type == "merger", type := "Mergers" ]
dta[ , mkt := mkt / 1000 ]

# Grand mean
max.year <- dta[ , max( year )]
dta1 <- dta[, .( count = mean( count ), mkt = mean( mkt ) ), by =.( type ) ]
dta1[, year := max.year + 1.5 ]
dta <- rbind( dta, dta1 )

# Reshape
dta <- melt( dta, id.vars = c( "year", "type" ) )
dta[ variable == "count", variable := "Number of decisions" ]
dta[ variable == "mkt", variable := "Overall affected market\n turnover (bln EUR)" ]

# Label for average
dta[, lab := as.character( year ) ]
dta[ year == max.year + 1.5, lab := "Avg." ]

# Label for totals
dta[ , tot := sum( value ), by = .( year, variable ) ]
dta[ , y_coord := tot + 7 ]
dta[ variable == "Number of decisions", y_coord := tot + 1.5 ]

# Chart
p <- ggplot( data = dta, aes( x = year, y = value, fill = type ) ) 
p <- p + geom_bar( stat = "identity" )
p <- p + scale_x_continuous( breaks = dta[, unique( year ) ], labels = dta[, unique( lab ) ] )
p <- p + facet_wrap( ~ variable, scales = "free_y")
p <- p + geom_text( aes( x = year, y = y_coord, 
                         label = round( tot, 1 ) ), size = 7 )
p <- p + theme_bw()
p <- p + theme( axis.title.x = element_blank(), axis.title.y = element_blank() )
p <- p + theme( axis.text.x = element_text( angle = 90, hjust = 1, vjust = 0.5 ) )
p <- p + theme( text = element_text( size = 30 ) )
p <- p + theme( legend.title = element_blank() )
p

# Save
w <- 1200    # width
r <- 1/2     # height to width
jpeg( file = "results/case_descriptives/cases_by_type.jpg",
     width = w, height = r*w, quality = 200 )
print( p )
dev.off()




